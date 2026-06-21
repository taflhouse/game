module Main (main) where

import Control.Concurrent.STM
import Control.Exception (catch, SomeException)
import Control.Monad (void)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv, getEnv)
import Database.PostgreSQL.Simple (connectPostgreSQL)

import Tafl.Types (Side(..), GameResult(..), gsTurn)
import Tafl.Rules (slugToVariant)
import Tafl.Protocol

import Server.Room
import Server.Auth (validateJWT)
import Server.DB (getProfile, updateGameResult, Profile(..))

main :: IO ()
main = do
  port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
  dbUrl <- getEnv "DATABASE_URL"
  jwtSecret <- getEnv "SUPABASE_JWT_SECRET"
  conn <- connectPostgreSQL (BS.pack dbUrl)
  roomsVar <- newTVarIO Map.empty
  let env = Env conn (T.pack jwtSecret) roomsVar
  TIO.putStrLn $ "Server starting on port " <> T.pack (show port)
  Warp.run port $ websocketsOr WS.defaultConnectionOptions (wsApp env) httpApp

-- | Simple HTTP health check.
httpApp :: Wai.Application
httpApp _req respond =
  respond $ Wai.responseLBS status200
    [("Content-Type", "text/plain")]
    "taflhouse server ok"

-- | Accept WebSocket connections.
wsApp :: Env -> WS.PendingConnection -> IO ()
wsApp env pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) (handleConnection env conn)

-- | Handle a single WebSocket connection lifecycle.
handleConnection :: Env -> WS.Connection -> IO ()
handleConnection env conn = do
  -- Step 1: Wait for CmAuth
  msgBytes <- WS.receiveData conn
  case decode (msgBytes :: LBS.ByteString) of
    Just (CmAuth token gameId) -> do
      authResult <- validateJWT (BS.pack (T.unpack (envJwtSecret env))) token
      case authResult of
        Left err -> sendErr conn ("Auth failed: " <> err)
        Right userId -> do
          mProfile <- getProfile (envConn env) userId
          case mProfile of
            Nothing -> sendErr conn "No profile found. Please create a username first."
            Just profile -> handleAuthenticated env conn userId (profileUsername profile) gameId
    _ -> sendErr conn "Expected auth message"

-- | Handle an authenticated connection — wait for create/join, then game loop.
handleAuthenticated :: Env -> WS.Connection -> Text -> Text -> Text -> IO ()
handleAuthenticated env conn userId username _initialGameId = do
  -- Step 2: Wait for CmCreateGame or CmJoinGame
  msgBytes <- WS.receiveData conn
  case decode (msgBytes :: LBS.ByteString) of
    Just (CmCreateGame variantStr inviteCode) ->
      case slugToVariant variantStr of
        Nothing -> sendErr conn ("Unknown variant: " <> variantStr)
        Just variant -> do
          let pc = PlayerConn userId username conn
          -- Use the invite code as a deterministic game ID prefix
          let gameId = inviteCode <> "-" <> T.take 8 userId
          room <- createRoom env gameId variant inviteCode pc "defender"
          sendToPlayer pc SmWaitingForOpponent
          TIO.putStrLn $ username <> " created game " <> gameId
          gameLoop env room pc DefenderSide

    Just (CmJoinGame inviteCode) -> do
      rooms <- readTVarIO (envRooms env)
      case findRoomByInvite inviteCode rooms of
        Nothing -> sendErr conn ("No game found with invite code: " <> inviteCode)
        Just room
          | grStatus room /= Waiting -> sendErr conn "Game already started or finished"
          | otherwise -> do
              let pc = PlayerConn userId username conn
              room' <- joinRoom env room pc
              TIO.putStrLn $ username <> " joined game " <> grGameId room'
              let joinerSide = case grAttacker room of
                    Nothing -> AttackerSide
                    Just _  -> DefenderSide
              gameLoop env room' pc joinerSide

    _ -> sendErr conn "Expected create_game or join_game"

-- | Main game loop: process moves, resign, draw offers.
gameLoop :: Env -> GameRoom -> PlayerConn -> Side -> IO ()
gameLoop env room0 pc side = do
  let loop room = do
        msgBytes <- WS.receiveData (pcConn pc)
        case decode (msgBytes :: LBS.ByteString) of
          Just (CmMove move) -> do
            result <- applyMove env room move side
            case result of
              Left err -> do
                sendToPlayer pc (SmMoveRejected err)
                loop room
              Right room'
                | grStatus room' == Finished -> pure ()
                | otherwise -> loop room'

          Just CmResign -> do
            void $ handleResign env room side
            pure ()

          Just CmOfferDraw -> do
            let room' = room { grDrawOffer = Just side }
            atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
            -- Send to opponent
            let opponent = case side of
                  AttackerSide -> grDefender room
                  DefenderSide -> grAttacker room
            mapM_ (\opp -> sendToPlayer opp SmDrawOffered) opponent
            loop room'

          Just CmAcceptDraw -> do
            case grDrawOffer room of
              Just offerSide | offerSide /= side -> do
                let result = GameResult True Nothing "Draw by agreement"
                    room' = room { grStatus = Finished }
                broadcastRoom room' (SmGameOver result)
                updateGameResult (envConn env) (grGameId room) "Draw by agreement" Nothing (gsTurn (grGameState room))
                atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
              _ -> loop room

          Just CmDeclineDraw -> do
            let room' = room { grDrawOffer = Nothing }
            atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
            let opponent = case side of
                  AttackerSide -> grDefender room
                  DefenderSide -> grAttacker room
            mapM_ (\opp -> sendToPlayer opp SmDrawDeclined) opponent
            loop room'

          _ -> loop room
  loop room0
    `catch` \(_ :: SomeException) -> do
      -- On disconnect, notify opponent
      rooms <- readTVarIO (envRooms env)
      case Map.lookup (grGameId room0) rooms of
        Just room | grStatus room /= Finished -> do
          let opponent = case side of
                AttackerSide -> grDefender room
                DefenderSide -> grAttacker room
          mapM_ (\opp -> sendToPlayer opp SmOpponentDisconnected) opponent
        _ -> pure ()

-- | Find a room by invite code.
findRoomByInvite :: Text -> Map Text GameRoom -> Maybe GameRoom
findRoomByInvite code rooms =
  case filter (\r -> grInviteCode r == code) (Map.elems rooms) of
    (r:_) -> Just r
    []    -> Nothing

-- | Send an error message and log it.
sendErr :: WS.Connection -> Text -> IO ()
sendErr conn msg = do
  TIO.putStrLn $ "Error: " <> msg
  WS.sendTextData conn (encode (SmError msg))
