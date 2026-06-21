module Server.Room
  ( GameRoom(..)
  , PlayerConn(..)
  , RoomStatus(..)
  , Env(..)
  , createRoom
  , joinRoom
  , applyMove
  , handleResign
  , sendToPlayer
  , broadcastRoom
  ) where

import Control.Concurrent.STM
import Data.Aeson (encode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Database.PostgreSQL.Simple (Connection)

import Tafl.Types
import Tafl.Rules (BoardVariant, variantSlug)
import Tafl.Game (initialState, act)
import Tafl.Move (isActionPossible)
import Tafl.Protocol

import Server.Auth (JWKStore)
import Server.DB (createGameRecord, updateGameResult, joinGameRecord)

-- | Server environment shared across handlers.
data Env = Env
  { envConn      :: !Connection
  , envJwkStore  :: !JWKStore
  , envRooms     :: TVar (Map Text GameRoom)
  }

-- | A connected player.
data PlayerConn = PlayerConn
  { pcUserId   :: !Text
  , pcUsername  :: !Text
  , pcConn     :: !WS.Connection
  }

-- | Room lifecycle status.
data RoomStatus = Waiting | Active | Finished
  deriving (Eq, Show)

-- | A game room holding the live game state and player connections.
data GameRoom = GameRoom
  { grGameId     :: !Text
  , grVariant    :: !BoardVariant
  , grGameState  :: !GameState
  , grAttacker   :: Maybe PlayerConn
  , grDefender   :: Maybe PlayerConn
  , grMoveList   :: [MoveAction]
  , grInviteCode :: !Text
  , grStatus     :: !RoomStatus
  , grDrawOffer  :: Maybe Side
  }

-- | Create a new game room. The creator joins on their chosen side.
createRoom :: Env -> Text -> BoardVariant -> Text -> PlayerConn -> Text -> IO GameRoom
createRoom env gameId variant inviteCode player side = do
  let gs = initialState variant
      (atk, def) = case side of
        "attacker" -> (Just player, Nothing)
        _          -> (Nothing, Just player)
      room = GameRoom
        { grGameId     = gameId
        , grVariant    = variant
        , grGameState  = gs
        , grAttacker   = atk
        , grDefender   = def
        , grMoveList   = []
        , grInviteCode = inviteCode
        , grStatus     = Waiting
        , grDrawOffer  = Nothing
        }
  createGameRecord (envConn env) gameId (variantSlug variant) inviteCode (pcUserId player) side
  atomically $ modifyTVar' (envRooms env) (Map.insert gameId room)
  pure room

-- | Join an existing room as the missing side.
joinRoom :: Env -> GameRoom -> PlayerConn -> IO GameRoom
joinRoom env room player = do
  let (room', creatorSide, joinerSide) = case grAttacker room of
        Nothing -> (room { grAttacker = Just player, grStatus = Active }, DefenderSide, AttackerSide)
        Just _  -> (room { grDefender = Just player, grStatus = Active }, AttackerSide, DefenderSide)
      joinerSideStr = sideToText joinerSide
  joinGameRecord (envConn env) (grGameId room) (pcUserId player) joinerSideStr
  atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
  -- Notify the creator
  let creatorConn = case creatorSide of
        AttackerSide -> grAttacker room
        DefenderSide -> grDefender room
  case creatorConn of
    Just creator -> sendToPlayer creator (SmGameStarted (grGameId room') (pcUsername player) creatorSide)
    Nothing -> pure ()
  -- Notify the joiner
  let creatorName = case creatorSide of
        AttackerSide -> maybe "" pcUsername (grAttacker room)
        DefenderSide -> maybe "" pcUsername (grDefender room)
  sendToPlayer player (SmGameStarted (grGameId room') creatorName joinerSide)
  pure room'

-- | Apply a move in the room, validating it with the game engine.
applyMove :: Env -> GameRoom -> MoveAction -> Side -> IO (Either Text GameRoom)
applyMove env room move playerSide = do
  let gs = grGameState room
      currentSide = turnSide gs
  if currentSide /= playerSide
    then pure (Left "Not your turn")
    else if not (isActionPossible gs move)
    then pure (Left "Invalid move")
    else do
      let gs' = act gs move
          nextSide = turnSide gs'
          caps = gsCaptures gs'
          room' = room
            { grGameState = gs'
            , grMoveList  = grMoveList room ++ [move]
            , grDrawOffer = Nothing
            }
      broadcastRoom room' (SmMoveMade move caps nextSide)
      let result = gsResult gs'
      if finished result
        then do
          let room'' = room' { grStatus = Finished }
              winnerStr = fmap sideToText (winner result)
          broadcastRoom room'' (SmGameOver result)
          updateGameResult (envConn env) (grGameId room)
            (desc result) winnerStr (gsTurn gs')
          atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room'')
          pure (Right room'')
        else do
          atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
          pure (Right room')

-- | Handle a player resigning.
handleResign :: Env -> GameRoom -> Side -> IO GameRoom
handleResign env room side = do
  let winnerSide = case side of
        AttackerSide -> DefenderSide
        DefenderSide -> AttackerSide
      result = GameResult True (Just winnerSide) "Resignation"
      room' = room { grStatus = Finished }
  broadcastRoom room' (SmResigned side)
  broadcastRoom room' (SmGameOver result)
  updateGameResult (envConn env) (grGameId room) "Resignation"
    (Just (sideToText winnerSide)) (gsTurn (grGameState room))
  atomically $ modifyTVar' (envRooms env) (Map.insert (grGameId room) room')
  pure room'

-- | Send a server message to a single player.
sendToPlayer :: PlayerConn -> ServerMsg -> IO ()
sendToPlayer pc msg =
  WS.sendTextData (pcConn pc) (encode msg)

-- | Broadcast a server message to all connected players in a room.
broadcastRoom :: GameRoom -> ServerMsg -> IO ()
broadcastRoom room msg = do
  let encoded = encode msg
  mapM_ (\pc -> WS.sendTextData (pcConn pc) encoded) (grAttacker room)
  mapM_ (\pc -> WS.sendTextData (pcConn pc) encoded) (grDefender room)

sideToText :: Side -> Text
sideToText AttackerSide = "attacker"
sideToText DefenderSide = "defender"
