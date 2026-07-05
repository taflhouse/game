{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module App.Game.Update (updateGame) where

import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Data.Maybe (fromMaybe, isJust, isNothing)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=), (.:), parseMaybe, withObject)
import qualified Miso.JSON as JSON
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, asyncCallback, asyncCallback1, asyncCallback2, Function(..))
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq)
import qualified Data.Map.Strict as Map
import Supabase.Miso.Realtime (Channel(..), subscribeToTable, subscribeToTableWithPresence, trackPresence, removeChannel)
import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import Tafl.Board
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Game (act, initialState)
import Tafl.Game.State
import Tafl.Game.Move (getPossibleMovesFrom)
import Tafl.AI (AiConfig(..), bestMove, evaluate)

import App.JSON (GameRow(..), Profile(..), ChatMessage(..), parseChatMessage)
import App.Model (Model, GameMode(..), TimeControl(..), ViewMode(..), GameInitData(..))
import App.Game.Model
import App.Game.Action
import App.Route (replayMoves, lookupVariant, playURI, configureURI)
import App.FFI
import Supabase.Miso.Core (successCallback, errorCallback)

updateGame :: GameRefs -> GameAction -> Effect Model GameProps GameModel GameAction
updateGame GameRefs{..} = \case
  GNoOp -> pure ()

  GPoofsDone ->
    modify $ \gm -> gm { gmCapturePoofs = [] }

  GameMount -> do
    props <- getProps
    case gpInitData props of
      NewLocalGame uuid variant mode aiSide aiDepth aiNodeLimit -> do
        let gs = initialState variant
        put $ initialGameModel
          { gmGameId = Just uuid
          , gmGameState = gs
          , gmVariant = variant
          , gmGameMode = mode
          , gmAiSide = aiSide
          , gmAiDepth = aiDepth
          , gmAiNodeLimit = aiNodeLimit
          , gmEvalScore = evaluate gs
          }
        case gpSession props of
          Just sess -> do
            let uid = userId (sessionUser sess)
                gameModeStr = case mode of
                  PracticeMode -> "local" :: MisoString
                  AiMode -> "ai"
                  MultiplayerMode -> "multiplayer"
                aiSideStr = if mode == AiMode
                  then Just (sideStr aiSide)
                  else Nothing
                aiDepthVal = if mode == AiMode
                  then Just aiDepth
                  else (Nothing :: Maybe Int)
                gameData = object
                  [ "id"          .= uuid
                  , "user_id"     .= uid
                  , "variant"     .= variantSlug variant
                  , "result_desc" .= ("in_progress" :: MisoString)
                  , "total_moves" .= (0 :: Int)
                  , "game_mode"   .= gameModeStr
                  , "ai_side"     .= aiSideStr
                  , "ai_depth"    .= aiDepthVal
                  , "moves"       .= ([] :: [MoveAction])
                  ]
            insert "games" gameData (InsertOptions Nothing Nothing) GGameCreated GGameCreateError
            io_ $ pushURI (playURI uuid)
          Nothing -> pure ()
        triggerAi grChannelRef grClockRef

      NewMultiplayerGame variant tc sidePref invCode uuid qrUrl isRated isMatchmaking creatorRating creatorRd -> do
        let gs = initialState variant
            mySide = case sidePref of
              "attacker" -> AttackerSide
              _ -> DefenderSide
            (initAtkMs, initDefMs) = case tc of
              BlitzControl totalMs -> (totalMs, totalMs)
              _ -> (0, 0)
        case gpSession props of
          Just sess -> do
            let uid = userId (sessionUser sess)
                displayName = case gpGuestName props of
                  Just gn -> gn
                  Nothing -> maybe "" pUsername (gpProfile props)
                (atkId, atkName, defId, defName) = case mySide of
                  AttackerSide -> (Just uid, Just displayName, Nothing :: Maybe MisoString, Nothing :: Maybe MisoString)
                  DefenderSide -> (Nothing :: Maybe MisoString, Nothing :: Maybe MisoString, Just uid, Just displayName)
                tcFields = case tc of
                  BlitzControl totalMs ->
                    [ "time_control"               .= ("blitz" :: MisoString)
                    , "attacker_time_remaining_ms" .= totalMs
                    , "defender_time_remaining_ms" .= totalMs
                    , "time_per_player_ms"         .= totalMs
                    ]
                  DailyControl perMoveSec ->
                    [ "time_control"          .= ("daily" :: MisoString)
                    , "time_per_move_seconds" .= perMoveSec
                    ]
                  NoTimeControl -> []
                matchmakingFields = if isMatchmaking
                  then [ "is_matchmaking"  .= True
                       , "creator_rating"  .= creatorRating
                       , "creator_rd"      .= creatorRd
                       ]
                  else []
                gameData = object $
                  [ "id"            .= uuid
                  , "user_id"       .= uid
                  , "variant"       .= variantSlug variant
                  , "result_desc"   .= ("in_progress" :: MisoString)
                  , "total_moves"   .= (0 :: Int)
                  , "game_mode"     .= ("multiplayer" :: MisoString)
                  , "moves"         .= ([] :: [MoveAction])
                  , "status"        .= ("waiting" :: MisoString)
                  , "invite_code"   .= invCode
                  , "current_turn"  .= ("attacker" :: MisoString)
                  , "attacker_id"   .= atkId
                  , "attacker_name" .= atkName
                  , "defender_id"   .= defId
                  , "defender_name" .= defName
                  , "is_rated"      .= isRated
                  ] ++ tcFields ++ matchmakingFields
            put $ initialGameModel
              { gmGameId = Just uuid
              , gmGameState = gs
              , gmVariant = variant
              , gmGameMode = MultiplayerMode
              , gmIsRated = isRated
              , gmIsMatchmaking = isMatchmaking
              , gmPlayerSide = Just mySide
              , gmInviteCode = Just invCode
              , gmQrDataUrl = Just qrUrl
              , gmTimeControl = tc
              , gmAttackerTimeMs = initAtkMs
              , gmDefenderTimeMs = initDefMs
              , gmAttackerName = if mySide == AttackerSide then Just displayName else Nothing
              , gmDefenderName = if mySide == DefenderSide then Just displayName else Nothing
              }
            insert "games" gameData (InsertOptions Nothing Nothing) GGameCreated GGameCreateError
            subscribeToTableWithPresence ("game:" <> uuid) "games" ("id=eq." <> uuid)
              GRealtimeChange GPresenceSync GRealtimeSubscribed GRealtimeError
            subscribeToTable ("chat:" <> uuid) "game_chat" ("game_id=eq." <> uuid)
              GChatReceived GChatSubscribed GChatError
            subscribeVoiceBroadcast grVoiceChannelRef uuid
            -- Start matchmaking polling timer
            when isMatchmaking $
              startMatchmakingTimer grChannelRef
            io_ $ pushURI (playURI uuid)
          Nothing -> pure ()

      JoinGame gr -> do
        let variant = fromMaybe Tablut (lookupVariant (grwVariant gr))
            gs0 = initialState variant
            (hist, gs) = replayMoves gs0 (grwMoves gr)
            gid = grwId gr
            tc = parseTimeControl gr
            mySide = if isNothing (grwAttackerId gr) then AttackerSide else DefenderSide
            oppName = case mySide of
              AttackerSide -> grwDefenderName gr
              DefenderSide -> grwAttackerName gr
        put $ applyClockFromRow gr $ initialGameModel
          { gmGameId = Just gid
          , gmGameMode = MultiplayerMode
          , gmIsRated = grwIsRated gr
          , gmGameState = gs
          , gmVariant = variant
          , gmHistory = hist
          , gmMoveList = grwMoves gr
          , gmPlayerSide = Just mySide
          , gmOpponentName = oppName
          , gmAttackerName = grwAttackerName gr
          , gmDefenderName = grwDefenderName gr
          , gmAttackerId = grwAttackerId gr
          , gmDefenderId = grwDefenderId gr
          , gmEvalScore = evaluate gs
          }
        subscribeToTableWithPresence ("game:" <> gid) "games" ("id=eq." <> gid)
          GRealtimeChange GPresenceSync GRealtimeSubscribed GRealtimeError
        subscribeToTable ("chat:" <> gid) "game_chat" ("game_id=eq." <> gid)
          GChatReceived GChatSubscribed GChatError
        subscribeVoiceBroadcast grVoiceChannelRef gid
        selectWithFilters "game_chat" "*" [eq "game_id" gid]
          (FetchOptions Nothing Nothing Nothing Nothing) GChatHistoryLoaded GChatHistoryError
        io_ $ pushURI (playURI gid)
        case gpSession props of
          Just sess -> do
            let uid = userId (sessionUser sess)
                displayName = case gpGuestName props of
                  Just gn -> gn
                  Nothing -> maybe "" pUsername (gpProfile props)
            case tc of
              NoTimeControl ->
                io (pure (GCompleteJoinWithClock uid displayName "" Nothing))
              BlitzControl _ ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  sink (GCompleteJoinWithClock uid displayName nowStr Nothing)
              DailyControl perMoveSec ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  deadlineStr <- js_addSecondsISO nowStr perMoveSec
                  sink (GCompleteJoinWithClock uid displayName nowStr (Just deadlineStr))
          Nothing -> pure ()

      ResumeGame gr -> do
        let variant = fromMaybe Tablut (lookupVariant (grwVariant gr))
            gs0 = initialState variant
            (hist, gs) = replayMoves gs0 (grwMoves gr)
            gid = grwId gr
            gameMode = case grwGameMode gr of
              Just "local"       -> PracticeMode
              Just "multiplayer" -> MultiplayerMode
              Just "ai"          -> AiMode
              _ -> -- Fallback for old rows without game_mode
                if grwStatus gr `elem` ["waiting", "active"]
                   || (grwStatus gr == "finished" && fromMaybe "" (grwInviteCode gr) /= "")
                then MultiplayerMode
                else AiMode
        case gpSession props of
          Just sess -> do
            let uid = userId (sessionUser sess)
                mySide
                  | grwAttackerId gr == Just uid = Just AttackerSide
                  | grwDefenderId gr == Just uid = Just DefenderSide
                  | otherwise = Nothing
                oppName = case mySide of
                  Just AttackerSide -> grwDefenderName gr
                  Just DefenderSide -> grwAttackerName gr
                  Nothing -> Nothing
            put $ applyClockFromRow gr $ initialGameModel
              { gmGameId = Just gid
              , gmGameMode = gameMode
              , gmIsRated = grwIsRated gr
              , gmVariant = variant
              , gmGameState = gs
              , gmHistory = hist
              , gmMoveList = grwMoves gr
              , gmPlayerSide = mySide
              , gmOpponentName = oppName
              , gmAttackerName = grwAttackerName gr
              , gmDefenderName = grwDefenderName gr
              , gmAttackerId = grwAttackerId gr
              , gmDefenderId = grwDefenderId gr
              , gmEvalScore = evaluate gs
              , gmInviteCode = grwInviteCode gr
              }
            case grwInviteCode gr of
              Just code | grwStatus gr == "waiting" ->
                withSink $ \sink -> do
                  origin <- js_getOrigin
                  qr <- js_generateQRDataURL (origin <> "/join/" <> code)
                  sink (GSetQrDataUrl qr)
              _ -> pure ()
            when (grwStatus gr `elem` ["waiting", "active", "finished"]) $ do
              subscribeToTableWithPresence ("game:" <> gid) "games" ("id=eq." <> gid)
                GRealtimeChange GPresenceSync GRealtimeSubscribed GRealtimeError
              subscribeToTable ("chat:" <> gid) "game_chat" ("game_id=eq." <> gid)
                GChatReceived GChatSubscribed GChatError
              subscribeVoiceBroadcast grVoiceChannelRef gid
              selectWithFilters "game_chat" "*" [eq "game_id" gid]
                (FetchOptions Nothing Nothing Nothing Nothing) GChatHistoryLoaded GChatHistoryError
            when (grwStatus gr == "active") $
              case parseTimeControl gr of
                BlitzControl _ -> startBlitzClock grChannelRef grClockRef
                DailyControl _ -> startDailyClock grClockRef
                _ -> pure ()
          Nothing -> do
            put $ applyClockFromRow gr $ initialGameModel
              { gmGameId = Just gid
              , gmGameMode = gameMode
              , gmIsRated = grwIsRated gr
              , gmVariant = variant
              , gmGameState = gs
              , gmHistory = hist
              , gmMoveList = grwMoves gr
              , gmAttackerName = grwAttackerName gr
              , gmDefenderName = grwDefenderName gr
              , gmAttackerId = grwAttackerId gr
              , gmDefenderId = grwDefenderId gr
              , gmEvalScore = evaluate gs
              , gmInviteCode = grwInviteCode gr
              }
            case grwInviteCode gr of
              Just code | grwStatus gr == "waiting" ->
                withSink $ \sink -> do
                  origin <- js_getOrigin
                  qr <- js_generateQRDataURL (origin <> "/join/" <> code)
                  sink (GSetQrDataUrl qr)
              _ -> pure ()
            when (grwStatus gr `elem` ["waiting", "active", "finished"]) $ do
              subscribeToTableWithPresence ("game:" <> gid) "games" ("id=eq." <> gid)
                GRealtimeChange GPresenceSync GRealtimeSubscribed GRealtimeError
              subscribeToTable ("chat:" <> gid) "game_chat" ("game_id=eq." <> gid)
                GChatReceived GChatSubscribed GChatError
              subscribeVoiceBroadcast grVoiceChannelRef gid
              selectWithFilters "game_chat" "*" [eq "game_id" gid]
                (FetchOptions Nothing Nothing Nothing Nothing) GChatHistoryLoaded GChatHistoryError
            when (grwStatus gr == "active") $
              case parseTimeControl gr of
                BlitzControl _ -> startBlitzClock grChannelRef grClockRef
                DailyControl _ -> startDailyClock grClockRef
                _ -> pure ()

  GameUnmount -> do
    gm <- get
    -- Clean up matchmaking timer
    case gmMatchmakingTimerId gm of
      Just tid -> io_ $ js_clearInterval tid
      Nothing  -> pure ()
    io_ $ do
      mCh <- readIORef grChannelRef
      case mCh of
        Just ch -> removeChannel ch
        Nothing -> pure ()
      writeIORef grChannelRef Nothing
      mChatCh <- readIORef grChatChannelRef
      case mChatCh of
        Just ch -> removeChannel ch
        Nothing -> pure ()
      writeIORef grChatChannelRef Nothing
      mVoiceCh <- readIORef grVoiceChannelRef
      case mVoiceCh of
        Just ch -> removeChannel ch
        Nothing -> pure ()
      writeIORef grVoiceChannelRef Nothing
      mTid <- readIORef grClockRef
      case mTid of
        Just tid -> js_stopGameClock tid
        Nothing -> js_stopGameClock 0
      writeIORef grClockRef Nothing
      -- Tear down voice
      mPc <- readIORef grPeerConnRef
      mStream <- readIORef grMediaStreamRef
      voiceTeardownIO mPc mStream
      writeIORef grPeerConnRef Nothing
      writeIORef grMediaStreamRef Nothing
      -- Tear down video
      mVidStream <- readIORef grVideoStreamRef
      case mVidStream of
        Just vs -> js_voiceStopVideoStream vs
        Nothing -> pure ()
      writeIORef grVideoStreamRef Nothing
    mailParent $ object ["type" .= ("game_unmounted" :: MisoString)]

  GCellClicked coords -> do
    gm <- get
    let activeGs = displayedGameState gm
        gs = activeGs
        board = gsBoard gs
        side = turnSide gs
        piece = pieceAt board coords
        aiBlocked = (gmGameMode gm == AiMode && gmAiSide gm == side)
                 || gmAiOpponent gm == Just side
        mpBlocked = gmGameMode gm == MultiplayerMode && gmPlayerSide gm /= Just side
        browsing = gmBrowseIndex gm /= Nothing
        mpBrowseBlocked = browsing && gmGameMode gm == MultiplayerMode
    if finished (gsResult (gmGameState gm)) || gmAiThinking gm || aiBlocked || mpBlocked || mpBrowseBlocked
      then pure ()
      else case gmSelected gm of
        Just sel | coords `elem` gmValidMoves gm -> do
          let move = MoveAction sel coords
              gs' = act activeGs move
              (newHist, newMoves) = case gmBrowseIndex gm of
                Just i -> (take i (gmHistory gm ++ [gmGameState gm]), take i (gmMoveList gm))
                Nothing -> (gmHistory gm, gmMoveList gm)
          modify $ const $ gm
            { gmGameState = gs'
            , gmSelected = Nothing
            , gmValidMoves = []
            , gmHistory = newHist ++ [activeGs]
            , gmMoveList = newMoves ++ [move]
            , gmFullHistory = Nothing
            , gmFullMoveList = Nothing
            , gmBrowseIndex = Nothing
            , gmEvalScore = evaluate gs'
            , gmAnimateMove = Just move
            , gmCapturePoofs = [(c, pieceAt (gsBoard activeGs) c) | c <- gsCaptures gs']
            }
          io_ (if null (gsCaptures gs') then js_playMoveSound else js_playCaptureSound)
          when (not (null (gsCaptures gs'))) $
            withSink $ \sink -> do
              threadDelay 400000
              sink GPoofsDone
          when (gmGameMode gm == MultiplayerMode) $ do
            case gmTimeControl gm of
              BlitzControl _ ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  sink (GWriteMpMoveWithClock nowStr Nothing)
              DailyControl perMoveSec ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  deadlineStr <- js_addSecondsISO nowStr perMoveSec
                  sink (GWriteMpMoveWithClock nowStr (Just deadlineStr))
              NoTimeControl ->
                io (pure (GWriteMpMoveWithClock "" Nothing))
          when (finished (gsResult gs') && gmGameMode gm /= MultiplayerMode) $ saveGame grChannelRef grClockRef
          triggerAi grChannelRef grClockRef
        Just sel | sel == coords ->
          modify $ const $ gm { gmSelected = Nothing, gmValidMoves = [] }
        _ | canControl side piece -> do
          let moves = getPossibleMovesFrom gs coords
          modify $ const $ gm { gmSelected = Just coords, gmValidMoves = moves }
        _ ->
          modify $ const $ gm { gmSelected = Nothing, gmValidMoves = [] }

  GAiMoveComplete move -> do
    gm <- get
    if gmAiThinking gm
      then do
        let gs = gmGameState gm
            gs' = act gs move
        modify $ const $ gm
          { gmGameState = gs'
          , gmSelected = Nothing
          , gmValidMoves = []
          , gmAiThinking = False
          , gmHistory = gmHistory gm ++ [gs]
          , gmMoveList = gmMoveList gm ++ [move]
          , gmFullHistory = Nothing
          , gmFullMoveList = Nothing
          , gmEvalScore = evaluate gs'
          , gmAnimateMove = Just move
          , gmCapturePoofs = [(c, pieceAt (gsBoard gs) c) | c <- gsCaptures gs']
          }
        io_ (if null (gsCaptures gs') then js_playMoveSound else js_playCaptureSound)
        when (not (null (gsCaptures gs'))) $
          withSink $ \sink -> do
            threadDelay 400000
            sink GPoofsDone
        -- In multiplayer-AI games, write the AI move to DB
        case gmAiOpponent gm of
          Just _ -> case gmTimeControl gm of
            BlitzControl _ ->
              withSink $ \sink -> do
                nowStr <- js_nowISO
                sink (GWriteMpMoveWithClock nowStr Nothing)
            DailyControl perMoveSec ->
              withSink $ \sink -> do
                nowStr <- js_nowISO
                deadlineStr <- js_addSecondsISO nowStr perMoveSec
                sink (GWriteMpMoveWithClock nowStr (Just deadlineStr))
            NoTimeControl ->
              io (pure (GWriteMpMoveWithClock "" Nothing))
          Nothing ->
            when (finished (gsResult gs')) $ saveGame grChannelRef grClockRef
      else pure ()

  GGotoMove i -> do
    gm <- get
    let allStates = gmHistory gm ++ [gmGameState gm]
        lastIdx = length allStates - 1
        idx = if i >= lastIdx then Nothing else Just (max 0 i)
    modify $ \x -> x { gmBrowseIndex = idx, gmAnimateMove = Nothing, gmCapturePoofs = [] }

  GUndo -> do
    gm <- get
    case gmHistory gm of
      _ | gmGameMode gm == MultiplayerMode
          && not (finished (gsResult (gmGameState gm))) -> pure ()
      [] -> pure ()
      _ -> do
        let prev = last (gmHistory gm)
            newHistory = init (gmHistory gm)
            canBrowse = finished (gsResult (gmGameState gm))
                       || gmGameMode gm `elem` [PracticeMode, AiMode]
            fh = case gmFullHistory gm of
              Just fs -> Just fs
              Nothing | canBrowse -> Just (gmHistory gm ++ [gmGameState gm])
              _ -> Nothing
            fm = case gmFullMoveList gm of
              Just ms' -> Just ms'
              Nothing | canBrowse -> Just (gmMoveList gm)
              _ -> Nothing
        put $ gm
          { gmGameState = prev
          , gmHistory = newHistory
          , gmMoveList = take (length newHistory) (gmMoveList gm)
          , gmFullHistory = fh
          , gmFullMoveList = fm
          , gmSelected = Nothing
          , gmValidMoves = []
          , gmAiThinking = False
          , gmEvalScore = evaluate prev
          , gmAnimateMove = Nothing
          , gmCapturePoofs = []
          }

  GRealtimeChange val -> do
    gm <- get
    case parseRealtimeRow val of
      Nothing -> pure ()
      Just gr -> do
        let remoteMoves = grwMoves gr
            localMoves = gmMoveList gm
            variant = fromMaybe (gmVariant gm) (lookupVariant (grwVariant gr))

        when (grwStatus gr == "active"
              && (gmAttackerName gm == Nothing || gmDefenderName gm == Nothing)) $ do
          -- Cancel matchmaking timer when opponent joins
          case gmMatchmakingTimerId gm of
            Just tid -> do
              io_ $ js_clearInterval tid
              modify $ \x -> x { gmMatchmakingTimerId = Nothing, gmIsMatchmaking = False }
            Nothing -> pure ()
          let oppName = case gmPlayerSide gm of
                Just AttackerSide -> grwDefenderName gr
                Just DefenderSide -> grwAttackerName gr
                Nothing -> Nothing
          modify $ \x -> applyClockFromRow gr $ x
            { gmOpponentName = oppName
            , gmAttackerName = grwAttackerName gr
            , gmDefenderName = grwDefenderName gr
            , gmAttackerId = grwAttackerId gr
            , gmDefenderId = grwDefenderId gr
            }
          -- Detect rated -> casual downgrade
          when (gmIsRated gm && not (grwIsRated gr)) $ do
            modify $ \x -> x { gmIsRated = False }
            mailParent $ object ["type" .= ("rated_downgraded" :: MisoString)]
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock grChannelRef grClockRef
            DailyControl _ -> startDailyClock grClockRef
            _ -> pure ()

        when (length remoteMoves > length localMoves) $ do
          let gs0 = initialState variant
              (hist, gs) = replayMoves gs0 remoteMoves
              oldBoard = if null hist then gsBoard gs else gsBoard (last hist)
              poofs = [(c, pieceAt oldBoard c) | c <- gsCaptures gs]
          modify $ \x -> applyClockFromRow gr $ x
            { gmGameState = gs
            , gmHistory = hist
            , gmMoveList = remoteMoves
            , gmSelected = Nothing
            , gmValidMoves = []
            , gmBrowseIndex = Nothing
            , gmAnimateMove = if not (null remoteMoves) then Just (last remoteMoves) else Nothing
            , gmCapturePoofs = poofs
            }
          io_ (if null poofs then js_playMoveSound else js_playCaptureSound)
          when (not (null poofs)) $
            withSink $ \sink -> do
              threadDelay 400000
              sink GPoofsDone
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock grChannelRef grClockRef
            DailyControl _ -> startDailyClock grClockRef
            _ -> pure ()

        case grwDrawOfferedBy gr of
          Just offeredBy | Just mySide <- gmPlayerSide gm
                         , sideStr mySide /= offeredBy
                         -> modify $ \x -> x { gmDrawOffered = True }
          Nothing -> modify $ \x -> x { gmDrawOffered = False }
          _ -> pure ()

        -- Interest status on waiting matchmaking games
        when (gmIsMatchmaking gm && grwStatus gr == "waiting") $ do
          case grwInterestStatus gr of
            Just "viewing" | not (gmInterestShown gm) -> do
              modify $ \x -> x { gmInterestShown = True }
              -- Extend timeout: subtract 3 ticks to give more time
              modify $ \x -> x { gmMatchmakingTicks = max 0 (gmMatchmakingTicks x - 3) }
              -- Restart timer if it was stopped
              case gmMatchmakingTimerId gm of
                Nothing -> startMatchmakingTimer grChannelRef
                Just _  -> pure ()
            Just "declined" -> do
              modify $ \x -> x { gmInterestShown = False }
              -- Stop timer and show AI fallback (same as tick >= 6)
              case gmMatchmakingTimerId gm of
                Just tid -> do
                  io_ $ js_clearInterval tid
                  modify $ \x -> x { gmMatchmakingTimerId = Nothing, gmMatchmakingTicks = 6 }
                Nothing ->
                  modify $ \x -> x { gmMatchmakingTicks = 6 }
            _ -> pure ()

        -- Rematch offer detection
        case grwRematchOfferedBy gr of
          Just offeredBy | Just mySide <- gmPlayerSide gm
                         , sideStr mySide /= offeredBy
                         -> modify $ \x -> x { gmRematchOffered = True }
          Nothing -> modify $ \x -> x { gmRematchOffered = False, gmRematchPending = False }
          _ -> pure ()

        -- Rematch navigation: when rematch_game_id is set, navigate both players
        case grwRematchGameId gr of
          Just newGid | isNothing (gmRematchGameId gm) -> do
            modify $ \x -> x { gmRematchGameId = Just newGid }
            io_ $ pushURI (playURI newGid)
          _ -> pure ()

        when (grwResultDesc gr /= "in_progress" && grwStatus gr == "finished") $ do
          let winSide = case grwWinner gr of
                Just "attacker" -> Just AttackerSide
                Just "defender" -> Just DefenderSide
                _ -> Nothing
              result = GameResult True winSide (fromMisoString (grwResultDesc gr))
          modify $ \x -> x { gmGameState = (gmGameState x) { gsResult = result }
                           , gmOpponentNotice = Nothing }
          stopClock' grClockRef

  GRealtimeSubscribed ch -> do
    io_ $ writeIORef grChannelRef (Just ch)
    gm <- get
    let (role, sideVal) = case gmPlayerSide gm of
          Just s  -> ("player" :: MisoString, Just (sideStr s))
          Nothing -> ("spectator", Nothing)
    io_ $ trackPresence ch (object ["role" .= role, "side" .= sideVal])

  GPresenceSync val -> do
    gm <- get
    let pi' = parsePresence val
        prevOnline = gmOpponentOnline gm
        oppOnline = case gmPlayerSide gm of
          Just AttackerSide -> piDefenderOnline pi'
          Just DefenderSide -> piAttackerOnline pi'
          Nothing           -> True
        isPlayer = isJust (gmPlayerSide gm)
        gameActive = not (finished (gsResult (gmGameState gm)))
        hasOpp = isJust (gmOpponentName gm)
        notice
          | isPlayer && hasOpp && gameActive && prevOnline && not oppOnline
            = Just "Opponent disconnected"
          | isPlayer && hasOpp && not prevOnline && oppOnline
            = Just "Opponent reconnected"
          | otherwise = Nothing
    modify $ \x -> x
      { gmSpectatorCount = piSpectatorCount pi'
      , gmOpponentOnline = oppOnline
      , gmOpponentNotice = case notice of
          Just n  -> Just n
          Nothing -> gmOpponentNotice x
      }
    when (notice == Just "Opponent reconnected") $
      withSink $ \sink -> do
        threadDelay 3000000
        sink GDismissNotice

  GRealtimeError _ -> pure ()

  GMoveUpdated _ -> pure ()

  GMoveUpdateError msg ->
    mailParent $ object ["type" .= ("toast" :: MisoString), "msg" .= ("Move update failed: " <> msg)]

  GWriteMpMoveWithClock nowStr mDeadlineStr -> do
    gm <- get
    case gmTimeControl gm of
      NoTimeControl -> pure ()
      _ -> modify $ \x -> x { gmLastMoveAt = Just nowStr
                            , gmMoveDeadline = mDeadlineStr }
    let gs = gmGameState gm
        newMoves' = gmMoveList gm
        nextTurn = case turnSide gs of
          AttackerSide -> "attacker" :: MisoString
          DefenderSide -> "defender"
        clockFields = case gmTimeControl gm of
          BlitzControl _ ->
            [ "attacker_time_remaining_ms" .= gmAttackerTimeMs gm
            , "defender_time_remaining_ms" .= gmDefenderTimeMs gm
            , "last_move_at" .= nowStr
            ]
          DailyControl _ ->
            [ "last_move_at" .= nowStr ] ++
            maybe [] (\d -> ["move_deadline" .= d]) mDeadlineStr
          NoTimeControl -> []
    case gmGameId gm of
      Just gid -> do
        let baseFields = if finished (gsResult gs)
              then [ "moves"       .= newMoves'
                   , "current_turn" .= nextTurn
                   , "total_moves" .= length newMoves'
                   , "result_desc" .= ms (desc (gsResult gs))
                   , "winner"      .= fmap (\s -> case s of
                       AttackerSide -> "attacker" :: MisoString
                       DefenderSide -> "defender") (winner (gsResult gs))
                   , "status"      .= ("finished" :: MisoString)
                   ]
              else [ "moves"       .= newMoves'
                   , "current_turn" .= nextTurn
                   , "total_moves" .= length newMoves'
                   ]
            updateData = object (baseFields ++ clockFields)
        updateTable "games" updateData
          [eq "id" gid]
          (UpdateOptions Nothing)
          GMoveUpdated GMoveUpdateError
      Nothing -> pure ()
    if finished (gsResult gs)
      then do
        stopClock' grClockRef
        case gmGameId gm of
          Just gid' -> triggerRatingUpdate gid'
          Nothing   -> pure ()
      else case gmTimeControl gm of
        BlitzControl _ -> startBlitzClock grChannelRef grClockRef
        DailyControl _ -> startDailyClock grClockRef
        _ -> pure ()

  GCompleteJoinWithClock uid displayName nowStr mDeadlineStr -> do
    gm <- get
    props <- getProps
    case (gmGameId gm, gmPlayerSide gm) of
      (Just gid, Just mySide) -> do
        let isAnon = case gpSession props of
              Just sess -> amProvider (userAppMetadata (sessionUser sess)) == "anonymous"
              Nothing   -> True
            baseJoinFields = case mySide of
              AttackerSide ->
                [ "attacker_id"   .= uid
                , "attacker_name" .= displayName
                , "status"        .= ("active" :: MisoString)
                ]
              DefenderSide ->
                [ "defender_id"   .= uid
                , "defender_name" .= displayName
                , "status"        .= ("active" :: MisoString)
                ]
            ratedDowngrade = if gmIsRated gm && isAnon
                             then ["is_rated" .= False]
                             else []
            tcFields = case gmTimeControl gm of
              BlitzControl _ -> [ "last_move_at" .= nowStr ]
              DailyControl _ ->
                [ "last_move_at" .= nowStr ] ++
                maybe [] (\d -> ["move_deadline" .= d]) mDeadlineStr
              NoTimeControl -> []
        case gmTimeControl gm of
          NoTimeControl -> pure ()
          _ -> modify $ \x -> x { gmLastMoveAt = Just nowStr
                                , gmMoveDeadline = mDeadlineStr }
        when (gmIsRated gm && isAnon) $
          modify $ \x -> x { gmIsRated = False }
        updateTable "games" (object (baseJoinFields ++ tcFields ++ ratedDowngrade))
          [eq "id" gid]
          (UpdateOptions Nothing)
          GMoveUpdated GMoveUpdateError
        case gmTimeControl gm of
          BlitzControl _ -> startBlitzClock grChannelRef grClockRef
          DailyControl _ -> startDailyClock grClockRef
          _ -> pure ()
      _ -> pure ()

  GResign -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $ do
      stopClock' grClockRef
      case (gmGameId gm, gmPlayerSide gm) of
        (Just gid, Just mySide) -> do
          let winnerSide = case mySide of
                AttackerSide -> "defender" :: MisoString
                DefenderSide -> "attacker"
              resignDesc = sideStr mySide <> " resigned"
              updateData = object
                [ "result_desc" .= resignDesc
                , "winner"      .= winnerSide
                , "status"      .= ("finished" :: MisoString)
                ]
          updateTable "games" updateData
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
          triggerRatingUpdate gid
        _ -> pure ()

  GOfferDraw -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $
      case (gmGameId gm, gmPlayerSide gm) of
        (Just gid, Just mySide) ->
          updateTable "games"
            (object ["draw_offered_by" .= sideStr mySide])
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
        _ -> pure ()

  GAcceptDraw -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $ do
      stopClock' grClockRef
      case gmGameId gm of
        Just gid -> do
          let updateData = object
                [ "result_desc"     .= ("Draw agreed" :: MisoString)
                , "status"          .= ("finished" :: MisoString)
                , "draw_offered_by" .= (Nothing :: Maybe MisoString)
                ]
          updateTable "games" updateData
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
          triggerRatingUpdate gid
          modify $ \x -> x { gmDrawOffered = False }
        Nothing -> pure ()

  GDeclineDraw -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $
      case gmGameId gm of
        Just gid -> do
          updateTable "games"
            (object ["draw_offered_by" .= (Nothing :: Maybe MisoString)])
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
          modify $ \x -> x { gmDrawOffered = False }
        Nothing -> pure ()

  GCopyGameLink -> do
    gm <- get
    case gmGameId gm of
      Just gid -> do
        io_ $ js_copyToClipboard ("https://taflhouse.com/games/" <> gid)
        mailParent $ object ["type" .= ("toast" :: MisoString), "msg" .= ("Link copied!" :: MisoString)]
      Nothing -> pure ()

  GCopyInviteCode code -> do
    io_ $ do
      origin <- js_getOrigin
      js_copyToClipboard (origin <> "/join/" <> code)
    mailParent $ object ["type" .= ("toast" :: MisoString), "msg" .= ("Link copied!" :: MisoString)]

  GSetQrDataUrl url ->
    modify $ \gm -> gm { gmQrDataUrl = Just url }

  GClockTick atkMs defMs ->
    modify $ \gm -> gm { gmAttackerTimeMs = atkMs, gmDefenderTimeMs = defMs }

  GClockTimeout sideStr' -> do
    gm <- get
    let loserSide = if sideStr' == "attacker" then AttackerSide else DefenderSide
        winnerSide = if sideStr' == "attacker" then DefenderSide else AttackerSide
        resultDesc = sideStr loserSide <> " lost on time"
        result = GameResult True (Just winnerSide) (fromMisoString resultDesc)
    modify $ \x -> x
      { gmGameState = (gmGameState x) { gsResult = result }
      , gmAttackerTimeMs = if sideStr' == "attacker" then 0 else gmAttackerTimeMs x
      , gmDefenderTimeMs = if sideStr' == "defender" then 0 else gmDefenderTimeMs x
      }
    stopClock' grClockRef
    when (gmGameMode gm == MultiplayerMode) $
      case gmGameId gm of
        Just gid -> do
          let updateData = object
                [ "result_desc" .= resultDesc
                , "winner"      .= sideStr winnerSide
                , "status"      .= ("finished" :: MisoString)
                , "attacker_time_remaining_ms" .= if sideStr' == "attacker" then (0 :: Int) else gmAttackerTimeMs gm
                , "defender_time_remaining_ms" .= if sideStr' == "defender" then (0 :: Int) else gmDefenderTimeMs gm
                ]
          updateTable "games" updateData
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
          triggerRatingUpdate gid
        Nothing -> pure ()

  GClockStarted tid ->
    io_ $ writeIORef grClockRef (Just tid)

  GStopClock ->
    stopClock' grClockRef

  GDailyTick ->
    modify $ \gm -> gm { gmDailyTick = gmDailyTick gm + 1 }

  GToggleZenMode -> do
    gm <- get
    let entering = gmViewMode gm == NormalView
    modify $ \x -> x
      { gmViewMode = if entering then ZenView else NormalView
      , gmZenHint = entering
      }
    when entering $ do
      withSink $ \sink -> do
        threadDelay 4000000
        sink GDismissZenHint

  GDismissZenHint ->
    modify $ \gm -> gm { gmZenHint = False }

  GToggleFullscreen -> do
    modify $ \gm -> gm { gmIsFullscreen = not (gmIsFullscreen gm) }
    io_ js_toggleFullscreen

  -- Chat -------------------------------------------------------------------

  GToggleChat -> modify $ \gm -> gm
    { gmChatOpen = not (gmChatOpen gm)
    , gmChatUnread = if not (gmChatOpen gm) then 0 else gmChatUnread gm
    }

  GSetChatInput t -> modify $ \gm -> gm { gmChatInput = t }

  GSendChat -> do
    gm <- get
    props <- getProps
    let msg = gmChatInput gm
        chan = case gmPlayerSide gm of
          Just _  -> "player" :: MisoString
          Nothing -> "spectator"
    when (msg /= "" && isJust (gmGameId gm)) $
      case gpSession props of
        Just sess -> do
          let uid = userId (sessionUser sess)
              senderName = case gpGuestName props of
                Just gn -> gn
                Nothing -> maybe "" pUsername (gpProfile props)
          case gmGameId gm of
            Just gid -> do
              let localMsg = ChatMessage
                    { cmSender    = senderName
                    , cmMessage   = msg
                    , cmChannel   = chan
                    , cmCreatedAt = ""
                    }
              modify $ \x -> x
                { gmChatInput = ""
                , gmChatMessages = gmChatMessages x ++ [localMsg]
                }
              insert "game_chat"
                (object [ "game_id"     .= gid
                        , "user_id"     .= uid
                        , "sender_name" .= senderName
                        , "message"     .= msg
                        , "channel"     .= chan
                        ])
                (InsertOptions Nothing Nothing)
                GChatInserted GChatInsertError
            Nothing -> pure ()
        Nothing -> pure ()

  GChatInserted _ -> pure ()
  GChatInsertError _ -> pure ()

  GChatReceived val ->
    case parseChatMessage val of
      Just msg -> do
        gm <- get
        -- Echo suppression: skip if last message matches (optimistic add)
        let dominated = case reverse (gmChatMessages gm) of
              (prev:_) -> cmSender prev == cmSender msg
                        && cmMessage prev == cmMessage msg
                        && cmChannel prev == cmChannel msg
                        && cmCreatedAt prev == ""
              [] -> False
        if dominated
          then -- Replace optimistic entry with server version (has timestamp)
            modify $ \x -> x
              { gmChatMessages = init (gmChatMessages x) ++ [msg] }
          else modify $ \x -> x
            { gmChatMessages = gmChatMessages x ++ [msg]
            , gmChatUnread = if gmChatOpen x then 0 else gmChatUnread x + 1
            }
      Nothing -> pure ()

  GChatSubscribed ch ->
    io_ $ writeIORef grChatChannelRef (Just ch)

  GChatError _ -> pure ()

  GToggleSpectatorChat -> modify $ \gm -> gm
    { gmShowSpectatorChat = not (gmShowSpectatorChat gm) }

  GChatHistoryLoaded val ->
    case fromJSON val of
      Success msgs -> modify $ \gm -> gm { gmChatMessages = (msgs :: [ChatMessage]) }
      Error _ -> pure ()

  GChatHistoryError _ -> pure ()

  -- Voice chat -------------------------------------------------------------

  GVoiceInvite -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode && isJust (gmPlayerSide gm)
          && gmVoiceState gm == VoiceIdle) $ do
      modify $ \x -> x { gmVoiceState = VoiceInviteSent, gmVoiceError = Nothing }
      withSink $ \sink -> do
        successCb <- Function <$> asyncCallback1 (\streamVal -> sink (GVoiceGotMedia streamVal))
        errorCb   <- Function <$> asyncCallback1 (\errVal -> do
          errStr <- fromJSValUnchecked errVal
          sink (GVoiceMediaError errStr))
        js_voiceGetUserMedia successCb errorCb

  GVoiceAccept -> do
    gm <- get
    when (gmVoiceState gm == VoiceInviteReceived) $ do
      modify $ \x -> x { gmVoiceState = VoiceConnecting, gmVoiceError = Nothing }
      withSink $ \sink -> do
        successCb <- Function <$> asyncCallback1 (\streamVal -> sink (GVoiceGotMedia streamVal))
        errorCb   <- Function <$> asyncCallback1 (\errVal -> do
          errStr <- fromJSValUnchecked errVal
          sink (GVoiceMediaError errStr))
        js_voiceGetUserMedia successCb errorCb

  GVoiceDecline -> do
    gm <- get
    when (gmVoiceState gm == VoiceInviteReceived) $ do
      modify $ \x -> x { gmVoiceState = VoiceIdle }
      sendVoiceBroadcast grVoiceChannelRef gm "voice-decline" []

  GVoiceEnd -> do
    gm <- get
    when (gmVoiceState gm /= VoiceIdle) $ do
      sendVoiceBroadcast grVoiceChannelRef gm "voice-end" []
      io_ $ do
        mPc <- readIORef grPeerConnRef
        mStream <- readIORef grMediaStreamRef
        voiceTeardownIO mPc mStream
        writeIORef grPeerConnRef Nothing
        writeIORef grMediaStreamRef Nothing
        -- Clean up video
        mVidStream <- readIORef grVideoStreamRef
        case mVidStream of
          Just vs -> js_voiceStopVideoStream vs
          Nothing -> pure ()
        writeIORef grVideoStreamRef Nothing
        js_voiceDetachLocalVideo
      modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceMuted = False
                       , gmCameraOn = False, gmRemoteVideoOn = False
                       , gmVideoViewMode = VideoPiP }
      io_ js_clearPipDragTransform

  GVoiceToggleMute -> do
    gm <- get
    when (gmVoiceState gm == VoiceConnected) $ do
      withSink $ \sink -> do
        mStream <- readIORef grMediaStreamRef
        case mStream of
          Just stream -> do
            muted <- js_voiceToggleMute stream
            sink GNoOp  -- trigger re-render indirectly
            pure ()
          Nothing -> pure ()
      modify $ \x -> x { gmVoiceMuted = not (gmVoiceMuted x) }

  GVoiceGotMedia streamVal -> do
    io_ $ writeIORef grMediaStreamRef (Just streamVal)
    gm <- get
    case gmVoiceState gm of
      VoiceInviteSent -> do
        -- We initiated: send invite, wait for accept
        sendVoiceBroadcast grVoiceChannelRef gm "voice-invite" []
      VoiceConnecting -> do
        -- We accepted: send accept, wait for offer
        sendVoiceBroadcast grVoiceChannelRef gm "voice-accept" []
      _ -> pure ()

  GVoiceMediaError errStr -> do
    io_ $ do
      writeIORef grMediaStreamRef Nothing
    modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceError = Just errStr }

  GVoiceBroadcastReceived val -> do
    gm <- get
    let mMsgType = parseMaybe (\v -> withObject "bc" (\o -> o .: "type") v) val :: Maybe MisoString
        mFrom    = parseMaybe (\v -> withObject "bc" (\o -> o .: "from") v) val :: Maybe MisoString
        mySideStr = maybe "" sideStr (gmPlayerSide gm)
    case mMsgType of
      Nothing -> pure ()
      Just msgType
        -- Filter out own messages
        | mFrom == Just mySideStr -> pure ()
        | msgType == "voice-invite" -> do
            when (gmVoiceState gm == VoiceIdle) $
              modify $ \x -> x { gmVoiceState = VoiceInviteReceived, gmVoiceError = Nothing }
        | msgType == "voice-accept" -> do
            when (gmVoiceState gm == VoiceInviteSent) $ do
              modify $ \x -> x { gmVoiceState = VoiceConnecting }
              -- Create PC, add local stream, create offer
              withSink $ \sink -> do
                iceCb <- Function <$> asyncCallback1 (\candVal -> do
                  candStr <- fromJSValUnchecked candVal
                  sink (GVoiceIceCandidate candStr))
                trackCb <- Function <$> asyncCallback2 (\kindVal streamVal -> do
                  kind <- fromJSValUnchecked kindVal
                  case (kind :: MisoString) of
                    "audio"       -> do
                      js_playAudioFromStream streamVal
                      sink GVoiceRemoteTrack
                    "video"       -> do
                      js_createRemoteVideo streamVal
                      sink GVideoRemoteTrackOn
                    "video-ended" -> do
                      js_removeRemoteVideo
                      sink GVideoRemoteTrackOff
                    _             -> pure ())
                pc <- js_voiceCreatePeerConnection iceCb trackCb
                writeIORef grPeerConnRef (Just pc)
                mStream <- readIORef grMediaStreamRef
                case mStream of
                  Just stream -> js_voiceAddStreamToPc pc stream
                  Nothing -> pure ()
                offerOk <- Function <$> asyncCallback1 (\sdpVal -> do
                  sdpStr <- fromJSValUnchecked sdpVal
                  sink (GVoiceOfferCreated sdpStr))
                offerErr <- Function <$> asyncCallback1 (\errVal -> do
                  errStr <- fromJSValUnchecked errVal
                  sink (GVoiceOfferError errStr))
                js_voiceCreateOffer pc offerOk offerErr
        | msgType == "voice-decline" -> do
            when (gmVoiceState gm == VoiceInviteSent) $
              modify $ \x -> x { gmVoiceState = VoiceIdle }
        | msgType == "voice-offer" -> do
            let mSdp = parseMaybe (\v -> withObject "bc" (\o -> o .: "sdp") v) val :: Maybe MisoString
            when (gmVoiceState gm == VoiceConnecting) $ do
              case mSdp of
                Nothing -> pure ()
                Just sdpStr -> withSink $ \sink -> do
                  iceCb <- Function <$> asyncCallback1 (\candVal -> do
                    cStr <- fromJSValUnchecked candVal
                    sink (GVoiceIceCandidate cStr))
                  trackCb <- Function <$> asyncCallback2 (\kindVal streamVal -> do
                    kind <- fromJSValUnchecked kindVal
                    case (kind :: MisoString) of
                      "audio"       -> do
                        js_playAudioFromStream streamVal
                        sink GVoiceRemoteTrack
                      "video"       -> do
                        js_createRemoteVideo streamVal
                        sink GVideoRemoteTrackOn
                      "video-ended" -> do
                        js_removeRemoteVideo
                        sink GVideoRemoteTrackOff
                      _             -> pure ())
                  pc <- js_voiceCreatePeerConnection iceCb trackCb
                  writeIORef grPeerConnRef (Just pc)
                  mStream <- readIORef grMediaStreamRef
                  case mStream of
                    Just stream -> js_voiceAddStreamToPc pc stream
                    Nothing -> pure ()
                  sdpJsv <- toJSVal sdpStr
                  answerOk <- Function <$> asyncCallback1 (\ansVal -> do
                    ansStr <- fromJSValUnchecked ansVal
                    sink (GVoiceAnswerCreated ansStr))
                  answerErr <- Function <$> asyncCallback1 (\errVal -> do
                    errStr <- fromJSValUnchecked errVal
                    sink (GVoiceAnswerError errStr))
                  js_voiceCreateAnswer pc sdpJsv answerOk answerErr
            -- Renegotiation: reuse existing PC when already connected
            when (gmVoiceState gm == VoiceConnected) $ do
              case mSdp of
                Nothing -> pure ()
                Just sdpStr -> withSink $ \sink -> do
                  mPc <- readIORef grPeerConnRef
                  case mPc of
                    Just pc -> do
                      sdpJsv <- toJSVal sdpStr
                      answerOk <- Function <$> asyncCallback1 (\ansVal -> do
                        ansStr <- fromJSValUnchecked ansVal
                        sink (GVoiceAnswerCreated ansStr))
                      answerErr <- Function <$> asyncCallback1 (\errVal -> do
                        errStr <- fromJSValUnchecked errVal
                        sink (GVoiceAnswerError errStr))
                      js_voiceCreateAnswer pc sdpJsv answerOk answerErr
                    Nothing -> pure ()
        | msgType == "voice-answer" -> do
            let mSdp = parseMaybe (\v -> withObject "bc" (\o -> o .: "sdp") v) val :: Maybe MisoString
            case mSdp of
              Nothing -> pure ()
              Just sdpStr -> withSink $ \sink -> do
                mPc <- readIORef grPeerConnRef
                case mPc of
                  Nothing -> pure ()
                  Just pc -> do
                    sdpJsv <- toJSVal sdpStr
                    okCb <- Function <$> asyncCallback (sink GVoiceRemoteAnswerSet)
                    errCb <- Function <$> asyncCallback1 (\errVal -> do
                      errStr <- fromJSValUnchecked errVal
                      sink (GVoiceRemoteAnswerError errStr))
                    js_voiceSetRemoteAnswer pc sdpJsv okCb errCb
        | msgType == "voice-ice" -> do
            let mCand = parseMaybe (\v -> withObject "bc" (\o -> o .: "candidate") v) val :: Maybe MisoString
            case mCand of
              Nothing -> pure ()
              Just candStr -> withSink $ \sink -> do
                mPc <- readIORef grPeerConnRef
                case mPc of
                  Nothing -> pure ()
                  Just pc -> do
                    candJsv <- toJSVal candStr
                    okCb <- Function <$> asyncCallback (sink GVoiceIceCandidateAdded)
                    errCb <- Function <$> asyncCallback1 (\errVal -> do
                      errStr <- fromJSValUnchecked errVal
                      sink (GVoiceIceCandidateError errStr))
                    js_voiceAddIceCandidate pc candJsv okCb errCb
        | msgType == "voice-end" -> do
            io_ $ do
              mPc <- readIORef grPeerConnRef
              mStream <- readIORef grMediaStreamRef
              voiceTeardownIO mPc mStream
              writeIORef grPeerConnRef Nothing
              writeIORef grMediaStreamRef Nothing
              -- Clean up video
              mVidStream <- readIORef grVideoStreamRef
              case mVidStream of
                Just vs -> js_voiceStopVideoStream vs
                Nothing -> pure ()
              writeIORef grVideoStreamRef Nothing
              js_voiceDetachLocalVideo
            modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceMuted = False
                             , gmCameraOn = False, gmRemoteVideoOn = False
                             , gmVideoViewMode = VideoPiP }
            io_ js_clearPipDragTransform
        | otherwise -> pure ()

  GVoiceBroadcastSubscribed ch ->
    io_ $ writeIORef grVoiceChannelRef (Just ch)

  GVoiceBroadcastError _ -> pure ()

  GVoiceOfferCreated sdpStr -> do
    gm <- get
    sendVoiceBroadcast grVoiceChannelRef gm "voice-offer" ["sdp" .= sdpStr]

  GVoiceOfferError errStr ->
    modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceError = Just errStr }

  GVoiceAnswerCreated sdpStr -> do
    gm <- get
    sendVoiceBroadcast grVoiceChannelRef gm "voice-answer" ["sdp" .= sdpStr]

  GVoiceAnswerError errStr ->
    modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceError = Just errStr }

  GVoiceRemoteAnswerSet ->
    pure ()  -- ICE will complete the connection

  GVoiceRemoteAnswerError errStr ->
    modify $ \x -> x { gmVoiceState = VoiceIdle, gmVoiceError = Just errStr }

  GVoiceIceCandidate candStr -> do
    gm <- get
    sendVoiceBroadcast grVoiceChannelRef gm "voice-ice" ["candidate" .= candStr]

  GVoiceIceCandidateAdded -> pure ()

  GVoiceIceCandidateError _ -> pure ()

  GVoiceRemoteTrack ->
    modify $ \x -> x { gmVoiceState = VoiceConnected }

  -- Video --------------------------------------------------------------------

  GVideoToggleCamera -> do
    gm <- get
    when (gmVoiceState gm == VoiceConnected) $ do
      if gmCameraOn gm
        then do
          -- Turn camera OFF: stop video, remove from PC, renegotiate
          io_ $ do
            mVidStream <- readIORef grVideoStreamRef
            case mVidStream of
              Just vs -> js_voiceStopVideoStream vs
              Nothing -> pure ()
            writeIORef grVideoStreamRef Nothing
            mPc <- readIORef grPeerConnRef
            case mPc of
              Just pc -> js_voiceRemoveVideoFromPc pc
              Nothing -> pure ()
            js_voiceDetachLocalVideo
          modify $ \x -> x { gmCameraOn = False }
          -- Renegotiate so remote side learns video was removed
          withSink $ \sink -> do
            mPc <- readIORef grPeerConnRef
            case mPc of
              Just pc -> do
                offerOk <- Function <$> asyncCallback1 (\sdpVal -> do
                  sdpStr <- fromJSValUnchecked sdpVal
                  sink (GVoiceOfferCreated sdpStr))
                offerErr <- Function <$> asyncCallback1 (\errVal -> do
                  errStr <- fromJSValUnchecked errVal
                  sink (GVoiceOfferError errStr))
                js_voiceCreateOffer pc offerOk offerErr
              Nothing -> pure ()
        else do
          -- Turn camera ON: request video media
          withSink $ \sink -> do
            successCb <- Function <$> asyncCallback1 (\streamVal -> sink (GVideoGotMedia streamVal))
            errorCb   <- Function <$> asyncCallback1 (\errVal -> do
              errStr <- fromJSValUnchecked errVal
              sink (GVideoMediaError errStr))
            js_voiceGetVideoMedia successCb errorCb

  GVideoGotMedia streamVal -> do
    io_ $ writeIORef grVideoStreamRef (Just streamVal)
    -- Add video track to existing PC and attach local preview
    withSink $ \sink -> do
      mPc <- readIORef grPeerConnRef
      case mPc of
        Just pc -> do
          js_voiceAddVideoToPc pc streamVal
          js_voiceAttachLocalVideo streamVal
          -- Renegotiate
          offerOk <- Function <$> asyncCallback1 (\sdpVal -> do
            sdpStr <- fromJSValUnchecked sdpVal
            sink (GVoiceOfferCreated sdpStr))
          offerErr <- Function <$> asyncCallback1 (\errVal -> do
            errStr <- fromJSValUnchecked errVal
            sink (GVoiceOfferError errStr))
          js_voiceCreateOffer pc offerOk offerErr
        Nothing -> pure ()
    modify $ \x -> x { gmCameraOn = True }
    io_ js_makePipDraggable

  GVideoMediaError errStr -> do
    io_ $ writeIORef grVideoStreamRef Nothing
    modify $ \x -> x { gmCameraOn = False, gmVoiceError = Just errStr }

  GVideoRemoteTrackOn -> do
    modify $ \x -> x { gmRemoteVideoOn = True }
    io_ js_makePipDraggable

  GVideoRemoteTrackOff ->
    modify $ \x -> x { gmRemoteVideoOn = False }

  GVideoSetViewMode mode -> do
    case mode of
      VideoTheater -> io_ js_clearPipDragTransform
      VideoPiP     -> io_ js_makePipDraggable
    modify $ \x -> x { gmVideoViewMode = mode }

  -- Matchmaking -----------------------------------------------------------

  GMatchmakingTimerStarted tid ->
    modify $ \x -> x { gmMatchmakingTimerId = Just tid }

  GMatchmakingTick -> do
    gm <- get
    when (gmIsMatchmaking gm) $ do
      let ticks = gmMatchmakingTicks gm
          newTicks = ticks + 1
      modify $ \x -> x { gmMatchmakingTicks = newTicks }
      when (newTicks >= 6) $ do
        -- Stop timer but keep the game in waiting status
        case gmMatchmakingTimerId gm of
          Just tid -> do
            io_ $ js_clearInterval tid
            modify $ \x -> x { gmMatchmakingTimerId = Nothing }
          Nothing -> pure ()

  GCancelMatchmaking -> do
    gm <- get
    -- Cancel the matchmaking timer
    case gmMatchmakingTimerId gm of
      Just tid -> io_ $ js_clearInterval tid
      Nothing  -> pure ()
    modify $ \x -> x { gmMatchmakingTimerId = Nothing, gmIsMatchmaking = False }
    -- Cancel the waiting game
    case gmGameId gm of
      Just gid ->
        updateTable "games"
          (object ["status" .= ("cancelled" :: MisoString)])
          [eq "id" gid]
          (UpdateOptions Nothing)
          GMatchmakingCancelled GMatchmakingCancelError
      Nothing -> pure ()
    -- Navigate back to config screen
    io_ $ pushURI (configureURI "multiplayer")

  GMatchmakingCancelled _ -> pure ()

  GMatchmakingCancelError _ -> pure ()

  GAcceptAiFallback -> do
    gm <- get
    props <- getProps
    case (gmGameId gm, gmPlayerSide gm, gpSession props) of
      (Just gid, Just mySide, Just sess) -> do
        let uid = userId (sessionUser sess)
            aiSide = case mySide of
              AttackerSide -> DefenderSide
              DefenderSide -> AttackerSide
            -- Fill in AI side's player fields
            aiFields = case aiSide of
              AttackerSide ->
                [ "attacker_id"   .= uid
                , "attacker_name" .= ("AI" :: MisoString)
                ]
              DefenderSide ->
                [ "defender_id"   .= uid
                , "defender_name" .= ("AI" :: MisoString)
                ]
            updateData = object $
              [ "status"          .= ("active" :: MisoString)
              , "is_rated"        .= False
              , "is_matchmaking"  .= False
              ] ++ aiFields
        updateTable "games" updateData
          [eq "id" gid]
          (UpdateOptions Nothing)
          GMoveUpdated GMoveUpdateError
        modify $ \x -> x
          { gmAiOpponent   = Just aiSide
          , gmIsRated       = False
          , gmIsMatchmaking = False
          , gmOpponentName  = Just "AI"
          , gmAiDepth       = 4
          , gmAiNodeLimit   = 10000
          , gmAttackerName  = if aiSide == AttackerSide then Just "AI" else gmAttackerName x
          , gmDefenderName  = if aiSide == DefenderSide then Just "AI" else gmDefenderName x
          , gmAttackerId    = if aiSide == AttackerSide then Just uid else gmAttackerId x
          , gmDefenderId    = if aiSide == DefenderSide then Just uid else gmDefenderId x
          }
        -- Start clock if blitz
        case gmTimeControl gm of
          BlitzControl _ -> startBlitzClock grChannelRef grClockRef
          DailyControl _ -> startDailyClock grClockRef
          _ -> pure ()
        -- Trigger AI if it's the AI's turn (attacker goes first)
        triggerAi grChannelRef grClockRef
      _ -> pure ()

  GKeepSearching -> do
    gm <- get
    -- Clear any existing timer
    case gmMatchmakingTimerId gm of
      Just tid -> io_ $ js_clearInterval tid
      Nothing  -> pure ()
    modify $ \x -> x { gmMatchmakingTicks = 0, gmMatchmakingTimerId = Nothing }
    -- Restart the matchmaking timer
    startMatchmakingTimer grChannelRef

  -- Persistence ------------------------------------------------------------

  GGameSaved _ ->
    mailParent $ object ["type" .= ("game_finished" :: MisoString)]

  GGameSaveError _ -> pure ()

  GGameCreated _ -> pure ()

  GGameCreateError _ -> pure ()

  -- Rating
  GRatingUpdated _ ->
    mailParent $ object ["type" .= ("rating_updated" :: MisoString)]

  GRatingUpdateError _ -> pure ()  -- idempotent; will succeed next time

  -- Presence ----------------------------------------------------------------

  GDismissNotice ->
    modify $ \gm -> gm { gmOpponentNotice = Nothing }

  -- Rematch -----------------------------------------------------------------

  GRequestRematch -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode && finished (gsResult (gmGameState gm))) $
      case (gmGameId gm, gmPlayerSide gm) of
        (Just gid, Just mySide) -> do
          modify $ \x -> x { gmRematchPending = True }
          updateTable "games"
            (object ["rematch_offered_by" .= sideStr mySide])
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
        _ -> pure ()

  GAcceptRematch -> do
    gm <- get
    props <- getProps
    when (gmGameMode gm == MultiplayerMode && gmRematchOffered gm) $
      case (gmGameId gm, gpSession props) of
        (Just oldGid, Just _) ->
          withSink $ \sink -> do
            uuid <- js_generateUUID
            nowStr <- js_nowISO
            sink (GRematchInserted (object ["uuid" .= uuid, "nowStr" .= nowStr, "oldGid" .= oldGid]))
        _ -> pure ()

  GRematchInserted val -> do
    gm <- get
    props <- getProps
    let mUuid   = parseMaybe (withObject "r" $ \o -> o .: "uuid") val :: Maybe MisoString
        mNow    = parseMaybe (withObject "r" $ \o -> o .: "nowStr") val :: Maybe MisoString
        mOldGid = parseMaybe (withObject "r" $ \o -> o .: "oldGid") val :: Maybe MisoString
    case (mUuid, mNow, mOldGid, gpSession props) of
      (Just newUuid, Just nowStr, Just oldGid, Just sess) -> do
        let uid = userId (sessionUser sess)
            variant = gmVariant gm
            newAtkId = gmDefenderId gm
            newAtkName = gmDefenderName gm
            newDefId = gmAttackerId gm
            newDefName = gmAttackerName gm
            tcFields = case gmTimeControl gm of
              BlitzControl totalMs ->
                [ "time_control"               .= ("blitz" :: MisoString)
                , "attacker_time_remaining_ms" .= totalMs
                , "defender_time_remaining_ms" .= totalMs
                , "time_per_player_ms"         .= totalMs
                , "last_move_at"               .= nowStr
                ]
              DailyControl perMoveSec ->
                [ "time_control"          .= ("daily" :: MisoString)
                , "time_per_move_seconds" .= perMoveSec
                , "last_move_at"          .= nowStr
                ]
              NoTimeControl -> []
            gameData = object $
              [ "id"            .= newUuid
              , "user_id"       .= uid
              , "variant"       .= variantSlug variant
              , "result_desc"   .= ("in_progress" :: MisoString)
              , "total_moves"   .= (0 :: Int)
              , "game_mode"     .= ("multiplayer" :: MisoString)
              , "moves"         .= ([] :: [MoveAction])
              , "status"        .= ("active" :: MisoString)
              , "current_turn"  .= ("attacker" :: MisoString)
              , "attacker_id"   .= newAtkId
              , "attacker_name" .= newAtkName
              , "defender_id"   .= newDefId
              , "defender_name" .= newDefName
              , "is_rated"      .= gmIsRated gm
              ] ++ tcFields
        modify $ \x -> x { gmRematchGameId = Just newUuid }
        insert "games" gameData (InsertOptions Nothing Nothing) GGameCreated GRematchInsertError
        updateTable "games"
          (object ["rematch_game_id" .= newUuid])
          [eq "id" oldGid]
          (UpdateOptions Nothing)
          GMoveUpdated GMoveUpdateError
        io_ $ pushURI (playURI newUuid)
      _ -> pure ()

  GDeclineRematch -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $
      case gmGameId gm of
        Just gid -> do
          updateTable "games"
            (object ["rematch_offered_by" .= (Nothing :: Maybe MisoString)])
            [eq "id" gid]
            (UpdateOptions Nothing)
            GMoveUpdated GMoveUpdateError
          modify $ \x -> x { gmRematchOffered = False }
        Nothing -> pure ()

  GRematchInsertError msg ->
    mailParent $ object ["type" .= ("toast" :: MisoString), "msg" .= ("Rematch failed: " <> msg)]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

triggerAi :: IORef (Maybe Channel) -> IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
triggerAi _channelRef _clockRef = do
  gm <- get
  let gs = gmGameState gm
      shouldTrigger = (gmGameMode gm == AiMode && gmAiSide gm == turnSide gs)
                   || (gmAiOpponent gm == Just (turnSide gs))
  if shouldTrigger && not (finished (gsResult gs))
    then do
      modify $ \x -> x { gmAiThinking = True }
      let cfg = AiConfig (gmAiDepth gm) (gmAiNodeLimit gm)
      withSink $ \sink -> do
        threadDelay 100000
        case bestMove cfg gs of
          Nothing -> sink GNoOp
          Just move -> sink (GAiMoveComplete move)
    else pure ()

sideStr :: Side -> MisoString
sideStr AttackerSide = "attacker"
sideStr DefenderSide = "defender"

parseRealtimeRow :: Value -> Maybe GameRow
parseRealtimeRow val =
  case parseMaybe parsePayload val of
    Just gr -> Just gr
    Nothing -> Nothing
  where
    parsePayload = withObject "RealtimePayload" $ \o -> do
      newVal <- o .: "new"
      parseJSON newVal

parseTimeControl :: GameRow -> TimeControl
parseTimeControl gr = case grwTimeControl gr of
  Just "blitz" -> BlitzControl (fromMaybe 0 (grwTimePerPlayerMs gr))
  Just "daily" -> DailyControl (fromMaybe 0 (grwTimePerMoveSec gr))
  _ -> NoTimeControl

applyClockFromRow :: GameRow -> GameModel -> GameModel
applyClockFromRow gr gm = gm
  { gmTimeControl = parseTimeControl gr
  , gmAttackerTimeMs = fromMaybe 0 (grwAttackerTimeMs gr)
  , gmDefenderTimeMs = fromMaybe 0 (grwDefenderTimeMs gr)
  , gmLastMoveAt = grwLastMoveAt gr
  , gmMoveDeadline = grwMoveDeadline gr
  }

startBlitzClock :: IORef (Maybe Channel) -> IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
startBlitzClock _channelRef clockRef = do
  stopClock' clockRef
  gm <- get
  -- Don't start the clock until the first move has been made.
  -- The attacker's time shouldn't count down before they've moved.
  when (not (null (gmMoveList gm))) $ case gmTimeControl gm of
    BlitzControl _ -> do
      let atkMs = gmAttackerTimeMs gm
          defMs = gmDefenderTimeMs gm
          turn = turnSide (gmGameState gm)
          turnStr = sideStr turn
      withSink $ \sink -> do
        turnJsv <- toJSVal turnStr
        lmaJsv <- toJSVal (fromMaybe "" (gmLastMoveAt gm))
        tickCb <- Function <$> asyncCallback2 (\atkV defV -> do
          atk <- fromJSValUnchecked atkV
          def' <- fromJSValUnchecked defV
          sink (GClockTick atk def'))
        timeoutCb <- Function <$> asyncCallback1 (\sideV -> do
          s <- fromJSValUnchecked sideV
          sink (GClockTimeout s))
        tid <- js_startGameClock atkMs defMs turnJsv lmaJsv tickCb timeoutCb
        writeIORef clockRef (Just tid)
        sink (GClockStarted tid)
    _ -> pure ()

startDailyClock :: IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
startDailyClock clockRef = do
  stopClock' clockRef
  gm <- get
  when (not (null (gmMoveList gm))) $ case gmTimeControl gm of
    DailyControl _ -> do
      withSink $ \sink -> do
        tickCb <- Function <$> asyncCallback (sink GDailyTick)
        tid <- js_startDailyClock tickCb
        writeIORef clockRef (Just tid)
        sink (GClockStarted tid)
    _ -> pure ()

stopClock' :: IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
stopClock' clockRef = io_ $ do
  mTid <- readIORef clockRef
  case mTid of
    Just tid -> js_stopGameClock tid
    Nothing -> js_stopGameClock 0
  writeIORef clockRef Nothing

saveGame :: IORef (Maybe Channel) -> IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
saveGame _channelRef _clockRef = do
  gm <- get
  props <- getProps
  let gs = gmGameState gm
      result = gsResult gs
      winnerStr = fmap (\s -> case s of
        AttackerSide -> "attacker" :: MisoString
        DefenderSide -> "defender") (winner result)
      gameModeStr = case gmGameMode gm of
        PracticeMode -> "local" :: MisoString
        AiMode -> "ai"
        MultiplayerMode -> "multiplayer"
      aiSideStr = if gmGameMode gm == AiMode
        then Just (sideStr (gmAiSide gm))
        else Nothing
      aiDepthVal = if gmGameMode gm == AiMode
        then Just (gmAiDepth gm)
        else (Nothing :: Maybe Int)
  case (gpSession props, gmGameId gm) of
    (Just _, Just gid) -> do
      let updateData = object
            [ "result_desc" .= ms (desc result)
            , "winner"      .= winnerStr
            , "total_moves" .= gsTurn gs
            , "moves"       .= gmMoveList gm
            ]
      updateTable "games" updateData
        [eq "id" gid]
        (UpdateOptions Nothing)
        GGameSaved GGameSaveError
    _ -> do
      let gameData = object
            [ "variant"     .= variantSlug (gmVariant gm)
            , "winner"      .= winnerStr
            , "result_desc" .= ms (desc result)
            , "total_moves" .= gsTurn gs
            , "game_mode"   .= gameModeStr
            , "ai_side"     .= aiSideStr
            , "ai_depth"    .= aiDepthVal
            , "moves"       .= gmMoveList gm
            ]
      io_ $ saveLocalGameIO gameData
  mailParent $ object ["type" .= ("game_finished" :: MisoString)]

-- | Parsed presence state for player/spectator tracking.
data PresenceInfo = PresenceInfo
  { piSpectatorCount :: !Int
  , piAttackerOnline :: !Bool
  , piDefenderOnline :: !Bool
  }

-- | Parse a Supabase presence state value into structured presence info.
--
-- The presence state is @{ key: [{ role: "spectator"|"player", side: "attacker"|"defender" }], ... }@.
parsePresence :: Value -> PresenceInfo
parsePresence (JSON.Object m) =
  let entries = [ e | JSON.Array arr <- Map.elems m, JSON.Object e <- arr ]
      specs   = length [ () | e <- entries
                       , Just (JSON.String r) <- [Map.lookup "role" e]
                       , r == "spectator" ]
      hasPlayer s = any (\e ->
        case (Map.lookup "role" e, Map.lookup "side" e) of
          (Just (JSON.String "player"), Just (JSON.String s')) -> s == s'
          _ -> False) entries
  in PresenceInfo specs (hasPlayer "attacker") (hasPlayer "defender")
parsePresence _ = PresenceInfo 0 False False

displayedGameState :: GameModel -> GameState
displayedGameState gm = case gmBrowseIndex gm of
  Nothing -> gmGameState gm
  Just i -> let allStates = gmHistory gm ++ [gmGameState gm]
            in if i >= 0 && i < length allStates
               then allStates !! i
               else gmGameState gm

-- | Subscribe to a voice broadcast channel for the given game ID.
subscribeVoiceBroadcast :: IORef (Maybe Channel) -> MisoString -> Effect Model GameProps GameModel GameAction
subscribeVoiceBroadcast voiceChRef gameId = do
  withSink $ \sink -> do
    chNameJsv <- toJSVal ("voice:" <> gameId :: MisoString)
    evtNameJsv <- toJSVal ("voice" :: MisoString)
    msgCb <- Function <$> asyncCallback1 (\payloadVal -> do
      v <- fromJSValUnchecked payloadVal
      sink (GVoiceBroadcastReceived v))
    subCb <- Function <$> asyncCallback1 (\chVal -> do
      let ch = Channel chVal
      writeIORef voiceChRef (Just ch)
      sink (GVoiceBroadcastSubscribed ch))
    errCb <- Function <$> asyncCallback1 (\errVal -> do
      errStr <- fromJSValUnchecked errVal
      sink (GVoiceBroadcastError errStr))
    js_subscribeBroadcast chNameJsv evtNameJsv msgCb subCb errCb

-- | Send a voice broadcast message.
sendVoiceBroadcast :: IORef (Maybe Channel) -> GameModel -> MisoString -> [JSON.Pair] -> Effect Model GameProps GameModel GameAction
sendVoiceBroadcast voiceChRef gm msgType extraFields = do
  let mySideStr = maybe "" sideStr (gmPlayerSide gm)
      payload = object (["type" .= msgType, "from" .= mySideStr] ++ extraFields)
  io_ $ do
    mCh <- readIORef voiceChRef
    case mCh of
      Just (Channel chJsv) -> js_sendBroadcast chJsv "voice" payload
      Nothing -> pure ()

-- | Trigger a Supabase RPC call to compute Glicko-2 ratings for a finished game.
triggerRatingUpdate :: MisoString -> Effect Model GameProps GameModel GameAction
triggerRatingUpdate gameId =
  withSink $ \sink -> do
    let ratingOk :: Value -> GameAction
        ratingOk _ = GRatingUpdated (object [])
    okCb  <- successCallback sink (\_ -> GRatingUpdated (object [])) ratingOk
    errCb <- errorCallback sink GRatingUpdateError
    js_runSupabaseRpc "update_ratings" (object ["p_game_id" .= gameId]) okCb errCb

-- | Start a periodic matchmaking search timer (every 3 seconds).
startMatchmakingTimer :: IORef (Maybe Channel) -> Effect Model GameProps GameModel GameAction
startMatchmakingTimer _channelRef = do
  withSink $ \sink -> do
    cb <- Function <$> asyncCallback (sink GMatchmakingTick)
    tid <- js_setInterval cb 3000
    sink (GMatchmakingTimerStarted tid)

-- | Internal action for storing the matchmaking timer ID.
-- Handled inline in the case expression below.

-- | Tear down WebRTC peer connection and media stream (null-safe).
-- Uses empty-string JSVals as falsy stand-ins for null when a ref is Nothing,
-- since the JS voiceTeardown checks truthiness before operating.
voiceTeardownIO :: Maybe JSVal -> Maybe JSVal -> IO ()
voiceTeardownIO mPc mStream = do
  falsyVal <- toJSVal ("" :: MisoString)
  js_voiceTeardown (fromMaybe falsyVal mPc) (fromMaybe falsyVal mStream)
