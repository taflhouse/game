{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Game.Update (updateGame) where

import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), object, (.=), (.:), parseMaybe, withObject)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, asyncCallback, asyncCallback1, asyncCallback2, Function(..))
import Supabase.Miso.Database (insert, updateTable, InsertOptions(..), UpdateOptions(..), eq)
import Supabase.Miso.Realtime (Channel, subscribeToTable, removeChannel)
import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import Tafl.Board
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Game (act, initialState)
import Tafl.Game.State
import Tafl.Game.Move (getPossibleMovesFrom)
import Tafl.AI (AiConfig(..), bestMove, evaluate)

import App.JSON (GameRow(..), Profile(..))
import App.Model (Model, GameMode(..), TimeControl(..), ViewMode(..), GameInitData(..))
import App.Game.Model
import App.Game.Action
import App.Route (replayMoves, lookupVariant, playURI)
import App.FFI

updateGame :: IORef (Maybe Channel) -> IORef (Maybe Int) -> GameAction -> Effect Model GameProps GameModel GameAction
updateGame channelRef clockRef = \case
  GNoOp -> pure ()

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
          Nothing -> pure ()
        io_ $ pushURI (playURI uuid)
        triggerAi channelRef clockRef

      NewMultiplayerGame variant tc sidePref invCode uuid qrUrl -> do
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
                  ] ++ tcFields
            put $ initialGameModel
              { gmGameId = Just uuid
              , gmGameState = gs
              , gmVariant = variant
              , gmGameMode = MultiplayerMode
              , gmPlayerSide = Just mySide
              , gmInviteCode = Just invCode
              , gmQrDataUrl = Just qrUrl
              , gmTimeControl = tc
              , gmAttackerTimeMs = initAtkMs
              , gmDefenderTimeMs = initDefMs
              }
            insert "games" gameData (InsertOptions Nothing Nothing) GGameCreated GGameCreateError
            subscribeToTable ("game:" <> uuid) "games" ("id=eq." <> uuid)
              GRealtimeChange GRealtimeSubscribed GRealtimeError
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
          , gmGameState = gs
          , gmVariant = variant
          , gmHistory = hist
          , gmMoveList = grwMoves gr
          , gmPlayerSide = Just mySide
          , gmOpponentName = oppName
          , gmEvalScore = evaluate gs
          }
        subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
          GRealtimeChange GRealtimeSubscribed GRealtimeError
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
            isMultiplayer = grwStatus gr `elem` ["waiting", "active"]
                           || (grwStatus gr == "finished" && fromMaybe "" (grwInviteCode gr) /= "")
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
              , gmGameMode = if isMultiplayer then MultiplayerMode else AiMode
              , gmVariant = variant
              , gmGameState = gs
              , gmHistory = hist
              , gmMoveList = grwMoves gr
              , gmPlayerSide = mySide
              , gmOpponentName = oppName
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
            when (grwStatus gr `elem` ["waiting", "active"]) $
              subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
                GRealtimeChange GRealtimeSubscribed GRealtimeError
            when (grwStatus gr == "active") $
              case parseTimeControl gr of
                BlitzControl _ -> startBlitzClock channelRef clockRef
                DailyControl _ -> startDailyClock clockRef
                _ -> pure ()
          Nothing -> do
            put $ applyClockFromRow gr $ initialGameModel
              { gmGameId = Just gid
              , gmGameMode = if isMultiplayer then MultiplayerMode else AiMode
              , gmVariant = variant
              , gmGameState = gs
              , gmHistory = hist
              , gmMoveList = grwMoves gr
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

  GameUnmount -> do
    io_ $ do
      mCh <- readIORef channelRef
      case mCh of
        Just ch -> removeChannel ch
        Nothing -> pure ()
      writeIORef channelRef Nothing
      mTid <- readIORef clockRef
      case mTid of
        Just tid -> js_stopGameClock tid
        Nothing -> js_stopGameClock 0
      writeIORef clockRef Nothing
    mailParent $ object ["type" .= ("game_unmounted" :: MisoString)]

  GCellClicked coords -> do
    gm <- get
    let activeGs = displayedGameState gm
        gs = activeGs
        board = gsBoard gs
        side = turnSide gs
        piece = pieceAt board coords
        aiBlocked = gmGameMode gm == AiMode && gmAiSide gm == side
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
            }
          io_ js_playMoveSound
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
          when (finished (gsResult gs') && gmGameMode gm /= MultiplayerMode) $ saveGame channelRef clockRef
          triggerAi channelRef clockRef
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
          }
        io_ js_playMoveSound
        when (finished (gsResult gs')) $ saveGame channelRef clockRef
      else pure ()

  GGotoMove i -> do
    gm <- get
    let allStates = gmHistory gm ++ [gmGameState gm]
        lastIdx = length allStates - 1
        idx = if i >= lastIdx then Nothing else Just (max 0 i)
    modify $ \x -> x { gmBrowseIndex = idx }

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
          }

  GRealtimeChange val -> do
    gm <- get
    case parseRealtimeRow val of
      Nothing -> pure ()
      Just gr -> do
        let remoteMoves = grwMoves gr
            localMoves = gmMoveList gm
            variant = fromMaybe (gmVariant gm) (lookupVariant (grwVariant gr))

        when (grwStatus gr == "active" && gmOpponentName gm == Nothing) $ do
          let oppName = case gmPlayerSide gm of
                Just AttackerSide -> grwDefenderName gr
                Just DefenderSide -> grwAttackerName gr
                Nothing -> Nothing
          modify $ \x -> applyClockFromRow gr $ x { gmOpponentName = oppName }
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock channelRef clockRef
            DailyControl _ -> startDailyClock clockRef
            _ -> pure ()

        when (length remoteMoves > length localMoves) $ do
          let gs0 = initialState variant
              (hist, gs) = replayMoves gs0 remoteMoves
          modify $ \x -> applyClockFromRow gr $ x
            { gmGameState = gs
            , gmHistory = hist
            , gmMoveList = remoteMoves
            , gmSelected = Nothing
            , gmValidMoves = []
            , gmBrowseIndex = Nothing
            }
          io_ js_playMoveSound
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock channelRef clockRef
            DailyControl _ -> startDailyClock clockRef
            _ -> pure ()

        case grwDrawOfferedBy gr of
          Just offeredBy | Just mySide <- gmPlayerSide gm
                         , sideStr mySide /= offeredBy
                         -> modify $ \x -> x { gmDrawOffered = True }
          Nothing -> modify $ \x -> x { gmDrawOffered = False }
          _ -> pure ()

        when (grwResultDesc gr /= "in_progress" && grwStatus gr == "finished") $ do
          let winSide = case grwWinner gr of
                Just "attacker" -> Just AttackerSide
                Just "defender" -> Just DefenderSide
                _ -> Nothing
              result = GameResult True winSide (fromMisoString (grwResultDesc gr))
          modify $ \x -> x { gmGameState = (gmGameState x) { gsResult = result } }
          stopClock' clockRef

  GRealtimeSubscribed ch ->
    io_ $ writeIORef channelRef (Just ch)

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
      then stopClock' clockRef
      else case gmTimeControl gm of
        BlitzControl _ -> startBlitzClock channelRef clockRef
        DailyControl _ -> startDailyClock clockRef
        _ -> pure ()

  GCompleteJoinWithClock uid displayName nowStr mDeadlineStr -> do
    gm <- get
    case (gmGameId gm, gmPlayerSide gm) of
      (Just gid, Just mySide) -> do
        let baseJoinFields = case mySide of
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
        updateTable "games" (object (baseJoinFields ++ tcFields))
          [eq "id" gid]
          (UpdateOptions Nothing)
          GMoveUpdated GMoveUpdateError
        case gmTimeControl gm of
          BlitzControl _ -> startBlitzClock channelRef clockRef
          DailyControl _ -> startDailyClock clockRef
          _ -> pure ()
      _ -> pure ()

  GResign -> do
    gm <- get
    when (gmGameMode gm == MultiplayerMode) $ do
      stopClock' clockRef
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
      stopClock' clockRef
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
    stopClock' clockRef
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
        Nothing -> pure ()

  GClockStarted tid ->
    io_ $ writeIORef clockRef (Just tid)

  GStopClock ->
    stopClock' clockRef

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

  GGameSaved _ ->
    mailParent $ object ["type" .= ("game_finished" :: MisoString)]

  GGameSaveError _ -> pure ()

  GGameCreated _ -> pure ()

  GGameCreateError _ -> pure ()

triggerAi :: IORef (Maybe Channel) -> IORef (Maybe Int) -> Effect Model GameProps GameModel GameAction
triggerAi _channelRef _clockRef = do
  gm <- get
  let gs = gmGameState gm
  if gmGameMode gm == AiMode && not (finished (gsResult gs)) && gmAiSide gm == turnSide gs
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
  case gmTimeControl gm of
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
  case gmTimeControl gm of
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

displayedGameState :: GameModel -> GameState
displayedGameState gm = case gmBrowseIndex gm of
  Nothing -> gmGameState gm
  Just i -> let allStates = gmHistory gm ++ [gmGameState gm]
            in if i >= 0 && i < length allStates
               then allStates !! i
               else gmGameState gm
