{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Update (updateModel) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=), (.:), parseMaybe, withObject)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, asyncCallback, asyncCallback1, asyncCallback2, Function(..))
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq)
import Supabase.Miso.Realtime (subscribeToTable, removeChannel)

import Tafl.Board
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Game (act, initialState)
import Tafl.Game.State
import Tafl.Game.Move (getPossibleMovesFrom)
import Tafl.AI (AiConfig(..), bestMove, evaluate)

import App.JSON (GameRow(..), Profile(..), GameRecord(..))
import App.Model
import App.Action
import App.Route
import App.FFI

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  SetGameMode mode -> do
    modify $ \m -> m { mGameMode = mode }
    io_ $ pushURI configureURI

  SetVariant variant ->
    modify $ \m -> m { mVariant = variant }

  StartGame ->
    withSink $ \sink -> do
      uuid <- js_generateUUID
      sink (InitGame uuid)

  GotoHome ->
    io_ $ pushURI homeURI

  GotoSignIn ->
    io_ $ pushURI signInURI

  GotoSignUp ->
    io_ $ pushURI signUpURI

  GotoConfig ->
    io_ $ pushURI configURI

  GotoJoin ->
    io_ $ pushURI joinBareURI

  ToggleConfigExpand ->
    modify $ \m -> m { mConfigExpanded = not (mConfigExpanded m), mConfigModeChosen = False }

  ToggleQuoteRef -> do
    m <- get
    let opening = not (mShowQuoteRef m)
        gen = mQuoteRefGen m + 1
    modify $ \x -> x { mShowQuoteRef = opening, mQuoteRefGen = gen }
    when opening $ do
      withSink $ \sink -> do
        threadDelay 5000000
        sink (DismissQuoteRefTimed gen)

  DismissQuoteRef ->
    modify $ \m -> m { mShowQuoteRef = False }

  DismissQuoteRefTimed gen -> do
    m <- get
    when (mQuoteRefGen m == gen) $
      modify $ \x -> x { mShowQuoteRef = False }

  ShowToast msg -> do
    modify $ \m -> m { mToast = Just msg }
    withSink $ \sink -> do
      threadDelay 5000000
      sink DismissToast

  DismissToast ->
    modify $ \m -> m { mToast = Nothing }

  SetQrDataUrl url ->
    modify $ \m -> m { mQrDataUrl = Just url }

  ToggleDepthInfo ->
    modify $ \m -> m { mShowDepthInfo = not (mShowDepthInfo m) }

  ToggleNodesInfo ->
    modify $ \m -> m { mShowNodesInfo = not (mShowNodesInfo m) }

  HandleURI uri -> do
    modify $ \x -> x { mToast = Nothing }
    m <- get
    case parseRoute uri of
      PlayRoute uuid
        | mGameId m == Just uuid && mScreen m == GameScreen -> pure ()
        | otherwise -> do
            modify $ \x -> x { mScreen = LoadingScreen, mGameId = Just uuid }
            selectWithFilters "games" "*"
              [eq "id" uuid]
              (FetchOptions Nothing Nothing)
              ResumeGameLoaded ResumeGameLoadError
      GameRoute uuid -> do
        modify $ \x -> x
          { mScreen = LoadingScreen
          , mReplayGame = Nothing
          , mReplayStates = []
          , mReplayIndex = 0
          , mReplayNotFound = False
          }
        selectWithFilters "games" "*"
          [eq "id" uuid]
          (FetchOptions Nothing Nothing)
          ReplayLoaded ReplayLoadError
      HomeRoute -> do
        m' <- get
        case mRealtimeChannel m' of
          Just ch -> io_ $ removeChannel ch
          Nothing -> pure ()
        stopClock
        modify $ \x -> x { mScreen = HomeScreen, mAiThinking = False, mGameId = Nothing
                         , mRealtimeChannel = Nothing, mOpponentName = Nothing, mPlayerSide = Nothing
                         , mInviteCode = Nothing, mDrawOffered = False
                         , mTimeControl = NoTimeControl, mClockTimerId = Nothing, mDailyTick = 0 }
        loadPastGames
      SignInRoute ->
        modify $ \x -> x { mScreen = SignInScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      SignUpRoute ->
        modify $ \x -> x { mScreen = SignUpScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      ConfigRoute ->
        modify $ \x -> x { mScreen = ConfigScreen, mMoveList = [], mGameId = Nothing, mConfigModeChosen = False
                         , mTimeControl = NoTimeControl }
      ConfigureRoute ->
        modify $ \x -> x { mScreen = ConfigureScreen, mMoveList = [], mGameId = Nothing }
      ProfileRoute ->
        modify $ \x -> x { mScreen = ProfileScreen }
      ProfileEditRoute -> do
        m' <- get
        modify $ \x -> x
          { mScreen = ProfileEditScreen
          , mEditUsername = maybe "" pUsername (mProfile m')
          , mEditDisplayName = maybe "" (maybe "" id . pDisplayName) (mProfile m')
          }
      JoinRoute mCode -> do
        modify $ \x -> x
          { mScreen = JoinScreen
          , mJoinCodeInput = fromMaybe (mJoinCodeInput x) mCode
          }
        case mCode of
          Just _  -> withSink $ \sink -> sink JoinMultiplayerGame
          Nothing -> pure ()

  CellClicked coords -> do
    m <- get
    let activeGs = displayedGameState m
        gs    = activeGs
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
        aiBlocked = mGameMode m == AiMode && mAiSide m == side
        mpBlocked = mGameMode m == MultiplayerMode && mPlayerSide m /= Just side
        browsing = mBrowseIndex m /= Nothing
        mpBrowseBlocked = browsing && mGameMode m == MultiplayerMode
    if finished (gsResult (mGameState m)) || mAiThinking m || aiBlocked || mpBlocked || mpBrowseBlocked
      then pure ()
      else case mSelected m of
        Just sel | coords `elem` mValidMoves m -> do
          let move = MoveAction sel coords
              gs' = act activeGs move
              -- If browsing, truncate history to the browse point
              (newHist, newMoves) = case mBrowseIndex m of
                Just i  -> (take i (mHistory m ++ [mGameState m]), take i (mMoveList m))
                Nothing -> (mHistory m, mMoveList m)
          modify $ const $ m { mGameState = gs', mSelected = Nothing, mValidMoves = []
                             , mHistory = newHist ++ [activeGs]
                             , mMoveList = newMoves ++ [move]
                             , mFullHistory = Nothing, mFullMoveList = Nothing
                             , mBrowseIndex = Nothing
                             , mEvalScore = evaluate gs' }
          io_ js_playMoveSound
          -- In multiplayer, update the game row in Supabase
          when (mGameMode m == MultiplayerMode) $ do
            -- Dispatch DB write via continuation action (IO needed for timestamps)
            case mTimeControl m of
              BlitzControl _ ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  sink (WriteMpMoveWithClock nowStr Nothing)
              DailyControl perMoveSec ->
                withSink $ \sink -> do
                  nowStr <- js_nowISO
                  deadlineStr <- js_addSecondsISO nowStr perMoveSec
                  sink (WriteMpMoveWithClock nowStr (Just deadlineStr))
              NoTimeControl ->
                io (pure (WriteMpMoveWithClock "" Nothing))
          when (finished (gsResult gs') && mGameMode m /= MultiplayerMode) saveGame
          triggerAi
        Just sel | sel == coords ->
          modify $ const $ m { mSelected = Nothing, mValidMoves = [] }
        _ | canControl side piece -> do
          let moves = getPossibleMovesFrom gs coords
          modify $ const $ m { mSelected = Just coords, mValidMoves = moves }
        _ ->
          modify $ const $ m { mSelected = Nothing, mValidMoves = [] }

  AiMoveComplete move -> do
    m <- get
    if mAiThinking m
      then do
        let gs = mGameState m
            gs' = act gs move
        modify $ const $ m
          { mGameState = gs', mSelected = Nothing
          , mValidMoves = [], mAiThinking = False
          , mHistory = mHistory m ++ [gs]
          , mMoveList = mMoveList m ++ [move]
          , mFullHistory = Nothing, mFullMoveList = Nothing
          , mEvalScore = evaluate gs' }
        io_ js_playMoveSound
        when (finished (gsResult gs')) saveGame
      else pure ()

  SetAiSide side ->
    modify $ \m -> m { mAiSide = side }

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

  GotoMove i -> do
    m <- get
    let allStates = mHistory m ++ [mGameState m]
        lastIdx = length allStates - 1
        idx = if i >= lastIdx then Nothing else Just (max 0 i)
    modify $ \x -> x { mBrowseIndex = idx }

  Undo -> do
    m <- get
    case mHistory m of
      _ | mGameMode m == MultiplayerMode
          && not (finished (gsResult (mGameState m))) -> pure ()
      [] -> pure ()
      _  -> do
        let prev = last (mHistory m)
            newHistory = init (mHistory m)
            canBrowse = finished (gsResult (mGameState m))
                        || mGameMode m `elem` [PracticeMode, AiMode]
            -- Snapshot full state on first undo if browsable
            fh = case mFullHistory m of
              Just fs -> Just fs
              Nothing | canBrowse -> Just (mHistory m ++ [mGameState m])
              _       -> Nothing
            fm = case mFullMoveList m of
              Just ms' -> Just ms'
              Nothing | canBrowse -> Just (mMoveList m)
              _        -> Nothing
        put $ m
          { mGameState    = prev
          , mHistory      = newHistory
          , mMoveList     = take (length newHistory) (mMoveList m)
          , mFullHistory  = fh
          , mFullMoveList = fm
          , mSelected     = Nothing
          , mValidMoves   = []
          , mAiThinking   = False
          , mEvalScore    = evaluate prev
          }

  -- Auth actions
  SetAuthEmail e ->
    modify $ \m -> m { mAuthEmail = e }

  SetAuthPassword p ->
    modify $ \m -> m { mAuthPassword = p }

  DoSignIn -> do
    m <- get
    modify $ \x -> x { mAuthLoading = True, mAuthError = Nothing, mAuthMessage = Nothing }
    let creds = SignInCredentials
          { sicEmail = Email (mAuthEmail m)
          , sicPassword = Password (mAuthPassword m)
          }
    signInWithPassword creds AuthSuccess AuthError

  DoSignUp -> do
    m <- get
    modify $ \x -> x { mAuthLoading = True, mAuthError = Nothing, mAuthMessage = Nothing }
    let signup = SignUpEmail
          { sueEmail = Email (mAuthEmail m)
          , suePassword = mAuthPassword m
          , sueOptions = Nothing
          }
    signUpEmail signup AuthSuccess AuthError

  AuthSuccess resp -> do
    case adSession (arData resp) of
      Just sess -> do
        modify $ \m -> m
          { mSession      = Just sess
          , mAuthEmail    = ""
          , mAuthPassword = ""
          , mAuthError    = Nothing
          , mAuthMessage  = Nothing
          , mAuthLoading  = False
          , mLocalGames   = []
          }
        migrateLocalGames sess
        loadProfile sess
        io_ $ pushURI homeURI
      Nothing ->
        modify $ \m -> m
          { mAuthEmail    = ""
          , mAuthPassword = ""
          , mAuthError    = Nothing
          , mAuthMessage  = Just "Check your email to confirm your account."
          , mAuthLoading  = False
          }

  AuthError msg ->
    modify $ \m -> m { mAuthError = Just (friendlyAuthError msg), mAuthLoading = False }

  AnonAuthSuccess resp -> do
    case adSession (arData resp) of
      Just sess -> do
        let uid = userId (sessionUser sess)
            gName = guestNameFromId uid
        modify $ \m -> m
          { mSession   = Just sess
          , mGuestName = Just gName
          }
        m <- get
        case mDeferredMpAction m of
          Just DeferCreate ->
            withSink $ \sink -> sink CreateMultiplayerGame
          Just DeferJoin ->
            withSink $ \sink -> sink JoinMultiplayerGame
          Nothing -> pure ()
        modify $ \x -> x { mDeferredMpAction = Nothing }
      Nothing ->
        modify $ \m -> m { mToast = Just "Anonymous sign-in failed", mDeferredMpAction = Nothing }

  AnonAuthError msg ->
    modify $ \m -> m { mToast = Just ("Sign-in failed: " <> msg), mDeferredMpAction = Nothing }

  DoSignOut -> do
    m <- get
    case mRealtimeChannel m of
      Just ch -> io_ $ removeChannel ch
      Nothing -> pure ()
    signOut defaultSignOutOptions SignOutSuccess AuthError
    modify $ \x -> x
      { mSession         = Nothing
      , mScreen          = HomeScreen
      , mPastGames       = []
      , mLocalGames      = []
      , mAuthError       = Nothing
      , mProfile         = Nothing
      , mNeedsUsername    = False
      , mProfileDropdown = False
      , mViewMode        = NormalView
      , mGuestName       = Nothing
      , mRealtimeChannel = Nothing
      }
    io_ $ pushURI homeURI

  SignOutSuccess _ -> pure ()

  SessionRestored mSess -> do
    modify $ \m -> m { mSession = mSess }
    m <- get
    -- If game was loaded before session was available, re-fetch to set
    -- player side, opponent name, and subscribe to realtime.
    case (mSess, mScreen m, mGameId m) of
      (Just _, GameScreen, Just gid)
        | mGameMode m == MultiplayerMode, Nothing <- mPlayerSide m ->
            selectWithFilters "games" "*"
              [eq "id" gid]
              (FetchOptions Nothing Nothing)
              ResumeGameLoaded ResumeGameLoadError
      _ -> pure ()
    case mSess of
      Just sess
        | amProvider (userAppMetadata (sessionUser sess)) == "anonymous" -> do
            let uid = userId (sessionUser sess)
            modify $ \m -> m { mGuestName = Just (guestNameFromId uid) }
        | otherwise -> do
            loadPastGames
            loadProfile sess
      Nothing -> pure ()

  GameSaved _ -> loadPastGames
  GameSaveError _ -> pure ()

  LocalGamesLoaded games ->
    modify $ \m -> m { mLocalGames = games }

  DoMigrateGames uid games -> do
    mapM_ (\gr -> do
      let gameData = object
            [ "user_id"     .= uid
            , "variant"     .= grVariant gr
            , "winner"      .= grWinner gr
            , "result_desc" .= grResultDesc gr
            , "total_moves" .= grTotalMoves gr
            , "game_mode"   .= grGameMode gr
            , "ai_side"     .= grAiSide gr
            , "ai_depth"    .= grAiDepth gr
            , "played_at"   .= grPlayedAt gr
            , "moves"       .= grMoves gr
            ]
      insert "games" gameData
        (InsertOptions Nothing Nothing)
        GameSaved GameSaveError
      ) games
    io_ js_clearLocalGames

  GamesLoaded val ->
    case fromJSON val of
      Success games -> modify $ \m -> m { mPastGames = games, mGamesLoading = False }
      Error _       -> modify $ \m -> m { mPastGames = [], mGamesLoading = False }

  GamesLoadError _ ->
    modify $ \m -> m { mGamesLoading = False }

  ToggleTheme ->
    io_ js_toggleDarkMode

  GotoReplay gid ->
    io_ $ pushURI (emptyURI { uriPath = "games/" <> gid })

  ReplayLoaded val ->
    case fromJSON val of
      Success games -> case (games :: [GameRecord]) of
        (gr:_) -> do
          case (lookupVariant (grVariant gr), grMoves gr) of
            (Just variant, Just moves) -> do
              let initial = initialState variant
                  states = scanl act initial moves
              modify $ \m -> m
                { mScreen       = ReplayScreen
                , mReplayGame   = Just gr
                , mReplayStates = states
                , mReplayIndex  = 0
                , mEvalScore    = evaluate initial
                }
            _ -> modify $ \m -> m
              { mScreen       = ReplayScreen
              , mReplayGame   = Just gr
              , mReplayStates = []
              , mReplayIndex  = 0
              }
        [] -> modify $ \m -> m { mScreen = ReplayScreen, mReplayNotFound = True }
      Error _ -> modify $ \m -> m { mScreen = ReplayScreen, mReplayNotFound = True }

  ReplayLoadError _ -> modify $ \m -> m { mScreen = ReplayScreen, mReplayNotFound = True }

  ReplayGotoMove i -> do
    m <- get
    let maxIdx = length (mReplayStates m) - 1
        idx = max 0 (min maxIdx i)
    modify $ \x -> x { mReplayIndex = idx
                      , mEvalScore = evaluate (mReplayStates m !! idx) }

  InitGame uuid -> do
    m <- get
    let gs = initialState (mVariant m)
    put $ m
      { mGameId     = Just uuid
      , mScreen     = GameScreen
      , mGameState  = gs
      , mSelected   = Nothing
      , mValidMoves = []
      , mAiThinking = False
      , mHistory    = []
      , mMoveList   = []
      , mEvalScore  = evaluate gs
      }
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            gameModeStr' = case mGameMode m of
              PracticeMode       -> "local" :: MisoString
              AiMode          -> "ai"
              MultiplayerMode -> "multiplayer"
            aiSideStr' = if mGameMode m == AiMode
              then Just (case mAiSide m of
                AttackerSide -> "attacker" :: MisoString
                DefenderSide -> "defender")
              else Nothing
            aiDepthVal' = if mGameMode m == AiMode
              then Just (mAiDepth m)
              else (Nothing :: Maybe Int)
            gameData = object
              [ "id"          .= uuid
              , "user_id"     .= uid
              , "variant"     .= variantSlug (mVariant m)
              , "result_desc" .= ("in_progress" :: MisoString)
              , "total_moves" .= (0 :: Int)
              , "game_mode"   .= gameModeStr'
              , "ai_side"     .= aiSideStr'
              , "ai_depth"    .= aiDepthVal'
              , "moves"       .= ([] :: [MoveAction])
              ]
        insert "games" gameData
          (InsertOptions Nothing Nothing)
          GameCreated GameCreateError
      Nothing -> pure ()
    io_ $ pushURI (playURI uuid)
    triggerAi

  GameCreated _ -> pure ()

  GameCreateError _ ->
    pure ()

  GameUpdated _ -> pure ()
  GameUpdateError _ -> pure ()

  CopyGameLink -> do
    m <- get
    case mGameId m of
      Just gid -> do
        io_ $ js_copyToClipboard ("https://taflhouse.com/games/" <> gid)
        modify $ \x -> x { mToast = Just "Link copied!" }
        withSink $ \sink -> do
          threadDelay 3000000
          sink DismissToast
      Nothing -> pure ()

  -- Profile actions
  SetUsernameInput s ->
    modify $ \m -> m { mUsernameInput = s }

  SubmitUsername -> do
    m <- get
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            uname = mUsernameInput m
        insert "profiles"
          (object ["id" .= uid, "username" .= uname])
          (InsertOptions Nothing Nothing)
          ProfileCreated ProfileCreateError
      Nothing -> pure ()

  ProfileCreated _ -> do
    m <- get
    modify $ \x -> x
      { mProfile = Just (Profile (mUsernameInput m) Nothing)
      , mNeedsUsername = False
      , mUsernameInput = ""
      }

  ProfileCreateError _ ->
    modify $ \m -> m { mAuthError = Just "Something went wrong. Please try again." }

  ProfileLoaded val ->
    case fromJSON val of
      Success profiles -> case (profiles :: [Profile]) of
        (p:_) ->
          modify $ \m -> m { mProfile = Just p, mNeedsUsername = False }
        [] ->
          modify $ \m -> m { mNeedsUsername = True }
      Error _ ->
        modify $ \m -> m { mNeedsUsername = True }

  ProfileLoadError _ ->
    modify $ \m -> m { mNeedsUsername = True }

  ToggleProfileDropdown ->
    modify $ \m -> m { mProfileDropdown = not (mProfileDropdown m) }

  GotoProfile -> do
    modify $ \m -> m { mProfileDropdown = False }
    io_ $ pushURI profileURI

  GotoProfileEdit -> do
    m <- get
    modify $ \x -> x
      { mProfileDropdown = False
      , mEditUsername = maybe "" pUsername (mProfile m)
      , mEditDisplayName = maybe "" (maybe "" id . pDisplayName) (mProfile m)
      }
    io_ $ pushURI profileEditURI

  SetEditUsername s ->
    modify $ \m -> m { mEditUsername = s }

  SetEditDisplayName s ->
    modify $ \m -> m { mEditDisplayName = s }

  SubmitProfileEdit -> do
    m <- get
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            uData = object
              [ "username"     .= mEditUsername m
              , "display_name" .= mEditDisplayName m
              ]
        updateTable "profiles" uData
          [eq "id" uid]
          (UpdateOptions Nothing)
          ProfileUpdated ProfileUpdateError
      Nothing -> pure ()

  ProfileUpdated _ -> do
    m <- get
    modify $ \x -> x
      { mProfile = Just (Profile (mEditUsername m) (Just (mEditDisplayName m)))
      }
    io_ $ pushURI profileURI

  ProfileUpdateError _ ->
    modify $ \m -> m { mAuthError = Just "Something went wrong. Please try again." }

  -- Multiplayer actions

  CreateMultiplayerGame -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferCreate }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> io $ do
        invCode <- generateInviteCode
        uuid <- js_generateUUID
        origin <- js_getOrigin
        qrUrl <- js_generateQRDataURL (origin <> "/join/" <> invCode)
        pure (InitMultiplayerGame invCode uuid qrUrl)

  InitMultiplayerGame invCode uuid qrUrl -> do
    m <- get
    let gs = initialState (mVariant m)
        mySide = case mSidePreference m of
          "attacker" -> AttackerSide
          _          -> DefenderSide
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            displayName = case mGuestName m of
              Just gn -> gn
              Nothing -> maybe "" pUsername (mProfile m)
            (atkId, atkName, defId, defName) = case mySide of
              AttackerSide -> (Just uid, Just displayName, Nothing :: Maybe MisoString, Nothing :: Maybe MisoString)
              DefenderSide -> (Nothing :: Maybe MisoString, Nothing :: Maybe MisoString, Just uid, Just displayName)
            tcFields = case mTimeControl m of
              BlitzControl totalMs ->
                [ "time_control"               .= ("blitz" :: MisoString)
                , "attacker_time_remaining_ms"  .= totalMs
                , "defender_time_remaining_ms"  .= totalMs
                , "time_per_player_ms"          .= totalMs
                ]
              DailyControl perMoveSec ->
                [ "time_control"            .= ("daily" :: MisoString)
                , "time_per_move_seconds"   .= perMoveSec
                ]
              NoTimeControl -> []
            gameData = object $
              [ "id"            .= uuid
              , "user_id"       .= uid
              , "variant"       .= variantSlug (mVariant m)
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
            (initAtkMs, initDefMs) = case mTimeControl m of
              BlitzControl totalMs -> (totalMs, totalMs)
              _                    -> (0, 0)
        modify $ \x -> x
          { mGameId     = Just uuid
          , mGameMode   = MultiplayerMode
          , mGameState  = gs
          , mSelected   = Nothing
          , mValidMoves = []
          , mHistory    = []
          , mMoveList   = []
          , mInviteCode = Just invCode
          , mQrDataUrl  = Just qrUrl
          , mScreen     = GameScreen
          , mPlayerSide = Just mySide
          , mOpponentName = Nothing
          , mAttackerTimeMs = initAtkMs
          , mDefenderTimeMs = initDefMs
          , mLastMoveAt     = Nothing
          , mMoveDeadline   = Nothing
          }
        insert "games" gameData
          (InsertOptions Nothing Nothing)
          GameCreated GameCreateError
        subscribeToTable ("game:" <> uuid) "games" ("id=eq." <> uuid)
          RealtimeChange RealtimeSubscribed RealtimeError
        io_ $ pushURI (playURI uuid)
      Nothing -> pure ()

  JoinMultiplayerGame -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferJoin }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> do
        let code = mJoinCodeInput m
        when (code /= "") $
          selectWithFilters "games" "*"
            [eq "invite_code" code, eq "status" ("waiting" :: MisoString)]
            (FetchOptions Nothing Nothing)
            GameFoundToJoin GameJoinError

  GameFoundToJoin val -> do
    m <- get
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) -> do
          case mSession m of
            Just sess -> do
              let uid = userId (sessionUser sess)
                  displayName = case mGuestName m of
                    Just gn -> gn
                    Nothing -> maybe "" pUsername (mProfile m)
                  tc = parseTimeControl gr
                  -- Determine which side is open
                  mySide = case grwAttackerId gr of
                    Nothing -> AttackerSide
                    Just _  -> DefenderSide
                  variant = fromMaybe Tablut (lookupVariant (grwVariant gr))
                  gs0 = initialState variant
                  (hist, gs) = replayMoves gs0 (grwMoves gr)
                  oppName = case mySide of
                    AttackerSide -> grwDefenderName gr
                    DefenderSide -> grwAttackerName gr
                  gid = grwId gr
              -- Set up model state (timestamps set by CompleteJoinWithClock)
              modify $ \x -> applyClockFromRow gr $ x
                { mGameId       = Just gid
                , mGameMode     = MultiplayerMode
                , mGameState    = gs
                , mVariant      = variant
                , mSelected     = Nothing
                , mValidMoves   = []
                , mHistory      = hist
                , mMoveList     = grwMoves gr
                , mPlayerSide   = Just mySide
                , mOpponentName = oppName
                , mScreen       = GameScreen
                , mInviteCode   = Nothing
                }
              subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
                RealtimeChange RealtimeSubscribed RealtimeError
              io_ $ pushURI (playURI gid)
              -- Compute timestamps in IO and dispatch DB write
              -- Capture uid/displayName now to avoid race with session changes
              case tc of
                NoTimeControl ->
                  io (pure (CompleteJoinWithClock uid displayName "" Nothing))
                BlitzControl _ ->
                  withSink $ \sink -> do
                    nowStr <- js_nowISO
                    sink (CompleteJoinWithClock uid displayName nowStr Nothing)
                DailyControl perMoveSec ->
                  withSink $ \sink -> do
                    nowStr <- js_nowISO
                    deadlineStr <- js_addSecondsISO nowStr perMoveSec
                    sink (CompleteJoinWithClock uid displayName nowStr (Just deadlineStr))
            Nothing -> pure ()
        [] ->
          modify $ \x -> x { mToast = Just "No waiting game found with that code." }
      Error _ ->
        modify $ \x -> x { mToast = Just "Failed to look up game." }

  GameJoinError msg ->
    modify $ \m -> m { mToast = Just ("Join error: " <> msg) }

  GameJoinedOk _ -> pure ()

  GameJoinUpdateError msg ->
    modify $ \m -> m { mToast = Just ("Failed to join: " <> msg) }

  RealtimeChange val -> do
    m <- get
    -- Parse the new row from the Realtime payload
    case parseRealtimeRow val of
      Nothing -> pure ()
      Just gr -> do
        let remoteMoves = grwMoves gr
            localMoves  = mMoveList m
            variant     = fromMaybe (mVariant m) (lookupVariant (grwVariant gr))

        -- Opponent joined: status changed to active, we have no opponent name yet
        when (grwStatus gr == "active" && mOpponentName m == Nothing) $ do
          let oppName = case mPlayerSide m of
                Just AttackerSide -> grwDefenderName gr
                Just DefenderSide -> grwAttackerName gr
                Nothing           -> Nothing
          modify $ \x -> applyClockFromRow gr $ x { mOpponentName = oppName }
          -- Start clock when opponent joins
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock
            DailyControl _ -> startDailyClock
            _              -> pure ()

        -- Opponent moved: remote has more moves than local
        when (length remoteMoves > length localMoves) $ do
          let gs0 = initialState variant
              (hist, gs) = replayMoves gs0 remoteMoves
          modify $ \x -> applyClockFromRow gr $ x
            { mGameState = gs
            , mHistory   = hist
            , mMoveList  = remoteMoves
            , mSelected  = Nothing
            , mValidMoves = []
            , mBrowseIndex = Nothing
            }
          io_ js_playMoveSound
          -- Restart clock for new active player
          case parseTimeControl gr of
            BlitzControl _ -> startBlitzClock
            DailyControl _ -> startDailyClock
            _              -> pure ()

        -- Draw offered by opponent
        case grwDrawOfferedBy gr of
          Just offeredBy | Just mySide <- mPlayerSide m
                         , sideStr mySide /= offeredBy
                         -> modify $ \x -> x { mDrawOffered = True }
          Nothing        -> modify $ \x -> x { mDrawOffered = False }
          _              -> pure ()

        -- Game over (result changed from in_progress)
        when (grwResultDesc gr /= "in_progress" && grwStatus gr == "finished") $ do
          let winSide = case grwWinner gr of
                Just "attacker" -> Just AttackerSide
                Just "defender" -> Just DefenderSide
                _               -> Nothing
              result = GameResult True winSide (fromMisoString (grwResultDesc gr))
          modify $ \x -> x { mGameState = (mGameState x) { gsResult = result } }
          stopClock

  RealtimeSubscribed ch ->
    modify $ \m -> m { mRealtimeChannel = Just ch }

  RealtimeError msg ->
    modify $ \m -> m { mToast = Just ("Realtime error: " <> msg) }

  MoveUpdated _ -> pure ()
  MoveUpdateError msg ->
    modify $ \m -> m { mToast = Just ("Move update failed: " <> msg) }

  ResumeGameLoaded val -> do
    m <- get
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) -> do
          let variant = fromMaybe Tablut (lookupVariant (grwVariant gr))
              gs0 = initialState variant
              (hist, gs) = replayMoves gs0 (grwMoves gr)
              gid = grwId gr
          case mSession m of
            Just sess -> do
              let uid = userId (sessionUser sess)
                  mySide
                    | grwAttackerId gr == Just uid = Just AttackerSide
                    | grwDefenderId gr == Just uid = Just DefenderSide
                    | otherwise = Nothing
                  oppName = case mySide of
                    Just AttackerSide -> grwDefenderName gr
                    Just DefenderSide -> grwAttackerName gr
                    Nothing           -> Nothing
                  isMultiplayer = grwStatus gr `elem` ["waiting", "active"]
                                 || (grwStatus gr == "finished" && fromMaybe "" (grwInviteCode gr) /= "")
              modify $ \x -> applyClockFromRow gr $ x
                { mGameId       = Just gid
                , mGameMode     = if isMultiplayer then MultiplayerMode else mGameMode x
                , mVariant      = variant
                , mGameState    = gs
                , mHistory      = hist
                , mMoveList     = grwMoves gr
                , mPlayerSide   = mySide
                , mOpponentName = oppName
                , mEvalScore    = evaluate gs
                , mInviteCode   = grwInviteCode gr
                , mQrDataUrl    = Nothing
                , mScreen       = GameScreen
                }
              case grwInviteCode gr of
                Just code | grwStatus gr == "waiting" ->
                  withSink $ \sink -> do
                    origin <- js_getOrigin
                    qr <- js_generateQRDataURL (origin <> "/join/" <> code)
                    sink (SetQrDataUrl qr)
                _ -> pure ()
              -- Subscribe if game is still active
              when (grwStatus gr `elem` ["waiting", "active"]) $
                subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
                  RealtimeChange RealtimeSubscribed RealtimeError
              -- Start clock if game is active
              when (grwStatus gr == "active") $
                case parseTimeControl gr of
                  BlitzControl _ -> startBlitzClock
                  DailyControl _ -> startDailyClock
                  _              -> pure ()
            Nothing -> do
              -- Load session-independent state so the correct screen displays.
              -- Player side, opponent name, and realtime will be set when
              -- SessionRestored fires and triggers a re-fetch.
              let isMultiplayer = grwStatus gr `elem` ["waiting", "active"]
                                 || (grwStatus gr == "finished" && fromMaybe "" (grwInviteCode gr) /= "")
              modify $ \x -> applyClockFromRow gr $ x
                { mGameId       = Just gid
                , mGameMode     = if isMultiplayer then MultiplayerMode else mGameMode x
                , mVariant      = variant
                , mGameState    = gs
                , mHistory      = hist
                , mMoveList     = grwMoves gr
                , mEvalScore    = evaluate gs
                , mInviteCode   = grwInviteCode gr
                , mQrDataUrl    = Nothing
                , mScreen       = GameScreen
                }
              case grwInviteCode gr of
                Just code | grwStatus gr == "waiting" ->
                  withSink $ \sink -> do
                    origin <- js_getOrigin
                    qr <- js_generateQRDataURL (origin <> "/join/" <> code)
                    sink (SetQrDataUrl qr)
                _ -> pure ()
        [] -> modify $ \x -> x { mToast = Just "Game not found." }
      Error _ -> modify $ \x -> x { mToast = Just "Failed to load game." }

  ResumeGameLoadError msg ->
    modify $ \m -> m { mToast = Just ("Load error: " <> msg) }

  SetJoinCodeInput s ->
    modify $ \m -> m { mJoinCodeInput = s }

  Resign -> do
    m <- get
    when (mGameMode m == MultiplayerMode) $ do
      stopClock
      case (mSession m, mGameId m, mPlayerSide m) of
        (Just _, Just gid, Just mySide) -> do
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
            MoveUpdated MoveUpdateError
        _ -> pure ()

  OfferDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode) $
      case (mGameId m, mPlayerSide m) of
        (Just gid, Just mySide) ->
          updateTable "games"
            (object ["draw_offered_by" .= sideStr mySide])
            [eq "id" gid]
            (UpdateOptions Nothing)
            MoveUpdated MoveUpdateError
        _ -> pure ()

  AcceptDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode) $ do
      stopClock
      case mGameId m of
        Just gid -> do
          let updateData = object
                [ "result_desc"    .= ("Draw agreed" :: MisoString)
                , "status"         .= ("finished" :: MisoString)
                , "draw_offered_by" .= (Nothing :: Maybe MisoString)
                ]
          updateTable "games" updateData
            [eq "id" gid]
            (UpdateOptions Nothing)
            MoveUpdated MoveUpdateError
          modify $ \x -> x { mDrawOffered = False }
        Nothing -> pure ()

  DeclineDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode) $
      case mGameId m of
        Just gid -> do
          updateTable "games"
            (object ["draw_offered_by" .= (Nothing :: Maybe MisoString)])
            [eq "id" gid]
            (UpdateOptions Nothing)
            MoveUpdated MoveUpdateError
          modify $ \x -> x { mDrawOffered = False }
        Nothing -> pure ()

  SetSidePreference s ->
    modify $ \m -> m { mSidePreference = s }

  CopyInviteCode code -> do
    io_ $ do
      origin <- js_getOrigin
      js_copyToClipboard (origin <> "/join/" <> code)
    modify $ \m -> m { mToast = Just "Link copied!" }
    withSink $ \sink -> do
      threadDelay 3000000
      sink DismissToast

  ToggleZenMode -> do
    m <- get
    let entering = mViewMode m == NormalView
    modify $ \x -> x { mViewMode = if entering then ZenView else NormalView
                      , mZenHint = entering }
    when entering $ do
      withSink $ \sink -> do
        threadDelay 4000000
        sink DismissZenHint

  DismissZenHint ->
    modify $ \m -> m { mZenHint = False }

  DocumentDblClick -> do
    m <- get
    when (mScreen m `elem` [GameScreen, ReplayScreen]) $
      updateModel ToggleZenMode

  ToggleFullscreen -> do
    modify $ \m -> m { mIsFullscreen = not (mIsFullscreen m) }
    io_ js_toggleFullscreen

  SetTimeControl tc ->
    modify $ \m -> m { mTimeControl = tc }

  ClockTick atkMs defMs ->
    modify $ \m -> m { mAttackerTimeMs = atkMs, mDefenderTimeMs = defMs }

  ClockStarted tid ->
    modify $ \m -> m { mClockTimerId = Just tid }

  StopClock -> stopClock

  DailyTick -> modify $ \m -> m { mDailyTick = mDailyTick m + 1 }

  ClockTimeout sideStr' -> do
    m <- get
    -- Optimistically show timeout result in UI
    let loserSide = if sideStr' == "attacker" then AttackerSide else DefenderSide
        winnerSide = if sideStr' == "attacker" then DefenderSide else AttackerSide
        resultDesc = sideStr loserSide <> " lost on time"
        result = GameResult True (Just winnerSide) (fromMisoString resultDesc)
    modify $ \x -> x { mGameState = (mGameState x) { gsResult = result }
                      , mAttackerTimeMs = if sideStr' == "attacker" then 0 else mAttackerTimeMs x
                      , mDefenderTimeMs = if sideStr' == "defender" then 0 else mDefenderTimeMs x
                      }
    stopClock
    -- Write to DB if we're connected
    when (mGameMode m == MultiplayerMode) $
      case mGameId m of
        Just gid -> do
          let updateData = object
                [ "result_desc" .= resultDesc
                , "winner"      .= sideStr winnerSide
                , "status"      .= ("finished" :: MisoString)
                , "attacker_time_remaining_ms" .= if sideStr' == "attacker" then (0 :: Int) else mAttackerTimeMs m
                , "defender_time_remaining_ms" .= if sideStr' == "defender" then (0 :: Int) else mDefenderTimeMs m
                ]
          updateTable "games" updateData
            [eq "id" gid]
            (UpdateOptions Nothing)
            MoveUpdated MoveUpdateError
        Nothing -> pure ()

  -- Continuation: write the multiplayer move to DB with IO-computed timestamps
  WriteMpMoveWithClock nowStr mDeadlineStr -> do
    m <- get
    -- Update model with timestamps (skip for untimed games)
    case mTimeControl m of
      NoTimeControl -> pure ()
      _ -> modify $ \x -> x { mLastMoveAt = Just nowStr
                             , mMoveDeadline = mDeadlineStr }
    let gs = mGameState m
        newMoves' = mMoveList m
        nextTurn = case turnSide gs of
          AttackerSide -> "attacker" :: MisoString
          DefenderSide -> "defender"
        clockFields = case mTimeControl m of
          BlitzControl _ ->
            [ "attacker_time_remaining_ms" .= mAttackerTimeMs m
            , "defender_time_remaining_ms" .= mDefenderTimeMs m
            , "last_move_at"               .= nowStr
            ]
          DailyControl _ ->
            [ "last_move_at"  .= nowStr ] ++
            maybe [] (\d -> ["move_deadline" .= d]) mDeadlineStr
          NoTimeControl -> []
    case mGameId m of
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
          MoveUpdated MoveUpdateError
      Nothing -> pure ()
    -- Restart or stop clock
    if finished (gsResult gs)
      then stopClock
      else case mTimeControl m of
        BlitzControl _ -> startBlitzClock
        DailyControl _ -> startDailyClock
        _              -> pure ()

  -- Continuation: write the game join to DB with IO-computed timestamps
  CompleteJoinWithClock uid displayName nowStr mDeadlineStr -> do
    m <- get
    case (mGameId m, mPlayerSide m) of
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
            tcFields = case mTimeControl m of
              BlitzControl _ -> [ "last_move_at" .= nowStr ]
              DailyControl _ ->
                [ "last_move_at" .= nowStr ] ++
                maybe [] (\d -> ["move_deadline" .= d]) mDeadlineStr
              NoTimeControl -> []
        case mTimeControl m of
          NoTimeControl -> pure ()
          _ -> modify $ \x -> x { mLastMoveAt = Just nowStr
                                , mMoveDeadline = mDeadlineStr }
        updateTable "games" (object (baseJoinFields ++ tcFields))
          [eq "id" gid]
          (UpdateOptions Nothing)
          GameJoinedOk GameJoinUpdateError
        -- Start clock
        case mTimeControl m of
          BlitzControl _ -> startBlitzClock
          DailyControl _ -> startDailyClock
          _              -> pure ()
      _ -> pure ()

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

-- | Check if the AI should move and trigger the search if so.
triggerAi :: Effect ROOT () Model Action
triggerAi = do
  m <- get
  let gs = mGameState m
  if mGameMode m == AiMode && not (finished (gsResult gs)) && mAiSide m == turnSide gs
    then do
      modify $ \x -> x { mAiThinking = True }
      let cfg = AiConfig (mAiDepth m) (mAiNodeLimit m)
      withSink $ \sink -> do
        threadDelay 100000
        case bestMove cfg gs of
          Nothing   -> sink NoOp
          Just move -> sink (AiMoveComplete move)
    else pure ()

-- | Convert a Side to its DB string representation.
sideStr :: Side -> MisoString
sideStr AttackerSide = "attacker"
sideStr DefenderSide = "defender"

-- | Parse the "new" row from a Supabase Realtime Postgres Changes payload.
parseRealtimeRow :: Value -> Maybe GameRow
parseRealtimeRow val =
  case parseMaybe parsePayload val of
    Just gr -> Just gr
    Nothing -> Nothing
  where
    parsePayload = withObject "RealtimePayload" $ \o -> do
      newVal <- o .: "new"
      parseJSON newVal

-- | Parse a TimeControl from GameRow fields.
parseTimeControl :: GameRow -> TimeControl
parseTimeControl gr = case grwTimeControl gr of
  Just "blitz" -> BlitzControl (fromMaybe 0 (grwTimePerPlayerMs gr))
  Just "daily" -> DailyControl (fromMaybe 0 (grwTimePerMoveSec gr))
  _            -> NoTimeControl

-- | Set Model clock fields from a GameRow.
applyClockFromRow :: GameRow -> Model -> Model
applyClockFromRow gr m = m
  { mTimeControl    = parseTimeControl gr
  , mAttackerTimeMs = fromMaybe 0 (grwAttackerTimeMs gr)
  , mDefenderTimeMs = fromMaybe 0 (grwDefenderTimeMs gr)
  , mLastMoveAt     = grwLastMoveAt gr
  , mMoveDeadline   = grwMoveDeadline gr
  }

-- | Start the blitz clock timer, dispatching ClockTick and ClockTimeout.
startBlitzClock :: Effect ROOT () Model Action
startBlitzClock = do
  m <- get
  -- Stop any existing timer and clear the ID synchronously
  case mClockTimerId m of
    Just tid -> io_ $ js_stopGameClock tid
    Nothing  -> pure ()
  modify $ \x -> x { mClockTimerId = Nothing }
  case mTimeControl m of
    BlitzControl _ -> do
      let atkMs = mAttackerTimeMs m
          defMs = mDefenderTimeMs m
          turn  = turnSide (mGameState m)
          turnStr = sideStr turn
      withSink $ \sink -> do
        turnJsv <- toJSVal turnStr
        lmaJsv  <- toJSVal (fromMaybe "" (mLastMoveAt m))
        tickCb    <- Function <$> asyncCallback2 (\atkV defV -> do
          atk <- fromJSValUnchecked atkV
          def' <- fromJSValUnchecked defV
          sink (ClockTick atk def'))
        timeoutCb <- Function <$> asyncCallback1 (\sideV -> do
          s <- fromJSValUnchecked sideV
          sink (ClockTimeout s))
        tid <- js_startGameClock atkMs defMs turnJsv lmaJsv tickCb timeoutCb
        sink (ClockStarted tid)
    _ -> modify $ \x -> x { mClockTimerId = Nothing }

-- | Start a daily clock that ticks every 30s to refresh the deadline display.
startDailyClock :: Effect ROOT () Model Action
startDailyClock = do
  m <- get
  case mClockTimerId m of
    Just tid -> io_ $ js_stopGameClock tid
    Nothing  -> pure ()
  modify $ \x -> x { mClockTimerId = Nothing }
  case mTimeControl m of
    DailyControl _ -> do
      withSink $ \sink -> do
        tickCb <- Function <$> asyncCallback (sink DailyTick)
        tid <- js_startDailyClock tickCb
        sink (ClockStarted tid)
    _ -> modify $ \x -> x { mClockTimerId = Nothing }

-- | Stop the blitz clock timer.
-- Always calls JS to clear the singleton timer, even if mClockTimerId is stale.
stopClock :: Effect ROOT () Model Action
stopClock = do
  m <- get
  case mClockTimerId m of
    Just tid -> io_ $ js_stopGameClock tid
    Nothing  -> io_ $ js_stopGameClock 0  -- clears singleton _gameClockId in JS
  modify $ \x -> x { mClockTimerId = Nothing }

-- | Save the current finished game (Supabase update if authenticated with gameId, localStorage if guest).
saveGame :: Effect ROOT () Model Action
saveGame = do
  m <- get
  let gs = mGameState m
      result = gsResult gs
      winnerStr = fmap (\s -> case s of
        AttackerSide -> "attacker" :: MisoString
        DefenderSide -> "defender") (winner result)
      gameModeStr = case mGameMode m of
        PracticeMode       -> "local" :: MisoString
        AiMode          -> "ai"
        MultiplayerMode -> "multiplayer"
      aiSideStr = if mGameMode m == AiMode
        then Just (case mAiSide m of
          AttackerSide -> "attacker" :: MisoString
          DefenderSide -> "defender")
        else Nothing
      aiDepthVal = if mGameMode m == AiMode
        then Just (mAiDepth m)
        else (Nothing :: Maybe Int)
  case (mSession m, mGameId m) of
    (Just _, Just gid) -> do
      let updateData = object
            [ "result_desc" .= ms (desc result)
            , "winner"      .= winnerStr
            , "total_moves" .= gsTurn gs
            , "moves"       .= mMoveList m
            ]
      updateTable "games" updateData
        [eq "id" gid]
        (UpdateOptions Nothing)
        GameUpdated GameUpdateError
    _ -> do
      let gameData = object
            [ "variant"     .= variantSlug (mVariant m)
            , "winner"      .= winnerStr
            , "result_desc" .= ms (desc result)
            , "total_moves" .= gsTurn gs
            , "game_mode"   .= gameModeStr
            , "ai_side"     .= aiSideStr
            , "ai_depth"    .= aiDepthVal
            , "moves"       .= mMoveList m
            ]
      io_ $ saveLocalGameIO gameData

-- | Load past games (Supabase if authenticated, localStorage if guest).
loadPastGames :: Effect ROOT () Model Action
loadPastGames = do
  m <- get
  case mSession m of
    Nothing ->
      withSink $ \sink -> do
        okCb <- successCallback sink
          (\_ -> LocalGamesLoaded [])
          LocalGamesLoaded
        errCb <- errorCallback sink
          (\_ -> LocalGamesLoaded [])
        js_loadLocalGames okCb errCb
    Just sess -> do
      modify $ \x -> x { mGamesLoading = True }
      let uid = userId (sessionUser sess)
      selectWithFilters "games" "*"
        [eq "user_id" uid, neq "result_desc" ("in_progress" :: MisoString)]
        (FetchOptions Nothing Nothing)
        GamesLoaded GamesLoadError

-- | Migrate local games to Supabase on auth success.
migrateLocalGames :: Session -> Effect ROOT () Model Action
migrateLocalGames sess =
  withSink $ \sink -> do
    okCb <- successCallback sink
      (\_ -> NoOp)
      (\games -> if null (games :: [GameRecord]) then NoOp
                 else DoMigrateGames (userId (sessionUser sess)) games)
    errCb <- errorCallback sink
      (\_ -> NoOp)
    js_loadLocalGames okCb errCb

-- | Load the user's profile from Supabase.
loadProfile :: Session -> Effect ROOT () Model Action
loadProfile sess = do
  let uid = userId (sessionUser sess)
  selectWithFilters "profiles" "*"
    [eq "id" uid]
    (FetchOptions Nothing Nothing)
    ProfileLoaded ProfileLoadError

-- | Get the game state to display (current or browsed).
displayedGameState :: Model -> GameState
displayedGameState m = case mBrowseIndex m of
  Nothing -> mGameState m
  Just i  -> let allStates = mHistory m ++ [mGameState m]
             in if i >= 0 && i < length allStates
                then allStates !! i
                else mGameState m
