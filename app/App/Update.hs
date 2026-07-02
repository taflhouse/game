{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Update (updateModel) where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=), (.:), parseMaybe, withObject)
import Miso.Lens (assign, use)
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously, getSession, getUser
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq)
import Supabase.Miso.Realtime (Channel, subscribeToTable, removeChannel)


import App.JSON (GameRow(..), Profile(..), GameRecord(..))
import App.Model
import App.Action
import App.Route
import App.FFI

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: IORef (Maybe Channel) -> Action -> Effect ROOT () Model Action
updateModel loungeChannelRef = \case
  NoOp -> pure ()

  -- Navigation -----------------------------------------------------------

  StartGame ->
    io (StartGameWithId <$> js_generateUUID)

  StartGameWithId uuid -> do
    m <- get
    modify $ \x -> x
      { mGameInitData = Just (NewLocalGame uuid (mVariant m) (mGameMode m)
                               (mAiSide m) (mAiDepth m) (mAiNodeLimit m))
      , mScreen = GameScreen
      }

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

  -- Game config ----------------------------------------------------------

  SetGameMode mode -> do
    modify $ \m -> m
      { mGameMode = mode
      , mTimeControl = case mode of
          MultiplayerMode -> BlitzControl 1200000  -- 20 min default
          _               -> NoTimeControl
      }
    io_ $ pushURI (configureURI (gameModeSlug mode))

  SetVariant variant ->
    modify $ \m -> m { mVariant = variant }

  SetAiSide side ->
    modify $ \m -> m { mAiSide = side }

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

  -- URI handling ---------------------------------------------------------

  GotoLounge ->
    io_ $ pushURI loungeURI

  HandleURI uri -> do
    modify $ \x -> x { mToast = Nothing }
    m <- get
    -- Clean up lounge channel when leaving LoungeScreen
    when (mScreen m == LoungeScreen && not (isLoungeRoute (parseRoute uri))) $
      io_ $ do
        mCh <- readIORef loungeChannelRef
        case mCh of
          Just ch -> removeChannel ch
          Nothing -> pure ()
        writeIORef loungeChannelRef Nothing
    case parseRoute uri of
      PlayRoute uuid
        | mScreen m == GameScreen
        , Just initData <- mGameInitData m
        , gameInitUuid initData == uuid -> pure ()
        | otherwise -> do
            modify $ \x -> x { mScreen = LoadingScreen }
            selectWithFilters "games" "*"
              [eq "id" uuid]
              (FetchOptions Nothing Nothing)
              ResumeGameLoaded ResumeGameLoadError
      GameRoute uuid ->
        modify $ \x -> x
          { mScreen       = ReplayScreen
          , mReplayGameId = Just uuid
          }
      HomeRoute -> do
        modify $ \x -> x { mScreen = HomeScreen, mGameInitData = Nothing }
        loadPastGames
      SignInRoute -> do
        modify $ \x -> x { mScreen = SignInScreen }
        assign (mAuth . authError) Nothing
        assign (mAuth . authMessage) Nothing
      SignUpRoute -> do
        modify $ \x -> x { mScreen = SignUpScreen }
        assign (mAuth . authError) Nothing
        assign (mAuth . authMessage) Nothing
      ConfigRoute ->
        modify $ \x -> x { mScreen = ConfigScreen, mConfigModeChosen = False
                         , mTimeControl = NoTimeControl }
      ConfigureRoute mSlug ->
        modify $ \x -> x
          { mScreen = ConfigureScreen
          , mGameMode = fromMaybe AiMode (mSlug >>= slugToGameMode)
          }
      ProfileRoute ->
        modify $ \x -> x { mScreen = ProfileScreen }
      ProfileEditRoute -> do
        m' <- get
        modify $ \x -> x
          { mScreen          = ProfileEditScreen
          , mEditUsername     = maybe "" pUsername (mProfile m')
          , mEditDisplayName = maybe "" (maybe "" id . pDisplayName) (mProfile m')
          }
      LoungeRoute -> do
        -- Clean up any existing lounge channel before re-subscribing
        io_ $ do
          mCh <- readIORef loungeChannelRef
          case mCh of
            Just ch -> removeChannel ch
            Nothing -> pure ()
          writeIORef loungeChannelRef Nothing
        modify $ \x -> x
          { mScreen = LoungeScreen
          , mLoungeLoading = True
          , mLoungeFilter = Nothing
          }
        loadLoungeGames
        subscribeToTable "lounge" "games" ""
          LoungeRealtimeChange LoungeRealtimeSubscribed LoungeRealtimeError
      JoinRoute mCode -> do
        modify $ \x -> x
          { mScreen        = JoinScreen
          , mJoinCodeInput = fromMaybe (mJoinCodeInput x) mCode
          }
        case mCode of
          Just _ -> do
            m' <- get
            when (hasDisplayName m') $
              withSink $ \sink -> sink JoinMultiplayerGame
          Nothing -> pure ()

  -- Home UI --------------------------------------------------------------

  ToggleQuoteRef -> do
    m <- get
    let opening = not (mShowQuoteRef m)
        gen = mQuoteRefGen m + 1
    modify $ \x -> x { mShowQuoteRef = opening, mQuoteRefGen = gen }
    when opening $
      withSink $ \sink -> do
        threadDelay 5000000
        sink (DismissQuoteRefTimed gen)

  DismissQuoteRef ->
    modify $ \m -> m { mShowQuoteRef = False }

  DismissQuoteRefTimed gen -> do
    m <- get
    when (mQuoteRefGen m == gen) $
      modify $ \x -> x { mShowQuoteRef = False }

  ToggleTheme ->
    io_ js_toggleDarkMode

  -- Toast ----------------------------------------------------------------

  ShowToast msg -> do
    modify $ \m -> m { mToast = Just msg }
    withSink $ \sink -> do
      threadDelay 5000000
      sink DismissToast

  DismissToast ->
    modify $ \m -> m { mToast = Nothing }

  -- Config UI ------------------------------------------------------------

  ToggleDepthInfo ->
    modify $ \m -> m { mShowDepthInfo = not (mShowDepthInfo m) }

  ToggleNodesInfo ->
    modify $ \m -> m { mShowNodesInfo = not (mShowNodesInfo m) }

  -- Auth -----------------------------------------------------------------

  SetAuthEmail e ->
    assign (mAuth . authEmail) e

  SetAuthPassword p ->
    assign (mAuth . authPassword) p

  DoSignIn -> do
    email <- use (mAuth . authEmail)
    pwd   <- use (mAuth . authPassword)
    assign (mAuth . authLoading) True
    assign (mAuth . authError) Nothing
    assign (mAuth . authMessage) Nothing
    let creds = SignInCredentials
          { sicEmail    = Email email
          , sicPassword = Password pwd
          }
    signInWithPassword creds AuthSuccess AuthError

  DoSignUp -> do
    email <- use (mAuth . authEmail)
    pwd   <- use (mAuth . authPassword)
    assign (mAuth . authLoading) True
    assign (mAuth . authError) Nothing
    assign (mAuth . authMessage) Nothing
    let signup = SignUpEmail
          { sueEmail    = Email email
          , suePassword = pwd
          , sueOptions  = Nothing
          }
    signUpEmail signup AuthSuccess AuthError

  AuthSuccess resp ->
    case adSession (arData resp) of
      Just sess -> do
        modify $ \m -> m
          { mSession    = Just sess
          , mLocalGames = []
          }
        assign mAuth initAuthState
        migrateLocalGames sess
        loadProfile sess
        io_ $ pushURI homeURI
      Nothing -> do
        assign mAuth initAuthState
        assign (mAuth . authMessage) (Just "Check your email to confirm your account.")

  AuthError msg -> do
    assign (mAuth . authError) (Just (friendlyAuthError msg))
    assign (mAuth . authLoading) False

  AnonAuthSuccess resp ->
    case adSession (arData resp) of
      Just sess -> do
        let uid   = userId (sessionUser sess)
            gName = guestNameFromId uid
        modify $ \m -> m { mSession = Just sess
                         , mGuestName = Just (if mJoinNameInput m /= ""
                                              then mJoinNameInput m else gName) }
        m <- get
        case mDeferredMpAction m of
          Just DeferCreate -> withSink $ \sink -> sink CreateMultiplayerGame
          Just DeferJoin   -> withSink $ \sink -> sink JoinMultiplayerGame
          Nothing          -> pure ()
        modify $ \x -> x { mDeferredMpAction = Nothing }
      Nothing ->
        modify $ \m -> m { mToast = Just "Anonymous sign-in failed", mDeferredMpAction = Nothing }

  AnonAuthError msg ->
    modify $ \m -> m { mToast = Just ("Sign-in failed: " <> msg), mDeferredMpAction = Nothing }

  DoSignOut -> do
    m <- get
    when (mScreen m == LoungeScreen) $
      io_ $ do
        mCh <- readIORef loungeChannelRef
        case mCh of
          Just ch -> removeChannel ch
          Nothing -> pure ()
        writeIORef loungeChannelRef Nothing
    signOut defaultSignOutOptions SignOutSuccess AuthError
    assign (mAuth . authError) Nothing
    modify $ \x -> x
      { mSession         = Nothing
      , mScreen          = HomeScreen
      , mPastGames       = []
      , mLocalGames      = []
      , mProfile         = Nothing
      , mNeedsUsername    = False
      , mProfileDropdown = False
      , mViewMode        = NormalView
      , mGuestName       = Nothing
      , mGameInitData    = Nothing
      }
    io_ $ pushURI homeURI

  SignOutSuccess _ -> pure ()

  CheckSession ->
    getSession
      (maybe (SessionRestored Nothing) ValidateSession)
      (\_ -> SessionRestored Nothing)

  ValidateSession sess ->
    getUser
      (\mUser -> case mUser of
        Just _  -> SessionRestored (Just sess)
        Nothing -> DoSignOut)
      (\_ -> DoSignOut)

  SessionRestored mSess -> do
    modify $ \m -> m { mSession = mSess, mSessionChecked = True }
    case mSess of
      Just sess
        | amProvider (userAppMetadata (sessionUser sess)) == "anonymous" -> do
            let uid = userId (sessionUser sess)
            modify $ \m' -> m' { mGuestName = Just (guestNameFromId uid) }
            m' <- get
            when (mScreen m' == JoinScreen && mJoinCodeInput m' /= "") $
              withSink $ \sink -> sink JoinMultiplayerGame
        | otherwise -> do
            loadPastGames
            loadProfile sess
      Nothing -> pure ()

  -- Games / migration ----------------------------------------------------

  GamesLoaded val ->
    case fromJSON val of
      Success games -> modify $ \m -> m { mPastGames = games, mGamesLoading = False }
      Error _       -> modify $ \m -> m { mPastGames = [], mGamesLoading = False }

  GamesLoadError _ ->
    modify $ \m -> m { mGamesLoading = False }

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
      insert "games" gameData (InsertOptions Nothing Nothing) (\_ -> NoOp) (\_ -> NoOp)
      ) games
    io_ js_clearLocalGames
    loadPastGames

  -- Profile --------------------------------------------------------------

  SetUsernameInput s ->
    modify $ \m -> m { mUsernameInput = s }

  SubmitUsername -> do
    m <- get
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
        insert "profiles"
          (object ["id" .= uid, "username" .= mUsernameInput m])
          (InsertOptions Nothing Nothing)
          ProfileCreated ProfileCreateError
      Nothing -> pure ()

  ProfileCreated _ -> do
    m <- get
    modify $ \x -> x
      { mProfile       = Just (Profile (mUsernameInput m) Nothing)
      , mNeedsUsername  = False
      , mUsernameInput  = ""
      }

  ProfileCreateError _ ->
    assign (mAuth . authError) (Just "Something went wrong. Please try again.")

  ProfileLoaded val ->
    case fromJSON val of
      Success profiles -> case (profiles :: [Profile]) of
        (p:_) -> do
          modify $ \m -> m { mProfile = Just p, mNeedsUsername = False }
          m' <- get
          when (mScreen m' == JoinScreen && mJoinCodeInput m' /= "") $
            withSink $ \sink -> sink JoinMultiplayerGame
        []    -> modify $ \m -> m { mNeedsUsername = True }
      Error _ -> modify $ \m -> m { mNeedsUsername = True }

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
      { mProfileDropdown  = False
      , mEditUsername      = maybe "" pUsername (mProfile m)
      , mEditDisplayName   = maybe "" (maybe "" id . pDisplayName) (mProfile m)
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
        let uid   = userId (sessionUser sess)
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
      { mProfile = Just (Profile (mEditUsername m) (Just (mEditDisplayName m))) }
    io_ $ pushURI profileURI

  ProfileUpdateError _ ->
    assign (mAuth . authError) (Just "Something went wrong. Please try again.")

  -- Multiplayer setup ----------------------------------------------------

  CreateMultiplayerGame -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferCreate }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> do
        when (mJoinNameInput m /= "") $
          modify $ \x -> x { mGuestName = Just (mJoinNameInput x) }
        io $ do
          invCode <- generateInviteCode
          uuid    <- js_generateUUID
          origin  <- js_getOrigin
          qrUrl   <- js_generateQRDataURL (origin <> "/join/" <> invCode)
          pure (InitMultiplayerGame invCode uuid qrUrl)

  InitMultiplayerGame invCode uuid qrUrl -> do
    m <- get
    modify $ \x -> x
      { mGameInitData = Just (NewMultiplayerGame (mVariant m) (mTimeControl m)
                               (mSidePreference m) invCode uuid qrUrl)
      , mScreen = GameScreen
      }

  JoinMultiplayerGame -> do
    m <- get
    if not (hasDisplayName m || mJoinNameInput m /= "")
      then pure ()  -- Wait for name input
      else do
        -- For non-anonymous logged-in users without a profile, save the name
        when (mJoinNameInput m /= "") $
          case (mSession m, mProfile m) of
            (Just sess, Nothing)
              | amProvider (userAppMetadata (sessionUser sess)) /= "anonymous" -> do
                  let uid = userId (sessionUser sess)
                  insert "profiles"
                    (object ["id" .= uid, "username" .= mJoinNameInput m])
                    (InsertOptions Nothing Nothing)
                    (\_ -> NoOp) (\_ -> NoOp)
                  modify $ \x -> x
                    { mProfile = Just (Profile (mJoinNameInput x) Nothing)
                    , mNeedsUsername = False
                    }
            _ -> pure ()
        m' <- get
        case mSession m' of
          Nothing -> do
            modify $ \x -> x { mDeferredMpAction = Just DeferJoin }
            signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
          Just _ -> do
            when (mJoinNameInput m' /= "") $
              modify $ \x -> x { mGuestName = Just (mJoinNameInput x) }
            let code = mJoinCodeInput m'
            when (code /= "") $
              selectWithFilters "games" "*"
                [eq "invite_code" code, eq "status" ("waiting" :: MisoString)]
                (FetchOptions Nothing Nothing)
                GameFoundToJoin GameJoinError

  GameFoundToJoin val ->
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) ->
          modify $ \m -> m
            { mGameInitData = Just (JoinGame gr)
            , mScreen       = GameScreen
            }
        [] ->
          modify $ \m -> m { mToast = Just "No waiting game found with that code." }
      Error _ ->
        modify $ \m -> m { mToast = Just "Failed to look up game." }

  GameJoinError msg ->
    modify $ \m -> m { mToast = Just ("Join error: " <> msg) }

  ResumeGameLoaded val ->
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) ->
          modify $ \m -> m
            { mGameInitData = Just (ResumeGame gr)
            , mScreen       = GameScreen
            }
        [] -> modify $ \m -> m { mToast = Just "Game not found." }
      Error _ -> modify $ \m -> m { mToast = Just "Failed to load game." }

  ResumeGameLoadError msg ->
    modify $ \m -> m { mToast = Just ("Load error: " <> msg) }

  SetJoinCodeInput s ->
    modify $ \m -> m { mJoinCodeInput = s }

  SetJoinNameInput s ->
    modify $ \m -> m { mJoinNameInput = s }

  SetSidePreference s ->
    modify $ \m -> m { mSidePreference = s }

  SetTimeControl tc ->
    modify $ \m -> m { mTimeControl = tc }

  -- Lounge ---------------------------------------------------------------

  LoungeOpenLoaded val ->
    case fromJSON val of
      Success games -> modify $ \x -> x { mLoungeOpen = games, mLoungeLoading = False }
      Error _       -> modify $ \x -> x { mLoungeOpen = [], mLoungeLoading = False }

  LoungeLiveLoaded val ->
    case fromJSON val of
      Success games ->
        withSink $ \sink -> do
          recent <- filterRecentGames games
          sink (LoungeLiveFiltered recent)
      Error _       -> modify $ \x -> x { mLoungeLive = [], mLoungeLoading = False }

  LoungeLiveFiltered games ->
    modify $ \x -> x { mLoungeLive = games, mLoungeLoading = False }

  LoungeLoadError _ ->
    modify $ \x -> x { mLoungeLoading = False }

  LoungeRealtimeChange _ -> do
    m <- get
    when (mScreen m == LoungeScreen) $
      loadLoungeGames

  LoungeRealtimeSubscribed ch ->
    io_ $ writeIORef loungeChannelRef (Just ch)

  LoungeRealtimeError _ -> pure ()

  SetLoungeFilter mFilter ->
    modify $ \x -> x { mLoungeFilter = mFilter }

  JoinFromLounge code ->
    io_ $ pushURI (joinURI code)

  -- Replay ---------------------------------------------------------------

  GotoReplay gid ->
    io_ $ pushURI (emptyURI { uriPath = "games/" <> gid })

  -- View mode (replay only; game component manages its own) --------------

  ToggleZenMode -> do
    m <- get
    let entering = mViewMode m == NormalView
    modify $ \x -> x { mViewMode = if entering then ZenView else NormalView
                      , mZenHint  = entering }
    when entering $
      withSink $ \sink -> do
        threadDelay 4000000
        sink DismissZenHint

  DismissZenHint ->
    modify $ \m -> m { mZenHint = False }

  DocumentDblClick -> do
    m <- get
    when (mScreen m == ReplayScreen) $
      updateModel loungeChannelRef ToggleZenMode

  ToggleFullscreen ->
    modify $ \m -> m { mIsFullscreen = not (mIsFullscreen m) }

  Undo -> pure ()  -- game component handles undo internally

  -- Game component mailbox -----------------------------------------------

  GameMailbox val ->
    case parseMaybe (withObject "Mailbox" (\o -> o .: "type")) val of
      Just ("toast" :: MisoString) ->
        case parseMaybe (withObject "Mailbox" (\o -> o .: "msg")) val of
          Just msg -> updateModel loungeChannelRef (ShowToast msg)
          Nothing  -> pure ()
      Just "game_finished" -> loadPastGames
      Just "game_unmounted" ->
        modify $ \m -> m { mGameInitData = Nothing }
      Just "toggle_zen" -> updateModel loungeChannelRef ToggleZenMode
      Just "toggle_fullscreen" -> updateModel loungeChannelRef ToggleFullscreen
      Just "replay_unmounted" ->
        modify $ \m -> m { mReplayGameId = Nothing }
      _ -> pure ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract the UUID from any GameInitData variant.
gameInitUuid :: GameInitData -> MisoString
gameInitUuid (NewLocalGame uuid _ _ _ _ _)        = uuid
gameInitUuid (NewMultiplayerGame _ _ _ _ uuid _)   = uuid
gameInitUuid (JoinGame gr)                         = grwId gr
gameInitUuid (ResumeGame gr)                       = grwId gr

-- | Load past games (Supabase if authenticated, localStorage if guest).
loadPastGames :: Effect ROOT () Model Action
loadPastGames = do
  m <- get
  case mSession m of
    Nothing ->
      withSink $ \sink -> do
        okCb  <- successCallback sink
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
    okCb  <- successCallback sink
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

-- | Does the user already have a display name (profile username or guest name)?
hasDisplayName :: Model -> Bool
hasDisplayName m = case mProfile m of
  Just p | pUsername p /= "" -> True
  _ -> mGuestName m /= Nothing

-- | Load all open and live games for the lounge (filtering is done client-side).
loadLoungeGames :: Effect ROOT () Model Action
loadLoungeGames = do
  selectWithFilters "games" "*"
    [eq "status" ("waiting" :: MisoString)]
    (FetchOptions Nothing Nothing)
    LoungeOpenLoaded LoungeLoadError
  selectWithFilters "games" "*"
    [eq "status" ("active" :: MisoString)]
    (FetchOptions Nothing Nothing)
    LoungeLiveLoaded LoungeLoadError

-- | Filter active games to only those with activity in the last 30 minutes.
filterRecentGames :: [GameRow] -> IO [GameRow]
filterRecentGames = filterM isRecent
  where
    thirtyMinMs = 30 * 60 * 1000 :: Int
    isRecent gr = case grwLastMoveAt gr of
      Just ts -> do
        elapsed <- js_elapsedMs ts
        pure (elapsed < thirtyMinMs)
      Nothing -> pure False  -- no last_move_at means no moves yet; skip

-- | Check if a Route is the LoungeRoute.
isLoungeRoute :: Route -> Bool
isLoungeRoute LoungeRoute = True
isLoungeRoute _           = False
