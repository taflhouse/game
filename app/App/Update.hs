{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Update (updateModel) where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=), (.:), parseMaybe, withObject)
import Miso.Lens (assign, use)
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq)


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
    modify $ \m -> m { mGameMode = mode }
    io_ $ pushURI configureURI

  SetVariant variant ->
    modify $ \m -> m { mVariant = variant }

  SetAiSide side ->
    modify $ \m -> m { mAiSide = side }

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

  -- URI handling ---------------------------------------------------------

  HandleURI uri -> do
    modify $ \x -> x { mToast = Nothing }
    m <- get
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
      ConfigureRoute ->
        modify $ \x -> x { mScreen = ConfigureScreen }
      ProfileRoute ->
        modify $ \x -> x { mScreen = ProfileScreen }
      ProfileEditRoute -> do
        m' <- get
        modify $ \x -> x
          { mScreen          = ProfileEditScreen
          , mEditUsername     = maybe "" pUsername (mProfile m')
          , mEditDisplayName = maybe "" (maybe "" id . pDisplayName) (mProfile m')
          }
      JoinRoute mCode -> do
        modify $ \x -> x
          { mScreen        = JoinScreen
          , mJoinCodeInput = fromMaybe (mJoinCodeInput x) mCode
          }
        case mCode of
          Just _  -> withSink $ \sink -> sink JoinMultiplayerGame
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
        modify $ \m -> m { mSession = Just sess, mGuestName = Just gName }
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

  SessionRestored mSess -> do
    modify $ \m -> m { mSession = mSess }
    case mSess of
      Just sess
        | amProvider (userAppMetadata (sessionUser sess)) == "anonymous" -> do
            let uid = userId (sessionUser sess)
            modify $ \m' -> m' { mGuestName = Just (guestNameFromId uid) }
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
        (p:_) -> modify $ \m -> m { mProfile = Just p, mNeedsUsername = False }
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
      Just _ -> io $ do
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

  SetSidePreference s ->
    modify $ \m -> m { mSidePreference = s }

  SetTimeControl tc ->
    modify $ \m -> m { mTimeControl = tc }

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
      updateModel ToggleZenMode

  ToggleFullscreen ->
    modify $ \m -> m { mIsFullscreen = not (mIsFullscreen m) }

  Undo -> pure ()  -- game component handles undo internally

  -- Game component mailbox -----------------------------------------------

  GameMailbox val ->
    case parseMaybe (withObject "Mailbox" (\o -> o .: "type")) val of
      Just ("toast" :: MisoString) ->
        case parseMaybe (withObject "Mailbox" (\o -> o .: "msg")) val of
          Just msg -> updateModel (ShowToast msg)
          Nothing  -> pure ()
      Just "game_finished" -> loadPastGames
      Just "game_unmounted" ->
        modify $ \m -> m { mGameInitData = Nothing }
      Just "toggle_zen" -> updateModel ToggleZenMode
      Just "toggle_fullscreen" -> updateModel ToggleFullscreen
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
