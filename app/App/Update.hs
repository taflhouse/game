{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Update (updateModel) where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Miso hiding ((!!))
import Miso.DSL (fromJSValUnchecked, asyncCallback, asyncCallback1, Function(..))
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=), (.:), parseMaybe, withObject)
import Miso.Lens (assign, use)
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously, getSession, getUser
  , SignUpEmail(..), SignUpEmailOptions(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions, defaultSignUpEmailOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq, gt)
import Supabase.Miso.Realtime (Channel, subscribeToTable, removeChannel)


import Tafl.Rules (variantSlug)

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

  GotoLearn ->
    io_ $ pushURI learnURI

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
    io_ $ pushURI homeURI

  HandleURI uri -> do
    modify $ \x -> x { mToast = Nothing }
    m <- get
    -- Clean up lounge channel when leaving home screen (lounge)
    when (mScreen m == HomeScreen && not (isHomeRoute (parseRoute uri))) $
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
              (FetchOptions Nothing Nothing Nothing Nothing)
              ResumeGameLoaded ResumeGameLoadError
      GameRoute uuid ->
        modify $ \x -> x
          { mScreen       = ReplayScreen
          , mReplayGameId = Just uuid
          }
      HomeRoute -> do
        -- Clean up any existing lounge channel before re-subscribing
        io_ $ do
          mCh <- readIORef loungeChannelRef
          case mCh of
            Just ch -> removeChannel ch
            Nothing -> pure ()
          writeIORef loungeChannelRef Nothing
        modify $ \x -> x
          { mScreen = HomeScreen
          , mGameInitData = Nothing
          , mLoungeLoading = True
          , mLoungeFilter = Nothing
          , mLoungeOpen = []
          , mLoungeLive = []
          , mRankings = []
          }
        loadLoungeGames
        loadRankings
        subscribeToTable "lounge" "games" ""
          LoungeRealtimeChange LoungeRealtimeSubscribed LoungeRealtimeError
      PlayerRoute uname -> do
        modify $ \x -> x
          { mScreen = PlayerScreen
          , mPlayerDetail = Nothing
          , mPlayerGames = []
          , mPlayerGamesLoading = True
          }
        selectWithFilters "profiles" "*"
          [eq "username" uname]
          (FetchOptions Nothing Nothing Nothing Nothing)
          PlayerProfileLoaded PlayerProfileLoadError
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
      LoungeRoute ->
        io_ $ pushURI homeURI  -- lounge is now the home screen
      YourGamesRoute -> do
        modify $ \x -> x { mScreen = YourGamesScreen, mGamesLoading = True }
        loadPastGames
      LearnRoute ->
        modify $ \x -> x { mScreen = LearnScreen, mTutorialLessonId = Nothing }
      LearnLessonRoute lid ->
        modify $ \x -> x { mScreen = LearnScreen, mTutorialLessonId = Just lid }
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
    m <- get
    assign (mAuth . authLoading) True
    assign (mAuth . authError) Nothing
    assign (mAuth . authMessage) Nothing
    let hasJoinCode = mDeferredMpAction m == Just DeferJoin && mJoinCodeInput m /= ""
    if hasJoinCode
      then io $ do
        origin <- js_getOrigin
        let opts = Just defaultSignUpEmailOptions
              { sueEmailRedirectTo = Just (origin <> "/join/" <> mJoinCodeInput m) }
            signup = SignUpEmail
              { sueEmail = Email email, suePassword = pwd, sueOptions = opts }
        pure (DoSignUpWith signup)
      else do
        let signup = SignUpEmail
              { sueEmail = Email email, suePassword = pwd, sueOptions = Nothing }
        signUpEmail signup AuthSuccess AuthError

  DoSignUpWith signup ->
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
        m <- get
        case mDeferredMpAction m of
          Just DeferJoin -> do
            modify $ \x -> x { mDeferredMpAction = Nothing, mPendingRatedJoin = Nothing }
            io_ $ pushURI (joinURI (mJoinCodeInput m))
          _ -> io_ $ pushURI homeURI
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
                                              then mJoinNameInput m else gName)
                         , mJoinNameInput = if mJoinNameInput m /= ""
                                              then mJoinNameInput m else gName }
        m <- get
        case mDeferredMpAction m of
          Just DeferCreate         -> withSink $ \sink -> sink CreateMultiplayerGame
          Just DeferJoin           -> withSink $ \sink -> sink JoinMultiplayerGame
          Just DeferFindMatch      -> withSink $ \sink -> sink FindMatch
          Just DeferToggleInterest -> withSink $ \sink -> sink ToggleMatchInterest
          Nothing                  -> pure ()
        modify $ \x -> x { mDeferredMpAction = Nothing }
      Nothing ->
        modify $ \m -> m { mToast = Just "Anonymous sign-in failed", mDeferredMpAction = Nothing }

  AnonAuthError msg ->
    modify $ \m -> m { mToast = Just ("Sign-in failed: " <> msg), mDeferredMpAction = Nothing }

  DoSignOut -> do
    m <- get
    when (mScreen m == HomeScreen) $
      io_ $ do
        mCh <- readIORef loungeChannelRef
        case mCh of
          Just ch -> removeChannel ch
          Nothing -> pure ()
        writeIORef loungeChannelRef Nothing
    -- Clean up match interest channel
    case mMatchInterestChannel m of
      Just ch -> io_ $ removeChannel ch
      Nothing -> pure ()
    io_ $ js_setLocalStorage "taflhouse_ready" "false"
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
      , mMatchInterested      = False
      , mMatchInterestChannel = Nothing
      , mMatchToast           = Nothing
      , mMatchModal           = Nothing
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
    -- If a game was fetched while session was being restored, show it now.
    pendingM <- get
    case (mGameInitData pendingM, mScreen pendingM) of
      (Just (ResumeGame gr), LoadingScreen)
        | grwStatus gr == "finished" && not (isParticipant mSess gr) ->
            io_ $ replaceURI (gamePermalinkURI (grwId gr))
        | otherwise ->
            modify $ \x -> x { mScreen = GameScreen }
      _ -> pure ()
    case mSess of
      Just sess
        | amProvider (userAppMetadata (sessionUser sess)) == "anonymous" -> do
            let uid = userId (sessionUser sess)
                gName = guestNameFromId uid
            modify $ \m' -> m' { mGuestName = Just gName
                               , mJoinNameInput = if mJoinNameInput m' == ""
                                                    then gName else mJoinNameInput m' }
            m' <- get
            when (mScreen m' == JoinScreen && mJoinCodeInput m' /= "") $
              withSink $ \sink -> sink JoinMultiplayerGame
        | otherwise -> do
            loadPastGames
            loadProfile sess
      Nothing -> pure ()
    -- Restore "Ready" toggle and preferences from localStorage
    when (mSess /= Nothing) $
      withSink $ \sink -> do
        val <- js_getLocalStorage "taflhouse_ready"
        when (val == "true") $ do
          anyVal   <- js_getLocalStorage "taflhouse_match_any"
          ratedVal <- js_getLocalStorage "taflhouse_match_rated"
          timedVal <- js_getLocalStorage "taflhouse_match_timed"
          sideVal  <- js_getLocalStorage "taflhouse_match_side"
          sink (SetMatchAny (anyVal /= "false"))
          when (ratedVal /= "") $ sink (SetMatchWantRated ratedVal)
          when (timedVal /= "") $ sink (SetMatchWantTimed timedVal)
          when (sideVal  /= "") $ sink (SetMatchWantSide sideVal)
          sink ConfirmMatchFilters
    -- Check push notification status
    withSink $ \sink -> do
      permState <- js_getNotificationPermissionState
      brave     <- js_isBraveBrowser
      firefox   <- js_isFirefoxBrowser
      safari    <- js_isSafariBrowser
      edge      <- js_isEdgeBrowser
      macOS     <- js_isMacOS
      sink (InitPushStatus permState brave firefox safari edge macOS)

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
    let uid = maybe "" (userId . sessionUser) (mSession m)
    modify $ \x -> x
      { mProfile       = Just (Profile uid (mUsernameInput m) Nothing 1500.0 350.0 0)
      , mNeedsUsername  = False
      , mUsernameInput  = ""
      }
    m' <- get
    when (mScreen m' == JoinScreen && mJoinCodeInput m' /= "") $
      withSink $ \sink -> sink JoinMultiplayerGame

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

  GotoYourGames -> do
    modify $ \m -> m { mProfileDropdown = False }
    io_ $ pushURI yourGamesURI

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
    let oldProfile = mProfile m
        pid = maybe "" pId oldProfile
        r  = maybe 1500.0 pRating oldProfile
        rd = maybe 350.0 pRatingRd oldProfile
        gr = maybe 0 pGamesRated oldProfile
    modify $ \x -> x
      { mProfile = Just (Profile pid (mEditUsername m) (Just (mEditDisplayName m)) r rd gr) }
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
    let isAnon = case mSession m of
          Just sess -> amProvider (userAppMetadata (sessionUser sess)) == "anonymous"
          Nothing   -> True
        rated = mIsRated m && not isAnon
    modify $ \x -> x
      { mGameInitData = Just (NewMultiplayerGame (mVariant m) (mTimeControl m)
                               (mSidePreference m) invCode uuid qrUrl rated
                               False Nothing Nothing (mInviteExpiry m))
      , mScreen = GameScreen
      }

  JoinMultiplayerGame -> do
    modify $ \x -> x { mPendingRatedJoin = Nothing }
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
                    { mProfile = Just (Profile uid (mJoinNameInput x) Nothing 1500.0 350.0 0)
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
                (FetchOptions Nothing Nothing Nothing Nothing)
                GameFoundToJoin GameJoinError

  GameFoundToJoin val ->
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) -> do
          m <- get
          let isAnon = case mSession m of
                Just sess -> amProvider (userAppMetadata (sessionUser sess)) == "anonymous"
                Nothing   -> True
          if grwIsRated gr && isAnon
            then modify $ \x -> x { mPendingRatedJoin = Just gr }
            else modify $ \x -> x
              { mGameInitData = Just (JoinGame gr)
              , mScreen       = GameScreen
              }
        [] -> do
          m <- get
          let code = mJoinCodeInput m
          if code /= ""
            then selectWithFilters "games" "*"
                   [eq "invite_code" code]
                   (FetchOptions Nothing Nothing Nothing Nothing)
                   InviteCodeLookup (\_ -> ShowToast "No game found with that code.")
            else modify $ \m' -> m' { mToast = Just "No waiting game found with that code." }
      Error _ ->
        modify $ \m -> m { mToast = Just "Failed to look up game." }

  InviteCodeLookup val ->
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) -> do
          m <- get
          if isParticipant (mSession m) gr
            then io_ $ replaceURI (playURI (grwId gr))
            else io_ $ replaceURI loungeURI
        [] -> modify $ \m -> m { mToast = Just "No game found with that code." }
      Error _ -> modify $ \m -> m { mToast = Just "Failed to look up game." }

  GameJoinError msg ->
    modify $ \m -> m { mToast = Just ("Join error: " <> msg) }

  ResumeGameLoaded val ->
    case fromJSON val of
      Success rows -> case (rows :: [GameRow]) of
        (gr:_) -> do
          m <- get
          if mSessionChecked m && grwStatus gr == "finished" && not (isParticipant (mSession m) gr)
            then io_ $ replaceURI (gamePermalinkURI (grwId gr))
            else modify $ \x -> x
              { mGameInitData = Just (ResumeGame gr)
              -- Wait for session check before mounting so the game component
              -- sees the restored session and correctly identifies the player.
              , mScreen = if mSessionChecked m then GameScreen else mScreen x
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

  SetRated b ->
    modify $ \m -> m { mIsRated = b }

  SetInviteExpiry e ->
    modify $ \m -> m { mInviteExpiry = e }

  JoinRatedWithSignIn -> do
    modify $ \x -> x { mDeferredMpAction = Just DeferJoin }
    io_ $ pushURI signInURI

  JoinRatedAsGuest -> do
    m <- get
    case mPendingRatedJoin m of
      Just gr -> modify $ \x -> x
        { mPendingRatedJoin = Nothing
        , mGameInitData     = Just (JoinGame gr)
        , mScreen           = GameScreen
        }
      Nothing -> pure ()

  -- Matchmaking ----------------------------------------------------------

  FindMatch -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferFindMatch }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> do
        when (mJoinNameInput m /= "") $
          modify $ \x -> x { mGuestName = Just (mJoinNameInput x) }
        let isAnon = case mSession m of
              Just sess -> amProvider (userAppMetadata (sessionUser sess)) == "anonymous"
            rated = mIsRated m && not isAnon
            creatorR  = if rated then Just (maybe 1500.0 pRating (mProfile m)) else Nothing
            creatorRd' = if rated then Just (maybe 350.0 pRatingRd (mProfile m)) else Nothing
        io $ do
          invCode <- generateInviteCode
          uuid    <- js_generateUUID
          origin  <- js_getOrigin
          qrUrl   <- js_generateQRDataURL (origin <> "/join/" <> invCode)
          pure (InitMatchmakingGame invCode uuid qrUrl rated creatorR creatorRd')

  InitMatchmakingGame invCode uuid qrUrl rated creatorR creatorRd' -> do
    m <- get
    let sidePref = mSidePreference m
        resolvedPref = if sidePref == "either"
          then case filter (/= '-') (fromMisoString uuid :: String) of
                 (c:_) -> if c < '8' then "attacker" else "defender"
                 []    -> "attacker"
          else sidePref
    modify $ \x -> x
      { mGameInitData = Just (NewMultiplayerGame (mVariant m) (mTimeControl m)
                               resolvedPref invCode uuid qrUrl rated
                               True creatorR creatorRd' Expiry10Min)
      , mScreen = GameScreen
      }

  -- Match interest (passive side) ----------------------------------------

  ToggleMatchInterest -> do
    m <- get
    if mMatchInterested m
      then do
        -- Turn OFF: unsubscribe and clear state
        case mMatchInterestChannel m of
          Just ch -> io_ $ removeChannel ch
          Nothing -> pure ()
        modify $ \x -> x
          { mMatchInterested      = False
          , mMatchInterestChannel = Nothing
          , mMatchToast           = Nothing
          , mMatchModal           = Nothing
          }
        io_ $ js_setLocalStorage "taflhouse_ready" "false"
      else do
        -- Check if user has seen explanation before
        withSink $ \sink -> do
          seen <- js_getLocalStorage "taflhouse_ready_seen"
          if seen /= "true"
            then sink ShowReadyPopover       -- step 0: explanation
            else sink ConfirmReadyPopover    -- step 1: filters

  ShowReadyPopover ->
    modify $ \x -> x { mMatchReadyStep = Just 0 }

  DismissReadyPopover ->
    modify $ \x -> x { mMatchReadyStep = Nothing }

  ConfirmReadyPopover -> do
    -- Transition from explanation to filters step
    io_ $ js_setLocalStorage "taflhouse_ready_seen" "true"
    modify $ \x -> x { mMatchReadyStep = Just 1 }

  ConfirmMatchFilters -> do
    m <- get
    modify $ \x -> x { mMatchReadyStep = Nothing }
    -- Persist filter preferences
    io_ $ do
      js_setLocalStorage "taflhouse_match_any" (if mMatchAny m then "true" else "false")
      js_setLocalStorage "taflhouse_match_rated" (mMatchWantRated m)
      js_setLocalStorage "taflhouse_match_timed" (mMatchWantTimed m)
      js_setLocalStorage "taflhouse_match_side" (mMatchWantSide m)
    activateMatchInterest

  SetMatchAny v -> modify $ \x -> x { mMatchAny = v }
  SetMatchWantRated v -> modify $ \x -> x { mMatchWantRated = v }
  SetMatchWantTimed v -> modify $ \x -> x { mMatchWantTimed = v }
  SetMatchWantSide v -> modify $ \x -> x { mMatchWantSide = v }

  MatchInterestSubscribed ch -> do
    modify $ \x -> x { mMatchInterestChannel = Just ch }

  MatchInterestError _ -> pure ()

  MatchInterestChange val -> do
    m <- get
    when (mMatchInterested m) $
      case parseRealtimePayload val of
        Nothing -> pure ()
        Just gr ->
          when (matchesPrefs m gr) $
            modify $ \x -> x { mMatchToast = Just gr }

  MatchInterestInitialLoad val -> do
    m <- get
    when (mMatchInterested m) $
      case fromJSON val of
        Success rows ->
          case bestMatch m (filter (matchesPrefs m) (rows :: [GameRow])) of
            Just best -> modify $ \x -> x { mMatchToast = Just best }
            Nothing   -> pure ()
        Error _ -> pure ()

  MatchInterestInitialError _ -> pure ()

  ViewMatchDetails gr -> do
    modify $ \x -> x { mMatchModal = Just gr, mMatchToast = Nothing }
    -- Notify creator that someone is viewing
    updateTable "games"
      (object ["interest_status" .= ("viewing" :: MisoString)])
      [eq "id" (grwId gr)]
      (UpdateOptions Nothing)
      DeclineMatchUpdated DeclineMatchError

  AcceptMatch gr -> do
    m <- get
    -- Turn off interest toggle and unsubscribe
    case mMatchInterestChannel m of
      Just ch -> io_ $ removeChannel ch
      Nothing -> pure ()
    modify $ \x -> x
      { mMatchInterested      = False
      , mMatchInterestChannel = Nothing
      , mMatchToast           = Nothing
      , mMatchModal           = Nothing
      , mGameInitData         = Just (JoinGame gr)
      , mScreen               = GameScreen
      }

  DeclineMatch gameId -> do
    modify $ \x -> x { mMatchModal = Nothing }
    updateTable "games"
      (object ["interest_status" .= ("declined" :: MisoString)])
      [eq "id" gameId]
      (UpdateOptions Nothing)
      DeclineMatchUpdated DeclineMatchError

  DeclineMatchUpdated _ -> pure ()

  DeclineMatchError _ -> pure ()

  DismissMatchToast ->
    modify $ \x -> x { mMatchToast = Nothing }

  -- Rankings / Player detail -----------------------------------------------

  RankingsLoaded val ->
    case fromJSON val of
      Success profiles -> modify $ \x -> x { mRankings = profiles }
      Error _          -> modify $ \x -> x { mRankings = [] }

  RankingsLoadError _ ->
    modify $ \x -> x { mRankings = [] }

  GotoPlayer uname ->
    io_ $ pushURI (playerURI uname)

  PlayerProfileLoaded val ->
    case fromJSON val of
      Success profiles -> case (profiles :: [Profile]) of
        (p:_) -> do
          modify $ \x -> x { mPlayerDetail = Just p }
          let pid = pId p
          -- Load games where this player was attacker
          selectWithFilters "games" "*"
            [eq "attacker_id" pid, eq "is_rated" True, neq "result_desc" ("in_progress" :: MisoString)]
            (FetchOptions Nothing Nothing (Just ("created_at", False)) Nothing)
            PlayerGamesLoaded PlayerGamesLoadError
          -- Load games where this player was defender
          selectWithFilters "games" "*"
            [eq "defender_id" pid, eq "is_rated" True, neq "result_desc" ("in_progress" :: MisoString)]
            (FetchOptions Nothing Nothing (Just ("created_at", False)) Nothing)
            PlayerGamesLoaded PlayerGamesLoadError
        [] -> modify $ \x -> x { mPlayerGamesLoading = False }
      Error _ -> modify $ \x -> x { mPlayerGamesLoading = False }

  PlayerProfileLoadError _ ->
    modify $ \x -> x { mPlayerGamesLoading = False }

  PlayerGamesLoaded val ->
    case fromJSON val of
      Success games -> modify $ \x -> x
        { mPlayerGames = dedup (mPlayerGames x ++ (games :: [GameRow]))
        , mPlayerGamesLoading = False
        }
      Error _ -> modify $ \x -> x { mPlayerGamesLoading = False }

  PlayerGamesLoadError _ ->
    modify $ \x -> x { mPlayerGamesLoading = False }

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
    when (mScreen m == HomeScreen) $
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

  -- Push notifications (app-level) ----------------------------------------

  InitPushStatus permState brave firefox safari edge macOS ->
    modify $ \m -> m { mPushStatus = permState, mIsBrave = brave, mIsFirefox = firefox, mIsSafari = safari, mIsEdge = edge, mIsMacOS = macOS }

  ShowPushBraveHelp ->
    modify $ \m -> m { mPushBraveHelp = True }

  BackFromPushBraveHelp ->
    modify $ \m -> m { mPushBraveHelp = False }

  TogglePushPopover ->
    modify $ \m -> m { mPushPopover = not (mPushPopover m) }

  DismissPushPopover ->
    modify $ \m -> m { mPushPopover = False, mPushBraveHelp = False }

  EnablePushNotifications -> do
    modify $ \m -> m { mPushPopover = False }
    withSink $ \sink -> do
      permOk  <- Function <$> asyncCallback1 (\_ -> sink (PushPermissionResult "granted"))
      permErr <- Function <$> asyncCallback1 (\errVal -> do
        errStr <- fromJSValUnchecked errVal
        sink (PushPermissionResult errStr))
      js_requestNotificationPermission permOk permErr

  PushPermissionResult result
    | result == "granted" -> do
        modify $ \m -> m { mPushStatus = "granted" }
        -- Now subscribe and save the push subscription
        withSink $ \sink -> do
          subOk  <- Function <$> asyncCallback1 (\subVal -> do
            subStr <- fromJSValUnchecked subVal
            sink (PushSubscriptionReady subStr))
          subErr <- Function <$> asyncCallback1 (\errVal -> do
            errStr <- fromJSValUnchecked errVal
            sink (PushSubscriptionError errStr))
          js_subscribeToPush subOk subErr
    | result == "denied" -> do
        modify $ \m -> m { mPushStatus = "denied" }
        updateModel loungeChannelRef (ShowToast "Notifications blocked by browser")
    | otherwise ->
        modify $ \m -> m { mPushStatus = "denied" }

  PushSubscriptionReady subJson -> do
    m <- get
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
        withSink $ \sink -> do
          saveOk  <- Function <$> asyncCallback (sink PushSubscriptionSaved)
          saveErr <- Function <$> asyncCallback1 (\errVal -> do
            errStr <- fromJSValUnchecked errVal
            sink (PushSubscriptionError errStr))
          js_savePushSubscription subJson uid saveOk saveErr
      Nothing -> pure ()

  PushSubscriptionSaved ->
    updateModel loungeChannelRef (ShowToast "Notifications enabled")

  PushSubscriptionError errMsg
    | errMsg == "brave_push_blocked" ->
        modify $ \m -> m { mPushPopover = True, mPushBraveHelp = True }
    | otherwise -> pure ()

  -- Game component mailbox -----------------------------------------------

  GameMailbox val ->
    case parseMaybe (withObject "Mailbox" (\o -> o .: "type")) val of
      Just ("toast" :: MisoString) ->
        case parseMaybe (withObject "Mailbox" (\o -> o .: "msg")) val of
          Just msg -> updateModel loungeChannelRef (ShowToast msg)
          Nothing  -> pure ()
      Just "game_finished" -> loadPastGames
      Just "rating_updated" -> do
        m <- get
        case mSession m of
          Just sess -> loadProfile sess
          Nothing   -> pure ()
      Just "rated_downgraded" ->
        updateModel loungeChannelRef (ShowToast "Your opponent joined as a guest \x2014 this game is now casual.")
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

-- | Is the user a participant (attacker or defender) in this game?
isParticipant :: Maybe Session -> GameRow -> Bool
isParticipant Nothing _ = False
isParticipant (Just sess) gr =
  let uid = userId (sessionUser sess)
  in grwAttackerId gr == Just uid || grwDefenderId gr == Just uid

-- | Extract the UUID from any GameInitData variant.
gameInitUuid :: GameInitData -> MisoString
gameInitUuid (NewLocalGame uuid _ _ _ _ _)                  = uuid
gameInitUuid (NewMultiplayerGame _ _ _ _ uuid _ _ _ _ _ _)  = uuid
gameInitUuid (JoinGame gr)                                  = grwId gr
gameInitUuid (ResumeGame gr)                                = grwId gr

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
        (FetchOptions Nothing Nothing Nothing Nothing)
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
    (FetchOptions Nothing Nothing Nothing Nothing)
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
    (FetchOptions Nothing Nothing Nothing Nothing)
    LoungeOpenLoaded LoungeLoadError
  selectWithFilters "games" "*"
    [eq "status" ("active" :: MisoString), eq "result_desc" ("in_progress" :: MisoString)]
    (FetchOptions Nothing Nothing Nothing Nothing)
    LoungeLiveLoaded LoungeLoadError

-- | Filter active games to only those with activity in the last 30 minutes.
filterRecentGames :: [GameRow] -> IO [GameRow]
filterRecentGames = filterM isRecent
  where
    thirtyMinMs = 30 * 60 * 1000 :: Int
    windowMs moves
      | moves < 10 = (moves + 1) * 60 * 1000
      | otherwise  = thirtyMinMs
    isRecent gr = case grwLastMoveAt gr of
      Just ts -> do
        elapsed <- js_elapsedMs ts
        pure (elapsed < windowMs (grwTotalMoves gr))
      Nothing -> pure False

-- | Check if a Route is the HomeRoute.
isHomeRoute :: Route -> Bool
isHomeRoute HomeRoute = True
isHomeRoute _         = False

-- | Parse a GameRow from a Supabase Realtime payload (extracts @new@ field).
parseRealtimePayload :: Value -> Maybe GameRow
parseRealtimePayload val =
  parseMaybe (\v -> withObject "RealtimePayload" (\o -> o .: "new" >>= parseJSON) v) val

-- | Does a matchmaking GameRow pass the user's preference filters?
matchesPrefs :: Model -> GameRow -> Bool
matchesPrefs m gr =
  grwStatus gr == "waiting"
  && grwIsMatchmaking gr
  -- Don't show own games
  && not (isOwnGame m gr)
  -- Variant must match the selected variant
  && grwVariant gr == ms (variantSlug (mVariant m))
  -- Preference filters (Any overrides all)
  && (mMatchAny m || passesFilters)
  where
    passesFilters =
      ratedOk && timedOk && sideOk

    ratedOk = case mMatchWantRated m of
      "rated"  -> grwIsRated gr
      "casual" -> not (grwIsRated gr)
      _        -> True  -- "either"

    timedOk = case mMatchWantTimed m of
      "timed"   -> grwTimeControl gr /= Nothing
      "untimed" -> grwTimeControl gr == Nothing
      _         -> True  -- "either"

    sideOk = case mMatchWantSide m of
      "attacker" -> grwAttackerId gr == Nothing   -- attacker slot open
      "defender" -> grwDefenderId gr == Nothing   -- defender slot open
      _          -> True  -- "either"

    isOwnGame mdl gr' = case mSession mdl of
      Just sess -> let uid = userId (sessionUser sess)
                   in grwAttackerId gr' == Just uid || grwDefenderId gr' == Just uid
      Nothing   -> False

-- | Pick the best match from a list of eligible games:
-- closest in rating (tiebreak: longest-waiting game first).
bestMatch :: Model -> [GameRow] -> Maybe GameRow
bestMatch _ []  = Nothing
bestMatch m grs =
  let myRating = maybe 1500.0 pRating (mProfile m)
      ratingDiff gr = case grwCreatorRating gr of
        Just r  -> abs (myRating - r)
        Nothing -> 0  -- unrated/casual games have no gap
      -- Sort by rating proximity, then by ID (earlier ID = longer waiting)
      sorted = sortOn (\gr -> (ratingDiff gr, grwId gr)) grs
  in case sorted of
       (best:_) -> Just best
       []       -> Nothing

-- | Load ranked players for the leaderboard.
loadRankings :: Effect ROOT () Model Action
loadRankings =
  selectWithFilters "profiles" "*"
    [gt "games_rated" (0 :: Int)]
    (FetchOptions Nothing Nothing (Just ("rating", False)) (Just 20))
    RankingsLoaded RankingsLoadError

-- | Deduplicate game rows by ID.
dedup :: [GameRow] -> [GameRow]
dedup = go []
  where
    go _    []     = []
    go seen (g:gs)
      | grwId g `elem` seen = go seen gs
      | otherwise           = g : go (grwId g : seen) gs

-- | Activate the match interest toggle: ensure auth, subscribe, and query.
activateMatchInterest :: Effect ROOT () Model Action
activateMatchInterest = do
  m <- get
  case mSession m of
    Nothing -> do
      modify $ \x -> x { mDeferredMpAction = Just DeferToggleInterest }
      signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
    Just _ -> do
      modify $ \x -> x { mMatchInterested = True }
      io_ $ js_setLocalStorage "taflhouse_ready" "true"
      subscribeToTable "matchinterest" "games" "is_matchmaking=eq.true"
        MatchInterestChange MatchInterestSubscribed MatchInterestError
      selectWithFilters "games" "*"
        [ eq "status" ("waiting" :: MisoString)
        , eq "is_matchmaking" True
        ]
        (FetchOptions Nothing Nothing Nothing Nothing)
        MatchInterestInitialLoad MatchInterestInitialError
