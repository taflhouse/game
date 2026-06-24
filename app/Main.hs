{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Miso hiding ((!!))
import Miso.CSS (style_)
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), Parser,
                  object, (.=), (.:), (.:?), (.!=), withObject, withText, parseMaybe)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, jsg, (#), asyncCallback, Function(..))
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq)
import Supabase.Miso.Realtime (subscribeToTable, removeChannel, Channel(..))

import qualified Data.Text as T

import Tafl.Types
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Move  (getPossibleMovesFrom)
import Tafl.Game  (act, initialState)
import Tafl.AI    (AiConfig(..), bestMove, evaluate)

-- ---------------------------------------------------------------------------
-- Miso.JSON instances (Miso uses its own ToJSON/FromJSON, not Data.Aeson)
-- ---------------------------------------------------------------------------

instance ToJSON Coords where
  toJSON (Coords r c) = toJSON [r, c]

instance FromJSON Coords where
  parseJSON v = do
    [r, c] <- parseJSON v
    pure (Coords r c)

instance ToJSON MoveAction where
  toJSON (MoveAction f t) = toJSON [toJSON f, toJSON t]

instance FromJSON MoveAction where
  parseJSON v = do
    [f, t] <- parseJSON v
    pure (MoveAction f t)

instance ToJSON Side where
  toJSON AttackerSide = toJSON ("attacker" :: T.Text)
  toJSON DefenderSide = toJSON ("defender" :: T.Text)

instance FromJSON Side where
  parseJSON = withText "Side" $ \case
    "attacker" -> pure AttackerSide
    "defender" -> pure DefenderSide
    _          -> fail "expected \"attacker\" or \"defender\""

instance ToJSON GameResult where
  toJSON (GameResult fin w d) = object
    [ "finished" .= fin
    , "winner"   .= w
    , "desc"     .= ms d
    ]

instance FromJSON GameResult where
  parseJSON = withObject "GameResult" $ \v ->
    GameResult <$> v .: "finished" <*> v .: "winner" <*> v .: "desc"

-- ---------------------------------------------------------------------------
-- GameRow: parsing game rows from Realtime payloads and DB queries
-- ---------------------------------------------------------------------------

data GameRow = GameRow
  { grwId           :: !MisoString
  , grwVariant      :: !MisoString
  , grwStatus       :: !MisoString
  , grwMoves        :: [MoveAction]
  , grwCurrentTurn  :: !MisoString
  , grwAttackerId   :: Maybe MisoString
  , grwAttackerName :: Maybe MisoString
  , grwDefenderId   :: Maybe MisoString
  , grwDefenderName :: Maybe MisoString
  , grwDrawOfferedBy :: Maybe MisoString
  , grwResultDesc   :: !MisoString
  , grwWinner       :: Maybe MisoString
  , grwTotalMoves   :: !Int
  , grwInviteCode   :: Maybe MisoString
  } deriving (Eq, Show)

instance FromJSON GameRow where
  parseJSON = withObject "GameRow" $ \v ->
    GameRow
      <$> v .: "id"
      <*> v .: "variant"
      <*> v .: "status"
      <*> v .:? "moves" .!= []
      <*> v .:? "current_turn" .!= "attacker"
      <*> v .:? "attacker_id"
      <*> v .:? "attacker_name"
      <*> v .:? "defender_id"
      <*> v .:? "defender_name"
      <*> v .:? "draw_offered_by"
      <*> v .:? "result_desc" .!= "in_progress"
      <*> v .:? "winner"
      <*> v .:? "total_moves" .!= 0
      <*> v .:? "invite_code"

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data GameMode = PracticeMode | AiMode | MultiplayerMode
  deriving (Eq, Show)

data Screen = HomeScreen | SignInScreen | SignUpScreen | ConfigScreen | JoinScreen | GameScreen | ReplayScreen | ProfileScreen | ProfileEditScreen
  deriving (Eq, Show)

data Profile = Profile
  { pUsername    :: !MisoString
  , pDisplayName :: Maybe MisoString
  } deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v ->
    Profile <$> v .: "username" <*> v .:? "display_name"

data GameRecord = GameRecord
  { grId         :: Maybe MisoString
  , grVariant    :: !MisoString
  , grResultDesc :: !MisoString
  , grGameMode   :: !MisoString
  , grPlayedAt   :: !MisoString
  , grWinner     :: Maybe MisoString
  , grAiSide     :: Maybe MisoString
  , grTotalMoves :: !Int
  , grAiDepth    :: Maybe Int
  , grMoves      :: Maybe [MoveAction]
  } deriving (Eq, Show)

instance FromJSON GameRecord where
  parseJSON = withObject "GameRecord" $ \v ->
    GameRecord
      <$> v .:? "id"
      <*> v .: "variant"
      <*> v .: "result_desc"
      <*> v .: "game_mode"
      <*> v .: "played_at"
      <*> v .: "winner"
      <*> v .: "ai_side"
      <*> v .: "total_moves"
      <*> v .: "ai_depth"
      <*> v .:? "moves"

data DeferredMpAction = DeferCreate | DeferJoin
  deriving (Eq, Show)

data ViewMode = NormalView | ZenView
  deriving (Eq, Show)

data Model = Model
  { mScreen         :: !Screen
  , mGameMode       :: !GameMode
  , mGameState      :: !GameState
  , mSelected       :: Maybe Coords
  , mValidMoves     :: [Coords]
  , mVariant        :: !BoardVariant
  , mAiSide         :: !Side
  , mAiThinking     :: !Bool
  , mAiDepth        :: !Int
  , mAiNodeLimit    :: !Int
  , mHistory        :: [GameState]
  , mSession        :: Maybe Session
  , mAuthEmail      :: !MisoString
  , mAuthPassword   :: !MisoString
  , mAuthError      :: Maybe MisoString
  , mAuthMessage    :: Maybe MisoString
  , mAuthLoading    :: !Bool
  , mPastGames      :: [GameRecord]
  , mGamesLoading   :: !Bool
  , mConfigExpanded :: !Bool
  , mShowQuoteRef  :: !Bool
  , mQuoteRefGen   :: !Int
  , mToast         :: Maybe MisoString
  , mShowDepthInfo :: !Bool
  , mShowNodesInfo :: !Bool
  , mLocalGames    :: [GameRecord]
  , mMoveList      :: [MoveAction]
  , mReplayGame    :: Maybe GameRecord
  , mReplayStates  :: [GameState]
  , mReplayIndex   :: !Int
  , mGameId        :: Maybe MisoString
  , mReplayNotFound  :: !Bool
  -- Multiplayer
  , mGuestName        :: Maybe MisoString
  , mRealtimeChannel :: Maybe Channel
  , mProfile         :: Maybe Profile
  , mNeedsUsername    :: !Bool
  , mUsernameInput   :: !MisoString
  , mInviteCode      :: Maybe MisoString
  , mJoinCodeInput   :: !MisoString
  , mOpponentName    :: Maybe MisoString
  , mPlayerSide      :: Maybe Side
  , mDrawOffered     :: !Bool
  , mProfileDropdown :: !Bool
  , mSidePreference  :: !MisoString
  , mEditUsername    :: !MisoString
  , mEditDisplayName :: !MisoString
  , mDeferredMpAction :: Maybe DeferredMpAction
  , mEvalScore        :: !Int
  , mFullHistory      :: Maybe [GameState]   -- ^ Preserved full state list for browsing
  , mFullMoveList     :: Maybe [MoveAction]  -- ^ Preserved full move list for browsing
  , mViewMode         :: !ViewMode
  , mIsFullscreen     :: !Bool
  , mZenHint          :: !Bool
  }

instance Eq Model where
  a == b = mScreen a == mScreen b
        && mGameMode a == mGameMode b
        && mGameState a == mGameState b
        && mSelected a == mSelected b
        && mValidMoves a == mValidMoves b
        && mVariant a == mVariant b
        && mAiSide a == mAiSide b
        && mAiThinking a == mAiThinking b
        && mAiDepth a == mAiDepth b
        && mAiNodeLimit a == mAiNodeLimit b
        && mHistory a == mHistory b
        && eqSession (mSession a) (mSession b)
        && mAuthEmail a == mAuthEmail b
        && mAuthPassword a == mAuthPassword b
        && mAuthError a == mAuthError b
        && mAuthMessage a == mAuthMessage b
        && mAuthLoading a == mAuthLoading b
        && mPastGames a == mPastGames b
        && mGamesLoading a == mGamesLoading b
        && mConfigExpanded a == mConfigExpanded b
        && mShowQuoteRef a == mShowQuoteRef b
        && mQuoteRefGen a == mQuoteRefGen b
        && mToast a == mToast b
        && mShowDepthInfo a == mShowDepthInfo b
        && mShowNodesInfo a == mShowNodesInfo b
        && mLocalGames a == mLocalGames b
        && mMoveList a == mMoveList b
        && mReplayGame a == mReplayGame b
        && mReplayStates a == mReplayStates b
        && mReplayIndex a == mReplayIndex b
        && mGameId a == mGameId b
        && mReplayNotFound a == mReplayNotFound b
        && mGuestName a == mGuestName b
        && mProfile a == mProfile b
        && mNeedsUsername a == mNeedsUsername b
        && mUsernameInput a == mUsernameInput b
        && mInviteCode a == mInviteCode b
        && mJoinCodeInput a == mJoinCodeInput b
        && mOpponentName a == mOpponentName b
        && mPlayerSide a == mPlayerSide b
        && mDrawOffered a == mDrawOffered b
        && mProfileDropdown a == mProfileDropdown b
        && mSidePreference a == mSidePreference b
        && mEditUsername a == mEditUsername b
        && mEditDisplayName a == mEditDisplayName b
        && mDeferredMpAction a == mDeferredMpAction b
        && mEvalScore a == mEvalScore b
        && mFullHistory a == mFullHistory b
        && mFullMoveList a == mFullMoveList b
        && mViewMode a == mViewMode b
        && mIsFullscreen a == mIsFullscreen b
        && mZenHint a == mZenHint b

eqSession :: Maybe Session -> Maybe Session -> Bool
eqSession Nothing Nothing = True
eqSession (Just x) (Just y) = sessionAccessToken x == sessionAccessToken y
eqSession _ _ = False

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

data Action
  = CellClicked Coords
  | NoOp
  | AiMoveComplete MoveAction
  | SetGameMode GameMode
  | SetVariant BoardVariant
  | SetAiSide Side
  | SetAiDepth Int
  | SetAiNodeLimit Int
  | GotoMove Int
  | StartGame
  | GotoHome
  | GotoSignIn
  | GotoSignUp
  | GotoConfig
  | GotoJoin
  | ToggleConfigExpand
  | HandleURI URI
  | Undo
  | SetAuthEmail MisoString
  | SetAuthPassword MisoString
  | DoSignUp
  | DoSignIn
  | DoSignOut
  | AuthSuccess AuthResponse
  | AuthError MisoString
  | AnonAuthSuccess AuthResponse
  | AnonAuthError MisoString
  | SignOutSuccess Value
  | SessionRestored (Maybe Session)
  | GameSaved Value
  | GameSaveError MisoString
  | GamesLoaded Value
  | GamesLoadError MisoString
  | ToggleTheme
  | ToggleQuoteRef
  | DismissQuoteRef
  | DismissQuoteRefTimed Int
  | ShowToast MisoString
  | DismissToast
  | ToggleDepthInfo
  | ToggleNodesInfo
  | LocalGamesLoaded [GameRecord]
  | DoMigrateGames MisoString [GameRecord]
  | GotoReplay MisoString
  | ReplayLoaded Value
  | ReplayLoadError MisoString
  | ReplayGotoMove Int
  | InitGame MisoString
  | GameCreated Value
  | GameCreateError MisoString
  | GameUpdated Value
  | GameUpdateError MisoString
  | CopyGameLink
  -- Profile
  | SetUsernameInput MisoString
  | SubmitUsername
  | ProfileCreated Value
  | ProfileCreateError MisoString
  | ProfileLoaded Value
  | ProfileLoadError MisoString
  | ToggleProfileDropdown
  | GotoProfile
  | GotoProfileEdit
  | SetEditUsername MisoString
  | SetEditDisplayName MisoString
  | SubmitProfileEdit
  | ProfileUpdated Value
  | ProfileUpdateError MisoString
  -- Multiplayer
  | CreateMultiplayerGame
  | InitMultiplayerGame MisoString MisoString  -- invCode uuid
  | JoinMultiplayerGame
  | GameFoundToJoin Value
  | GameJoinError MisoString
  | GameJoinedOk Value
  | GameJoinUpdateError MisoString
  | RealtimeChange Value
  | RealtimeSubscribed Channel
  | RealtimeError MisoString
  | MoveUpdated Value
  | MoveUpdateError MisoString
  | ResumeGameLoaded Value
  | ResumeGameLoadError MisoString
  | SetJoinCodeInput MisoString
  | Resign
  | OfferDraw
  | AcceptDraw
  | DeclineDraw
  | SetSidePreference MisoString
  | CopyInviteCode MisoString
  | ToggleZenMode
  | DismissZenHint
  | DocumentDblClick
  | ToggleFullscreen

-- ---------------------------------------------------------------------------
-- Routing
-- ---------------------------------------------------------------------------

data Route = HomeRoute | SignInRoute | SignUpRoute | ConfigRoute | ProfileRoute | ProfileEditRoute
           | PlayRoute MisoString      -- /play/<uuid> active game
           | GameRoute MisoString      -- /games/<uuid> replay/permalink
           | JoinRoute (Maybe MisoString) -- /join or /join/<invite_code>

variantSlugMs :: BoardVariant -> MisoString
variantSlugMs v = ms (variantSlug v)

variantName :: BoardVariant -> MisoString
variantName = \case
  Brandubh      -> "Brandubh 7x7"
  Tablut        -> "Tablut 9x9"
  Classic       -> "Copenhagen 11x11"
  Line          -> "Line 11x11"
  Tawlbwrdd     -> "Tawlbwrdd 11x11"
  Lewis         -> "Lewis 11x11"
  Parlett       -> "Parlett 13x13"
  DamienWalker  -> "Damien Walker 15x15"
  AleaEvangelii -> "Alea Evangelii 19x19"

parseRoute :: URI -> Route
parseRoute uri = case uriPath uri of
  "sign-in"  -> SignInRoute
  "sign-up"  -> SignUpRoute
  "new-game" -> ConfigRoute
  "profile/edit" -> ProfileEditRoute
  "profile"  -> ProfileRoute
  "join"     -> JoinRoute Nothing
  path
    | Just uuid <- msStripPrefix "play/" path
    , isUUID uuid -> PlayRoute uuid
    | Just uuid <- msStripPrefix "games/" path
    , isUUID uuid -> GameRoute uuid
    | Just code <- msStripPrefix "join/" path
    , not (null (fromMisoString code :: String)) -> JoinRoute (Just code)
    | otherwise   -> HomeRoute

msStripPrefix :: String -> MisoString -> Maybe MisoString
msStripPrefix pfx s =
  let str = fromMisoString s :: String
  in if pfx `isPrefixOf` str
     then Just (ms (drop (length pfx) str))
     else Nothing

isUUID :: MisoString -> Bool
isUUID s =
  let str = fromMisoString s :: String
  in length str == 36 && all (\c -> c `elem` ("0123456789abcdef-" :: [Char])) str

lookupVariant :: MisoString -> Maybe BoardVariant
lookupVariant slug = lookup slug [ (variantSlugMs v, v) | v <- [minBound .. maxBound] ]

-- | Replay a list of moves from an initial state, returning
--   (intermediateStates, finalState) so both mHistory and mGameState
--   can be populated in one pass.
replayMoves :: GameState -> [MoveAction] -> ([GameState], GameState)
replayMoves gs0 moves =
  let states = scanl act gs0 moves        -- gs0 : gs1 : ... : gsN
  in  (init states, last states)           -- history = all but last, final = last
  -- scanl always produces at least one element (gs0), so these are safe

friendlyAuthError :: MisoString -> MisoString
friendlyAuthError code = case (fromMisoString code :: String) of
  "email_not_confirmed" -> "Please check your email and confirm your account before signing in."
  "invalid_credentials" -> "Invalid email or password."
  "user_already_exists" -> "An account with this email already exists."
  _ -> "Something went wrong. Please try again."

homeURI :: URI
homeURI = emptyURI

signInURI :: URI
signInURI = emptyURI { uriPath = "sign-in" }

signUpURI :: URI
signUpURI = emptyURI { uriPath = "sign-up" }

configURI :: URI
configURI = emptyURI { uriPath = "new-game" }

playURI :: MisoString -> URI
playURI uuid = emptyURI { uriPath = "play/" <> uuid }

gamePermalinkURI :: MisoString -> URI
gamePermalinkURI uuid = emptyURI { uriPath = "games/" <> uuid }

profileURI :: URI
profileURI = emptyURI { uriPath = "profile" }

profileEditURI :: URI
profileEditURI = emptyURI { uriPath = "profile/edit" }

joinURI :: MisoString -> URI
joinURI code = emptyURI { uriPath = "join/" <> code }

joinBareURI :: URI
joinBareURI = emptyURI { uriPath = "join" }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
foreign import javascript unsafe "globalThis.playMoveSound()"
  js_playMoveSound :: IO ()
foreign import javascript unsafe "globalThis.getSupabaseSession($1,$2)"
  js_getSupabaseSession :: Function -> Function -> IO ()
foreign import javascript unsafe "globalThis.toggleTheme()"
  js_toggleDarkMode :: IO ()
foreign import javascript unsafe "globalThis.loadLocalGames($1,$2)"
  js_loadLocalGames :: Function -> Function -> IO ()
foreign import javascript unsafe "globalThis.clearLocalGames()"
  js_clearLocalGames :: IO ()
foreign import javascript unsafe "globalThis.generateUUID()"
  js_generateUUID_raw :: IO JSVal
foreign import javascript unsafe "globalThis.copyToClipboard($1)"
  js_copyToClipboard_raw :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.toggleFullscreen()"
  js_toggleFullscreen :: IO ()
foreign import javascript unsafe "globalThis.onDocumentDblClick($1)"
  js_onDocumentDblClick :: Function -> IO ()
foreign import javascript unsafe "globalThis.onKeyboardShortcut($1)"
  js_onKeyboardShortcut :: Function -> IO ()
#else
js_playMoveSound :: IO ()
js_playMoveSound = pure ()
js_getSupabaseSession :: Function -> Function -> IO ()
js_getSupabaseSession _ _ = pure ()
js_toggleDarkMode :: IO ()
js_toggleDarkMode = pure ()
js_loadLocalGames :: Function -> Function -> IO ()
js_loadLocalGames _ _ = pure ()
js_clearLocalGames :: IO ()
js_clearLocalGames = pure ()
js_generateUUID_raw :: IO JSVal
js_generateUUID_raw = toJSVal ("00000000-0000-0000-0000-000000000000" :: MisoString)
js_copyToClipboard_raw :: JSVal -> IO ()
js_copyToClipboard_raw _ = pure ()
js_toggleFullscreen :: IO ()
js_toggleFullscreen = pure ()
js_onDocumentDblClick :: Function -> IO ()
js_onDocumentDblClick _ = pure ()
js_onKeyboardShortcut :: Function -> IO ()
js_onKeyboardShortcut _ = pure ()
#endif

js_generateUUID :: IO MisoString
js_generateUUID = fromJSValUnchecked =<< js_generateUUID_raw

js_copyToClipboard :: MisoString -> IO ()
js_copyToClipboard s = toJSVal s >>= js_copyToClipboard_raw

-- | Generate a short random invite code (8 chars from UUID).
generateInviteCode :: IO MisoString
generateInviteCode = do
  uuid <- js_generateUUID
  let str = fromMisoString uuid :: String
      code = filter (/= '-') (take 8 str)
  pure (ms code)

-- | Generate a guest display name from a user ID.
guestNameFromId :: MisoString -> MisoString
guestNameFromId uid = "Guest-" <> ms (take 8 (filter (/= '-') (fromMisoString uid :: String)))

-- | Save a game record to localStorage via the Miso DSL.
saveLocalGameIO :: Value -> IO ()
saveLocalGameIO gameData = do
  val <- toJSVal gameData
  void $ jsg "globalThis" # "saveLocalGame" $ val

main :: IO ()
main = startApp defaultEvents app
  where
    app = Component
      { model            = initModel
      , hydrateModel     = Nothing
      , update           = updateModel
      , view             = viewModel
      , subs             = [ uriSub HandleURI
                           , \sink -> getURI >>= sink . HandleURI
                           , \sink -> do
                               okCb <- successCallback sink
                                 (\_ -> SessionRestored Nothing)
                                 (SessionRestored . Just)
                               errCb <- errorCallback sink
                                 (\_ -> SessionRestored Nothing)
                               js_getSupabaseSession okCb errCb
                           , \sink -> do
                               cb <- Function <$> asyncCallback (sink DocumentDblClick)
                               js_onDocumentDblClick cb
                           , \sink -> do
                               undoCb <- Function <$> asyncCallback (sink Undo)
                               js_onKeyboardShortcut undoCb
                           ]
      , styles           = []
      , scripts          = []
      , mountPoint       = Nothing
      , logLevel         = Off
      , mailbox          = const Nothing
      , bindings         = []
      , eventPropagation = False
      , mount            = Nothing
      , unmount          = Nothing
      }

initModel :: Model
initModel = Model
  { mScreen         = HomeScreen
  , mGameMode       = AiMode
  , mGameState      = initialState Tablut
  , mSelected       = Nothing
  , mValidMoves     = []
  , mVariant        = Tablut
  , mAiSide         = DefenderSide
  , mAiThinking     = False
  , mAiDepth        = 4
  , mAiNodeLimit    = 10000
  , mHistory        = []
  , mSession        = Nothing
  , mAuthEmail      = ""
  , mAuthPassword   = ""
  , mAuthError      = Nothing
  , mAuthMessage    = Nothing
  , mAuthLoading    = False
  , mPastGames      = []
  , mGamesLoading   = False
  , mConfigExpanded = False
  , mShowQuoteRef  = False
  , mQuoteRefGen   = 0
  , mToast         = Nothing
  , mShowDepthInfo = False
  , mShowNodesInfo = False
  , mLocalGames    = []
  , mMoveList      = []
  , mReplayGame    = Nothing
  , mReplayStates  = []
  , mReplayIndex   = 0
  , mGameId        = Nothing
  , mReplayNotFound  = False
  , mGuestName        = Nothing
  , mRealtimeChannel = Nothing
  , mProfile         = Nothing
  , mNeedsUsername    = False
  , mUsernameInput   = ""
  , mInviteCode      = Nothing
  , mJoinCodeInput   = ""
  , mOpponentName    = Nothing
  , mPlayerSide      = Nothing
  , mDrawOffered     = False
  , mProfileDropdown = False
  , mSidePreference  = "defender"
  , mEditUsername    = ""
  , mEditDisplayName = ""
  , mDeferredMpAction = Nothing
  , mEvalScore        = 0
  , mFullHistory      = Nothing
  , mFullMoveList     = Nothing
  , mViewMode         = NormalView
  , mIsFullscreen     = False
  , mZenHint          = False
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  SetGameMode mode ->
    modify $ \m -> m { mGameMode = mode }

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
    modify $ \m -> m { mConfigExpanded = not (mConfigExpanded m) }

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
            modify $ \x -> x { mScreen = GameScreen, mGameId = Just uuid }
            selectWithFilters "games" "*"
              [eq "id" uuid]
              (FetchOptions Nothing Nothing)
              ResumeGameLoaded ResumeGameLoadError
      GameRoute uuid -> do
        modify $ \x -> x
          { mScreen = ReplayScreen
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
        modify $ \x -> x { mScreen = HomeScreen, mAiThinking = False, mGameId = Nothing
                         , mRealtimeChannel = Nothing, mOpponentName = Nothing, mPlayerSide = Nothing
                         , mInviteCode = Nothing, mDrawOffered = False }
        loadPastGames
      SignInRoute ->
        modify $ \x -> x { mScreen = SignInScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      SignUpRoute ->
        modify $ \x -> x { mScreen = SignUpScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      ConfigRoute ->
        modify $ \x -> x { mScreen = ConfigScreen, mMoveList = [], mGameId = Nothing }
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
    let gs    = mGameState m
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
        aiBlocked = mGameMode m == AiMode && mAiSide m == side
        mpBlocked = mGameMode m == MultiplayerMode && mPlayerSide m /= Just side
    if finished (gsResult gs) || mAiThinking m || aiBlocked || mpBlocked
      then pure ()
      else case mSelected m of
        Just sel | coords `elem` mValidMoves m -> do
          let move = MoveAction sel coords
              gs' = act gs move
          modify $ const $ m { mGameState = gs', mSelected = Nothing, mValidMoves = []
                             , mHistory = mHistory m ++ [gs]
                             , mMoveList = mMoveList m ++ [move]
                             , mFullHistory = Nothing, mFullMoveList = Nothing
                             , mEvalScore = evaluate gs' }
          io_ js_playMoveSound
          -- In multiplayer, update the game row in Supabase
          when (mGameMode m == MultiplayerMode) $ do
            let newMoves = mMoveList m ++ [move]
                nextTurn = case turnSide gs' of
                  AttackerSide -> "attacker" :: MisoString
                  DefenderSide -> "defender"
            case mGameId m of
              Just gid -> do
                let updateData = if finished (gsResult gs')
                      then object
                        [ "moves"       .= newMoves
                        , "current_turn" .= nextTurn
                        , "total_moves" .= length newMoves
                        , "result_desc" .= ms (desc (gsResult gs'))
                        , "winner"      .= fmap (\s -> case s of
                            AttackerSide -> "attacker" :: MisoString
                            DefenderSide -> "defender") (winner (gsResult gs'))
                        , "status"      .= ("finished" :: MisoString)
                        ]
                      else object
                        [ "moves"       .= newMoves
                        , "current_turn" .= nextTurn
                        , "total_moves" .= length newMoves
                        ]
                updateTable "games" updateData
                  [eq "id" gid]
                  (UpdateOptions Nothing)
                  MoveUpdated MoveUpdateError
              Nothing -> pure ()
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
    -- Use the full history snapshot if browsing, otherwise current state
    let fullStates = case mFullHistory m of
          Just fs -> fs
          Nothing -> mHistory m ++ [mGameState m]
        fullMoves = case mFullMoveList m of
          Just fm -> fm
          Nothing -> mMoveList m
        lastIdx = length fullStates - 1
        canBrowse = finished (gsResult (last fullStates))
                    || mGameMode m `elem` [PracticeMode, AiMode]
    if canBrowse && i >= 0 && i <= lastIdx
      then put $ m
          { mGameState    = fullStates !! i
          , mHistory      = take i fullStates
          , mMoveList     = take i fullMoves
          , mFullHistory  = Just fullStates
          , mFullMoveList = Just fullMoves
          , mSelected     = Nothing
          , mValidMoves   = []
          , mAiThinking   = False
          , mEvalScore    = evaluate (fullStates !! i)
          }
      else if i >= 0 && i < length (mHistory m ++ [mGameState m])
      then put $ m
          { mGameState  = (mHistory m ++ [mGameState m]) !! i
          , mHistory    = take i (mHistory m ++ [mGameState m])
          , mMoveList   = take i (mMoveList m)
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
          , mEvalScore  = evaluate ((mHistory m ++ [mGameState m]) !! i)
          }
      else pure ()

  Undo -> do
    m <- get
    case mHistory m of
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
                { mReplayGame   = Just gr
                , mReplayStates = states
                , mReplayIndex  = length states - 1
                , mEvalScore    = evaluate (last states)
                }
            _ -> modify $ \m -> m
              { mReplayGame   = Just gr
              , mReplayStates = []
              , mReplayIndex  = 0
              }
        [] -> modify $ \m -> m { mReplayNotFound = True }
      Error _ -> modify $ \m -> m { mReplayNotFound = True }

  ReplayLoadError _ -> modify $ \m -> m { mReplayNotFound = True }

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
        pure (InitMultiplayerGame invCode uuid)

  InitMultiplayerGame invCode uuid -> do
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
            gameData = object
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
              ]
        modify $ \x -> x
          { mGameId     = Just uuid
          , mGameMode   = MultiplayerMode
          , mGameState  = gs
          , mSelected   = Nothing
          , mValidMoves = []
          , mHistory    = []
          , mMoveList   = []
          , mInviteCode = Just invCode
          , mScreen     = GameScreen
          , mPlayerSide = Just mySide
          , mOpponentName = Nothing
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
                  -- Determine which side is open
                  (mySide, updateData) = case grwAttackerId gr of
                    Nothing -> (AttackerSide, object
                      [ "attacker_id"   .= uid
                      , "attacker_name" .= displayName
                      , "status"        .= ("active" :: MisoString)
                      ])
                    Just _ -> (DefenderSide, object
                      [ "defender_id"   .= uid
                      , "defender_name" .= displayName
                      , "status"        .= ("active" :: MisoString)
                      ])
                  variant = fromMaybe Tablut (lookupVariant (grwVariant gr))
                  gs0 = initialState variant
                  (hist, gs) = replayMoves gs0 (grwMoves gr)
                  oppName = case mySide of
                    AttackerSide -> grwDefenderName gr
                    DefenderSide -> grwAttackerName gr
                  gid = grwId gr
              modify $ \x -> x
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
              updateTable "games" updateData
                [eq "id" gid]
                (UpdateOptions Nothing)
                GameJoinedOk GameJoinUpdateError
              subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
                RealtimeChange RealtimeSubscribed RealtimeError
              io_ $ pushURI (playURI gid)
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
          modify $ \x -> x { mOpponentName = oppName }

        -- Opponent moved: remote has more moves than local
        when (length remoteMoves > length localMoves) $ do
          let gs0 = initialState variant
              (hist, gs) = replayMoves gs0 remoteMoves
          modify $ \x -> x
            { mGameState = gs
            , mHistory   = hist
            , mMoveList  = remoteMoves
            , mSelected  = Nothing
            , mValidMoves = []
            }
          io_ js_playMoveSound

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
              modify $ \x -> x
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
                , mScreen       = GameScreen
                }
              -- Subscribe if game is still active
              when (grwStatus gr `elem` ["waiting", "active"]) $
                subscribeToTable ("game:" <> gid) "games" ("id=eq." <> gid)
                  RealtimeChange RealtimeSubscribed RealtimeError
            Nothing -> pure ()
        [] -> modify $ \x -> x { mToast = Just "Game not found." }
      Error _ -> modify $ \x -> x { mToast = Just "Failed to load game." }

  ResumeGameLoadError msg ->
    modify $ \m -> m { mToast = Just ("Load error: " <> msg) }

  SetJoinCodeInput s ->
    modify $ \m -> m { mJoinCodeInput = s }

  Resign -> do
    m <- get
    when (mGameMode m == MultiplayerMode) $
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
    when (mGameMode m == MultiplayerMode) $
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
    io_ $ js_copyToClipboard code
    modify $ \m -> m { mToast = Just "Copied!" }
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


-- ---------------------------------------------------------------------------
-- View: Top-level layout
-- ---------------------------------------------------------------------------

viewModel :: () -> Model -> View Model Action
viewModel _ m =
  let zen = mViewMode m == ZenView && mScreen m `elem` [GameScreen, ReplayScreen]
  in H.div_
    [ HP.class_ "fixed inset-0 flex flex-col bg-background font-sans"
    ]
    [ if zen then text "" else viewNavbar m
    , H.div_
        [ HP.class_ (if mScreen m == HomeScreen then "flex-1" else "flex-1 overflow-y-auto overscroll-none")
        ]
        [ H.div_
            [ HP.class_ (if zen
                then "flex flex-col items-center justify-center min-h-full px-4 mx-auto w-full max-w-7xl"
                else "flex flex-col items-center min-h-full pt-8 pb-12 px-4 mx-auto w-full max-w-7xl")
            ]
            [ if mNeedsUsername m && mGuestName m == Nothing && mScreen m /= SignInScreen && mScreen m /= SignUpScreen
                then viewUsernameGate m
                else case mScreen m of
                  HomeScreen    -> viewHome m
                  SignInScreen  -> viewSignIn m
                  SignUpScreen  -> viewSignUp m
                  ConfigScreen  -> viewConfig m
                  JoinScreen    -> viewJoin m
                  GameScreen    -> viewGame m
                  ReplayScreen  -> viewReplay m
                  ProfileScreen     -> viewProfile m
                  ProfileEditScreen -> viewProfileEdit m
            ]
        ]
    , viewToast m
    , viewZenHint m
    ]

viewToast :: Model -> View Model Action
viewToast m = case mToast m of
  Nothing -> text ""
  Just msg ->
    H.div_ []
      [ H.div_
          [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998") ]
          , SVG.onClick DismissToast
          ] []
      , H.div_
          [ HP.class_ "card px-4 py-2 text-sm text-foreground shadow-lg"
          , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
                   , ("transform", "translateX(-50%)"), ("z-index", "9999")
                   , ("user-select", "text"), ("cursor", "text")
                   ]
          ]
          [ text msg ]
      ]

-- ---------------------------------------------------------------------------
-- Navbar
-- ---------------------------------------------------------------------------

viewNavbar :: Model -> View Model Action
viewNavbar m =
  H.div_
    [ HP.class_ "border-b border-border bg-background/95 backdrop-blur shrink-0"
    ]
    [ H.div_
        [ HP.class_ "flex items-center justify-between px-4 py-3 mx-auto w-full max-w-7xl"
        ]
        [ -- Left: brand
          H.span_
            [ HP.class_ "text-xl font-bold tracking-widest text-foreground/80 cursor-pointer select-none"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoHome
            ]
            [ text "TAFLHOUSE" ]
        , -- Right: controls
          H.div_
            [ HP.class_ "flex items-center gap-4"
            ]
            (fullscreenToggleBtn m : themeToggleBtn : navAuthButtons m)
        ]
    ]

fullscreenToggleBtn :: Model -> View Model Action
fullscreenToggleBtn m
  | mScreen m `elem` [GameScreen, ReplayScreen] =
    H.button_
      [ HP.class_ "p-2 rounded-md text-foreground hover:bg-muted cursor-pointer"
      , style_ [("touch-action", "manipulation"), ("background", "none"), ("border", "none")]
      , SVG.onClick ToggleFullscreen
      , HP.title_ "Fullscreen"
      ]
      [ SVG.svg_
          [ SP.viewBox_ "0 0 24 24"
          , HP.width_ "18"
          , HP.height_ "18"
          , SP.fill_ "none"
          , SP.stroke_ "currentcolor"
          , SP.strokeWidth_ "2"
          , SP.strokeLinecap_ "round"
          , SP.strokeLinejoin_ "round"
          ]
          [ SVG.path_ [ SP.d_ "M8 3H5a2 2 0 0 0-2 2v3" ]
          , SVG.path_ [ SP.d_ "M21 8V5a2 2 0 0 0-2-2h-3" ]
          , SVG.path_ [ SP.d_ "M3 16v3a2 2 0 0 0 2 2h3" ]
          , SVG.path_ [ SP.d_ "M16 21h3a2 2 0 0 0 2-2v-3" ]
          ]
      ]
  | otherwise = text ""

themeToggleBtn :: View Model Action
themeToggleBtn =
  H.button_
    [ HP.class_ "p-2 rounded-md text-foreground hover:bg-muted cursor-pointer"
    , style_ [("touch-action", "manipulation"), ("background", "none"), ("border", "none")]
    , SVG.onClick ToggleTheme
    , HP.title_ "Toggle theme"
    ]
    [ iconSun, iconMoon ]

iconSun :: View Model Action
iconSun =
  SVG.svg_
    [ HP.class_ "hidden dark:block"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.circle_ [ SP.cx_ "12", SP.cy_ "12", SP.r_ "4" ]
    , SVG.path_ [ SP.d_ "M12 2v2" ]
    , SVG.path_ [ SP.d_ "M12 20v2" ]
    , SVG.path_ [ SP.d_ "m4.93 4.93 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m17.66 17.66 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "M2 12h2" ]
    , SVG.path_ [ SP.d_ "M20 12h2" ]
    , SVG.path_ [ SP.d_ "m6.34 17.66-1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m19.07 4.93-1.41 1.41" ]
    ]

iconMoon :: View Model Action
iconMoon =
  SVG.svg_
    [ HP.class_ "dark:hidden"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z" ] ]

navAuthButtons :: Model -> [View Model Action]
navAuthButtons m =
    (case mSession m of
      Just _ ->
        [ H.div_
            [ style_ [("position", "relative")] ]
            [ H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer flex items-center gap-1"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ToggleProfileDropdown
                ]
                [ -- User icon
                  SVG.svg_
                    [ SP.viewBox_ "0 0 24 24"
                    , HP.width_ "18"
                    , HP.height_ "18"
                    , SP.fill_ "none"
                    , SP.stroke_ "currentcolor"
                    , SP.strokeWidth_ "2"
                    , SP.strokeLinecap_ "round"
                    , SP.strokeLinejoin_ "round"
                    ]
                    [ SVG.path_ [ SP.d_ "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2" ]
                    , SVG.circle_ [ SP.cx_ "12", SP.cy_ "7", SP.r_ "4" ]
                    ]
                , H.span_
                    [ HP.class_ "hidden sm:inline" ]
                    [ text (maybe "" pUsername (mProfile m)) ]
                ]
            , if mProfileDropdown m
                then H.div_
                  [ HP.class_ "card p-2 flex flex-col gap-1"
                  , style_ [ ("position", "absolute"), ("right", "0"), ("top", "100%")
                           , ("margin-top", "0.5em"), ("min-width", "8rem"), ("z-index", "50")
                           ]
                  ]
                  [ H.button_
                      [ HP.class_ "text-sm text-left px-3 py-1.5 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground w-full"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick GotoProfile
                      ]
                      [ text "Profile" ]
                  , H.button_
                      [ HP.class_ "text-sm text-left px-3 py-1.5 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground w-full"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick DoSignOut
                      ]
                      [ text "Logout" ]
                  ]
                else text ""
            ]
        ]
      Nothing ->
        [ H.span_
            [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoSignIn
            ]
            [ text "Sign In" ]
        ]
    )

viewOrDivider :: View Model Action
viewOrDivider =
  H.div_
    [ HP.class_ "flex items-center gap-3 w-full"
    , style_ [("margin-top", "1em"), ("margin-bottom", "1em")]
    ]
    [ H.div_ [ HP.class_ "flex-1 border-t border-border" ] []
    , H.span_ [ HP.class_ "text-xs text-muted-foreground uppercase" ] [ text "or" ]
    , H.div_ [ HP.class_ "flex-1 border-t border-border" ] []
    ]

-- ---------------------------------------------------------------------------
-- Join Screen
-- ---------------------------------------------------------------------------

viewJoin :: Model -> View Model Action
viewJoin m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Join a Game" ]
        , H.p_
            [ HP.class_ "text-sm text-muted-foreground mb-4 text-center" ]
            [ text "Enter the invite code shared with you to play!" ]
        , H.div_
            [ HP.class_ "flex flex-col gap-3" ]
            [ H.input_
                [ HP.class_ "input w-full text-center"
                , HP.type_ "text"
                , HP.placeholder_ "Invite code"
                , HP.value_ (mJoinCodeInput m)
                , H.onInput SetJoinCodeInput
                ]
            , H.button_
                [ HP.class_ "btn w-full"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick JoinMultiplayerGame
                ]
                [ text "Join" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Home Screen
-- ---------------------------------------------------------------------------

viewHome :: Model -> View Model Action
viewHome m = case mSession m of
  Just _ | mGamesLoading m ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading games..." ]

  Just _ | not (null (mPastGames m)) ->
    H.div_
      [ HP.class_ "w-full max-w-2xl"
      , style_ [("margin-top", "4em")]
      ]
      [ H.div_
          [ HP.class_ "flex flex-col items-center mb-6" ]
          [ H.button_
              [ HP.class_ "btn"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GotoConfig
              ]
              [ text "New Game" ]
          , viewOrDivider
          , H.span_
              [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GotoJoin
              ]
              [ text "Join Game" ]
          ]
      , H.div_
          [ HP.class_ "flex justify-between items-center mb-4"
          ]
          [ H.h2_
              [ HP.class_ "text-lg font-semibold text-foreground"
              ]
              [ text "Your Games" ]
          ]
      , viewPastGamesTable (mPastGames m)
      ]

  Just _ ->
    H.div_
      [ HP.class_ "text-center max-w-md"
      , style_ [("margin-top", "4em")]
      ]
      [ H.button_
          [ HP.class_ "btn"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GotoConfig
          ]
          [ text "New Game" ]
      , viewOrDivider
      , H.span_
          [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GotoJoin
          ]
          [ text "Join Game" ]
      , H.p_
          [ HP.class_ "text-xl font-bold"
          , style_ [("margin-top", "2em")]
          ]
          [ text "No games yet" ]
      , H.p_
          [ HP.class_ "text-muted-foreground text-sm italic cursor-pointer"
          , style_ [("margin-top", "2em"), ("text-decoration", "underline dotted"), ("text-underline-offset", "4px")]
          , SVG.onClick (ShowToast "V\x01EBlusp\x00E1, stanza 59")
          ]
          [ text "\"The golden tafl pieces shall again be found in the grass.\"" ]
      ]

  Nothing ->
    H.div_
      [ HP.class_ "text-center max-w-md"
      , style_ [("margin-top", "4em")]
      ]
      [ H.p_
          [ HP.class_ "text-xl font-bold"
          ]
          [ text "Welcome! Feel free to settle in." ]
      , H.button_
          [ HP.class_ "btn"
          , style_ [("touch-action", "manipulation"), ("margin-top", "2em")]
          , SVG.onClick GotoConfig
          ]
          [ text "New Game" ]
      , viewOrDivider
      , H.span_
          [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GotoJoin
          ]
          [ text "Join Game" ]
      , H.div_
          [ style_ [("margin-top", "2em"), ("position", "relative")]
          ]
          [ H.p_
              [ HP.class_ "text-muted-foreground text-sm italic"
              , style_ [("text-decoration", "underline dotted"), ("text-underline-offset", "4px"), ("cursor", "pointer")]
              , SVG.onClick ToggleQuoteRef
              ]
              [ text "\"They played tafl in the meadow and were merry,\"" ]
          , if mShowQuoteRef m
              then H.div_ []
                [ H.div_
                    [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "49") ]
                    , SVG.onClick DismissQuoteRef
                    ] []
                , H.div_
                    [ HP.class_ "card p-4 text-left"
                    , style_ [ ("position", "absolute"), ("top", "100%"), ("left", "50%")
                             , ("transform", "translateX(-50%)"), ("margin-top", "0.5em")
                             , ("width", "18rem"), ("z-index", "50")
                             ]
                    ]
                    [ H.p_
                        [ HP.class_ "text-sm text-muted-foreground" ]
                        [ text "V\x01EBlusp\x00E1, stanza 8" ]
                    ]
                ]
              else text ""
          ]
      ]

-- ---------------------------------------------------------------------------
-- Past Games Table
-- ---------------------------------------------------------------------------

viewPastGamesTable :: [GameRecord] -> View Model Action
viewPastGamesTable games =
  H.div_
    [ HP.class_ "overflow-x-auto"
    ]
    [ H.table_
        [ HP.class_ "table w-full"
        ]
        [ H.thead_
            []
            [ H.tr_
                []
                [ H.th_ [] [ text "Variant" ]
                , H.th_ [] [ text "Mode" ]
                , H.th_ [] [ text "Result" ]
                , H.th_ [] [ text "Moves" ]
                , H.th_ [] [ text "Date" ]
                ]
            ]
        , H.tbody_
            []
            (map viewGameRow games)
        ]
    ]

viewGameRow :: GameRecord -> View Model Action
viewGameRow gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
      modeText = case grGameMode gr of
        "ai"          -> "vs AI"
        "local"       -> "Practice"
        "multiplayer" -> "Multiplayer"
        _             -> grGameMode gr
      cells =
        [ H.td_ [] [ text (grVariant gr) ]
        , H.td_ [] [ text modeText ]
        , H.td_ [] [ text winText ]
        , H.td_ [] [ text (ms (show (grTotalMoves gr))) ]
        , H.td_ [ HP.class_ "text-muted-foreground" ] [ text (grPlayedAt gr) ]
        ]
  in case grId gr of
    Just gid -> H.tr_
      [ HP.class_ "cursor-pointer hover:bg-muted/50"
      , SVG.onClick (GotoReplay gid)
      ] cells
    Nothing -> H.tr_ [] cells

-- ---------------------------------------------------------------------------
-- Sign In Screen
-- ---------------------------------------------------------------------------

viewSignIn :: Model -> View Model Action
viewSignIn m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign In" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignIn
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign In" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Don't have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignUp
                ]
                [ text "Sign Up" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Sign Up Screen
-- ---------------------------------------------------------------------------

viewSignUp :: Model -> View Model Action
viewSignUp m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign Up" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignUp
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign Up" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Already have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignIn
                ]
                [ text "Sign In" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Config Screen
-- ---------------------------------------------------------------------------

viewConfig :: Model -> View Model Action
viewConfig m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "New Game" ]
        , viewConfigSummary m
        , H.div_
            [ HP.class_ "mt-4 flex flex-col items-center gap-2"
            ]
            [ if mGameMode m == MultiplayerMode
                then H.button_
                  [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick CreateMultiplayerGame
                  ]
                  [ text "Create Game" ]
                else H.button_
                  [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick StartGame
                  ]
                  [ text "Start Game" ]
            , H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ToggleConfigExpand
                ]
                [ text (if mConfigExpanded m then "Hide options" else "Configure") ]
            ]
        , if mConfigExpanded m
            then viewConfigOptions m
            else H.div_ [] []
        ]
    ]

viewConfigSummary :: Model -> View Model Action
viewConfigSummary m =
  let modeText = case mGameMode m of
        PracticeMode       -> "Practice (2 players)"
        AiMode          -> "vs AI"
        MultiplayerMode -> "Multiplayer"
      boardText = variantName (mVariant m)
      aiInfo = if mGameMode m == AiMode
        then [ ("AI plays" :: MisoString, if mAiSide m == AttackerSide then "Attackers" else "Defenders")
             ]
        else []
      items = [("Mode", modeText), ("Board", boardText)] ++ aiInfo
  in H.div_
    [ HP.class_ "flex flex-col gap-1"
    ]
    (map summaryRow items)

summaryRow :: (MisoString, MisoString) -> View Model Action
summaryRow (label, val) =
  H.div_
    [ HP.class_ "flex justify-between py-1 border-b border-border text-sm"
    ]
    [ H.span_
        [ HP.class_ "text-muted-foreground"
        ]
        [ text label ]
    , H.span_
        [ HP.class_ "font-medium"
        ]
        [ text val ]
    ]

viewConfigOptions :: Model -> View Model Action
viewConfigOptions m =
  H.div_
    [ HP.class_ "mt-4 pt-4 border-t border-border flex flex-col gap-4"
    ]
    [ setupSection "Mode"
        [ setupBtn (SetGameMode PracticeMode) "Practice" (mGameMode m == PracticeMode)
        , setupBtn (SetGameMode AiMode) "vs AI" (mGameMode m == AiMode)
        , setupBtn (SetGameMode MultiplayerMode) "Multiplayer" (mGameMode m == MultiplayerMode)
        ]
    , setupSection "Board"
        [ setupBtn (SetVariant Brandubh) "Brandubh 7x7" (mVariant m == Brandubh)
        , setupBtn (SetVariant Tablut) "Tablut 9x9" (mVariant m == Tablut)
        , setupBtn (SetVariant Classic) "Copenhagen 11x11" (mVariant m == Classic)
        , setupBtn (SetVariant Parlett) "Parlett 13x13" (mVariant m == Parlett)
        , setupBtn (SetVariant DamienWalker) "Damien Walker 15x15" (mVariant m == DamienWalker)
        ]
    , if mGameMode m == AiMode then viewSetupAi m
      else if mGameMode m == MultiplayerMode then viewSetupMultiplayer m
      else H.div_ [] []
    ]

-- ---------------------------------------------------------------------------
-- Setup helpers (shared by config options)
-- ---------------------------------------------------------------------------

setupSection :: MisoString -> [View Model Action] -> View Model Action
setupSection label children =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text label ]
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center"
        ]
        children
    ]

setupBtn :: Action -> MisoString -> Bool -> View Model Action
setupBtn action label isActive =
  H.button_
    [ HP.class_ (if isActive then "btn btn-secondary" else "btn btn-outline text-foreground")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

setupBtnDisabled :: MisoString -> View Model Action
setupBtnDisabled label =
  H.button_
    [ HP.class_ "btn btn-outline text-muted-foreground opacity-60 cursor-not-allowed"
    , HP.disabled_
    ]
    [ text label ]

viewSetupAi :: Model -> View Model Action
viewSetupAi m =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text "AI SETTINGS" ]
    -- AI side
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center mb-2"
        ]
        [ setupBtn (SetAiSide AttackerSide) "AI plays Attackers" (mAiSide m == AttackerSide)
        , setupBtn (SetAiSide DefenderSide) "AI plays Defenders" (mAiSide m == DefenderSide)
        ]
    -- Depth
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "DEPTH"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleDepthInfo
                ]
                [ text "?" ]
            ]
        , if mShowDepthInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "How many moves ahead the AI looks. Higher = stronger but slower." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiDepth d) (ms (show d)) (mAiDepth m == d)
            | d <- [1..8]
            ]
        ]
    -- Node limit
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "NODES"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleNodesInfo
                ]
                [ text "?" ]
            ]
        , if mShowNodesInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "Max positions the AI evaluates per move. Caps search time. \x2018None\x2019 = unlimited." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiNodeLimit n) label (mAiNodeLimit m == n)
            | (n, label) <- [ (1000, "1K"), (5000, "5K")
                            , (10000, "10K"), (50000, "50K"), (100000, "100K")
                            , (0, "None")
                            ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Multiplayer Setup (inside config options)
-- ---------------------------------------------------------------------------

viewSetupMultiplayer :: Model -> View Model Action
viewSetupMultiplayer m =
  H.div_
    [ HP.class_ "text-center" ]
    [ setupSection "Your Side"
        [ setupBtn (SetSidePreference "attacker") "Attackers" (mSidePreference m == "attacker")
        , setupBtn (SetSidePreference "defender") "Defenders" (mSidePreference m == "defender")
        ]
    ]

-- ---------------------------------------------------------------------------
-- Username Registration Gate
-- ---------------------------------------------------------------------------

viewUsernameGate :: Model -> View Model Action
viewUsernameGate m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Choose Your Username" ]
        , H.p_
            [ HP.class_ "text-sm text-muted-foreground mb-4 text-center" ]
            [ text "3-20 characters, letters, numbers, and underscores only." ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit SubmitUsername
            ]
            [ H.input_
                [ HP.class_ "input w-full text-center"
                , HP.type_ "text"
                , HP.required_ True
                , HP.value_ (mUsernameInput m)
                , HP.placeholder_ "username"
                , H.onInput SetUsernameInput
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm text-center" ]
                  [ text err ]
            , H.button_
                [ HP.class_ "btn w-full" ]
                [ text "Continue" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Profile Screen
-- ---------------------------------------------------------------------------

viewProfile :: Model -> View Model Action
viewProfile m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Profile" ]
        , case mProfile m of
            Nothing ->
              H.div_
                [ HP.class_ "text-center text-muted-foreground" ]
                [ text "Loading..." ]
            Just profile ->
              H.div_
                [ HP.class_ "flex flex-col gap-3" ]
                [ summaryRow ("Username", pUsername profile)
                , summaryRow ("Display Name", maybe "-" id (pDisplayName profile))
                , summaryRow ("Games Played", ms (show (length (mPastGames m))))
                , let wins = length $ filter (\gr -> grWinner gr /= Nothing) (mPastGames m)
                      total = length (mPastGames m)
                      rate = if total > 0 then show (wins * 100 `div` total) <> "%" else "-"
                  in summaryRow ("Win Rate", ms rate)
                , H.button_
                    [ HP.class_ "btn w-full mt-2"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick GotoProfileEdit
                    ]
                    [ text "Edit Profile" ]
                ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Profile Edit Screen
-- ---------------------------------------------------------------------------

viewProfileEdit :: Model -> View Model Action
viewProfileEdit m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Edit Profile" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit SubmitProfileEdit
            ]
            [ H.label_
                [ HP.class_ "text-sm text-muted-foreground" ]
                [ text "Username" ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.required_ True
                , HP.value_ (mEditUsername m)
                , HP.placeholder_ "username"
                , H.onInput SetEditUsername
                ]
            , H.label_
                [ HP.class_ "text-sm text-muted-foreground" ]
                [ text "Display Name" ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.value_ (mEditDisplayName m)
                , HP.placeholder_ "Display Name"
                , H.onInput SetEditDisplayName
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm" ]
                  [ text err ]
            , H.button_
                [ HP.class_ "btn w-full"
                , style_ [("touch-action", "manipulation")]
                ]
                [ text "Save" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground" ]
            [ H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoProfile
                ]
                [ text "Cancel" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Game Screen
-- ---------------------------------------------------------------------------

viewGame :: Model -> View Model Action
viewGame m
  -- Show waiting screen when multiplayer game waiting for opponent
  | mGameMode m == MultiplayerMode, Nothing <- mOpponentName m =
    H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ H.div_
          [ HP.class_ "card p-6 w-full max-w-md text-center"
          , style_ [("margin-top", "4em")]
          ]
          [ H.h2_
              [ HP.class_ "text-xl font-bold mb-4" ]
              [ text "Waiting for opponent..." ]
          , H.div_
              [ HP.class_ "animate-pulse text-muted-foreground mb-4" ]
              [ text "Share the invite link to start" ]
          , case mInviteCode m of
              Just code -> H.div_
                [ HP.class_ "flex flex-col gap-2 items-center" ]
                [ H.div_
                    [ HP.class_ "font-mono text-lg tracking-widest text-foreground" ]
                    [ text code ]
                , H.button_
                    [ HP.class_ "btn btn-outline btn-sm text-foreground"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick (CopyInviteCode code)
                    ]
                    [ text "Copy Invite Code" ]
                ]
              Nothing -> text ""
          ]
      ]
  | otherwise =
    let zen = mViewMode m == ZenView
        showEval = mGameMode m /= MultiplayerMode
    in H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ H.div_
          -- margin-top set via #board-row in styles.css (reduced in fullscreen+zen on small screens)
          [ HP.id_ "board-row"
          , HP.class_ ("flex flex-row items-stretch justify-center gap-2" <> if zen then " zen" else "")
          ]
          [ if showEval && not zen then viewEvalBar m else text ""
          , viewBoardPanel m
          ]
      , if zen then text "" else viewStatus m
      , if zen then text "" else viewMoveHistory m
      , if zen then text ""
        else if mGameMode m == MultiplayerMode then viewMultiplayerControls m else text ""
      , if zen then text "" else viewShareLink m
      ]

-- ---------------------------------------------------------------------------
-- Replay Screen
-- ---------------------------------------------------------------------------

viewReplay :: Model -> View Model Action
viewReplay m
  | mReplayNotFound m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8"
      ]
      [ text "This game is private or doesn\x2019t exist." ]
  | Nothing <- mReplayGame m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading game..." ]
  | Just gr <- mReplayGame m =
    let zen = mViewMode m == ZenView
    in H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ if zen then text "" else viewReplayHeader gr
      , case mReplayStates m of
          [] -> H.div_
            [ HP.class_ "card p-6 text-center mt-4"
            ]
            [ text "No move data available for this game." ]
          states ->
            let gs = states !! mReplayIndex m
                n = boardSize (gsBoard gs)
            in H.div_
              [ HP.class_ "w-full flex flex-col items-center"
              ]
              [ H.div_
                  [ HP.class_ "flex flex-row items-stretch justify-center gap-2"
                  , style_ [("margin-top", "1em")]
                  ]
                  [ if not zen then viewEvalBar m else text ""
                  , viewReplayBoardPanel m gs
                  ]
              , viewReplayControls m n
              , if zen then text "" else viewReplayMoveList m n
              ]
      ]

viewReplayHeader :: GameRecord -> View Model Action
viewReplayHeader gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
  in H.div_
    [ HP.class_ "text-center mb-2"
    , style_ [("margin-top", "2em")]
    ]
    [ H.h2_
        [ HP.class_ "text-lg font-bold" ]
        [ text (grVariant gr) ]
    , H.p_
        [ HP.class_ "text-sm text-muted-foreground" ]
        [ text (winText <> " \x00B7 " <> ms (show (grTotalMoves gr)) <> " moves") ]
    ]

viewReplayBoardPanel :: Model -> GameState -> View Model Action
viewReplayBoardPanel m gs =
  let n = boardSize (gsBoard gs)
      totalPx = sqSize * n
      fs = mIsFullscreen m
      zen = mViewMode m == ZenView
      fsSize = if zen
        then "85vmin"
        else "clamp(50vmin, calc(100vh - 29rem), 85vmin)"
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border"
    , style_ (if fs
        then [("width", fsSize), ("height", fsSize)]
        else [("width", ms totalPx <> "px"), ("max-width", "calc(100vw - 3rem)")])
    ]
    [ viewReplaySVGBoard gs ]

viewReplaySVGBoard :: GameState -> View Model Action
viewReplaySVGBoard gs =
  let board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderReplayLastMove gs n
    )

renderReplayLastMove :: GameState -> Int -> [View Model Action]
renderReplayLastMove gs _n = case gsLastAction gs of
  Nothing -> []
  Just (MoveAction f t) ->
    let movedPiece = pieceAt (gsBoard gs) t
        hlColor = case movedPiece of
          King     -> "color-mix(in oklch, var(--piece-king) 40%, transparent)"
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 30%, transparent)"
          _        -> "color-mix(in oklch, var(--piece-defender) 50%, transparent)"
    in [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ hlColor
        ]
    | sq <- [f, t]
    ]

viewReplayControls :: Model -> Int -> View Model Action
viewReplayControls m n =
  let idx = mReplayIndex m
      maxIdx = length (mReplayStates m) - 1
  in H.div_
    [ HP.class_ "flex items-center justify-center gap-2 my-4 w-full"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ replayBtn (ReplayGotoMove 0) "|<" (idx > 0)
    , replayBtn (ReplayGotoMove (idx - 1)) "<" (idx > 0)
    , H.span_
        [ HP.class_ "text-sm font-mono text-muted-foreground min-w-[5em] text-center" ]
        [ text (ms (show idx) <> " / " <> ms (show maxIdx)) ]
    , replayBtn (ReplayGotoMove (idx + 1)) ">" (idx < maxIdx)
    , replayBtn (ReplayGotoMove maxIdx) ">|" (idx < maxIdx)
    , replayBtn ToggleZenMode "Zen" True
    ]

replayBtn :: Action -> MisoString -> Bool -> View Model Action
replayBtn action label enabled =
  H.button_
    [ HP.class_ (if enabled
        then "btn btn-outline btn-sm text-foreground"
        else "btn btn-outline btn-sm text-muted-foreground opacity-50 cursor-not-allowed")
    , style_ [("touch-action", "manipulation"), ("min-width", "2.5em")]
    , SVG.onClick (if enabled then action else NoOp)
    ]
    [ text label ]

viewReplayMoveList :: Model -> Int -> View Model Action
viewReplayMoveList m n =
  case grMoves =<< mReplayGame m of
    Nothing -> H.div_ [] []
    Just moves | null moves -> H.div_ [] []
    Just moves ->
      let states = mReplayStates m
          idx = mReplayIndex m
      in H.div_
        [ HP.class_ "flex flex-col gap-1 items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px")]
        ]
        [ H.div_
            [ HP.class_ "flex justify-between items-center w-full"
            , style_ [("margin-bottom", "0.4em")]
            ]
            [ H.span_
                [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase" ]
                [ text "MOVES" ]
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ replayMoveBtn i move n (i == idx) states
            | (i, move) <- reverse (zip [1..] moves)
            ]
        ]

replayMoveBtn :: Int -> MoveAction -> Int -> Bool -> [GameState] -> View Model Action
replayMoveBtn idx (MoveAction _f t) n isCurrent states =
  let gs = if idx < length states then states !! idx else states !! (length states - 1)
      moveSide = opponentSide gs
      movedPiece = pieceAt (gsBoard gs) t
      pointer = if isCurrent then "> " else "  "
      sideChar = case moveSide of
          AttackerSide -> "A"
          DefenderSide -> "D"
      la = case gsLastAction gs of
        Just (MoveAction f' t') -> pointer <> ms (show idx) <> ". " <> sideChar <> " "
              <> ms (coordStr n f') <> "-" <> ms (coordStr n t')
        _ -> pointer <> ms (show idx) <> ". " <> sideChar
      (textColor, borderColor) = case movedPiece of
        Attacker -> ("var(--piece-attacker)", "var(--piece-attacker)")
        King     -> ("var(--piece-king)", "var(--piece-king)")
        _        -> ("var(--piece-defender)", "var(--piece-defender)")
      activeCls = " border-l-2"
      moveStyle = [("color", textColor), ("border-left-color", borderColor)]
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls)
    , style_ (("touch-action", "manipulation") : moveStyle)
    , SVG.onClick (ReplayGotoMove idx)
    ]
    [ text la ]

-- ---------------------------------------------------------------------------
-- Board
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

-- | Evaluation bar shown left of the board. Positive = attackers favored, negative = defenders.
viewEvalBar :: Model -> View Model Action
viewEvalBar m =
  let score = mEvalScore m
      -- Clamp score to [-1500, 1500], then map to attacker % (0-100)
      clamped = max (-1500) (min 1500 score)
      attackerPct = 50.0 + fromIntegral clamped / 1500.0 * 50.0 :: Double
      defenderPct = 100.0 - attackerPct :: Double
      -- Display score divided by 100 for readability
      displayScore = if score >= 0
        then "+" <> ms (showScore score)
        else ms (showScore score)
  in H.div_
    [ HP.class_ "flex flex-col rounded overflow-hidden border border-border"
    , style_ [ ("width", "20px"), ("flex-shrink", "0"), ("position", "relative") ]
    ]
    [ -- Attacker portion (top)
      H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct attackerPct) <> "%")
                 , ("background", "var(--piece-attacker)") ]
        ] []
    , -- Defender portion (bottom)
      H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct defenderPct) <> "%")
                 , ("background", "var(--piece-defender)") ]
        ] []
    , -- Score label overlay
      H.div_
        [ HP.class_ "absolute text-center"
        , style_ [ ("font-size", "9px"), ("line-height", "1"), ("width", "20px")
                 , ("top", "50%"), ("transform", "translateY(-50%)")
                 , ("color", "var(--muted-foreground)"), ("pointer-events", "none")
                 , ("mix-blend-mode", "difference"), ("font-weight", "bold") ]
        ]
        [ text displayScore ]
    ]

showScore :: Int -> String
showScore s =
  let (q, r) = abs s `divMod` 100
      sign = if s < 0 then "-" else ""
  in sign ++ show q ++ "." ++ (if r < 10 then "0" else "") ++ show r

showPct :: Double -> String
showPct d =
  let n = round d :: Int
  in show n

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  let n = boardSize (gsBoard (mGameState m))
      totalPx = sqSize * n
      fs = mIsFullscreen m
      zen = mViewMode m == ZenView
      fsSize = if zen
        then "85vmin"
        else "clamp(50vmin, calc(100vh - 29rem), 85vmin)"
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border"
    , style_ (if fs
        then [("width", fsSize), ("height", fsSize)]
        else [("width", ms totalPx <> "px"), ("max-width", "calc(100vw - 3rem)")])
    ]
    [ viewSVGBoard m ]

viewSVGBoard :: Model -> View Model Action
viewSVGBoard m =
  let gs    = mGameState m
      board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ renderHighlights m n
    ++ renderValidDots m n
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderLastMove m n
    ++ [ renderClickTarget m n r c | r <- [0..n-1], c <- [0..n-1] ]
    )

svgDefs :: View Model Action
svgDefs =
  SVG.defs_ []
    [ SVG.filter_
        [ HP.id_ "pieceShadow"
        , SP.x_ "-20%", SP.y_ "-20%"
        , HP.width_ "140%", HP.height_ "160%"
        ]
        [ SVG.feDropShadow_
            [ SP.dx_ "0.3", SP.dy_ "0.7"
            , SP.stdDeviation_ "0.5"
            , SP.floodColor_ "#000000"
            , SP.floodOpacity_ "0.45"
            ]
        ]
    ]

-- Square background colors (themed via CSS variables)
renderSquareBg :: Int -> Int -> Int -> View Model Action
renderSquareBg _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , style_ [("fill", if even (r + c) then "var(--muted)" else "var(--accent)")]
    ]

-- Mark corners and center (throne)
renderSpecialSquares :: GameState -> Int -> [View Model Action]
renderSpecialSquares gs n =
  let center = n `div` 2
      w      = cornerBaseWidth (gsRules gs)
      corners = [ (r, c)
                | r <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                , c <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                ]
      markSquare (r, c) color =
        SVG.rect_
          [ SP.x_ (ms (c * sqSize + 2))
          , SP.y_ (ms (r * sqSize + 2))
          , HP.width_ (ms (sqSize - 4))
          , HP.height_ (ms (sqSize - 4))
          , SP.fill_ "none"
          , SP.stroke_ color
          , SP.strokeWidth_ "2"
          , SP.rx_ "3"
          ]
  in map (\pos -> markSquare pos "var(--piece-king)") corners
     ++ [markSquare (center, center) "var(--piece-defender)"]

-- Highlight selected square (colored by selected piece)
renderHighlights :: Model -> Int -> [View Model Action]
renderHighlights m _n = case mSelected m of
  Nothing -> []
  Just sc@(Coords r c) ->
    let hlColor = case pieceAt (gsBoard (mGameState m)) sc of
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 45%, transparent)"
          Defender -> "color-mix(in oklch, var(--piece-defender) 45%, transparent)"
          King     -> "color-mix(in oklch, var(--piece-king) 45%, transparent)"
          _        -> "rgba(80,200,120,0.45)"
    in [ SVG.rect_
        [ SP.x_ (ms (c * sqSize))
        , SP.y_ (ms (r * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ hlColor
        ]
    ]

-- Valid move dots (colored by selected piece)
renderValidDots :: Model -> Int -> [View Model Action]
renderValidDots m _n =
  let dotColor = case mSelected m of
        Nothing -> "rgba(80,200,120,0.6)"
        Just sc -> case pieceAt (gsBoard (mGameState m)) sc of
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 60%, transparent)"
          Defender -> "color-mix(in oklch, var(--piece-defender) 60%, transparent)"
          King     -> "color-mix(in oklch, var(--piece-king) 60%, transparent)"
          _        -> "rgba(80,200,120,0.6)"
  in [ SVG.circle_
      [ SP.cx_ (ms (col coord * sqSize + sqSize `div` 2))
      , SP.cy_ (ms (row coord * sqSize + sqSize `div` 2))
      , SP.r_ (ms (sqSize `div` 5))
      , SP.fill_ dotColor
      ]
  | coord <- mValidMoves m
  ]

-- Last move indicators (colored by the side that moved)
renderLastMove :: Model -> Int -> [View Model Action]
renderLastMove m _n = case gsLastAction (mGameState m) of
  Nothing -> []
  Just (MoveAction f t) ->
    let gs = mGameState m
        movedPiece = pieceAt (gsBoard gs) t
        hlColor = case movedPiece of
          King     -> "color-mix(in oklch, var(--piece-king) 40%, transparent)"
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 30%, transparent)"
          _        -> "color-mix(in oklch, var(--piece-defender) 50%, transparent)"
    in [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ hlColor
        ]
    | sq <- [f, t]
    ]

-- Piece rendering
renderPiece :: Int -> Int -> Int -> Piece -> View Model Action
renderPiece _n r c piece =
  let cx = c * sqSize + sqSize `div` 2
      cy = r * sqSize + sqSize `div` 2
      radius = sqSize `div` 2 - 4
      (fill, stroke, label) = case piece of
        Attacker -> ("var(--piece-attacker)", "var(--border)", "A" :: MisoString)
        Defender -> ("var(--piece-defender)", "var(--border)", "D")
        King     -> ("var(--piece-king)", "var(--piece-king-stroke)", "K")
        Empty    -> ("#000", "#000", "")
  in SVG.g_
    [ SP.filter_ "url(#pieceShadow)" ]
    [ SVG.circle_
        [ SP.cx_ (ms cx)
        , SP.cy_ (ms cy)
        , SP.r_ (ms radius)
        , SP.fill_ fill
        , SP.stroke_ stroke
        , SP.strokeWidth_ "2"
        ]
    , SVG.text_
        [ SP.x_ (ms cx)
        , SP.y_ (ms (cy + 1))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "central"
        , SP.fontSize_ (ms (sqSize `div` 3))
        , SP.fontWeight_ "bold"
        , SP.fill_ (case piece of
            Attacker -> "var(--piece-attacker-fg)"
            King     -> "var(--piece-king-fg)"
            Defender -> "var(--piece-defender-fg)"
            _        -> "#333")
        , SP.fontFamily_ "Arial, sans-serif"
        ]
        [ text label ]
    ]

-- Transparent click targets
renderClickTarget :: Model -> Int -> Int -> Int -> View Model Action
renderClickTarget m _n r c =
  let gs = mGameState m
      side = turnSide gs
      aiBlocked = mGameMode m == AiMode && mAiSide m == side
      mpBlocked = mGameMode m == MultiplayerMode && mPlayerSide m /= Just side
      blocked = mAiThinking m || aiBlocked || mpBlocked || finished (gsResult gs)
      cur = if blocked then "default" else "pointer"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , style_ [("cursor", cur), ("touch-action", "manipulation")]
    , SVG.onClick (CellClicked (Coords r c))
    ]

-- ---------------------------------------------------------------------------
-- Status & Controls
-- ---------------------------------------------------------------------------

viewStatus :: Model -> View Model Action
viewStatus m =
  let gs     = mGameState m
      n      = boardSize (gsBoard gs)
      result = gsResult gs
      side   = turnSide gs
      caps   = gsCaptures gs
      isAi   = mGameMode m == AiMode
      isMp   = mGameMode m == MultiplayerMode
      myTurn = mPlayerSide m == Just side
      baseCls = "text-center my-4 font-bold card px-3 w-full flex justify-center items-center"
      (cls, msg)
        | finished result = case winner result of
            Just AttackerSide -> (baseCls <> " text-destructive", "Attackers win! " <> desc result)
            Just DefenderSide -> (baseCls, "Defenders win! " <> desc result)
            Nothing           -> (baseCls, "Draw! " <> desc result)
        | mAiThinking m = (baseCls <> " text-muted-foreground animate-pulse", "AI thinking...")
        | isAi && mAiSide m == side = (baseCls,
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = (baseCls, "Your turn")
        | isMp && myTurn = (baseCls, "Your turn")
        | isMp = (baseCls <> " text-muted-foreground",
            maybe "Opponent" fromMisoString (mOpponentName m) <> "'s turn")
        | side == AttackerSide = (baseCls, "Attacker's turn")
        | otherwise            = (baseCls, "Defender's turn")
      borderColor
        | not (finished result) = "transparent"
        | otherwise = case winner result of
            Just AttackerSide -> "var(--piece-attacker)"
            Just DefenderSide -> "var(--piece-defender)"
            _                 -> "var(--muted-foreground)"
      capSuffix :: T.Text
      capSuffix
        | finished result || null caps = ""
        | otherwise = let c = length caps
                      in " · Captured " <> T.pack (show c) <> if c == 1 then " piece" else " pieces"
      fullMsg = msg <> capSuffix
  in H.div_
    [ HP.class_ cls
    , style_ [ ("max-width", ms (sqSize * n) <> "px")
             , ("min-height", "3.5rem")
             , ("border", "1px solid " <> borderColor)
             , ("border-radius", "0.375rem")
             ]
    ]
    [ text (ms fullMsg) ]

viewShareLink :: Model -> View Model Action
viewShareLink m =
  let result = gsResult (mGameState m)
  in if finished result
       then case (mGameId m, mSession m) of
         (Just gid, Just sess)
           | amProvider (userAppMetadata (sessionUser sess)) /= "anonymous"
             || mGameMode m == MultiplayerMode
             -> viewShareSection m gid
         _   -> text ""
       else text ""

viewShareSection :: Model -> MisoString -> View Model Action
viewShareSection m gid =
  let url = "https://taflhouse.com/games/" <> gid
      n   = boardSize (gsBoard (mGameState m))
  in H.div_
    [ HP.class_ "flex items-center gap-2 w-full mt-4"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ H.input_
        [ HP.class_ "input input-sm text-muted-foreground bg-transparent border border-border rounded flex-1"
        , HP.readonly_ True
        , HP.value_ url
        , style_ [("font-size", "0.8rem"), ("padding", "0.4rem 0.6rem")]
        ]
    , H.button_
        [ HP.class_ "btn btn-outline btn-sm text-foreground"
        , style_ [("touch-action", "manipulation"), ("white-space", "nowrap")]
        , SVG.onClick CopyGameLink
        ]
        [ text "Copy Link" ]
    ]

viewMultiplayerControls :: Model -> View Model Action
viewMultiplayerControls m =
  let gs = mGameState m
      n = boardSize (gsBoard gs)
      -- Check final state, not viewed state when browsing history
      finalResult = case mFullHistory m of
        Just fs -> gsResult (last fs)
        Nothing -> gsResult gs
      gameOver = finished finalResult
  in if gameOver then text ""
     else H.div_
       [ HP.class_ "flex items-center justify-center gap-2 mt-4"
       , style_ [("max-width", ms (sqSize * n) <> "px")]
       ]
       ([ H.button_
            [ HP.class_ "btn btn-outline btn-sm text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick Resign
            ]
            [ text "Resign" ]
        , if mDrawOffered m
            then H.div_
              [ HP.class_ "flex gap-1" ]
              [ H.button_
                  [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick AcceptDraw
                  ]
                  [ text "Accept Draw" ]
              , H.button_
                  [ HP.class_ "btn btn-outline btn-sm text-foreground"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick DeclineDraw
                  ]
                  [ text "Decline" ]
              ]
            else H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick OfferDraw
              ]
              [ text "Offer Draw" ]
        ] ++ case mOpponentName m of
          Just opp -> [ H.span_
            [ HP.class_ "text-sm text-muted-foreground ml-2" ]
            [ text ("vs " <> opp) ] ]
          Nothing -> [])

viewMoveHistory :: Model -> View Model Action
viewMoveHistory m
  | null (mHistory m) && isNothing (mFullHistory m) =
      let n = boardSize (gsBoard (mGameState m))
      in H.div_
        [ HP.class_ "flex justify-center items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px"), ("margin-top", "0.5em")]
        ]
        [ ctrlBtn ToggleZenMode "Zen" ]
  | otherwise =
      let displayStates = case mFullHistory m of
            Just fs -> fs
            Nothing -> mHistory m ++ [mGameState m]
          n = boardSize (gsBoard (mGameState m))
          viewIdx = length (mHistory m)  -- current viewing position
      in H.div_
        [ HP.class_ "flex flex-col gap-1 items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px")]
        ]
        [ H.div_
            [ HP.class_ "flex justify-between items-center w-full"
            , style_ [("margin-bottom", "0.4em")]
            ]
            [ H.span_
                [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase" ]
                [ text "HISTORY" ]
            , H.div_
                [ HP.class_ "flex gap-1" ]
                [ ctrlBtn ToggleZenMode "Zen"
                , ctrlBtn Undo "Undo"
                ]
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ moveBtn m i gs n (i == viewIdx)
            | (i, gs) <- reverse (zip [0..] displayStates)
            ]
        ]

moveBtn :: Model -> Int -> GameState -> Int -> Bool -> View Model Action
moveBtn m idx gs n isCurrent =
  let moveSide = opponentSide gs  -- side that made this move
      isHuman = case gsLastAction gs of
        Nothing -> False
        Just _  -> mGameMode m == PracticeMode || mAiSide m /= moveSide
      -- Determine piece type that moved (check destination square)
      movedPiece = case gsLastAction gs of
        Nothing            -> Empty
        Just (MoveAction _ t) -> pieceAt (gsBoard gs) t
      -- Current position: > prefix; human moves: bold; AI moves: dim
      pointer = if isCurrent then "> " else "  "
      moveLabel = case gsLastAction gs of
        Nothing -> "Start"
        Just (MoveAction f t) ->
          let sideChar = case moveSide of
                  AttackerSide -> "A"
                  DefenderSide -> "D"
          in ms (show idx) <> ". " <> sideChar <> " "
               <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      label = pointer <> moveLabel
      -- Color-code border and text by piece type for all moves
      (textColor, borderColor) = case movedPiece of
        Attacker -> ("var(--piece-attacker)", "var(--piece-attacker)")
        King     -> ("var(--piece-king)", "var(--piece-king)")
        Defender -> ("var(--piece-defender)", "var(--piece-defender)")
        Empty    -> ("var(--foreground)", "transparent")
      activeCls = " border-l-2"
      moveStyle = [("color", textColor), ("border-left-color", borderColor)]
      boldCls = if isHuman || isCurrent then " font-bold" else ""
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground" <> activeCls <> boldCls)
    , style_ (("touch-action", "manipulation") : moveStyle)
    , SVG.onClick (GotoMove idx)
    ]
    [ text label ]

coordStr :: Int -> Coords -> String
coordStr n (Coords r c) = [toEnum (fromEnum 'a' + c)] ++ show (n - r)


ctrlBtn :: Action -> MisoString -> View Model Action
ctrlBtn action label =
  H.button_
    [ HP.class_ "btn btn-outline btn-sm text-foreground"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

viewZenHint :: Model -> View Model Action
viewZenHint m
  | mZenHint m =
    H.div_
      [ HP.class_ "card px-4 py-2 text-sm text-muted-foreground shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
               , ("transform", "translateX(-50%)"), ("z-index", "9999")
               , ("pointer-events", "none")
               ]
      ]
      [ H.span_ [ HP.class_ "hidden sm:inline" ] [ text "Triple-click board to exit zen mode" ]
      , H.span_ [ HP.class_ "sm:hidden" ] [ text "Triple-tap board to exit zen mode" ]
      ]
  | otherwise = text ""
