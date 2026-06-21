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
                  object, (.=), (.:), (.:?), withObject, withText)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, jsg, (#))
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut, signInAnonymously
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions, defaultSignInAnonymouslyOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..), AppMetadata(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq, neq)

import Miso.WebSocket (connectJSON, sendJSON, WebSocket, emptyWebSocket, Closed)

import qualified Data.Text as T

import Tafl.Types
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Move  (getPossibleMovesFrom)
import Tafl.Game  (act, initialState)
import Tafl.AI    (AiConfig(..), bestMove)
import Tafl.Protocol (ClientMsg(..), ServerMsg(..))

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

-- | Helper to build a tagged JSON object for Miso.JSON.
misoTagged :: MisoString -> [(MisoString, Value)] -> Value
misoTagged t ps = object (("type" .= t) : ps)

-- | Convert Text to MisoString for JSON serialization.
txt :: T.Text -> MisoString
txt = ms

instance ToJSON ClientMsg where
  toJSON = \case
    CmAuth token gid mName -> misoTagged "auth"       ["token" .= txt token, "game_id" .= txt gid, "display_name" .= fmap txt mName]
    CmCreateGame v ic   -> misoTagged "create_game"  ["variant" .= txt v, "invite_code" .= txt ic]
    CmJoinGame ic       -> misoTagged "join_game"    ["invite_code" .= txt ic]
    CmMove ma           -> misoTagged "move"         ["action" .= ma]
    CmResign            -> misoTagged "resign"       []
    CmOfferDraw         -> misoTagged "offer_draw"   []
    CmAcceptDraw        -> misoTagged "accept_draw"  []
    CmDeclineDraw       -> misoTagged "decline_draw" []

instance FromJSON ClientMsg where
  parseJSON = withObject "ClientMsg" $ \v -> do
    tag <- v .: "type" :: Parser T.Text
    case tag of
      "auth"         -> CmAuth <$> v .: "token" <*> v .: "game_id" <*> v .:? "display_name"
      "create_game"  -> CmCreateGame <$> v .: "variant" <*> v .: "invite_code"
      "join_game"    -> CmJoinGame <$> v .: "invite_code"
      "move"         -> CmMove <$> v .: "action"
      "resign"       -> pure CmResign
      "offer_draw"   -> pure CmOfferDraw
      "accept_draw"  -> pure CmAcceptDraw
      "decline_draw" -> pure CmDeclineDraw
      _              -> fail ("unknown ClientMsg type: " <> show tag)

instance ToJSON ServerMsg where
  toJSON = \case
    SmError msg            -> misoTagged "error"                 ["message" .= txt msg]
    SmAuthOk               -> misoTagged "auth_ok"               []
    SmWaitingForOpponent   -> misoTagged "waiting"               []
    SmGameStarted g u s    -> misoTagged "game_started"          ["game_id" .= txt g, "opponent" .= txt u, "side" .= s]
    SmMoveMade ma cs nt    -> misoTagged "move_made"             ["action" .= ma, "captures" .= cs, "next_turn" .= nt]
    SmMoveRejected msg     -> misoTagged "move_rejected"         ["message" .= txt msg]
    SmGameOver r           -> misoTagged "game_over"             ["result" .= r]
    SmDrawOffered          -> misoTagged "draw_offered"          []
    SmDrawDeclined         -> misoTagged "draw_declined"         []
    SmOpponentConnected    -> misoTagged "opponent_connected"    []
    SmOpponentDisconnected -> misoTagged "opponent_disconnected" []
    SmResigned s           -> misoTagged "resigned"              ["side" .= s]

instance FromJSON ServerMsg where
  parseJSON = withObject "ServerMsg" $ \v -> do
    tag <- v .: "type" :: Parser T.Text
    case tag of
      "error"                 -> SmError <$> v .: "message"
      "auth_ok"               -> pure SmAuthOk
      "waiting"               -> pure SmWaitingForOpponent
      "game_started"          -> SmGameStarted <$> v .: "game_id" <*> v .: "opponent" <*> v .: "side"
      "move_made"             -> SmMoveMade <$> v .: "action" <*> v .: "captures" <*> v .: "next_turn"
      "move_rejected"         -> SmMoveRejected <$> v .: "message"
      "game_over"             -> SmGameOver <$> v .: "result"
      "draw_offered"          -> pure SmDrawOffered
      "draw_declined"         -> pure SmDrawDeclined
      "opponent_connected"    -> pure SmOpponentConnected
      "opponent_disconnected" -> pure SmOpponentDisconnected
      "resigned"              -> SmResigned <$> v .: "side"
      _                       -> fail ("unknown ServerMsg type: " <> show tag)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data GameMode = LocalMode | AiMode | MultiplayerMode
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
  , grIsPublic   :: !Bool
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
      <*> (maybe False id <$> v .:? "is_public")

data DeferredMpAction = DeferCreate | DeferJoin
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
  , mToast         :: Maybe MisoString
  , mShowDepthInfo :: !Bool
  , mShowNodesInfo :: !Bool
  , mLocalGames    :: [GameRecord]
  , mMoveList      :: [MoveAction]
  , mReplayGame    :: Maybe GameRecord
  , mReplayStates  :: [GameState]
  , mReplayIndex   :: !Int
  , mGameId        :: Maybe MisoString
  , mIsPublic        :: !Bool
  , mReplayNotFound  :: !Bool
  -- Multiplayer
  , mGuestName        :: Maybe MisoString
  , mWsConn          :: !WebSocket
  , mWsConnected     :: !Bool
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
        && mToast a == mToast b
        && mShowDepthInfo a == mShowDepthInfo b
        && mShowNodesInfo a == mShowNodesInfo b
        && mLocalGames a == mLocalGames b
        && mMoveList a == mMoveList b
        && mReplayGame a == mReplayGame b
        && mReplayStates a == mReplayStates b
        && mReplayIndex a == mReplayIndex b
        && mGameId a == mGameId b
        && mIsPublic a == mIsPublic b
        && mReplayNotFound a == mReplayNotFound b
        && mGuestName a == mGuestName b
        && mWsConnected a == mWsConnected b
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
  | ToggleGamePublic
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
  | WsConnect MisoString
  | WsConnected WebSocket
  | WsClosed Closed
  | WsError MisoString
  | WsReceived ServerMsg
  | WsSend ClientMsg
  | CreateMultiplayerGame
  | StartMultiplayerGame MisoString MisoString MisoString  -- invCode uuid wsUrl
  | JoinMultiplayerGame
  | StartJoinGame MisoString  -- wsUrl
  | SetJoinCodeInput MisoString
  | MoveRejected MisoString
  | Resign
  | OfferDraw
  | AcceptDraw
  | DeclineDraw
  | SetSidePreference MisoString
  | CopyInviteCode MisoString

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
  Lewis         -> "Lewis"
  Parlett       -> "Parlett"
  DamienWalker  -> "Damien Walker"
  AleaEvangelii -> "Alea Evangelii"

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
foreign import javascript unsafe "globalThis.getWsUrl()"
  js_getWsUrl_raw :: IO JSVal
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
js_getWsUrl_raw :: IO JSVal
js_getWsUrl_raw = toJSVal ("ws://localhost:3000" :: MisoString)
#endif

js_generateUUID :: IO MisoString
js_generateUUID = fromJSValUnchecked =<< js_generateUUID_raw

js_copyToClipboard :: MisoString -> IO ()
js_copyToClipboard s = toJSVal s >>= js_copyToClipboard_raw

js_getWsUrl :: IO MisoString
js_getWsUrl = fromJSValUnchecked =<< js_getWsUrl_raw

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
  , mToast         = Nothing
  , mShowDepthInfo = False
  , mShowNodesInfo = False
  , mLocalGames    = []
  , mMoveList      = []
  , mReplayGame    = Nothing
  , mReplayStates  = []
  , mReplayIndex   = 0
  , mGameId        = Nothing
  , mIsPublic        = False
  , mReplayNotFound  = False
  , mGuestName        = Nothing
  , mWsConn          = emptyWebSocket
  , mWsConnected     = False
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

  ToggleQuoteRef ->
    modify $ \m -> m { mShowQuoteRef = not (mShowQuoteRef m) }

  ShowToast msg -> do
    modify $ \m -> m { mToast = Just msg }
    withSink $ \sink -> do
      threadDelay 3000000
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
        | otherwise -> io_ $ pushURI (gamePermalinkURI uuid)
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
        modify $ \x -> x { mScreen = HomeScreen, mAiThinking = False, mGameId = Nothing }
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
                             , mMoveList = mMoveList m ++ [move] }
          io_ js_playMoveSound
          -- In multiplayer, send the move to the server
          when (mGameMode m == MultiplayerMode) $
            withSink $ \sink -> sink (WsSend (CmMove move))
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
          , mMoveList = mMoveList m ++ [move] }
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
        currentIdx = length allStates - 1
    if i >= 0 && i < currentIdx
      then put $ m
          { mGameState  = allStates !! i
          , mHistory    = take i allStates
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
          }
      else pure ()

  Undo -> do
    m <- get
    case mHistory m of
      [] -> pure ()
      _  -> do
        let prev = last (mHistory m)
        put $ m
          { mGameState  = prev
          , mHistory    = init (mHistory m)
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
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
    signOut defaultSignOutOptions SignOutSuccess AuthError
    modify $ \m -> m
      { mSession         = Nothing
      , mScreen          = HomeScreen
      , mPastGames       = []
      , mLocalGames      = []
      , mAuthError       = Nothing
      , mProfile         = Nothing
      , mNeedsUsername    = False
      , mProfileDropdown = False
      , mGuestName       = Nothing
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
                }
            _ -> modify $ \m -> m
              { mReplayGame   = Just gr
              , mReplayStates = []
              , mReplayIndex  = 0
              }
        [] -> modify $ \m -> m { mReplayNotFound = True }
      Error _ -> modify $ \m -> m { mReplayNotFound = True }

  ReplayLoadError _ -> pure ()

  ReplayGotoMove i -> do
    m <- get
    let maxIdx = length (mReplayStates m) - 1
    modify $ \x -> x { mReplayIndex = max 0 (min maxIdx i) }

  InitGame uuid -> do
    m <- get
    let gs = initialState (mVariant m)
    put $ m
      { mGameId     = Just uuid
      , mIsPublic   = False
      , mScreen     = GameScreen
      , mGameState  = gs
      , mSelected   = Nothing
      , mValidMoves = []
      , mAiThinking = False
      , mHistory    = []
      , mMoveList   = []
      }
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            gameModeStr' = case mGameMode m of
              LocalMode       -> "local" :: MisoString
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

  ToggleGamePublic -> do
    m <- get
    case (mSession m, mGameId m) of
      (Just _, Just gid) -> do
        let newVal = not (mIsPublic m)
        modify $ \x -> x { mIsPublic = newVal, mToast = Just (if newVal then "Game is now public" else "Game is now private") }
        updateTable "games"
          (object ["is_public" .= newVal])
          [eq "id" gid]
          (UpdateOptions Nothing)
          GameUpdated GameUpdateError
        withSink $ \sink -> do
          threadDelay 3000000
          sink DismissToast
      _ -> pure ()

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
  WsConnect wsUrl ->
    connectJSON wsUrl WsConnected WsClosed WsReceived WsError

  WsConnected ws -> do
    modify $ \m -> m { mWsConn = ws, mWsConnected = True }
    m <- get
    -- Send auth; server will reply with SmAuthOk, then we send create/join
    case mSession m of
      Just sess -> do
        let token = sessionAccessToken sess
            gid   = maybe "" id (mGameId m)
            mName = mGuestName m
        sendJSON ws (CmAuth (fromMisoString token) (fromMisoString gid) (fmap fromMisoString mName))
      Nothing -> pure ()

  WsClosed _ ->
    modify $ \m -> m { mWsConnected = False }

  WsError _msg ->
    modify $ \m -> m { mToast = Just "The server is having difficulty. Please try again soon.", mWsConnected = False }

  WsReceived serverMsg -> do
    m <- get
    case serverMsg of
      SmAuthOk -> do
        -- Auth succeeded, now send the create/join message
        let ws = mWsConn m
        case mInviteCode m of
          Just code ->
            let vSlug = variantSlug (mVariant m)
                ic    = fromMisoString code :: T.Text
            in sendJSON ws (CmCreateGame vSlug ic)
          Nothing
            | mJoinCodeInput m /= "" ->
                let jc = fromMisoString (mJoinCodeInput m) :: T.Text
                in sendJSON ws (CmJoinGame jc)
            | otherwise -> pure ()

      SmWaitingForOpponent ->
        pure ()

      SmGameStarted _gid opponent side -> do
        modify $ \x -> x
          { mPlayerSide  = Just side
          , mOpponentName = Just (ms opponent)
          , mScreen      = GameScreen
          }

      SmMoveMade move _caps _nextTurn -> do
        -- If it's the opponent's move, apply it
        let gs = mGameState m
            myTurn = mPlayerSide m == Just (turnSide gs)
        if not myTurn
          then do
            let gs' = act gs move
            modify $ \x -> x
              { mGameState = gs'
              , mHistory   = mHistory m ++ [gs]
              , mMoveList  = mMoveList m ++ [move]
              }
            io_ js_playMoveSound
          else
            -- It's confirmation of our own optimistic move; state already applied
            pure ()

      SmMoveRejected reason -> do
        -- Roll back the optimistic move
        case mHistory m of
          [] -> pure ()
          _  -> do
            let prev = last (mHistory m)
            modify $ \x -> x
              { mGameState = prev
              , mHistory   = init (mHistory m)
              , mMoveList  = if null (mMoveList m) then [] else init (mMoveList m)
              , mSelected  = Nothing
              , mValidMoves = []
              , mToast     = Just ("Move rejected: " <> ms reason)
              }
        withSink $ \sink -> do
          threadDelay 3000000
          sink DismissToast

      SmGameOver result ->
        modify $ \x -> x { mGameState = (mGameState x) { gsResult = result } }

      SmDrawOffered ->
        modify $ \x -> x { mDrawOffered = True }

      SmDrawDeclined ->
        modify $ \x -> x { mDrawOffered = False, mToast = Just "Draw declined" }

      SmOpponentConnected ->
        modify $ \x -> x { mToast = Just "Opponent connected" }

      SmOpponentDisconnected ->
        modify $ \x -> x { mToast = Just "Opponent disconnected" }

      SmResigned _side -> pure ()  -- SmGameOver follows

      SmError msg ->
        modify $ \x -> x { mToast = Just (ms msg) }

  WsSend clientMsg -> do
    m <- get
    sendJSON (mWsConn m) clientMsg

  CreateMultiplayerGame -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferCreate }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> io $ do
        invCode <- generateInviteCode
        uuid <- js_generateUUID
        wsUrl <- js_getWsUrl
        pure (StartMultiplayerGame invCode uuid wsUrl)

  StartMultiplayerGame invCode uuid wsUrl -> do
    m <- get
    let gs = initialState (mVariant m)
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
      , mIsPublic   = False
      }
    connectJSON wsUrl WsConnected WsClosed WsReceived WsError

  JoinMultiplayerGame -> do
    m <- get
    case mSession m of
      Nothing -> do
        modify $ \x -> x { mDeferredMpAction = Just DeferJoin }
        signInAnonymously defaultSignInAnonymouslyOptions AnonAuthSuccess AnonAuthError
      Just _ -> io $ do
        wsUrl <- js_getWsUrl
        pure (StartJoinGame wsUrl)

  StartJoinGame wsUrl -> do
    modify $ \x -> x
      { mGameMode   = MultiplayerMode
      , mInviteCode = Nothing  -- we're joining, not creating
      }
    connectJSON wsUrl WsConnected WsClosed WsReceived WsError

  SetJoinCodeInput s ->
    modify $ \m -> m { mJoinCodeInput = s }

  MoveRejected msg ->
    modify $ \m -> m { mToast = Just msg }

  Resign -> do
    m <- get
    when (mGameMode m == MultiplayerMode && mWsConnected m) $
      sendJSON (mWsConn m) CmResign

  OfferDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode && mWsConnected m) $
      sendJSON (mWsConn m) CmOfferDraw

  AcceptDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode && mWsConnected m) $ do
      sendJSON (mWsConn m) CmAcceptDraw
      modify $ \x -> x { mDrawOffered = False }

  DeclineDraw -> do
    m <- get
    when (mGameMode m == MultiplayerMode && mWsConnected m) $ do
      sendJSON (mWsConn m) CmDeclineDraw
      modify $ \x -> x { mDrawOffered = False }

  SetSidePreference s ->
    modify $ \m -> m { mSidePreference = s }

  CopyInviteCode code -> do
    io_ $ js_copyToClipboard code
    modify $ \m -> m { mToast = Just "Copied!" }
    withSink $ \sink -> do
      threadDelay 3000000
      sink DismissToast

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
        LocalMode       -> "local" :: MisoString
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
  H.div_
    [ HP.class_ "fixed inset-0 flex flex-col bg-background font-sans"
    ]
    [ viewNavbar m
    , H.div_
        [ HP.class_ (if mScreen m == HomeScreen then "flex-1" else "flex-1 overflow-y-auto overscroll-none")
        ]
        [ H.div_
            [ HP.class_ "flex flex-col items-center min-h-full pt-8 pb-12 px-4 mx-auto w-full max-w-7xl"
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
    ]

viewToast :: Model -> View Model Action
viewToast m = case mToast m of
  Nothing -> text ""
  Just msg ->
    H.div_
      [ HP.class_ "card px-4 py-2 text-sm text-foreground shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
               , ("transform", "translateX(-50%)"), ("z-index", "9999")
               , ("user-select", "text"), ("cursor", "text")
               ]
      ]
      [ text msg ]

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
            (themeToggleBtn : navAuthButtons m)
        ]
    ]

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
              then H.div_
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
        "local"       -> "Local"
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
        LocalMode       -> "Local (2 players)"
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
        [ setupBtn (SetGameMode LocalMode) "Local" (mGameMode m == LocalMode)
        , setupBtn (SetGameMode AiMode) "vs AI" (mGameMode m == AiMode)
        , setupBtn (SetGameMode MultiplayerMode) "Multiplayer" (mGameMode m == MultiplayerMode)
        ]
    , setupSection "Board"
        [ setupBtn (SetVariant Brandubh) "Brandubh 7x7" (mVariant m == Brandubh)
        , setupBtn (SetVariant Tablut) "Tablut 9x9" (mVariant m == Tablut)
        , setupBtn (SetVariant Classic) "Copenhagen 11x11" (mVariant m == Classic)
        , setupBtn (SetVariant Line) "Line 11x11" (mVariant m == Line)
        , setupBtn (SetVariant Tawlbwrdd) "Tawlbwrdd 11x11" (mVariant m == Tawlbwrdd)
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
    H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ viewBoardPanel m
      , viewStatus m
      , if mGameMode m == MultiplayerMode then viewMultiplayerControls m else text ""
      , viewMoveHistory m
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
    H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ viewReplayHeader gr
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
              [ viewReplayBoardPanel gs
              , viewReplayControls m n
              , viewReplayMoveList m n
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

viewReplayBoardPanel :: GameState -> View Model Action
viewReplayBoardPanel gs =
  let n = boardSize (gsBoard gs)
      totalPx = sqSize * n
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border w-full"
    , style_ [("max-width", ms totalPx <> "px"), ("margin-top", "1em")]
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
    [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(200,200,80,0.3)"
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
replayMoveBtn idx (MoveAction f t) n isCurrent states =
  let gs = if idx < length states then states !! idx else states !! (length states - 1)
      moveSide = opponentSide gs
      pointer = if isCurrent then "> " else "  "
      sideChar = case moveSide of
        AttackerSide -> "A"
        DefenderSide -> "D"
      label = pointer <> ms (show idx) <> ". " <> sideChar <> " "
            <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      activeCls = if isCurrent
        then " text-green-500 dark:text-green-400 border-l-2 border-green-500 dark:border-green-400"
        else " border-l-2 border-transparent"
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls)
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (ReplayGotoMove idx)
    ]
    [ text label ]

-- ---------------------------------------------------------------------------
-- Board
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  let n = boardSize (gsBoard (mGameState m))
      totalPx = sqSize * n
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border w-full"
    , style_ [("max-width", ms totalPx <> "px"), ("margin-top", "4em")]
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
  in map (\pos -> markSquare pos "var(--destructive)") corners
     ++ [markSquare (center, center) "var(--primary)"]

-- Highlight selected square
renderHighlights :: Model -> Int -> [View Model Action]
renderHighlights m _n = case mSelected m of
  Nothing -> []
  Just (Coords r c) ->
    [ SVG.rect_
        [ SP.x_ (ms (c * sqSize))
        , SP.y_ (ms (r * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(80,200,120,0.45)"
        ]
    ]

-- Valid move dots
renderValidDots :: Model -> Int -> [View Model Action]
renderValidDots m _n =
  [ SVG.circle_
      [ SP.cx_ (ms (col coord * sqSize + sqSize `div` 2))
      , SP.cy_ (ms (row coord * sqSize + sqSize `div` 2))
      , SP.r_ (ms (sqSize `div` 5))
      , SP.fill_ "rgba(80,200,120,0.6)"
      ]
  | coord <- mValidMoves m
  ]

-- Last move indicators
renderLastMove :: Model -> Int -> [View Model Action]
renderLastMove m _n = case gsLastAction (mGameState m) of
  Nothing -> []
  Just (MoveAction f t) ->
    [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(200,200,80,0.3)"
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
        Attacker -> ("#2a2a2a", "#111", "A" :: MisoString)
        Defender -> ("#e8e0d0", "#555", "D")
        King     -> ("#ffd700", "#8b6914", "K")
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
        , SP.fill_ (if piece == Attacker then "#ccc" else "#333")
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
      cur = if blocked then "not-allowed" else "pointer"
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
      (cls, msg)
        | finished result = case winner result of
            Just AttackerSide -> ("card p-3 my-4 w-full text-center font-bold text-destructive", "Attackers win! " <> desc result)
            Just DefenderSide -> ("card p-3 my-4 w-full text-center font-bold", "Defenders win! " <> desc result)
            Nothing           -> ("card p-3 my-4 w-full text-center font-bold", "Draw! " <> desc result)
        | mAiThinking m = ("text-center my-4 py-1 text-muted-foreground animate-pulse font-bold", "AI thinking...")
        | isAi && mAiSide m == side = ("text-center my-4 py-1 font-bold",
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = ("text-center my-4 py-1 font-bold", "Your turn")
        | isMp && myTurn = ("text-center my-4 py-1 font-bold", "Your turn")
        | isMp = ("text-center my-4 py-1 text-muted-foreground font-bold",
            maybe "Opponent" fromMisoString (mOpponentName m) <> "'s turn")
        | side == AttackerSide = ("text-center my-4 py-1 font-bold", "Attacker's turn")
        | otherwise            = ("text-center my-4 py-1 font-bold", "Defender's turn")
  in H.div_
    [ HP.class_ cls
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ text (ms msg)
    , if not (null caps) && not (finished result)
        then H.div_
          [ HP.class_ "mt-1 text-xs opacity-80 font-normal"
          ]
          [ text (ms ("Last move captured " ++ show (length caps) ++ " piece(s)")) ]
        else text ""
    , if finished result
        then case mGameId m of
          Just gid -> viewShareSection m gid
          Nothing  -> text ""
        else text ""
    ]

viewShareSection :: Model -> MisoString -> View Model Action
viewShareSection m gid =
  let shortGid = ms (take 8 (fromMisoString gid :: String))
  in H.div_
    [ HP.class_ "mt-3 pt-3 border-t border-border font-normal"
    ]
    [ H.div_
        [ HP.class_ "text-sm text-muted-foreground text-center mb-2" ]
        [ text ("taflhouse.com/games/" <> shortGid <> "\x2026") ]
    , H.div_
        [ HP.class_ "flex justify-center gap-2" ]
        [ H.button_
            [ HP.class_ "btn btn-outline btn-sm text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick CopyGameLink
            ]
            [ text "Copy Link" ]
        , case mSession m of
            Just _ -> H.button_
              [ HP.class_ (if mIsPublic m
                  then "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  else "btn btn-outline btn-sm text-foreground")
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick ToggleGamePublic
              ]
              [ text (if mIsPublic m then "\x25CF Public" else "\x25CB Private") ]
            Nothing -> text ""
        ]
    ]

viewMultiplayerControls :: Model -> View Model Action
viewMultiplayerControls m =
  let gs = mGameState m
      n = boardSize (gsBoard gs)
      gameOver = finished (gsResult gs)
  in if gameOver then text ""
     else H.div_
       [ HP.class_ "flex items-center justify-center gap-2"
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
  | null (mHistory m) = H.div_ [] []
  | otherwise =
      let allStates = mHistory m ++ [mGameState m]
          n = boardSize (gsBoard (mGameState m))
          currentIdx = length allStates - 1
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
            , ctrlBtn Undo "Undo"
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ moveBtn m i gs n (i == currentIdx)
            | (i, gs) <- reverse (zip [0..] allStates)
            ]
        ]

moveBtn :: Model -> Int -> GameState -> Int -> Bool -> View Model Action
moveBtn m idx gs n isCurrent =
  let moveSide = opponentSide gs  -- side that made this move
      isHuman = case gsLastAction gs of
        Nothing -> False
        Just _  -> mGameMode m == LocalMode || mAiSide m /= moveSide
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
      activeCls = if isCurrent then " text-green-500 dark:text-green-400 border-l-2 border-green-500 dark:border-green-400" else " border-l-2 border-transparent"
      boldCls = if isHuman || isCurrent then " font-bold" else " text-muted-foreground"
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls <> boldCls)
    , style_ [("touch-action", "manipulation")]
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
