module App.Model
  ( -- * Enums
    GameMode(..)
  , gameModeSlug
  , slugToGameMode
  , TimeControl(..)
  , InviteExpiry(..)
  , expirySeconds
  , Screen(..)
  , ViewMode(..)
  , DeferredMpAction(..)
    -- * Game init data
  , GameInitData(..)
    -- * Auth state
  , AuthState(..)
  , initAuthState
  , authEmail
  , authPassword
  , authError
  , authMessage
  , authLoading
    -- * Model
  , Model(..)
  , initModel
  , mAuth
  ) where

import Miso.String (MisoString)
import Miso.Lens (Lens, lens)
import Supabase.Miso.Auth (Session)
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Side(..))
import Tafl.Rules (BoardVariant(..))

import App.JSON (Profile, GameRow, GameRecord)

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

data GameMode = PracticeMode | AiMode | MultiplayerMode
  deriving (Eq, Show)

gameModeSlug :: GameMode -> MisoString
gameModeSlug PracticeMode    = "practice"
gameModeSlug AiMode          = "ai"
gameModeSlug MultiplayerMode = "multiplayer"

slugToGameMode :: MisoString -> Maybe GameMode
slugToGameMode "practice"    = Just PracticeMode
slugToGameMode "ai"          = Just AiMode
slugToGameMode "multiplayer" = Just MultiplayerMode
slugToGameMode _             = Nothing

data TimeControl
  = NoTimeControl
  | BlitzControl !Int    -- total milliseconds per player
  | DailyControl !Int    -- seconds per move
  deriving (Eq, Show)

data InviteExpiry
  = Expiry10Min
  | Expiry1Hour
  | Expiry1Day
  | Expiry1Week
  deriving (Eq, Show)

expirySeconds :: InviteExpiry -> Int
expirySeconds Expiry10Min = 600
expirySeconds Expiry1Hour = 3600
expirySeconds Expiry1Day  = 86400
expirySeconds Expiry1Week = 604800

data Screen = HomeScreen | SignInScreen | SignUpScreen | ConfigScreen | ConfigureScreen | JoinScreen | GameScreen | ReplayScreen | ProfileScreen | ProfileEditScreen | LoadingScreen | LoungeScreen | YourGamesScreen | PlayerScreen | LearnScreen
  deriving (Eq, Show)

data DeferredMpAction = DeferCreate | DeferJoin | DeferFindMatch | DeferToggleInterest
  deriving (Eq, Show)

data ViewMode = NormalView | ZenView
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Auth state
-- ---------------------------------------------------------------------------

data AuthState = AuthState
  { _authEmail    :: !MisoString
  , _authPassword :: !MisoString
  , _authError    :: Maybe MisoString
  , _authMessage  :: Maybe MisoString
  , _authLoading  :: !Bool
  } deriving (Eq)

initAuthState :: AuthState
initAuthState = AuthState "" "" Nothing Nothing False

authEmail, authPassword :: Lens AuthState MisoString
authEmail    = lens _authEmail    $ \r f -> r { _authEmail = f }
authPassword = lens _authPassword $ \r f -> r { _authPassword = f }

authError, authMessage :: Lens AuthState (Maybe MisoString)
authError   = lens _authError   $ \r f -> r { _authError = f }
authMessage = lens _authMessage $ \r f -> r { _authMessage = f }

authLoading :: Lens AuthState Bool
authLoading = lens _authLoading $ \r f -> r { _authLoading = f }

-- ---------------------------------------------------------------------------
-- Game init data (shared between root and game component)
-- ---------------------------------------------------------------------------

data GameInitData
  = NewLocalGame !MisoString !BoardVariant !GameMode !Side !Int !Int
    -- ^ uuid variant mode aiSide aiDepth aiNodeLimit
  | NewMultiplayerGame !BoardVariant !TimeControl !MisoString !MisoString !MisoString !MisoString !Bool !Bool !(Maybe Double) !(Maybe Double) !InviteExpiry
    -- ^ variant timeControl sidePreference invCode uuid qrDataUrl isRated isMatchmaking creatorRating creatorRd inviteExpiry
  | JoinGame !GameRow
    -- ^ joining via invite code (player not yet in the row)
  | ResumeGame !GameRow
    -- ^ resuming an existing game via /play/<uuid>
  deriving (Eq)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { -- Navigation
    mScreen           :: !Screen
  , mGameMode         :: !GameMode
  , mVariant          :: !BoardVariant
  , mGameInitData     :: Maybe GameInitData
  , mReplayGameId     :: Maybe MisoString
    -- AI config (set in configure, passed to game via props)
  , mAiSide           :: !Side
  , mAiDepth          :: !Int
  , mAiNodeLimit      :: !Int
    -- Auth
  , mSession          :: Maybe Session
  , mSessionChecked   :: !Bool
  , _mAuth            :: !AuthState
    -- Profile
  , mProfile          :: Maybe Profile
  , mNeedsUsername    :: !Bool
  , mUsernameInput    :: !MisoString
  , mEditUsername     :: !MisoString
  , mEditDisplayName  :: !MisoString
  , mProfileDropdown  :: !Bool
    -- Home
  , mPastGames        :: [GameRecord]
  , mGamesLoading     :: !Bool
  , mLocalGames       :: [GameRecord]
  , mShowQuoteRef     :: !Bool
  , mQuoteRefGen      :: !Int
    -- Multiplayer config
  , mIsRated          :: !Bool
  , mSidePreference   :: !MisoString
  , mTimeControl      :: !TimeControl
  , mInviteExpiry     :: !InviteExpiry
  , mJoinCodeInput    :: !MisoString
  , mJoinNameInput    :: !MisoString
  , mGuestName        :: Maybe MisoString
  , mDeferredMpAction :: Maybe DeferredMpAction
  , mPendingRatedJoin :: Maybe GameRow
  , mMatchInterested      :: !Bool
  , mMatchInterestChannel :: Maybe Channel
  , mMatchToast           :: Maybe GameRow
  , mMatchModal           :: Maybe GameRow
  , mMatchReadyStep       :: Maybe Int        -- Nothing=hidden, 0=explain, 1=filters
  , mMatchAny             :: !Bool
  , mMatchWantRated       :: !MisoString      -- "rated" / "casual" / "either"
  , mMatchWantTimed       :: !MisoString      -- "timed" / "untimed" / "either"
  , mMatchWantSide        :: !MisoString      -- "attacker" / "defender" / "either"
    -- View mode (used by replay; game component manages its own)
  , mViewMode         :: !ViewMode
  , mIsFullscreen     :: !Bool
  , mZenHint          :: !Bool
    -- UI chrome
  , mConfigExpanded   :: !Bool
  , mConfigModeChosen :: !Bool
  , mToast            :: Maybe MisoString
  , mShowDepthInfo    :: !Bool
  , mShowNodesInfo    :: !Bool
    -- Lounge
  , mLoungeOpen       :: [GameRow]
  , mLoungeLive       :: [GameRow]
  , mLoungeLoading    :: !Bool
  , mLoungeFilter     :: Maybe MisoString
    -- Rankings
  , mRankings         :: [Profile]
    -- Player detail
  , mPlayerDetail     :: Maybe Profile
  , mPlayerGames      :: [GameRow]
  , mPlayerGamesLoading :: !Bool
    -- Tutorial
  , mTutorialLessonId :: Maybe MisoString
    -- Push notifications (app-level)
  , mPushStatus       :: !MisoString     -- "default" / "granted" / "denied" / "unsupported"
  , mPushPopover      :: !Bool
  , mPushBraveHelp    :: !Bool           -- popover shows Brave step-by-step
  , mIsBrave          :: !Bool
  , mIsFirefox        :: !Bool
  , mIsSafari         :: !Bool
  , mIsEdge           :: !Bool
  , mIsMacOS          :: !Bool
  }

-- Manual Eq instance: skip mMatchInterestChannel (Channel wraps JSVal, no Eq)
instance Eq Model where
  a == b =
    mScreen a == mScreen b && mGameMode a == mGameMode b
    && mVariant a == mVariant b && mGameInitData a == mGameInitData b
    && mReplayGameId a == mReplayGameId b
    && mAiSide a == mAiSide b && mAiDepth a == mAiDepth b
    && mAiNodeLimit a == mAiNodeLimit b
    && mSession a == mSession b && mSessionChecked a == mSessionChecked b
    && _mAuth a == _mAuth b
    && mProfile a == mProfile b && mNeedsUsername a == mNeedsUsername b
    && mUsernameInput a == mUsernameInput b
    && mEditUsername a == mEditUsername b && mEditDisplayName a == mEditDisplayName b
    && mProfileDropdown a == mProfileDropdown b
    && mPastGames a == mPastGames b && mGamesLoading a == mGamesLoading b
    && mLocalGames a == mLocalGames b
    && mShowQuoteRef a == mShowQuoteRef b && mQuoteRefGen a == mQuoteRefGen b
    && mIsRated a == mIsRated b && mSidePreference a == mSidePreference b
    && mTimeControl a == mTimeControl b && mInviteExpiry a == mInviteExpiry b
    && mJoinCodeInput a == mJoinCodeInput b && mJoinNameInput a == mJoinNameInput b
    && mGuestName a == mGuestName b
    && mDeferredMpAction a == mDeferredMpAction b
    && mPendingRatedJoin a == mPendingRatedJoin b
    && mMatchInterested a == mMatchInterested b
    -- mMatchInterestChannel skipped (Channel has no Eq)
    && mMatchToast a == mMatchToast b && mMatchModal a == mMatchModal b
    && mMatchReadyStep a == mMatchReadyStep b
    && mMatchAny a == mMatchAny b
    && mMatchWantRated a == mMatchWantRated b
    && mMatchWantTimed a == mMatchWantTimed b
    && mMatchWantSide a == mMatchWantSide b
    && mViewMode a == mViewMode b && mIsFullscreen a == mIsFullscreen b
    && mZenHint a == mZenHint b
    && mConfigExpanded a == mConfigExpanded b
    && mConfigModeChosen a == mConfigModeChosen b
    && mToast a == mToast b
    && mShowDepthInfo a == mShowDepthInfo b && mShowNodesInfo a == mShowNodesInfo b
    && mLoungeOpen a == mLoungeOpen b && mLoungeLive a == mLoungeLive b
    && mLoungeLoading a == mLoungeLoading b && mLoungeFilter a == mLoungeFilter b
    && mRankings a == mRankings b
    && mPlayerDetail a == mPlayerDetail b && mPlayerGames a == mPlayerGames b
    && mPlayerGamesLoading a == mPlayerGamesLoading b
    && mTutorialLessonId a == mTutorialLessonId b
    && mPushStatus a == mPushStatus b
    && mPushPopover a == mPushPopover b
    && mPushBraveHelp a == mPushBraveHelp b
    && mIsBrave a == mIsBrave b
    && mIsFirefox a == mIsFirefox b
    && mIsSafari a == mIsSafari b
    && mIsEdge a == mIsEdge b
    && mIsMacOS a == mIsMacOS b

mAuth :: Lens Model AuthState
mAuth = lens _mAuth $ \r f -> r { _mAuth = f }

-- ---------------------------------------------------------------------------
-- Initial model
-- ---------------------------------------------------------------------------

initModel :: Model
initModel = Model
  { mScreen           = HomeScreen
  , mGameMode         = AiMode
  , mVariant          = Tablut
  , mGameInitData     = Nothing
  , mReplayGameId     = Nothing
  , mAiSide           = DefenderSide
  , mAiDepth          = 4
  , mAiNodeLimit      = 10000
  , mSession          = Nothing
  , mSessionChecked   = False
  , _mAuth            = initAuthState
  , mProfile          = Nothing
  , mNeedsUsername    = False
  , mUsernameInput    = ""
  , mEditUsername     = ""
  , mEditDisplayName  = ""
  , mProfileDropdown  = False
  , mPastGames        = []
  , mGamesLoading     = False
  , mLocalGames       = []
  , mShowQuoteRef     = False
  , mQuoteRefGen      = 0
  , mIsRated          = True
  , mSidePreference   = "either"
  , mTimeControl      = NoTimeControl
  , mInviteExpiry     = Expiry10Min
  , mJoinCodeInput    = ""
  , mJoinNameInput    = ""
  , mGuestName        = Nothing
  , mDeferredMpAction = Nothing
  , mPendingRatedJoin = Nothing
  , mMatchInterested      = False
  , mMatchInterestChannel = Nothing
  , mMatchToast           = Nothing
  , mMatchModal           = Nothing
  , mMatchReadyStep       = Nothing
  , mMatchAny             = True
  , mMatchWantRated       = "either"
  , mMatchWantTimed       = "either"
  , mMatchWantSide        = "either"
  , mViewMode         = NormalView
  , mIsFullscreen     = False
  , mZenHint          = False
  , mConfigExpanded   = False
  , mConfigModeChosen = False
  , mToast            = Nothing
  , mShowDepthInfo    = False
  , mShowNodesInfo    = False
  , mLoungeOpen       = []
  , mLoungeLive       = []
  , mLoungeLoading    = False
  , mLoungeFilter     = Nothing
  , mRankings         = []
  , mPlayerDetail     = Nothing
  , mPlayerGames      = []
  , mPlayerGamesLoading = False
  , mTutorialLessonId = Nothing
  , mPushStatus       = "default"
  , mPushPopover      = False
  , mPushBraveHelp    = False
  , mIsBrave          = False
  , mIsFirefox        = False
  , mIsSafari         = False
  , mIsEdge           = False
  , mIsMacOS          = False
  }
