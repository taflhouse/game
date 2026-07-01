module App.Game.Model
  ( GameModel(..)
  , GameProps(..)
  , initialGameModel
  ) where

import Miso.String (MisoString)
import Supabase.Miso.Auth (Session)

import Tafl.Board (Coords, Side(..), MoveAction)
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (initialState)
import Tafl.Game.State (GameState)

import App.JSON (Profile, ChatMessage)
import App.Model (GameMode(..), TimeControl(..), ViewMode(..), GameInitData(..))

-- | Props passed from the root component to the game component.
data GameProps = GameProps
  { gpSession   :: Maybe Session
  , gpProfile   :: Maybe Profile
  , gpGuestName :: Maybe MisoString
  , gpInitData  :: GameInitData
  } deriving (Eq)

-- | Game component model.
data GameModel = GameModel
  { -- Core game
    gmGameState    :: !GameState
  , gmSelected     :: Maybe Coords
  , gmValidMoves   :: [Coords]
  , gmHistory      :: [GameState]
  , gmBrowseIndex  :: Maybe Int
  , gmMoveList     :: [MoveAction]
  , gmEvalScore    :: !Int
  , gmFullHistory  :: Maybe [GameState]
  , gmFullMoveList :: Maybe [MoveAction]
  , gmGameId       :: Maybe MisoString
  , gmVariant      :: !BoardVariant
  , gmGameMode     :: !GameMode
    -- AI
  , gmAiSide       :: !Side
  , gmAiThinking   :: !Bool
  , gmAiDepth      :: !Int
  , gmAiNodeLimit  :: !Int
    -- Multiplayer
  , gmInviteCode    :: Maybe MisoString
  , gmQrDataUrl     :: Maybe MisoString
  , gmOpponentName  :: Maybe MisoString
  , gmPlayerSide    :: Maybe Side
  , gmAttackerName  :: Maybe MisoString
  , gmDefenderName  :: Maybe MisoString
  , gmDrawOffered   :: !Bool
  , gmSpectatorCount :: !Int
    -- Time control
  , gmTimeControl    :: !TimeControl
  , gmAttackerTimeMs :: !Int
  , gmDefenderTimeMs :: !Int
  , gmLastMoveAt     :: Maybe MisoString
  , gmMoveDeadline   :: Maybe MisoString
  , gmDailyTick      :: !Int
    -- View mode
  , gmViewMode     :: !ViewMode
  , gmIsFullscreen  :: !Bool
  , gmZenHint      :: !Bool
    -- Chat
  , gmChatMessages     :: [ChatMessage]
  , gmChatOpen         :: !Bool
  , gmChatInput        :: !MisoString
  , gmChatUnread       :: !Int
  , gmShowSpectatorChat :: !Bool
  } deriving (Eq)

initialGameModel :: GameModel
initialGameModel = GameModel
  { gmGameState    = initialState Tablut
  , gmSelected     = Nothing
  , gmValidMoves   = []
  , gmHistory      = []
  , gmBrowseIndex  = Nothing
  , gmMoveList     = []
  , gmEvalScore    = 0
  , gmFullHistory  = Nothing
  , gmFullMoveList = Nothing
  , gmGameId       = Nothing
  , gmVariant      = Tablut
  , gmGameMode     = AiMode
  , gmAiSide       = DefenderSide
  , gmAiThinking   = False
  , gmAiDepth      = 4
  , gmAiNodeLimit  = 10000
  , gmInviteCode   = Nothing
  , gmQrDataUrl    = Nothing
  , gmOpponentName  = Nothing
  , gmPlayerSide    = Nothing
  , gmAttackerName  = Nothing
  , gmDefenderName  = Nothing
  , gmDrawOffered  = False
  , gmSpectatorCount = 0
  , gmTimeControl  = NoTimeControl
  , gmAttackerTimeMs = 0
  , gmDefenderTimeMs = 0
  , gmLastMoveAt   = Nothing
  , gmMoveDeadline = Nothing
  , gmDailyTick    = 0
  , gmViewMode     = NormalView
  , gmIsFullscreen = False
  , gmZenHint      = False
  , gmChatMessages     = []
  , gmChatOpen         = False
  , gmChatInput        = ""
  , gmChatUnread       = 0
  , gmShowSpectatorChat = False
  }
