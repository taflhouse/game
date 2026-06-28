module App.Replay.Model
  ( ReplayProps(..)
  , ReplayModel(..)
  , initialReplayModel
  ) where

import Miso.String (MisoString)

import Tafl.Game.State (GameState)

import App.JSON (GameRecord)

-- | Props passed from the root component to the replay component.
data ReplayProps = ReplayProps
  { rpGameId       :: !MisoString
  , rpZenMode      :: !Bool
  , rpIsFullscreen :: !Bool
  } deriving (Eq)

-- | Replay component model.
data ReplayModel = ReplayModel
  { rmReplayGame     :: Maybe GameRecord
  , rmReplayStates   :: [GameState]
  , rmReplayIndex    :: !Int
  , rmReplayNotFound :: !Bool
  , rmEvalScore      :: !Int
  } deriving (Eq)

initialReplayModel :: ReplayModel
initialReplayModel = ReplayModel
  { rmReplayGame     = Nothing
  , rmReplayStates   = []
  , rmReplayIndex    = 0
  , rmReplayNotFound = False
  , rmEvalScore      = 0
  }
