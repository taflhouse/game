{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons.Types
  ( TutorialModule(..)
  , moduleSlug
  , HighlightStyle(..)
  , HighlightSquare(..)
  , StepKind(..)
  , TutorialStep(..)
  , TutorialLesson(..)
  ) where

import Miso.String (MisoString)

import Tafl.Board (Coords, MoveAction, Side(..), Board)
import Tafl.Rules (BoardVariant)
import Tafl.Game.State (GameState)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data TutorialModule = BeginnerModule | IntermediateModule | AdvancedModule
  deriving (Eq, Show)

moduleSlug :: TutorialModule -> MisoString
moduleSlug BeginnerModule     = "beginner"
moduleSlug IntermediateModule = "intermediate"
moduleSlug AdvancedModule     = "advanced"

data HighlightStyle = PulseHighlight | GlowHighlight
  deriving (Eq, Show)

data HighlightSquare = HighlightSquare Coords HighlightStyle
  deriving (Eq, Show)

data StepKind
  = InfoStep                    -- no move required, just Next/Back
  | MoveStep
      (Maybe [Coords])          -- allowedPieces (Nothing = any own piece)
      (Maybe [Coords])          -- allowedTargets (Nothing = normal rules)
      (Maybe MoveAction)        -- autoResponse after player moves
  | ChallengeStep
      (GameState -> Bool)        -- success predicate
      (Maybe MoveAction)        -- autoResponse after player's successful move

data TutorialStep = TutorialStep
  { tsInstruction      :: MisoString
  , tsDetail           :: Maybe MisoString
  , tsHint             :: Maybe MisoString
  , tsPlayerSide       :: Side
  , tsKind             :: StepKind
  , tsHighlightSquares :: [HighlightSquare]
  }

data TutorialLesson = TutorialLesson
  { tlId           :: MisoString
  , tlTitle        :: MisoString
  , tlModule       :: TutorialModule
  , tlDescription  :: MisoString
  , tlVariant      :: BoardVariant
  , tlInitialBoard :: Board
  , tlInitialTurn  :: Int
  , tlShowEvalBar  :: Bool
  , tlSteps        :: [TutorialStep]
  }
