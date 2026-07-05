{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Model
  ( TutorialModel(..)
  , TutorialProps(..)
  , initialTutorialModel
  ) where

import Miso.String (MisoString)

import Tafl.Board (Coords, MoveAction, Piece(..))
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (initialState)
import Tafl.Game.State (GameState)

import App.Tutorial.Lessons (TutorialLesson(..))

data TutorialProps = TutorialProps
  { tpLessonId :: Maybe MisoString
  } deriving (Eq)

data TutorialModel = TutorialModel
  { tmLesson           :: Maybe TutorialLesson
  , tmStepIndex        :: Int
  , tmGameState        :: GameState
  , tmSelected         :: Maybe Coords
  , tmValidMoves       :: [Coords]
  , tmAnimateMove      :: Maybe MoveAction
  , tmCapturePoofs     :: [(Coords, Piece)]
  , tmEvalScore        :: Int
  , tmShowCongrats     :: Bool
  , tmStepComplete     :: Bool
  , tmFailureCount     :: Int
  , tmShowHint         :: Bool
  , tmLessonNotFound   :: Bool
  , tmCompletedLessons :: [MisoString]
  , tmStateHistory     :: [GameState]   -- start-of-step game state, indexed by step number
  }

-- Manual Eq instance: compare tmLesson by tlId (StepKind contains functions)
instance Eq TutorialModel where
  a == b =
    fmap tlId (tmLesson a) == fmap tlId (tmLesson b)
    && tmStepIndex a == tmStepIndex b
    && tmGameState a == tmGameState b
    && tmSelected a == tmSelected b
    && tmValidMoves a == tmValidMoves b
    && tmAnimateMove a == tmAnimateMove b
    && tmCapturePoofs a == tmCapturePoofs b
    && tmEvalScore a == tmEvalScore b
    && tmShowCongrats a == tmShowCongrats b
    && tmStepComplete a == tmStepComplete b
    && tmFailureCount a == tmFailureCount b
    && tmShowHint a == tmShowHint b
    && tmLessonNotFound a == tmLessonNotFound b
    && tmCompletedLessons a == tmCompletedLessons b
    && tmStateHistory a == tmStateHistory b

initialTutorialModel :: TutorialModel
initialTutorialModel = TutorialModel
  { tmLesson           = Nothing
  , tmStepIndex        = 0
  , tmGameState        = initialState Tablut
  , tmSelected         = Nothing
  , tmValidMoves       = []
  , tmAnimateMove      = Nothing
  , tmCapturePoofs     = []
  , tmEvalScore        = 0
  , tmShowCongrats     = False
  , tmStepComplete     = False
  , tmFailureCount     = 0
  , tmShowHint         = False
  , tmLessonNotFound   = False
  , tmCompletedLessons = []
  , tmStateHistory     = []
  }
