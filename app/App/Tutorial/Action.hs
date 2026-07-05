module App.Tutorial.Action (TutorialAction(..)) where

import Miso.String (MisoString)

import Tafl.Board (Coords, MoveAction)

data TutorialAction
  = TutorialMount
  | TutorialUnmount
  | TCellClicked Coords
  | TNextStep
  | TBackStep
  | TAdvanceAfterDelay
  | TAutoResponseExec MoveAction
  | TAutoResponseDone
  | TSelectLesson MisoString
  | TBackToLessons
  | TDismissCongrats
  | TPoofsDone
  | TLoadProgress [MisoString]
