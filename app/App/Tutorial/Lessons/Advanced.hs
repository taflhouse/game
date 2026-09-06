{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons.Advanced (advancedLessons) where

import Tafl.Board
import Tafl.Rules (BoardVariant(..))
import Tafl.Game.Board (mkBoard)

import App.Tutorial.Lessons.Types

-- Shorthand
e, a, d, k :: Piece
e = Empty
a = Attacker
d = Defender
k = King

advancedLessons :: [TutorialLesson]
advancedLessons =
  [ lessonExitForts
  ]

-- ---------------------------------------------------------------------------
-- Lesson 10: Exit Forts
-- ---------------------------------------------------------------------------

lessonExitForts :: TutorialLesson
lessonExitForts = TutorialLesson
  { tlId           = "exit-forts"
  , tlTitle        = "Build an Exit Fort"
  , tlModule       = AdvancedModule
  , tlDescription  = "Create an unbreakable formation that guarantees a win."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, d, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, d, e, e]
      , [e, e, e, e, d, k, d, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "An exit fort is a formation where the king and defenders create an unbreakable wall on the edge. If attackers can never break through, the defender wins automatically."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 8 5) PulseHighlight
              , HighlightSquare (Coords 8 4) PulseHighlight
              , HighlightSquare (Coords 8 6) PulseHighlight
              , HighlightSquare (Coords 7 4) PulseHighlight
              , HighlightSquare (Coords 7 6) PulseHighlight
              , HighlightSquare (Coords 7 5) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Complete the fort. Move your defender to seal the wall."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the defender down to close the gap in the wall."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 5 5])
              (Just [Coords 7 5])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 5 5) PulseHighlight
              , HighlightSquare (Coords 7 5) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The fort is sealed. Attackers can't break through, so the defender wins."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      ]
  }
