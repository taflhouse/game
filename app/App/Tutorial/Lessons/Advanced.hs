{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons.Advanced (advancedLessons) where

import Tafl.Board
import Tafl.Rules (BoardVariant(..))
import Tafl.Game.Board (mkBoard)
import Tafl.Game.State (GameState(..), GameResult(..))

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
  , lessonReadingEval
  , lessonRatings
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

-- ---------------------------------------------------------------------------
-- Lesson 11: Reading the Eval Bar
-- ---------------------------------------------------------------------------

lessonReadingEval :: TutorialLesson
lessonReadingEval = TutorialLesson
  { tlId           = "reading-eval"
  , tlTitle        = "Reading the Eval Bar"
  , tlModule       = AdvancedModule
  , tlDescription  = "Understand who's winning at a glance."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, a, e, e, d, e, e, a, e]
      , [e, e, a, d, k, d, a, e, e]
      , [e, a, e, e, d, e, e, a, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = True  -- This is the lesson that introduces the eval bar
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "The eval bar shows who's winning. It appears to the left of the board."
          , tsDetail           = Just "Mostly red means attackers are ahead, mostly purple means defenders are ahead."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      , TutorialStep
          { tsInstruction      = "Capture the attacker and watch the eval bar shift."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the bottom defender up to sandwich the attacker."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 8 4])
              (Just [Coords 7 4])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 8 4) PulseHighlight
              , HighlightSquare (Coords 7 4) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The eval bar shifted. Use it to judge whether your moves are helping or hurting."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 12: Understanding Ratings
-- ---------------------------------------------------------------------------

lessonRatings :: TutorialLesson
lessonRatings = TutorialLesson
  { tlId           = "ratings"
  , tlTitle        = "Understanding Ratings"
  , tlModule       = AdvancedModule
  , tlDescription  = "Learn how the rating system works."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, a, a, a, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      , [a, e, e, e, d, e, e, e, a]
      , [a, a, d, d, k, d, d, a, a]
      , [a, e, e, e, d, e, e, e, a]
      , [e, e, e, e, d, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, a, a, a, e, e, e]
      ]
  , tlInitialTurn  = 0
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "Every player starts at a rating of 1500. Winning raises your rating, losing lowers it."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      , TutorialStep
          { tsInstruction      = "Beating a stronger opponent earns more points. Losing to a weaker opponent costs more."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      , TutorialStep
          { tsInstruction      = "To earn a rating, sign in and play rated games. Your rating becomes more accurate after more games."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      ]
  }
