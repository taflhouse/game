{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons.Intermediate (intermediateLessons) where

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

intermediateLessons :: [TutorialLesson]
intermediateLessons =
  [ lessonShieldWall
  , lessonDefendingKing
  , lessonEscapeRoutes
  , lessonPlayingAttacker
  ]

-- ---------------------------------------------------------------------------
-- Lesson 6: Shield Wall Capture
-- ---------------------------------------------------------------------------

lessonShieldWall :: TutorialLesson
lessonShieldWall = TutorialLesson
  { tlId           = "shield-wall"
  , tlTitle        = "Shield Wall Capture"
  , tlModule       = IntermediateModule
  , tlDescription  = "Capture multiple pieces at once by trapping them against the edge."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, a, a, e, e, e, e, e, e]
      , [e, d, d, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, d, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "A shield wall captures multiple enemies at once. Attackers are lined up on the top edge with defenders behind them."
          , tsDetail           = Just "To complete the wall, a defender must close the gap on the edge itself."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 0 1) PulseHighlight
              , HighlightSquare (Coords 0 2) PulseHighlight
              , HighlightSquare (Coords 1 1) PulseHighlight
              , HighlightSquare (Coords 1 2) PulseHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Close the shield wall! Move the defender onto the edge to trap both attackers."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the defender up to the top edge, next to the attackers."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 3 3])
              (Just [Coords 0 3])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 3 3) PulseHighlight
              , HighlightSquare (Coords 0 3) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Both attackers were captured! Shield walls are a game-changing swing."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 7: Protecting the King
-- ---------------------------------------------------------------------------

lessonDefendingKing :: TutorialLesson
lessonDefendingKing = TutorialLesson
  { tlId           = "defending-king"
  , tlTitle        = "Protecting the King"
  , tlModule       = IntermediateModule
  , tlDescription  = "Learn to block threats to your king."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, a, k, e, e, a, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, d, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "The king is on the throne, surrounded on 3 sides by attackers. If the last side is filled, the king is captured and you lose!"
          , tsDetail           = Just "On the throne the king needs all 4 sides surrounded to be captured. Next to the throne, only 3 sides (the throne counts as one)."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 3 4) PulseHighlight
              , HighlightSquare (Coords 4 3) PulseHighlight
              , HighlightSquare (Coords 5 4) PulseHighlight
              , HighlightSquare (Coords 4 5) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The attacker at the right can move to the open side. Block it before they do!"
          , tsDetail           = Nothing
          , tsHint             = Just "Move your defender to the green square next to the king."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 6 5])
              (Just [Coords 4 5])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 7) PulseHighlight
              , HighlightSquare (Coords 6 5) PulseHighlight
              , HighlightSquare (Coords 4 5) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The king is safe! Always watch for attackers lining up to fill the last side."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 8: Plan Your Escape
-- ---------------------------------------------------------------------------

lessonEscapeRoutes :: TutorialLesson
lessonEscapeRoutes = TutorialLesson
  { tlId           = "escape-routes"
  , tlTitle        = "Plan Your Escape"
  , tlModule       = IntermediateModule
  , tlDescription  = "Think multiple moves ahead to escape with the king."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, a, e, k, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, a, e]
      , [e, e, e, e, e, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "Real escapes require planning ahead. Move the king to a safe square."
          , tsDetail           = Just "Think several moves ahead, not just one."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = [HighlightSquare (Coords 4 4) PulseHighlight]
          }
      , TutorialStep
          { tsInstruction      = "Move the king down toward the bottom edge."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the king straight down."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 4 4])
              (Just [Coords 8 4])
              (Just (MoveAction (Coords 7 7) (Coords 7 4)))  -- attacker blocks
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 4) PulseHighlight
              , HighlightSquare (Coords 8 4) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The attacker moved to block. Can you still escape to a corner?"
          , tsDetail           = Nothing
          , tsHint             = Just "The king can go left to reach the bottom-left corner."
          , tsPlayerSide       = DefenderSide
          , tsKind             = ChallengeStep
              (\gs -> let b = gsBoard gs
                      in pieceAt b (Coords 0 0) == King
                         || pieceAt b (Coords 0 8) == King
                         || pieceAt b (Coords 8 0) == King
                         || pieceAt b (Coords 8 8) == King)
              Nothing
          , tsHighlightSquares = []
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 9: Playing as Attacker
-- ---------------------------------------------------------------------------

lessonPlayingAttacker :: TutorialLesson
lessonPlayingAttacker = TutorialLesson
  { tlId           = "playing-attacker"
  , tlTitle        = "Surround the King"
  , tlModule       = IntermediateModule
  , tlDescription  = "Switch sides and learn to trap the king as attacker."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, k, e, e, a, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, a, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      ]
  , tlInitialTurn  = 0  -- attacker's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "Now you're playing as the attacker. Your goal is to surround the king on all 4 sides."
          , tsDetail           = Just "On the throne, you need 4 pieces. Next to it, 3 pieces plus the throne."
          , tsHint             = Nothing
          , tsPlayerSide       = AttackerSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 4) PulseHighlight
              , HighlightSquare (Coords 3 4) PulseHighlight
              , HighlightSquare (Coords 5 4) PulseHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Close in on the king. Move an attacker to block the left side."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the bottom-left attacker up next to the king."
          , tsPlayerSide       = AttackerSide
          , tsKind             = MoveStep
              (Just [Coords 7 3])
              (Just [Coords 4 3])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 7 3) PulseHighlight
              , HighlightSquare (Coords 4 3) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The king is almost surrounded. Close the last gap to capture it!"
          , tsDetail           = Nothing
          , tsHint             = Just "Move the remaining attacker to the open side of the king."
          , tsPlayerSide       = AttackerSide
          , tsKind             = ChallengeStep
              (\gs -> finished (gsResult gs) && winner (gsResult gs) == Just AttackerSide)
              Nothing
          , tsHighlightSquares = []
          }
      ]
  }
