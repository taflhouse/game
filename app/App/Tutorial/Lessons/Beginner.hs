{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons.Beginner (beginnerLessons) where

import Tafl.Board
import Tafl.Rules (BoardVariant(..))
import Tafl.Game.Board (mkBoard)
import Tafl.Game.State (GameState(..))

import App.Tutorial.Lessons.Types

-- Shorthand
e, a, d, k :: Piece
e = Empty
a = Attacker
d = Defender
k = King

beginnerLessons :: [TutorialLesson]
beginnerLessons =
  [ lessonBoardAndPieces
  , lessonMovement
  , lessonFirstEscape
  , lessonCapturing
  , lessonSpecialSquares
  ]

-- ---------------------------------------------------------------------------
-- Lesson 1: The Board & Pieces (info-only)
-- ---------------------------------------------------------------------------

lessonBoardAndPieces :: TutorialLesson
lessonBoardAndPieces = TutorialLesson
  { tlId           = "board-and-pieces"
  , tlTitle        = "The Board & Pieces"
  , tlModule       = BeginnerModule
  , tlDescription  = "Meet the pieces and learn the goal of the game."
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
          { tsInstruction      = "This is Tablut, the oldest known Northern European board game. Two sides battle on a 9x9 board."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      , TutorialStep
          { tsInstruction      = "The attackers (red pieces) surround the board. Their goal is to capture the king."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 0 3) PulseHighlight
              , HighlightSquare (Coords 0 4) PulseHighlight
              , HighlightSquare (Coords 0 5) PulseHighlight
              , HighlightSquare (Coords 3 0) PulseHighlight
              , HighlightSquare (Coords 4 0) PulseHighlight
              , HighlightSquare (Coords 4 1) PulseHighlight
              , HighlightSquare (Coords 4 7) PulseHighlight
              , HighlightSquare (Coords 4 8) PulseHighlight
              , HighlightSquare (Coords 3 8) PulseHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The defenders (purple pieces) and their king (gold) start in the center. The king sits on the throne."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 4) PulseHighlight
              , HighlightSquare (Coords 2 4) PulseHighlight
              , HighlightSquare (Coords 3 4) PulseHighlight
              , HighlightSquare (Coords 4 2) PulseHighlight
              , HighlightSquare (Coords 4 3) PulseHighlight
              , HighlightSquare (Coords 4 5) PulseHighlight
              , HighlightSquare (Coords 4 6) PulseHighlight
              , HighlightSquare (Coords 5 4) PulseHighlight
              , HighlightSquare (Coords 6 4) PulseHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "The defender wins by moving the king to any corner. The attacker wins by surrounding the king."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 0 0) GlowHighlight
              , HighlightSquare (Coords 0 8) GlowHighlight
              , HighlightSquare (Coords 8 0) GlowHighlight
              , HighlightSquare (Coords 8 8) GlowHighlight
              ]
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 2: Movement
-- ---------------------------------------------------------------------------

lessonMovement :: TutorialLesson
lessonMovement = TutorialLesson
  { tlId           = "movement"
  , tlTitle        = "How Pieces Move"
  , tlModule       = BeginnerModule
  , tlDescription  = "Learn to move pieces along rows and columns."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn (odd = defender in Tablut)
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "Pieces move in straight lines: up, down, left, or right. They can move any number of empty squares."
          , tsDetail           = Just "Pieces cannot jump over other pieces or move diagonally."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = [HighlightSquare (Coords 4 4) PulseHighlight]
          }
      , TutorialStep
          { tsInstruction      = "Move the defender to the right edge of the board."
          , tsDetail           = Nothing
          , tsHint             = Just "Click the defender, then click the square at the right edge of its row."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 4 4])
              (Just [Coords 4 8])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 4) PulseHighlight
              , HighlightSquare (Coords 4 8) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Your previous position is marked on the board."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares = []
          }
      , TutorialStep
          { tsInstruction      = "Now move the defender down to the green square."
          , tsDetail           = Nothing
          , tsHint             = Just "Click the defender and then click the green square below it."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 4 8])
              (Just [Coords 7 8])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 8) PulseHighlight
              , HighlightSquare (Coords 7 8) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Can you move the defender to the flashing green square?"
          , tsDetail           = Nothing
          , tsHint             = Just "Look for the flashing square, then move the defender there."
          , tsPlayerSide       = DefenderSide
          , tsKind             = ChallengeStep
              (\gs -> pieceAt (gsBoard gs) (Coords 2 8) == Defender)
              Nothing
          , tsHighlightSquares = [HighlightSquare (Coords 2 8) GlowHighlight]
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 3: Escape with the King
-- ---------------------------------------------------------------------------

lessonFirstEscape :: TutorialLesson
lessonFirstEscape = TutorialLesson
  { tlId           = "first-escape"
  , tlTitle        = "Escape with the King"
  , tlModule       = BeginnerModule
  , tlDescription  = "Guide the king to a corner to win the game."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, k, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, a]
      , [e, e, e, e, e, e, e, a, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "The defender wins by moving the king to any corner. Only the king can land on corners."
          , tsDetail           = Nothing
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 5 6) PulseHighlight
              , HighlightSquare (Coords 0 0) GlowHighlight
              , HighlightSquare (Coords 0 8) GlowHighlight
              , HighlightSquare (Coords 8 0) GlowHighlight
              , HighlightSquare (Coords 8 8) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Move the king to the right edge."
          , tsDetail           = Nothing
          , tsHint             = Just "Click the king, then click the square at the right edge of its row."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 5 6])
              (Just [Coords 5 8])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 5 6) PulseHighlight
              , HighlightSquare (Coords 5 8) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Now get the king to a corner! Which one can it reach?"
          , tsDetail           = Just "The attackers are blocking some paths. Look for the open one."
          , tsHint             = Just "Try moving the king up to the top-right corner."
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
-- Lesson 4: Capturing
-- ---------------------------------------------------------------------------

lessonCapturing :: TutorialLesson
lessonCapturing = TutorialLesson
  { tlId           = "capturing"
  , tlTitle        = "Trap and Capture"
  , tlModule       = BeginnerModule
  , tlDescription  = "Learn to capture enemy pieces by sandwiching them."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, d, a, e, d, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "Capture enemies by sandwiching them between two of your pieces in a straight line."
          , tsDetail           = Just "The sandwich must be horizontal or vertical, never diagonal."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 3 3) PulseHighlight
              , HighlightSquare (Coords 3 4) PulseHighlight
              , HighlightSquare (Coords 3 6) PulseHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Sandwich the attacker! Move your defender to complete the trap."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the right defender left to sandwich the attacker between your two pieces."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 3 6])
              (Just [Coords 3 5])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 3 6) PulseHighlight
              , HighlightSquare (Coords 3 5) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Now capture the other attacker. No hints this time!"
          , tsDetail           = Just "Removing attackers clears the path for your king."
          , tsHint             = Just "Sandwich the attacker vertically between two defenders."
          , tsPlayerSide       = DefenderSide
          , tsKind             = ChallengeStep
              (\gs -> pieceAt (gsBoard gs) (Coords 6 4) == Empty)
              Nothing
          , tsHighlightSquares = []
          }
      ]
  }

-- ---------------------------------------------------------------------------
-- Lesson 5: Throne & Corner Captures
-- ---------------------------------------------------------------------------

lessonSpecialSquares :: TutorialLesson
lessonSpecialSquares = TutorialLesson
  { tlId           = "special-squares"
  , tlTitle        = "Throne & Corner Captures"
  , tlModule       = BeginnerModule
  , tlDescription  = "Use the throne and corners as allies in captures."
  , tlVariant      = Tablut
  , tlInitialBoard = mkBoard
      [ [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, a, e, e, e, e]
      , [e, e, e, e, e, e, e, e, e]
      , [e, e, e, e, d, e, e, e, e]
      , [e, a, e, e, e, d, e, e, e]
      ]
  , tlInitialTurn  = 1  -- defender's turn
  , tlShowEvalBar  = False
  , tlSteps        =
      [ TutorialStep
          { tsInstruction      = "The throne and corners act like extra pieces for captures. An enemy next to them can be captured with just one of your pieces on the other side."
          , tsDetail           = Just "These special squares help both sides."
          , tsHint             = Nothing
          , tsPlayerSide       = DefenderSide
          , tsKind             = InfoStep
          , tsHighlightSquares =
              [ HighlightSquare (Coords 4 4) GlowHighlight
              , HighlightSquare (Coords 0 0) GlowHighlight
              , HighlightSquare (Coords 0 8) GlowHighlight
              , HighlightSquare (Coords 8 0) GlowHighlight
              , HighlightSquare (Coords 8 8) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Trap the attacker against the throne. Move your defender up."
          , tsDetail           = Nothing
          , tsHint             = Just "Move the defender up so the attacker is sandwiched between your piece and the throne."
          , tsPlayerSide       = DefenderSide
          , tsKind             = MoveStep
              (Just [Coords 7 4])
              (Just [Coords 6 4])
              Nothing
          , tsHighlightSquares =
              [ HighlightSquare (Coords 7 4) PulseHighlight
              , HighlightSquare (Coords 6 4) GlowHighlight
              , HighlightSquare (Coords 4 4) GlowHighlight
              ]
          }
      , TutorialStep
          { tsInstruction      = "Now use a corner the same way. Can you capture the remaining attacker?"
          , tsDetail           = Just "The throne and corners are your allies; they act as invisible teammates."
          , tsHint             = Just "Move your defender so the attacker is sandwiched between it and the corner."
          , tsPlayerSide       = DefenderSide
          , tsKind             = ChallengeStep
              (\gs -> pieceAt (gsBoard gs) (Coords 8 1) == Empty)
              Nothing
          , tsHighlightSquares = []
          }
      ]
  }
