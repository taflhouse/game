module Tafl.Types
  ( -- * Piece
    Piece(..)
  , Side(..)
    -- * Coordinates
  , Coords(..)
  , MoveAction(..)
    -- * Board
  , Board
  , boardSize
  , pieceAt
  , insideBounds
    -- * Piece predicates
  , isEmpty
  , isKing
  , isAttacker
  , isDefender
  , isDefenderOrKing
  , sideOfPiece
  , canControl
    -- * Board position predicates
  , isCenter
  , isCorner
  , isEdge
  , isBase
    -- * Turn logic
  , turnSide
  , opponentSide
    -- * Rules
  , RuleSet(..)
    -- * Game state
  , GameResult(..)
  , GameState(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Piece types on the board.
data Piece = Empty | Attacker | Defender | King
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The two sides in a tafl game.
data Side = AttackerSide | DefenderSide
  deriving (Eq, Ord, Show)

-- | Board coordinates (0-indexed, row and column).
data Coords = Coords { row :: !Int, col :: !Int }
  deriving (Eq, Ord, Show)

-- | A move from one position to another.
data MoveAction = MoveAction { from :: !Coords, to :: !Coords }
  deriving (Eq, Show)

-- | The game board — a square grid of pieces.
type Board = Vector (Vector Piece)

-- | All configurable rules for a tafl variant.
data RuleSet = RuleSet
  { kingIsArmed            :: !Bool
  , kingCanReturnToCenter  :: !Bool
  , attackerCountToCapture :: !Int
  , repetitionTurnLimit    :: !Int
  , shieldWalls            :: !Bool
  , exitForts              :: !Bool
  , edgeEscape             :: !Bool
  , cornerBaseWidth        :: !Int
  , startingSide           :: !Side
  , saveBoardHistory       :: !Bool
  , saveActions            :: !Bool
  } deriving (Eq, Show)

-- | Result of a game (or in-progress indicator).
data GameResult = GameResult
  { finished :: !Bool
  , winner   :: Maybe Side
  , desc     :: !Text
  } deriving (Eq, Show)

-- | Complete game state.
data GameState = GameState
  { gsBoard        :: !Board
  , gsActions      :: [MoveAction]
  , gsBoardHistory :: Map Text Int
  , gsTurn         :: !Int
  , gsResult       :: !GameResult
  , gsCaptures     :: [Coords]
  , gsLastAction   :: Maybe MoveAction
  , gsRules        :: !RuleSet
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Board helpers
-- ---------------------------------------------------------------------------

-- | Side length of a square board.
boardSize :: Board -> Int
boardSize = V.length

-- | Piece at a given coordinate.
pieceAt :: Board -> Coords -> Piece
pieceAt board (Coords r c) = (board V.! r) V.! c

-- | Whether coordinates are within board bounds.
insideBounds :: Board -> Coords -> Bool
insideBounds board (Coords r c) =
  let n = V.length board
  in r >= 0 && r < n && c >= 0 && c < n

-- ---------------------------------------------------------------------------
-- Piece predicates
-- ---------------------------------------------------------------------------

isEmpty :: Board -> Coords -> Bool
isEmpty board coords = pieceAt board coords == Empty

isKing :: Board -> Coords -> Bool
isKing board coords = pieceAt board coords == King

isAttacker :: Board -> Coords -> Bool
isAttacker board coords = pieceAt board coords == Attacker

isDefender :: Board -> Coords -> Bool
isDefender board coords = pieceAt board coords == Defender

isDefenderOrKing :: Board -> Coords -> Bool
isDefenderOrKing board coords =
  let p = pieceAt board coords
  in p == Defender || p == King

-- | Which side does a piece belong to?
sideOfPiece :: Piece -> Maybe Side
sideOfPiece Attacker = Just AttackerSide
sideOfPiece Defender = Just DefenderSide
sideOfPiece King     = Just DefenderSide
sideOfPiece Empty    = Nothing

-- | Whether a side can move a given piece.
canControl :: Side -> Piece -> Bool
canControl AttackerSide Attacker = True
canControl DefenderSide Defender = True
canControl DefenderSide King     = True
canControl _            _        = False

-- ---------------------------------------------------------------------------
-- Position predicates
-- ---------------------------------------------------------------------------

-- | Whether coordinates are the center (throne) square.
isCenter :: Board -> Coords -> Bool
isCenter board (Coords r c) =
  let n = V.length board
      center = n `div` 2
  in r == center && c == center

-- | Whether coordinates are a corner square (width determined by rules).
isCorner :: GameState -> Coords -> Bool
isCorner gs (Coords r c) =
  let n = boardSize (gsBoard gs)
      w = cornerBaseWidth (gsRules gs)
      rowMatch = any (\ww -> r == ww || r == n - 1 - ww) [0 .. w - 1]
      colMatch = any (\ww -> c == ww || c == n - 1 - ww) [0 .. w - 1]
  in rowMatch && colMatch

-- | Whether coordinates are on the board edge but not a corner.
isEdge :: GameState -> Coords -> Bool
isEdge gs (Coords r c) =
  let n = boardSize (gsBoard gs)
      onEdge = r == 0 || r == n - 1 || c == 0 || c == n - 1
  in onEdge && not (isCorner gs (Coords r c))

-- | Whether coordinates are a "base" square (center or corner).
isBase :: GameState -> Coords -> Bool
isBase gs coords = isCenter (gsBoard gs) coords || isCorner gs coords

-- ---------------------------------------------------------------------------
-- Turn logic
-- ---------------------------------------------------------------------------

-- | Which side's turn is it?
turnSide :: GameState -> Side
turnSide gs =
  let evenTurn = gsTurn gs `mod` 2 == 0
      attackerStarts = startingSide (gsRules gs) == AttackerSide
  in if attackerStarts == evenTurn
     then AttackerSide
     else DefenderSide

-- | The opponent of the current turn's side.
opponentSide :: GameState -> Side
opponentSide gs = case turnSide gs of
  AttackerSide -> DefenderSide
  DefenderSide -> AttackerSide
