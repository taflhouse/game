module Tafl.Move
  ( getPossibleMovesFrom
  , getPossibleActions
  , isActionPossible
  , canMakeAMove
  ) where

import Tafl.Types

-- | Four orthogonal directions as (row delta, col delta).
dir4 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | Whether a piece can land on a given square.
-- Empty non-base squares are always valid.
-- The king can also land on corners, and on the center if rules allow.
canMovePieceHere :: GameState -> Piece -> Coords -> Bool
canMovePieceHere gs piece coords =
  let board  = gsBoard gs
      empty  = isEmpty board coords
      king   = piece == King
      base   = isBase gs coords
      center = isCenter board coords
      corner = isCorner gs coords
      centerOk = kingCanReturnToCenter (gsRules gs)
  in (empty && not base) || (king && ((centerOk && center) || corner))

-- | All squares reachable from a position by ray-walking in 4 directions.
-- Stops at the first occupied or non-landable square in each direction.
getPossibleMovesFrom :: GameState -> Coords -> [Coords]
getPossibleMovesFrom gs (Coords r c) = concatMap walk dir4
  where
    board = gsBoard gs
    piece = pieceAt board (Coords r c)
    n     = boardSize board

    walk (dr, dc) = go (r + dr) (c + dc)
      where
        go cr cc
          | cr < 0 || cr >= n || cc < 0 || cc >= n = []
          | canMovePieceHere gs piece (Coords cr cc) =
              Coords cr cc : go (cr + dr) (cc + dc)
          | otherwise = []

-- | All legal moves for the current side (or a specified side).
getPossibleActions :: GameState -> [MoveAction]
getPossibleActions gs = getPossibleActionsForSide gs (turnSide gs)

-- | All legal moves for a given side.
getPossibleActionsForSide :: GameState -> Side -> [MoveAction]
getPossibleActionsForSide gs side =
  [ MoveAction (Coords r c) dest
  | r <- [0 .. n - 1]
  , c <- [0 .. n - 1]
  , let piece = pieceAt board (Coords r c)
  , canControl side piece
  , dest <- getPossibleMovesFrom gs (Coords r c)
  ]
  where
    board = gsBoard gs
    n     = boardSize board

-- | Validate whether a specific move is legal.
isActionPossible :: GameState -> MoveAction -> Bool
isActionPossible gs (MoveAction f t) =
  let board = gsBoard gs
  in
  -- Both endpoints must be in bounds
  insideBounds board f
  && insideBounds board t
  -- Must be orthogonal (same row or same column)
  && (row f == row t || col f == col t)
  -- From square must not be empty
  && pieceAt board f /= Empty
  -- Current side must control the piece
  && canControl (turnSide gs) (pieceAt board f)
  -- To square must be empty
  && isEmpty board t
  -- Path must be clear and destination must be landable
  && pathClear gs (pieceAt board f) f t

-- | Check that every square along the path (exclusive of from, inclusive of to)
-- is landable for the given piece.
pathClear :: GameState -> Piece -> Coords -> Coords -> Bool
pathClear gs piece f t =
  let rowMove = row f == row t
      dr = if rowMove then 0 else if row t > row f then 1 else (-1)
      dc = if rowMove then (if col t > col f then 1 else (-1)) else 0
      go cr cc
        | cr == row t && cc == col t = canMovePieceHere gs piece (Coords cr cc)
        | not (canMovePieceHere gs piece (Coords cr cc)) = False
        | otherwise = go (cr + dr) (cc + dc)
  in go (row f + dr) (col f + dc)

-- | Whether a side has any legal move available. Short-circuits via lazy list.
canMakeAMove :: GameState -> Side -> Bool
canMakeAMove gs side = not (null (getPossibleActionsForSide gs side))
