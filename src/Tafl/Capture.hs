module Tafl.Capture
  ( checkCaptures
  ) where

import Tafl.Types

import qualified Data.Vector as V

-- | Four orthogonal directions as (row delta, col delta).
dir4 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | After a piece moves to @landing@, check all adjacent enemy pieces
-- for sandwich captures. Returns the list of captured coordinates.
checkCaptures :: GameState -> Coords -> [Coords]
checkCaptures gs landing =
  concatMap (checkDirection gs landing) dir4

-- | Check one direction from the landing square for a sandwich capture.
checkDirection :: GameState -> Coords -> (Int, Int) -> [Coords]
checkDirection gs landing (dr, dc) =
  let board  = gsBoard gs
      n      = boardSize board
      adj    = Coords (row landing + dr) (col landing + dc)
      beyond = Coords (row landing + 2 * dr) (col landing + 2 * dc)
  in
  -- Adjacent square must be in bounds and contain an enemy piece
  if not (insideBounds board adj) then []
  else if isEmpty board adj then []
  else
    let movingSide   = sideOfPiece (pieceAt board landing)
        adjPiece     = pieceAt board adj
        adjSide      = sideOfPiece adjPiece
    in
    -- Must be an enemy
    if movingSide == adjSide then []
    -- King has special capture rules
    else if adjPiece == King then checkKingCapture gs adj
    else
      -- Standard sandwich: the "beyond" square must contain a friendly piece
      -- OR be a hostile base (corner/center)
      if insideBounds board beyond
         && not (isEmpty board beyond)
         && sideOfPiece (pieceAt board beyond) == movingSide
      then [adj]
      -- Base squares (corners, empty throne) act as capture helpers
      else if isBase gs adj == False  -- adj is not on a base
            && insideBounds board beyond
            && isEmpty board beyond
            && isBase gs beyond
      then [adj]
      -- Out of bounds beyond
      else []

-- | Check if the king is captured. The king requires being surrounded
-- on all 4 sides by attackers (or hostile squares).
-- With attackerCountToCapture = 4 (Copenhagen), all 4 sides needed.
checkKingCapture :: GameState -> Coords -> [Coords]
checkKingCapture gs kingPos =
  let board  = gsBoard gs
      needed = attackerCountToCapture (gsRules gs)
      -- Count how many of the 4 neighbors are hostile to the king
      hostileCount = length $ filter (isHostileToKing gs) $
        map (\(dr,dc) -> Coords (row kingPos + dr) (col kingPos + dc)) dir4
  in if hostileCount >= needed then [kingPos] else []

-- | A square is hostile to the king if it contains an attacker,
-- is a corner, or is the empty throne.
isHostileToKing :: GameState -> Coords -> Bool
isHostileToKing gs coords =
  let board = gsBoard gs
  in if not (insideBounds board coords) then False
     else isAttacker board coords
          || isCorner gs coords
          || (isCenter board coords && isEmpty board coords)
