module Tafl.Game.Surround
  ( didAttackersSurroundDefenders
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Tafl.Board

-- | Four orthogonal directions.
dir4 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | Check if attackers have surrounded all defenders (and king) with
-- no path to the board edge. Uses a layered approach: finds the innermost
-- attacker wall around the king, checks if all defenders are inside,
-- and peels off layers recursively if some defenders are outside.
didAttackersSurroundDefenders :: Board -> Bool
didAttackersSurroundDefenders board =
  case findKing board of
    Nothing -> False
    Just kingPos ->
      let n = boardSize board
          -- Quick check: at least one attacker in each cardinal direction
          -- along the king's row and column. If any direction is clear,
          -- the king has an unblocked line to the edge.
          hasAbove = any (\r -> isAttacker board (Coords r (col kingPos)))
                         [0 .. row kingPos - 1]
          hasBelow = any (\r -> isAttacker board (Coords r (col kingPos)))
                         [row kingPos + 1 .. n - 1]
          hasLeft  = any (\c -> isAttacker board (Coords (row kingPos) c))
                         [0 .. col kingPos - 1]
          hasRight = any (\c -> isAttacker board (Coords (row kingPos) c))
                         [col kingPos + 1 .. n - 1]
      in if not (hasAbove && hasBelow && hasLeft && hasRight)
         then False
         else
           -- Find surrounding attackers via BFS from king
           let surroundingAttackers = possiblySurroundingAttackers board kingPos
               wallSet = Set.fromList surroundingAttackers
               -- Find all squares inside the enclosure
               interior = floodFillInterior board kingPos wallSet
               -- If interior reaches the board edge, not surrounded
               reachesEdge = any (\c ->
                 row c == 0 || row c == n - 1
                 || col c == 0 || col c == n - 1
                 ) (Set.toList interior)
           in if reachesEdge
              then False
              else
                -- Count defenders inside vs total on board
                let defInside = Set.size (Set.filter (isDefender board) interior)
                    defTotal  = countDefenders board
                in if defInside == defTotal
                   then True
                   else -- Some defenders outside: peel off this layer, recurse
                     let board' = Set.foldl' removePiece board wallSet
                     in didAttackersSurroundDefenders board'

-- | Find the king's position on the board.
findKing :: Board -> Maybe Coords
findKing board =
  let n = boardSize board
      kings = [Coords r c | r <- [0..n-1], c <- [0..n-1], isKing board (Coords r c)]
  in case kings of
    (k:_) -> Just k
    []    -> Nothing

-- | Count total non-king defenders on the board.
countDefenders :: Board -> Int
countDefenders board =
  let n = boardSize board
  in length [() | r <- [0..n-1], c <- [0..n-1], isDefender board (Coords r c)]

-- | BFS from start through non-attacker squares, collecting all
-- attackers orthogonally adjacent to the flood-fill region.
possiblySurroundingAttackers :: Board -> Coords -> [Coords]
possiblySurroundingAttackers board start =
  go [start] Set.empty Set.empty
  where
    go [] _ attackers = Set.toList attackers
    go (cur:queue) processed attackers
      | Set.member cur processed = go queue processed attackers
      | not (insideBounds board cur) = go queue (Set.insert cur processed) attackers
      | otherwise =
          let processed' = Set.insert cur processed
              neighbors = [Coords (row cur + dr) (col cur + dc) | (dr, dc) <- dir4]
              (attackers', queue') = foldl (\(as, q) nb ->
                if not (insideBounds board nb) || Set.member nb processed'
                   || Set.member nb as
                then (as, q)
                else if isAttacker board nb
                     then (Set.insert nb as, q)
                     else (as, nb : q)
                ) (attackers, queue) neighbors
          in go queue' processed' attackers'

-- | BFS from start through non-wall squares.
-- Returns all reachable non-wall, in-bounds coordinates.
floodFillInterior :: Board -> Coords -> Set Coords -> Set Coords
floodFillInterior board start wall =
  go [start] Set.empty Set.empty
  where
    go [] _ interior = interior
    go (cur:queue) processed interior
      | Set.member cur processed = go queue processed interior
      | not (insideBounds board cur) = go queue processed' interior
      | Set.member cur wall = go queue processed' interior
      | otherwise =
          let interior' = Set.insert cur interior
              neighbors = [Coords (row cur + dr) (col cur + dc) | (dr, dc) <- dir4]
              queue' = filter (\nb -> not (Set.member nb processed')) neighbors ++ queue
          in go queue' processed' interior'
      where
        processed' = Set.insert cur processed

-- | Remove a piece from the board (set to Empty).
removePiece :: Board -> Coords -> Board
removePiece board (Coords r c) =
  let oldRow = board V.! r
      newRow = oldRow V.// [(c, Empty)]
  in board V.// [(r, newRow)]
