module Tafl.Fort
  ( kingEscapedThroughFort
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Tafl.Types

-- | Four orthogonal directions.
dir4 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | Eight directions including diagonals (for connected-component search).
dir8 :: [(Int, Int)]
dir8 = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

-- | Pairs of opposite orthogonal directions for weak-point detection.
oppositeNeighborPairs :: [((Int, Int), (Int, Int))]
oppositeNeighborPairs = [((-1, 0), (1, 0)), ((0, -1), (0, 1))]

-- | Check if the king escaped through an exit fort after the last move.
-- Returns True when the last move placed the king on a non-corner edge
-- square inside a valid fort structure.
kingEscapedThroughFort :: GameState -> Bool
kingEscapedThroughFort gs =
  case gsLastAction gs of
    Nothing -> False
    Just move ->
      let landing = to move
          board = gsBoard gs
      in isKing board landing
         && isEdge gs landing
         && insideFort board landing

-- | Check if the king at the given position is inside a valid exit fort.
-- Requires: king on an edge row or column, at least 2 defenders on the
-- same row/column, and the fort structure passes validation.
insideFort :: Board -> Coords -> Bool
insideFort board kingPos =
  let n = boardSize board
      onTopOrBottom = row kingPos == 0 || row kingPos == n - 1
      onLeftOrRight = col kingPos == 0 || col kingPos == n - 1
  in if not onTopOrBottom && not onLeftOrRight
     then False
     else
       let defenderCount
             | onTopOrBottom =
                 length [c | c <- [0..n-1], isDefender board (Coords (row kingPos) c)]
             | otherwise =
                 length [r | r <- [0..n-1], isDefender board (Coords r (col kingPos))]
       in defenderCount >= 2 && fortSearchFromKing board kingPos

-- | Core fort validation. Checks that defenders form an impenetrable
-- connected wall spanning across the king's position on the edge.
-- The wall must connect from a defender before the king to a defender
-- after the king (along the edge line), contain no attackers inside,
-- and have no weak points that could be captured via sandwich.
fortSearchFromKing :: Board -> Coords -> Bool
fortSearchFromKing board kingPos =
  let n = boardSize board
      surrounding = possiblySurroundingDefenders board kingPos
      searchOnRow = row kingPos == 0 || row kingPos == n - 1

      onSameSearchLine c
        | searchOnRow = row c == row kingPos
        | otherwise   = col c == col kingPos
      beforeKing c
        | searchOnRow = col c < col kingPos
        | otherwise   = row c < row kingPos
      afterKing c
        | searchOnRow = col c > col kingPos
        | otherwise   = row c > row kingPos

      defendersOnLine = filter onSameSearchLine surrounding
      defendersBefore = filter beforeKing defendersOnLine
      defendersAfter  = filter afterKing defendersOnLine

      -- Starting defenders that are 8-connected to an after-king defender
      fortStarts = filter (\d ->
        let connected = connectedDefenders board d
        in any (`Set.member` connected) defendersAfter
        ) defendersBefore

      structures = deduplicateStructures board fortStarts

  in not (null fortStarts) && all (validateStructure board kingPos) structures

-- | Deduplicate connected-defender components from multiple start points.
deduplicateStructures :: Board -> [Coords] -> [Set Coords]
deduplicateStructures board starts =
  go starts Set.empty
  where
    go [] _ = []
    go (s:ss) seen =
      let comp = connectedDefenders board s
      in if Set.member comp seen
         then go ss seen
         else comp : go ss (Set.insert comp seen)

-- | Validate a fort structure: no attackers inside, no unresolvable
-- weak points. If weak points exist, remove them and recurse.
validateStructure :: Board -> Coords -> Set Coords -> Bool
validateStructure board kingPos structure =
  let (innerSet, attackerSet) = getInterior board kingPos structure
  in if not (Set.null attackerSet)
     then False
     else
       let smallest = smallestFortStructure board innerSet structure
           weakCoords = findWeakPoints board innerSet structure smallest
       in if Set.null weakCoords
          then True
          else
            -- Remove weak defenders and check if remaining structure is valid
            let board' = Set.foldl' removePiece board weakCoords
            in fortSearchFromKing board' kingPos

-- | Remove a piece from the board (set to Empty).
removePiece :: Board -> Coords -> Board
removePiece board (Coords r c) =
  let oldRow = board V.! r
      newRow = oldRow V.// [(c, Empty)]
  in board V.// [(r, newRow)]

-- | BFS from king outward through non-defender squares. Returns all
-- defenders that are orthogonally adjacent to the flood-fill region.
-- These are the defenders that could form the inner face of a fort wall.
possiblySurroundingDefenders :: Board -> Coords -> [Coords]
possiblySurroundingDefenders board start =
  go [start] Set.empty Set.empty
  where
    go [] _ defenders = Set.toList defenders
    go (cur:queue) processed defenders
      | Set.member cur processed = go queue processed defenders
      | not (insideBounds board cur) = go queue (Set.insert cur processed) defenders
      | otherwise =
          let processed' = Set.insert cur processed
              neighbors = [Coords (row cur + dr) (col cur + dc) | (dr, dc) <- dir4]
              (defenders', queue') = foldl (\(ds, q) n ->
                if not (insideBounds board n) || Set.member n processed'
                   || Set.member n ds
                then (ds, q)
                else if isDefender board n
                     then (Set.insert n ds, q)
                     else (ds, n : q)
                ) (defenders, queue) neighbors
          in go queue' processed' defenders'

-- | Find all defenders connected to a starting defender via 8-directional
-- adjacency (including diagonals).
connectedDefenders :: Board -> Coords -> Set Coords
connectedDefenders board start =
  go [start] Set.empty
  where
    go [] visited = visited
    go (cur:stack) visited
      | Set.member cur visited = go stack visited
      | otherwise =
          let visited' = Set.insert cur visited
              neighbors = [Coords (row cur + dr) (col cur + dc) | (dr, dc) <- dir8]
              next = filter (\n ->
                insideBounds board n
                && isDefender board n
                && not (Set.member n visited')
                ) neighbors
          in go (next ++ stack) visited'

-- | BFS from a position through non-wall squares. Returns (innerSet,
-- attackerSet) where innerSet contains empty, defender, and king squares
-- inside the closed structure, and attackerSet contains any attackers.
getInterior :: Board -> Coords -> Set Coords -> (Set Coords, Set Coords)
getInterior board start wall =
  go [start] Set.empty Set.empty Set.empty
  where
    go [] _ inner attackers = (inner, attackers)
    go (cur:queue) processed inner attackers
      | Set.member cur processed = go queue processed inner attackers
      | not (insideBounds board cur) = go queue processed' inner attackers
      | Set.member cur wall = go queue processed' inner attackers
      | otherwise =
          let p = pieceAt board cur
              inner'     = if p /= Attacker then Set.insert cur inner else inner
              attackers' = if p == Attacker then Set.insert cur attackers else attackers
              neighbors = [Coords (row cur + dr) (col cur + dc) | (dr, dc) <- dir4]
              queue' = filter (\n -> not (Set.member n processed')) neighbors ++ queue
          in go queue' processed' inner' attackers'
      where
        processed' = Set.insert cur processed

-- | Smallest fort structure: defenders from the full structure that are
-- orthogonally adjacent to at least one interior square.
smallestFortStructure :: Board -> Set Coords -> Set Coords -> Set Coords
smallestFortStructure board innerSet fullStructure =
  let adjacentDefs = Set.foldl' (\acc c ->
        let neighbors = [Coords (row c + dr) (col c + dc) | (dr, dc) <- dir4]
            defs = filter (\n -> insideBounds board n && isDefender board n) neighbors
        in foldl (flip Set.insert) acc defs
        ) Set.empty innerSet
  in Set.intersection fullStructure adjacentDefs

-- | Find weak defenders in the smallest fort structure. A defender is
-- weak if both squares in any opposite direction pair are threatening
-- (not a defender/king, not interior, not inside an eye).
findWeakPoints :: Board -> Set Coords -> Set Coords -> Set Coords -> Set Coords
findWeakPoints board innerSet fullStructure smallest =
  Set.filter isWeak smallest
  where
    isWeak coords = any (isWeakInPair coords) oppositeNeighborPairs

    isWeakInPair coords ((dr1, dc1), (dr2, dc2)) =
      let n1 = Coords (row coords + dr1) (col coords + dc1)
          n2 = Coords (row coords + dr2) (col coords + dc2)
      in isThreatening n1 && isThreatening n2

    isThreatening n =
      insideBounds board n
      && not (isDefenderOrKing board n)
      && not (Set.member n innerSet)
      && not (isInsideEye board n fullStructure)

-- | Check if a coordinate is inside an "eye" -- a closed pocket within
-- the fort structure that contains no attackers.
isInsideEye :: Board -> Coords -> Set Coords -> Bool
isInsideEye board coords wall =
  let (eyeInner, eyeAttackers) = getInterior board coords wall
  in Set.null eyeAttackers && Set.member coords eyeInner
