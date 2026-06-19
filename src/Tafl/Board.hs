module Tafl.Board
  ( variantBoard
  ) where

import qualified Data.Vector as V

import Tafl.Types (Piece(..), Board)
import Tafl.Rules (BoardVariant(..))

-- Shorthand for board literals
e, a, d, k :: Piece
e = Empty
a = Attacker
d = Defender
k = King

-- | Convert a list-of-lists into an immutable Vector board.
mkBoard :: [[Piece]] -> Board
mkBoard = V.fromList . map V.fromList

-- | Get the starting board layout for a variant.
variantBoard :: BoardVariant -> Board
variantBoard Brandubh      = boardBrandubh
variantBoard Tablut        = boardTablut
variantBoard Classic       = boardClassic
variantBoard Line          = boardLine
variantBoard Tawlbwrdd     = boardTawlbwrdd
variantBoard Lewis         = boardLewis
variantBoard Parlett       = boardParlett
variantBoard DamienWalker  = boardDamienWalker
variantBoard AleaEvangelii = boardAleaEvangelii

-- 7x7 Irish
boardBrandubh :: Board
boardBrandubh = mkBoard
  [ [e, e, e, a, e, e, e]
  , [e, e, e, a, e, e, e]
  , [e, e, e, d, e, e, e]
  , [a, a, d, k, d, a, a]
  , [e, e, e, d, e, e, e]
  , [e, e, e, a, e, e, e]
  , [e, e, e, a, e, e, e]
  ]

-- 9x9 Saami
boardTablut :: Board
boardTablut = mkBoard
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

-- 11x11 Copenhagen
boardClassic :: Board
boardClassic = mkBoard
  [ [e, e, e, a, a, a, a, a, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, e, e, e, e, e, e, e, e]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [a, e, e, e, d, d, d, e, e, e, a]
  , [a, a, e, d, d, k, d, d, e, a, a]
  , [a, e, e, e, d, d, d, e, e, e, a]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [e, e, e, e, e, e, e, e, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, a, a, a, a, a, e, e, e]
  ]

-- 11x11 Linear formation
boardLine :: Board
boardLine = mkBoard
  [ [e, e, e, a, a, a, a, a, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [a, a, d, d, d, k, d, d, d, a, a]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [a, e, e, e, e, d, e, e, e, e, a]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, a, a, a, a, a, e, e, e]
  ]

-- 11x11 Welsh
boardTawlbwrdd :: Board
boardTawlbwrdd = mkBoard
  [ [e, e, e, e, a, a, a, e, e, e, e]
  , [e, e, e, e, a, e, a, e, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [a, a, e, e, d, d, d, e, e, a, a]
  , [a, e, a, d, d, k, d, d, a, e, a]
  , [a, a, e, e, d, d, d, e, e, a, a]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [e, e, e, e, e, a, e, e, e, e, e]
  , [e, e, e, e, a, e, a, e, e, e, e]
  , [e, e, e, e, a, a, a, e, e, e, e]
  ]

-- 11x11 Lewis variant
boardLewis :: Board
boardLewis = mkBoard
  [ [e, e, e, e, a, a, a, e, e, e, e]
  , [e, e, e, e, a, a, a, e, e, e, e]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [a, a, e, e, e, d, e, e, e, a, a]
  , [a, a, d, d, d, k, d, d, d, a, a]
  , [a, a, e, e, e, d, e, e, e, a, a]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [e, e, e, e, e, d, e, e, e, e, e]
  , [e, e, e, e, a, a, a, e, e, e, e]
  , [e, e, e, e, a, a, a, e, e, e, e]
  ]

-- 13x13 David Parlett variant
boardParlett :: Board
boardParlett = mkBoard
  [ [e, e, e, e, a, a, a, a, a, e, e, e, e]
  , [e, e, e, e, e, a, e, a, e, e, e, e, e]
  , [e, e, e, e, e, e, a, e, e, e, e, e, e]
  , [e, e, e, e, e, e, d, e, e, e, e, e, e]
  , [a, e, e, e, d, e, e, e, d, e, e, e, a]
  , [a, a, e, e, e, d, d, d, e, e, e, a, a]
  , [a, e, a, d, e, d, k, d, e, d, a, e, a]
  , [a, a, e, e, e, d, d, d, e, e, e, a, a]
  , [a, e, e, e, d, e, e, e, d, e, e, e, a]
  , [e, e, e, e, e, e, d, e, e, e, e, e, e]
  , [e, e, e, e, e, e, a, e, e, e, e, e, e]
  , [e, e, e, e, e, a, e, a, e, e, e, e, e]
  , [e, e, e, e, a, a, a, a, a, e, e, e, e]
  ]

-- 15x15 Damien Walker variant
boardDamienWalker :: Board
boardDamienWalker = mkBoard
  [ [e, e, e, e, e, a, a, a, a, a, e, e, e, e, e]
  , [e, e, e, e, e, e, a, a, a, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, a, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, a, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, d, e, e, e, e, e, e, e]
  , [a, e, e, e, e, e, d, d, d, e, e, e, e, e, a]
  , [a, a, e, e, e, d, e, d, e, d, e, e, e, a, a]
  , [a, a, a, a, d, d, d, k, d, d, d, a, a, a, a]
  , [a, a, e, e, e, d, e, d, e, d, e, e, e, a, a]
  , [a, e, e, e, e, e, d, d, d, e, e, e, e, e, a]
  , [e, e, e, e, e, e, e, d, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, a, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, a, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, a, a, a, e, e, e, e, e, e]
  , [e, e, e, e, e, a, a, a, a, a, e, e, e, e, e]
  ]

-- 19x19 Historical manuscript
boardAleaEvangelii :: Board
boardAleaEvangelii = mkBoard
  [ [e, e, a, e, e, a, e, e, e, e, e, e, e, a, e, e, a, e, e]
  , [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e]
  , [a, e, e, e, e, a, e, e, e, e, e, e, e, a, e, e, e, e, a]
  , [e, e, e, e, e, e, e, a, e, a, e, a, e, e, e, e, e, e, e]
  , [e, e, e, e, e, e, a, e, d, e, d, e, a, e, e, e, e, e, e]
  , [a, e, a, e, e, a, e, e, e, e, e, e, e, a, e, e, a, e, a]
  , [e, e, e, e, a, e, e, e, e, d, e, e, e, e, a, e, e, e, e]
  , [e, e, e, a, e, e, e, e, d, e, d, e, e, e, e, a, e, e, e]
  , [e, e, e, e, d, e, e, d, e, d, e, d, e, e, d, e, e, e, e]
  , [e, e, e, a, e, e, d, e, d, k, d, e, d, e, e, a, e, e, e]
  , [e, e, e, e, d, e, e, d, e, d, e, d, e, e, d, e, e, e, e]
  , [e, e, e, a, e, e, e, e, d, e, d, e, e, e, e, a, e, e, e]
  , [e, e, e, e, a, e, e, e, e, d, e, e, e, e, a, e, e, e, e]
  , [a, e, a, e, e, a, e, e, e, e, e, e, e, a, e, e, a, e, a]
  , [e, e, e, e, e, e, a, e, d, e, d, e, a, e, e, e, e, e, e]
  , [e, e, e, e, e, e, e, a, e, a, e, a, e, e, e, e, e, e, e]
  , [a, e, e, e, e, a, e, e, e, e, e, e, e, a, e, e, e, e, a]
  , [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e]
  , [e, e, a, e, e, a, e, e, e, e, e, e, e, a, e, e, a, e, e]
  ]
