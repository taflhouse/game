{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Tafl.Rules
  ( -- * Rules
    RuleSet(..)
    -- * Default rule sets
  , copenhagen
    -- * Board variants
  , BoardVariant(..)
  , variantDefaultRules
  , variantSlug
  , slugToVariant
  ) where

import Data.Text (Text)
import Tafl.Board (Side(..))

-- | All configurable rules for a tafl variant.
data RuleSet = RuleSet
  { kingIsArmed            :: !Bool
  , kingCanReturnToCenter  :: !Bool
  , attackerCountToCapture :: !Int
  , repetitionTurnLimit    :: !Int
  , shieldWalls            :: !Bool
  , exitForts              :: !Bool
  , exitFortRequiresMobileKing :: !Bool
  , edgeEscape             :: !Bool
  , cornerBaseWidth        :: !Int
  , startingSide           :: !Side
  , saveBoardHistory       :: !Bool
  , saveActions            :: !Bool
  , skipExpensiveChecks    :: !Bool
  } deriving (Eq, Show)

-- | Copenhagen tafl rules — the standard modern ruleset.
copenhagen :: RuleSet
copenhagen = RuleSet
  { kingIsArmed            = True
  , kingCanReturnToCenter  = True
  , attackerCountToCapture = 4
  , repetitionTurnLimit    = 3
  , shieldWalls            = True
  , exitFortRequiresMobileKing = True
  , exitForts              = True
  , edgeEscape             = False
  , cornerBaseWidth        = 1
  , startingSide           = AttackerSide
  , saveBoardHistory       = True
  , saveActions            = True
  , skipExpensiveChecks    = False
  }

-- | All available board variants.
data BoardVariant
  = Brandubh          -- ^ 7x7 Irish
  | Tablut            -- ^ 9x9 Saami
  | Classic           -- ^ 11x11 Copenhagen
  | Line              -- ^ 11x11 Linear formation
  | Tawlbwrdd         -- ^ 11x11 Welsh
  | Lewis             -- ^ 11x11 Lewis variant
  | Parlett           -- ^ 13x13 David Parlett variant
  | DamienWalker      -- ^ 15x15 Damien Walker variant
  | AleaEvangelii     -- ^ 19x19 Historical manuscript
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Default rules for a given variant.
-- Alea Evangelii uses wider corners; all others use Copenhagen defaults.
--
-- Exit forts are a Copenhagen invention, tuned for 11x11's 24-against-12 force
-- ratio. The smallest legal fort is a king in an edge pocket: three defenders,
-- or four once the king must also be able to move. That extra piece is a
-- quarter of Brandubh's entire defending force -- a 7x7 fort would need all
-- four defenders placed exactly -- and an eighth of Tablut's. Below 11x11 the
-- mobility clause is therefore relaxed, so the rule stays reachable on boards
-- it was never designed for.
variantDefaultRules :: BoardVariant -> RuleSet
variantDefaultRules AleaEvangelii = copenhagen { cornerBaseWidth = 2 }
variantDefaultRules Brandubh      = copenhagen { exitFortRequiresMobileKing = False }
variantDefaultRules Tablut        = copenhagen { exitFortRequiresMobileKing = False }
variantDefaultRules _             = copenhagen

-- | URL-safe slug for a variant.
variantSlug :: BoardVariant -> Text
variantSlug = \case
  Brandubh      -> "brandubh"
  Tablut        -> "tablut"
  Classic       -> "copenhagen"
  Line          -> "line"
  Tawlbwrdd     -> "tawlbwrdd"
  Lewis         -> "lewis"
  Parlett       -> "parlett"
  DamienWalker  -> "damien-walker"
  AleaEvangelii -> "alea-evangelii"

-- | Look up a variant by its slug.
slugToVariant :: Text -> Maybe BoardVariant
slugToVariant = \case
  "brandubh"       -> Just Brandubh
  "tablut"         -> Just Tablut
  "copenhagen"     -> Just Classic
  "line"           -> Just Line
  "tawlbwrdd"      -> Just Tawlbwrdd
  "lewis"          -> Just Lewis
  "parlett"        -> Just Parlett
  "damien-walker"  -> Just DamienWalker
  "alea-evangelii" -> Just AleaEvangelii
  _                -> Nothing
