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
variantDefaultRules :: BoardVariant -> RuleSet
variantDefaultRules AleaEvangelii = copenhagen { cornerBaseWidth = 2 }
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
