module Tafl.Rules
  ( -- * Default rule sets
    copenhagen
    -- * Board variants
  , BoardVariant(..)
  , variantDefaultRules
  ) where

import Tafl.Types (RuleSet(..), Side(..))

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
