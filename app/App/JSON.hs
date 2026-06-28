{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module App.JSON
  ( -- * Miso.JSON orphan instances for tafl types
    -- (Coords, MoveAction, Side, GameResult)

    -- * Data types + FromJSON
    GameRow(..)
  , Profile(..)
  , GameRecord(..)
  ) where

import Miso.String (MisoString, ms)
import Miso.JSON (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), (.!=), withObject, withText)

import qualified Data.Text as T

import Tafl.Board (Coords(..), MoveAction(..), Side(..))
import Tafl.Game.State (GameResult(..))

-- ---------------------------------------------------------------------------
-- Miso.JSON instances (Miso uses its own ToJSON/FromJSON, not Data.Aeson)
-- ---------------------------------------------------------------------------

instance ToJSON Coords where
  toJSON (Coords r c) = toJSON [r, c]

instance FromJSON Coords where
  parseJSON v = do
    [r, c] <- parseJSON v
    pure (Coords r c)

instance ToJSON MoveAction where
  toJSON (MoveAction f t) = toJSON [toJSON f, toJSON t]

instance FromJSON MoveAction where
  parseJSON v = do
    [f, t] <- parseJSON v
    pure (MoveAction f t)

instance ToJSON Side where
  toJSON AttackerSide = toJSON ("attacker" :: T.Text)
  toJSON DefenderSide = toJSON ("defender" :: T.Text)

instance FromJSON Side where
  parseJSON = withText "Side" $ \case
    "attacker" -> pure AttackerSide
    "defender" -> pure DefenderSide
    _          -> fail "expected \"attacker\" or \"defender\""

instance ToJSON GameResult where
  toJSON (GameResult fin w d) = object
    [ "finished" .= fin
    , "winner"   .= w
    , "desc"     .= ms d
    ]

instance FromJSON GameResult where
  parseJSON = withObject "GameResult" $ \v ->
    GameResult <$> v .: "finished" <*> v .: "winner" <*> v .: "desc"

-- ---------------------------------------------------------------------------
-- GameRow: parsing game rows from Realtime payloads and DB queries
-- ---------------------------------------------------------------------------

data GameRow = GameRow
  { grwId           :: !MisoString
  , grwVariant      :: !MisoString
  , grwStatus       :: !MisoString
  , grwMoves        :: [MoveAction]
  , grwCurrentTurn  :: !MisoString
  , grwAttackerId   :: Maybe MisoString
  , grwAttackerName :: Maybe MisoString
  , grwDefenderId   :: Maybe MisoString
  , grwDefenderName :: Maybe MisoString
  , grwDrawOfferedBy :: Maybe MisoString
  , grwResultDesc   :: !MisoString
  , grwWinner       :: Maybe MisoString
  , grwTotalMoves   :: !Int
  , grwInviteCode   :: Maybe MisoString
  -- Time control
  , grwTimeControl       :: Maybe MisoString    -- "blitz" | "daily" | null
  , grwAttackerTimeMs    :: Maybe Int            -- remaining ms
  , grwDefenderTimeMs    :: Maybe Int            -- remaining ms
  , grwLastMoveAt        :: Maybe MisoString     -- ISO 8601
  , grwMoveDeadline      :: Maybe MisoString     -- ISO 8601
  , grwTimePerMoveSec    :: Maybe Int
  , grwTimePerPlayerMs   :: Maybe Int
  } deriving (Eq, Show)

instance FromJSON GameRow where
  parseJSON = withObject "GameRow" $ \v ->
    GameRow
      <$> v .: "id"
      <*> v .: "variant"
      <*> v .: "status"
      <*> v .:? "moves" .!= []
      <*> v .:? "current_turn" .!= "attacker"
      <*> v .:? "attacker_id"
      <*> v .:? "attacker_name"
      <*> v .:? "defender_id"
      <*> v .:? "defender_name"
      <*> v .:? "draw_offered_by"
      <*> v .:? "result_desc" .!= "in_progress"
      <*> v .:? "winner"
      <*> v .:? "total_moves" .!= 0
      <*> v .:? "invite_code"
      <*> v .:? "time_control"
      <*> v .:? "attacker_time_remaining_ms"
      <*> v .:? "defender_time_remaining_ms"
      <*> v .:? "last_move_at"
      <*> v .:? "move_deadline"
      <*> v .:? "time_per_move_seconds"
      <*> v .:? "time_per_player_ms"

-- ---------------------------------------------------------------------------
-- Profile
-- ---------------------------------------------------------------------------

data Profile = Profile
  { pUsername    :: !MisoString
  , pDisplayName :: Maybe MisoString
  } deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v ->
    Profile <$> v .: "username" <*> v .:? "display_name"

-- ---------------------------------------------------------------------------
-- GameRecord (past game summaries)
-- ---------------------------------------------------------------------------

data GameRecord = GameRecord
  { grId         :: Maybe MisoString
  , grVariant    :: !MisoString
  , grResultDesc :: !MisoString
  , grGameMode   :: !MisoString
  , grPlayedAt   :: !MisoString
  , grWinner     :: Maybe MisoString
  , grAiSide     :: Maybe MisoString
  , grTotalMoves :: !Int
  , grAiDepth    :: Maybe Int
  , grMoves      :: Maybe [MoveAction]
  } deriving (Eq, Show)

instance FromJSON GameRecord where
  parseJSON = withObject "GameRecord" $ \v ->
    GameRecord
      <$> v .:? "id"
      <*> v .: "variant"
      <*> v .: "result_desc"
      <*> v .: "game_mode"
      <*> v .: "played_at"
      <*> v .: "winner"
      <*> v .: "ai_side"
      <*> v .: "total_moves"
      <*> v .: "ai_depth"
      <*> v .:? "moves"
