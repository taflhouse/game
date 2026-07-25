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
  , ChatMessage(..)
  , parseChatMessage
    -- * Tournament types
  , TournamentRow(..)
  , TournamentPlayerRow(..)
  , TournamentPairingRow(..)
  ) where

import Miso.String (MisoString, ms)
import Miso.JSON (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), (.!=), withObject, withText, parseMaybe)

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
  , grwGameMode          :: Maybe MisoString
  , grwIsRated           :: !Bool
  , grwRematchOfferedBy  :: Maybe MisoString
  , grwRematchGameId     :: Maybe MisoString
  , grwIsMatchmaking     :: !Bool
  , grwCreatorRating     :: Maybe Double
  , grwCreatorRd         :: Maybe Double
  , grwInterestStatus    :: Maybe MisoString
  , grwTournamentId      :: Maybe MisoString
  , grwTournamentPairingId :: Maybe MisoString
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
      <*> v .:? "game_mode"
      <*> v .:? "is_rated" .!= True
      <*> v .:? "rematch_offered_by"
      <*> v .:? "rematch_game_id"
      <*> v .:? "is_matchmaking" .!= False
      <*> v .:? "creator_rating"
      <*> v .:? "creator_rd"
      <*> v .:? "interest_status"
      <*> v .:? "tournament_id"
      <*> v .:? "tournament_pairing_id"

-- ---------------------------------------------------------------------------
-- Profile
-- ---------------------------------------------------------------------------

data Profile = Profile
  { pId         :: !MisoString
  , pUsername    :: !MisoString
  , pDisplayName :: Maybe MisoString
  , pRating     :: !Double
  , pRatingRd   :: !Double
  , pGamesRated :: !Int
  } deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v ->
    Profile
      <$> v .: "id"
      <*> v .: "username"
      <*> v .:? "display_name"
      <*> v .:? "rating"      .!= 1500.0
      <*> v .:? "rating_rd"   .!= 350.0
      <*> v .:? "games_rated" .!= 0

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

-- ---------------------------------------------------------------------------
-- ChatMessage
-- ---------------------------------------------------------------------------

data ChatMessage = ChatMessage
  { cmSender    :: !MisoString
  , cmMessage   :: !MisoString
  , cmChannel   :: !MisoString  -- "player" or "spectator"
  , cmCreatedAt :: !MisoString
  } deriving (Eq, Show)

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v ->
    ChatMessage
      <$> v .: "sender_name"
      <*> v .: "message"
      <*> v .: "channel"
      <*> v .: "created_at"

-- | Parse a Realtime INSERT payload into a ChatMessage.
-- Extracts the @"new"@ field from the Postgres Changes event.
parseChatMessage :: Value -> Maybe ChatMessage
parseChatMessage val =
  parseMaybe (withObject "RealtimePayload" $ \o -> do
    newVal <- o .: "new"
    parseJSON newVal) val

-- ---------------------------------------------------------------------------
-- TournamentRow
-- ---------------------------------------------------------------------------

data TournamentRow = TournamentRow
  { trId                :: !MisoString
  , trName              :: !MisoString
  , trDescription       :: Maybe MisoString
  , trOrganizerId       :: !MisoString
  , trOrganizerName     :: !MisoString
  , trFormat            :: !MisoString
  , trVariant           :: !MisoString
  , trTimeControl       :: Maybe MisoString
  , trTimePerPlayerMs   :: Maybe Int
  , trTimePerMoveSec    :: Maybe Int
  , trIsRated           :: !Bool
  , trMaxPlayers        :: Maybe Int
  , trIsPrivate         :: !Bool
  , trInviteCode        :: Maybe MisoString
  , trRoundIntervalMin  :: !Int
  , trForfeitTimeoutMin :: !Int
  , trStatus            :: !MisoString
  , trCurrentRound      :: !Int
  , trTotalRounds       :: Maybe Int
  , trStartedAt         :: Maybe MisoString
  , trFinishedAt        :: Maybe MisoString
  , trNextRoundAt       :: Maybe MisoString
  , trCreatedAt         :: !MisoString
  } deriving (Eq, Show)

instance FromJSON TournamentRow where
  parseJSON = withObject "TournamentRow" $ \v ->
    TournamentRow
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "description"
      <*> v .: "organizer_id"
      <*> v .: "organizer_name"
      <*> v .: "format"
      <*> v .: "variant"
      <*> v .:? "time_control"
      <*> v .:? "time_per_player_ms"
      <*> v .:? "time_per_move_seconds"
      <*> v .:? "is_rated" .!= True
      <*> v .:? "max_players"
      <*> v .:? "is_private" .!= False
      <*> v .:? "invite_code"
      <*> v .:? "round_interval_minutes" .!= 0
      <*> v .:? "forfeit_timeout_minutes" .!= 1440
      <*> v .:? "status" .!= "registration"
      <*> v .:? "current_round" .!= 0
      <*> v .:? "total_rounds"
      <*> v .:? "started_at"
      <*> v .:? "finished_at"
      <*> v .:? "next_round_at"
      <*> v .:? "created_at" .!= ""

-- ---------------------------------------------------------------------------
-- TournamentPlayerRow
-- ---------------------------------------------------------------------------

data TournamentPlayerRow = TournamentPlayerRow
  { tpId              :: !MisoString
  , tpTournamentId    :: !MisoString
  , tpPlayerId        :: !MisoString
  , tpPlayerName      :: !MisoString
  , tpScore           :: !Double
  , tpWins            :: !Int
  , tpLosses          :: !Int
  , tpDraws           :: !Int
  , tpBuchholz        :: !Double
  , tpGamesPlayed     :: !Int
  , tpAttackerWins    :: !Int
  , tpAttackerLosses  :: !Int
  , tpAttackerDraws   :: !Int
  , tpDefenderWins    :: !Int
  , tpDefenderLosses  :: !Int
  , tpDefenderDraws   :: !Int
  , tpIsActive        :: !Bool
  , tpSeed            :: Maybe Int
  } deriving (Eq, Show)

instance FromJSON TournamentPlayerRow where
  parseJSON = withObject "TournamentPlayerRow" $ \v ->
    TournamentPlayerRow
      <$> v .: "id"
      <*> v .: "tournament_id"
      <*> v .: "player_id"
      <*> v .: "player_name"
      <*> v .:? "score" .!= 0
      <*> v .:? "wins" .!= 0
      <*> v .:? "losses" .!= 0
      <*> v .:? "draws" .!= 0
      <*> v .:? "buchholz" .!= 0
      <*> v .:? "games_played" .!= 0
      <*> v .:? "attacker_wins" .!= 0
      <*> v .:? "attacker_losses" .!= 0
      <*> v .:? "attacker_draws" .!= 0
      <*> v .:? "defender_wins" .!= 0
      <*> v .:? "defender_losses" .!= 0
      <*> v .:? "defender_draws" .!= 0
      <*> v .:? "is_active" .!= True
      <*> v .:? "seed"

-- ---------------------------------------------------------------------------
-- TournamentPairingRow
-- ---------------------------------------------------------------------------

data TournamentPairingRow = TournamentPairingRow
  { tprId             :: !MisoString
  , tprTournamentId   :: !MisoString
  , tprRoundNumber    :: !Int
  , tprPairingOrder   :: !Int
  , tprPlayer1Id      :: !MisoString
  , tprPlayer2Id      :: Maybe MisoString
  , tprPlayer1Side    :: !MisoString
  , tprGameId         :: Maybe MisoString
  , tprWinnerId       :: Maybe MisoString
  , tprResult         :: Maybe MisoString
  } deriving (Eq, Show)

instance FromJSON TournamentPairingRow where
  parseJSON = withObject "TournamentPairingRow" $ \v ->
    TournamentPairingRow
      <$> v .: "id"
      <*> v .: "tournament_id"
      <*> v .: "round_number"
      <*> v .:? "pairing_order" .!= 0
      <*> v .: "player_1_id"
      <*> v .:? "player_2_id"
      <*> v .:? "player_1_side" .!= "attacker"
      <*> v .:? "game_id"
      <*> v .:? "winner_id"
      <*> v .:? "result"
