{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Tafl.Protocol
  ( ClientMsg(..)
  , ServerMsg(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Pair)
import Data.Text (Text)
import Tafl.Types (Coords, MoveAction, Side, GameResult)

-- | Client → Server messages.
data ClientMsg
  = CmAuth Text Text              -- ^ JWT token, game_id
  | CmCreateGame Text Text        -- ^ variant slug, invite_code
  | CmJoinGame Text               -- ^ invite_code
  | CmMove MoveAction             -- ^ make a move
  | CmResign
  | CmOfferDraw
  | CmAcceptDraw
  | CmDeclineDraw
  deriving (Eq, Show)

-- | Server → Client messages.
data ServerMsg
  = SmError Text                   -- ^ error message
  | SmWaitingForOpponent           -- ^ game created, waiting
  | SmGameStarted Text Text Side   -- ^ game_id, opponent_username, your_side
  | SmMoveMade MoveAction [Coords] Side  -- ^ move, captures, next_turn
  | SmMoveRejected Text            -- ^ reason
  | SmGameOver GameResult          -- ^ result
  | SmDrawOffered                  -- ^ opponent offered draw
  | SmDrawDeclined                 -- ^ opponent declined
  | SmOpponentConnected
  | SmOpponentDisconnected
  | SmResigned Side                -- ^ who resigned
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

instance ToJSON ClientMsg where
  toJSON = \case
    CmAuth token gid   -> tagged "auth"        ["token" .= token, "game_id" .= gid]
    CmCreateGame v ic   -> tagged "create_game" ["variant" .= v, "invite_code" .= ic]
    CmJoinGame ic       -> tagged "join_game"   ["invite_code" .= ic]
    CmMove ma           -> tagged "move"        ["action" .= ma]
    CmResign            -> tagged "resign"      []
    CmOfferDraw         -> tagged "offer_draw"  []
    CmAcceptDraw        -> tagged "accept_draw" []
    CmDeclineDraw       -> tagged "decline_draw"[]

instance FromJSON ClientMsg where
  parseJSON = withObject "ClientMsg" $ \v -> do
    tag <- v .: "type" :: Parser Text
    case tag of
      "auth"         -> CmAuth <$> v .: "token" <*> v .: "game_id"
      "create_game"  -> CmCreateGame <$> v .: "variant" <*> v .: "invite_code"
      "join_game"    -> CmJoinGame <$> v .: "invite_code"
      "move"         -> CmMove <$> v .: "action"
      "resign"       -> pure CmResign
      "offer_draw"   -> pure CmOfferDraw
      "accept_draw"  -> pure CmAcceptDraw
      "decline_draw" -> pure CmDeclineDraw
      _              -> fail ("unknown ClientMsg type: " <> show tag)

instance ToJSON ServerMsg where
  toJSON = \case
    SmError msg            -> tagged "error"            ["message" .= msg]
    SmWaitingForOpponent   -> tagged "waiting"          []
    SmGameStarted g u s    -> tagged "game_started"     ["game_id" .= g, "opponent" .= u, "side" .= s]
    SmMoveMade ma cs nt    -> tagged "move_made"        ["action" .= ma, "captures" .= cs, "next_turn" .= nt]
    SmMoveRejected msg     -> tagged "move_rejected"    ["message" .= msg]
    SmGameOver r           -> tagged "game_over"        ["result" .= r]
    SmDrawOffered          -> tagged "draw_offered"     []
    SmDrawDeclined         -> tagged "draw_declined"    []
    SmOpponentConnected    -> tagged "opponent_connected"    []
    SmOpponentDisconnected -> tagged "opponent_disconnected" []
    SmResigned s           -> tagged "resigned"         ["side" .= s]

instance FromJSON ServerMsg where
  parseJSON = withObject "ServerMsg" $ \v -> do
    tag <- v .: "type" :: Parser Text
    case tag of
      "error"                 -> SmError <$> v .: "message"
      "waiting"               -> pure SmWaitingForOpponent
      "game_started"          -> SmGameStarted <$> v .: "game_id" <*> v .: "opponent" <*> v .: "side"
      "move_made"             -> SmMoveMade <$> v .: "action" <*> v .: "captures" <*> v .: "next_turn"
      "move_rejected"         -> SmMoveRejected <$> v .: "message"
      "game_over"             -> SmGameOver <$> v .: "result"
      "draw_offered"          -> pure SmDrawOffered
      "draw_declined"         -> pure SmDrawDeclined
      "opponent_connected"    -> pure SmOpponentConnected
      "opponent_disconnected" -> pure SmOpponentDisconnected
      "resigned"              -> SmResigned <$> v .: "side"
      _                       -> fail ("unknown ServerMsg type: " <> show tag)

-- | Helper to build a tagged JSON object.
tagged :: Text -> [Pair] -> Value
tagged t ps = object (("type" .= t) : ps)
