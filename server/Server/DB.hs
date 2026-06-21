module Server.DB
  ( Profile(..)
  , GameRow(..)
  , getProfile
  , createGameRecord
  , updateGameStatus
  , updateGameResult
  , joinGameRecord
  , getGameByInviteCode
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

-- | A user profile.
data Profile = Profile
  { profileId          :: Text
  , profileUsername     :: Text
  , profileDisplayName :: Maybe Text
  } deriving (Eq, Show)

instance FromRow Profile where
  fromRow = Profile <$> field <*> field <*> field

-- | A game record from the database.
data GameRow = GameRow
  { growId         :: Text
  , growVariant    :: Text
  , growAttackerId :: Maybe Text
  , growDefenderId :: Maybe Text
  , growStatus     :: Text
  , growInviteCode :: Maybe Text
  } deriving (Eq, Show)

instance FromRow GameRow where
  fromRow = GameRow <$> field <*> field <*> field <*> field <*> field <*> field

-- | Look up a user profile by user ID.
getProfile :: Connection -> Text -> IO (Maybe Profile)
getProfile conn uid = do
  rows <- query conn
    "SELECT id, username, display_name FROM profiles WHERE id = ?"
    (Only uid)
  pure $ case rows of
    [p] -> Just p
    _   -> Nothing

-- | Insert a new multiplayer game record. Creator picks a side.
createGameRecord :: Connection -> Text -> Text -> Text -> Text -> Text -> IO ()
createGameRecord conn gameId variant inviteCode creatorId side = do
  let (atkId, defId) = case side :: Text of
        "attacker" -> (Just creatorId, Nothing :: Maybe Text)
        _          -> (Nothing :: Maybe Text, Just creatorId)
  _ <- execute conn
    "INSERT INTO games (id, variant, attacker_id, defender_id, status, invite_code, \
    \current_turn, result_desc, total_moves, game_mode) \
    \VALUES (?, ?, ?, ?, 'waiting', ?, 'attacker', 'in_progress', 0, 'multiplayer')"
    (gameId, variant, atkId, defId, inviteCode)
  pure ()

-- | Set the second player on a game, activating it.
joinGameRecord :: Connection -> Text -> Text -> Text -> IO ()
joinGameRecord conn gameId joinerId side =
  case side :: Text of
    "attacker" -> do
      _ <- execute conn
        "UPDATE games SET attacker_id = ?, status = 'active' WHERE id = ?"
        (joinerId, gameId)
      pure ()
    _ -> do
      _ <- execute conn
        "UPDATE games SET defender_id = ?, status = 'active' WHERE id = ?"
        (joinerId, gameId)
      pure ()

-- | Update a game's status (e.g. waiting -> active -> finished).
updateGameStatus :: Connection -> Text -> Text -> IO ()
updateGameStatus conn gameId status = do
  _ <- execute conn
    "UPDATE games SET status = ? WHERE id = ?"
    (status, gameId)
  pure ()

-- | Record the final result of a game.
updateGameResult :: Connection -> Text -> Text -> Maybe Text -> Int -> IO ()
updateGameResult conn gameId resultDesc mWinner totalMoves = do
  _ <- execute conn
    "UPDATE games SET status = 'finished', result_desc = ?, winner = ?, total_moves = ? WHERE id = ?"
    (resultDesc, mWinner, totalMoves, gameId)
  pure ()

-- | Look up a game by its invite code.
getGameByInviteCode :: Connection -> Text -> IO (Maybe GameRow)
getGameByInviteCode conn code = do
  rows <- query conn
    "SELECT id, variant, attacker_id, defender_id, status, invite_code \
    \FROM games WHERE invite_code = ?"
    (Only code)
  pure $ case rows of
    [g] -> Just g
    _   -> Nothing
