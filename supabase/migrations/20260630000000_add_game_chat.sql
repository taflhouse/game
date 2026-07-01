CREATE TABLE game_chat (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  game_id    UUID NOT NULL REFERENCES games(id) ON DELETE CASCADE,
  user_id    UUID NOT NULL REFERENCES auth.users(id),
  sender_name TEXT NOT NULL,
  message    TEXT NOT NULL CHECK (char_length(message) <= 500),
  channel    TEXT NOT NULL DEFAULT 'player' CHECK (channel IN ('player', 'spectator')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Expose game_chat to the API (PostgREST).
GRANT SELECT, INSERT ON game_chat TO anon, authenticated, service_role;

ALTER TABLE game_chat ENABLE ROW LEVEL SECURITY;

-- Anyone authenticated can insert their own messages
CREATE POLICY game_chat_insert ON game_chat
  FOR INSERT TO authenticated
  WITH CHECK (user_id = auth.uid());

-- Spectator messages visible to everyone
CREATE POLICY game_chat_select_spectator ON game_chat
  FOR SELECT TO authenticated
  USING (channel = 'spectator');

-- Player messages only visible to the two players in the game
CREATE POLICY game_chat_select_player ON game_chat
  FOR SELECT TO authenticated
  USING (
    channel = 'player'
    AND EXISTS (
      SELECT 1 FROM games
      WHERE games.id = game_chat.game_id
      AND (games.attacker_id = auth.uid() OR games.defender_id = auth.uid())
    )
  );

-- Enable Realtime for this table
ALTER PUBLICATION supabase_realtime ADD TABLE game_chat;
