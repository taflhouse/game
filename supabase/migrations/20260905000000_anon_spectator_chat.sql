-- Spectators watch games without an account: the games table is already
-- world-readable, and an invite link to a game in progress now sends a
-- non-participant straight to the board. The spectator chat channel should be
-- readable on the same terms, or a signed-out viewer sees an empty panel and
-- only discovers the conversation after identifying themselves.
--
-- The player channel is untouched and stays restricted to the two players.
-- Inserting is still authenticated-only, so sending a message remains the
-- point at which a viewer has to say who they are.
DROP POLICY IF EXISTS game_chat_select_spectator ON game_chat;

CREATE POLICY game_chat_select_spectator ON game_chat
  FOR SELECT TO anon, authenticated
  USING (channel = 'spectator');
