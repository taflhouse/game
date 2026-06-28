-- Function to check and auto-finish games that have timed out.
CREATE OR REPLACE FUNCTION check_game_timeouts() RETURNS void AS $$
BEGIN
  -- Blitz: active player's remaining_ms - elapsed since last_move_at <= 0
  UPDATE games SET
    status = 'finished',
    result_desc = CASE current_turn
      WHEN 'attacker' THEN 'Attacker lost on time'
      WHEN 'defender' THEN 'Defender lost on time' END,
    winner = CASE current_turn
      WHEN 'attacker' THEN 'defender'
      WHEN 'defender' THEN 'attacker' END,
    attacker_time_remaining_ms = CASE current_turn WHEN 'attacker' THEN 0 ELSE attacker_time_remaining_ms END,
    defender_time_remaining_ms = CASE current_turn WHEN 'defender' THEN 0 ELSE defender_time_remaining_ms END
  WHERE status = 'active' AND time_control = 'blitz' AND last_move_at IS NOT NULL
    AND ((current_turn = 'attacker' AND attacker_time_remaining_ms - (EXTRACT(EPOCH FROM (now() - last_move_at)) * 1000)::BIGINT <= 0)
      OR (current_turn = 'defender' AND defender_time_remaining_ms - (EXTRACT(EPOCH FROM (now() - last_move_at)) * 1000)::BIGINT <= 0));

  -- Daily: deadline passed
  UPDATE games SET
    status = 'finished',
    result_desc = CASE current_turn
      WHEN 'attacker' THEN 'Attacker lost on time'
      WHEN 'defender' THEN 'Defender lost on time' END,
    winner = CASE current_turn
      WHEN 'attacker' THEN 'defender'
      WHEN 'defender' THEN 'attacker' END
  WHERE status = 'active' AND time_control = 'daily' AND move_deadline IS NOT NULL AND move_deadline < now();
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- Schedule with pg_cron if available (not available in local Supabase dev).
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    PERFORM cron.schedule('check-game-timeouts', '30 seconds', 'SELECT check_game_timeouts()');
  END IF;
END $$;
