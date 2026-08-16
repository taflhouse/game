-- The pg_cron extension was never actually enabled in this project (prior
-- migrations only scheduled jobs `IF EXISTS (SELECT 1 FROM pg_extension ...)`,
-- which silently no-ops when the extension isn't installed). As a result,
-- check_game_timeouts(), cancel_expired_waiting_games(), and
-- check_tournament_forfeits() have never actually run on a schedule, leaving
-- games that timed out weeks ago stuck at status='active' indefinitely.
CREATE EXTENSION IF NOT EXISTS pg_cron;

GRANT USAGE ON SCHEMA cron TO postgres;

-- check_game_timeouts() only ever handled blitz/daily games. Games with no
-- time control that were joined but never played (0 moves) had no path to
-- ever leave 'active', since there's no clock to expire. Extend it to cancel
-- those too, after a generous grace period.
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

  -- Untimed games that were joined but never played: nothing will ever
  -- expire them via a clock, so cancel them outright after a grace period.
  UPDATE games SET status = 'cancelled'
  WHERE status = 'active' AND time_control IS NULL AND total_moves = 0
    AND played_at < now() - interval '2 hours';
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- Re-schedule every cron job that previous migrations tried to install
-- while the extension was missing. cron.schedule() upserts by job name, so
-- this is safe to run whether or not a stale copy already exists.
-- Note: pg_cron's interval syntax only accepts 1-59 seconds; '60 seconds' as
-- used by the original (never-actually-run) tournament migration is invalid
-- and would fail to schedule. Standard cron syntax is used instead for the
-- once-a-minute job.
SELECT cron.schedule('check-game-timeouts', '30 seconds', 'SELECT check_game_timeouts()');
SELECT cron.schedule('cancel-expired-waiting-games', '30 seconds', 'SELECT cancel_expired_waiting_games()');
SELECT cron.schedule('check-tournament-forfeits', '* * * * *', 'SELECT check_tournament_forfeits()');

-- One-time cleanup of the backlog that piled up while the cron never ran.
SELECT check_game_timeouts();
