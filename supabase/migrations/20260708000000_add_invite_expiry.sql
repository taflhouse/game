-- Add expires_at column for invite game auto-cancellation.
ALTER TABLE games ADD COLUMN IF NOT EXISTS expires_at TIMESTAMPTZ;

-- Function to cancel expired waiting games.
CREATE OR REPLACE FUNCTION cancel_expired_waiting_games() RETURNS void AS $$
BEGIN
  UPDATE games SET status = 'cancelled'
  WHERE status = 'waiting' AND expires_at IS NOT NULL AND expires_at < now();
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

-- Schedule cron job to cancel expired waiting games every 30 seconds.
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    PERFORM cron.schedule(
      'cancel-expired-waiting-games',
      '30 seconds',
      'SELECT cancel_expired_waiting_games()'
    );
  END IF;
END $$;

-- Delete old games that were never played (no moves, stuck in waiting/cancelled),
-- only those created on or before 2026-07-08.
DELETE FROM games
WHERE total_moves = 0
  AND status IN ('waiting', 'cancelled')
  AND played_at <= '2026-07-09T23:59:59Z';
