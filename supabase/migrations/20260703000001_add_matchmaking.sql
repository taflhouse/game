-- Add matchmaking columns to games table
ALTER TABLE games ADD COLUMN IF NOT EXISTS is_matchmaking BOOLEAN DEFAULT false;
ALTER TABLE games ADD COLUMN IF NOT EXISTS creator_rating DOUBLE PRECISION;
ALTER TABLE games ADD COLUMN IF NOT EXISTS creator_rd DOUBLE PRECISION;

-- Allow anyone to view matchmaking games that are waiting
CREATE POLICY "Anyone can view matchmaking games"
  ON games FOR SELECT
  USING (is_matchmaking = true AND status = 'waiting');
