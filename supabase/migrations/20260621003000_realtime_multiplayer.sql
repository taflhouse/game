-- Add display name columns and draw tracking for multiplayer.
ALTER TABLE games ADD COLUMN IF NOT EXISTS attacker_name TEXT;
ALTER TABLE games ADD COLUMN IF NOT EXISTS defender_name TEXT;
ALTER TABLE games ADD COLUMN IF NOT EXISTS draw_offered_by TEXT;

-- Enable Realtime on games table.
ALTER PUBLICATION supabase_realtime ADD TABLE games;

-- Replace single-owner update policy with multiplayer-aware policy.
DROP POLICY IF EXISTS "Users can update own games" ON games;
CREATE POLICY "Players can update games" ON games FOR UPDATE
  USING (
    auth.uid() IN (user_id, attacker_id, defender_id)
    OR (status = 'waiting' AND (attacker_id IS NULL OR defender_id IS NULL))
  )
  WITH CHECK (
    auth.uid() IN (user_id, attacker_id, defender_id)
    OR (status = 'waiting' AND (attacker_id IS NULL OR defender_id IS NULL))
  );

-- Allow finding waiting games by invite code.
CREATE POLICY "Anyone can view waiting games" ON games FOR SELECT
  USING (status = 'waiting');
