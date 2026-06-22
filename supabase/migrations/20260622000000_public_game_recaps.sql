-- Make all games publicly viewable (recaps are read-only, no sensitive data).
ALTER TABLE games ALTER COLUMN is_public SET DEFAULT true;
UPDATE games SET is_public = true WHERE is_public = false;

-- Replace per-user and public-flag SELECT policies with a single open policy.
DROP POLICY IF EXISTS "Users can view own games" ON games;
DROP POLICY IF EXISTS "Anyone can view public games" ON games;
CREATE POLICY "Anyone can view games" ON games FOR SELECT USING (true);
