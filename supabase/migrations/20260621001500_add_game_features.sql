-- Add public visibility flag for game permalinks.
ALTER TABLE games ADD COLUMN is_public BOOLEAN DEFAULT false NOT NULL;

-- Allow anyone to view games marked as public.
CREATE POLICY "Anyone can view public games"
  ON games FOR SELECT USING (is_public = true);

-- Allow users to update their own games (result, moves, visibility).
CREATE POLICY "Users can update own games"
  ON games FOR UPDATE
  USING (auth.uid() = user_id)
  WITH CHECK (auth.uid() = user_id);
