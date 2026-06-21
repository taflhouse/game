-- Create the games table for storing completed game records.
CREATE TABLE IF NOT EXISTS games (
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id UUID REFERENCES auth.users(id),
  variant TEXT NOT NULL,
  winner TEXT,
  result_desc TEXT NOT NULL,
  total_moves INTEGER NOT NULL,
  game_mode TEXT NOT NULL,
  ai_side TEXT,
  ai_depth INTEGER,
  played_at TIMESTAMPTZ DEFAULT now(),
  moves JSONB
);

-- Row Level Security: users can only see and insert their own games.
ALTER TABLE games ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can view own games"
  ON games FOR SELECT
  USING (auth.uid() = user_id);

CREATE POLICY "Users can insert own games"
  ON games FOR INSERT
  WITH CHECK (auth.uid() = user_id);
