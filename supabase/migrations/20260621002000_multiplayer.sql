-- Profiles table for multiplayer usernames.
CREATE TABLE profiles (
  id           UUID PRIMARY KEY REFERENCES auth.users(id) ON DELETE CASCADE,
  username     TEXT UNIQUE NOT NULL,
  display_name TEXT,
  created_at   TIMESTAMPTZ DEFAULT now(),
  CONSTRAINT username_format CHECK (username ~ '^[a-zA-Z0-9_]{3,20}$')
);

ALTER TABLE profiles ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can view all profiles"
  ON profiles FOR SELECT USING (true);

CREATE POLICY "Users can insert own profile"
  ON profiles FOR INSERT WITH CHECK (auth.uid() = id);

CREATE POLICY "Users can update own profile"
  ON profiles FOR UPDATE USING (auth.uid() = id);

-- Expose profiles to the API (PostgREST).
GRANT SELECT, INSERT, UPDATE ON profiles TO anon, authenticated, service_role;

-- Extend games table for multiplayer.
ALTER TABLE games
  ADD COLUMN attacker_id   UUID REFERENCES auth.users(id),
  ADD COLUMN defender_id   UUID REFERENCES auth.users(id),
  ADD COLUMN status        TEXT DEFAULT 'finished',
  ADD COLUMN invite_code   TEXT UNIQUE,
  ADD COLUMN current_turn  TEXT DEFAULT 'attacker';

-- Allow players to view multiplayer games they participate in.
CREATE POLICY "Players can view own multiplayer games"
  ON games FOR SELECT
  USING (auth.uid() = attacker_id OR auth.uid() = defender_id);
