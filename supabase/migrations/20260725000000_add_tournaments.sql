-- ============================================================================
-- Tournament system: tournaments, players, pairings
-- ============================================================================

-- Enable pg_net for HTTP calls from triggers (if not already enabled)
CREATE EXTENSION IF NOT EXISTS pg_net WITH SCHEMA extensions;

-- ---------------------------------------------------------------------------
-- tournaments
-- ---------------------------------------------------------------------------

CREATE TABLE tournaments (
  id                      UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  organizer_id            UUID NOT NULL REFERENCES auth.users(id),
  organizer_name          TEXT NOT NULL,
  name                    TEXT NOT NULL,
  description             TEXT,
  format                  TEXT NOT NULL CHECK (format IN ('swiss','round_robin','single_elimination')),
  variant                 TEXT NOT NULL,
  time_control            TEXT CHECK (time_control IN ('blitz','daily')),
  time_per_player_ms      BIGINT,
  time_per_move_seconds   INT,
  is_rated                BOOLEAN NOT NULL DEFAULT true,
  max_players             INT,
  is_private              BOOLEAN NOT NULL DEFAULT false,
  invite_code             TEXT UNIQUE,
  round_interval_minutes  INT NOT NULL DEFAULT 0,
  forfeit_timeout_minutes INT NOT NULL DEFAULT 1440,
  status                  TEXT NOT NULL DEFAULT 'registration'
                          CHECK (status IN ('registration','active','finished','cancelled')),
  current_round           INT NOT NULL DEFAULT 0,
  total_rounds            INT,
  started_at              TIMESTAMPTZ,
  finished_at             TIMESTAMPTZ,
  next_round_at           TIMESTAMPTZ,
  created_at              TIMESTAMPTZ DEFAULT now()
);

ALTER TABLE tournaments ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view public tournaments"
  ON tournaments FOR SELECT
  USING (is_private = false OR organizer_id = auth.uid()
         OR EXISTS (SELECT 1 FROM tournament_players tp
                    WHERE tp.tournament_id = id AND tp.player_id = auth.uid()));

CREATE POLICY "Authenticated users can create tournaments"
  ON tournaments FOR INSERT TO authenticated
  WITH CHECK (organizer_id = auth.uid());

CREATE POLICY "Organizer can update own tournament"
  ON tournaments FOR UPDATE
  USING (organizer_id = auth.uid());

GRANT SELECT, INSERT, UPDATE ON tournaments TO authenticated, service_role;
ALTER PUBLICATION supabase_realtime ADD TABLE tournaments;

-- ---------------------------------------------------------------------------
-- tournament_players
-- ---------------------------------------------------------------------------

CREATE TABLE tournament_players (
  id              UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  tournament_id   UUID NOT NULL REFERENCES tournaments(id) ON DELETE CASCADE,
  player_id       UUID NOT NULL REFERENCES auth.users(id),
  player_name     TEXT NOT NULL,
  score           DOUBLE PRECISION NOT NULL DEFAULT 0,
  wins            INT NOT NULL DEFAULT 0,
  losses          INT NOT NULL DEFAULT 0,
  draws           INT NOT NULL DEFAULT 0,
  buchholz        DOUBLE PRECISION NOT NULL DEFAULT 0,
  games_played    INT NOT NULL DEFAULT 0,
  attacker_wins   INT NOT NULL DEFAULT 0,
  attacker_losses INT NOT NULL DEFAULT 0,
  attacker_draws  INT NOT NULL DEFAULT 0,
  defender_wins   INT NOT NULL DEFAULT 0,
  defender_losses INT NOT NULL DEFAULT 0,
  defender_draws  INT NOT NULL DEFAULT 0,
  is_active       BOOLEAN NOT NULL DEFAULT true,
  seed            INT,
  registered_at   TIMESTAMPTZ DEFAULT now(),
  UNIQUE(tournament_id, player_id)
);

ALTER TABLE tournament_players ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view tournament players"
  ON tournament_players FOR SELECT USING (true);

CREATE POLICY "Authenticated users can join"
  ON tournament_players FOR INSERT TO authenticated
  WITH CHECK (player_id = auth.uid());

GRANT SELECT, INSERT, UPDATE ON tournament_players TO authenticated, service_role;
ALTER PUBLICATION supabase_realtime ADD TABLE tournament_players;

-- ---------------------------------------------------------------------------
-- tournament_pairings
-- ---------------------------------------------------------------------------

CREATE TABLE tournament_pairings (
  id              UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  tournament_id   UUID NOT NULL REFERENCES tournaments(id) ON DELETE CASCADE,
  round_number    INT NOT NULL,
  pairing_order   INT NOT NULL DEFAULT 0,
  player_1_id     UUID NOT NULL REFERENCES auth.users(id),
  player_2_id     UUID REFERENCES auth.users(id),
  player_1_side   TEXT NOT NULL CHECK (player_1_side IN ('attacker','defender')),
  game_id         UUID REFERENCES games(id),
  winner_id       UUID REFERENCES auth.users(id),
  result          TEXT CHECK (result IN ('player_1','player_2','draw','bye','forfeit_1','forfeit_2')),
  created_at      TIMESTAMPTZ DEFAULT now()
);

ALTER TABLE tournament_pairings ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view pairings"
  ON tournament_pairings FOR SELECT USING (true);

GRANT SELECT ON tournament_pairings TO authenticated;
GRANT ALL ON tournament_pairings TO service_role;
ALTER PUBLICATION supabase_realtime ADD TABLE tournament_pairings;

-- ---------------------------------------------------------------------------
-- Add FK columns on games table
-- ---------------------------------------------------------------------------

ALTER TABLE games ADD COLUMN tournament_id UUID REFERENCES tournaments(id);
ALTER TABLE games ADD COLUMN tournament_pairing_id UUID REFERENCES tournament_pairings(id);

-- ---------------------------------------------------------------------------
-- Trigger: update pairing result when game finishes
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_tournament_pairing_result()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
BEGIN
  IF NEW.tournament_pairing_id IS NULL THEN RETURN NEW; END IF;
  IF NEW.status <> 'finished' THEN RETURN NEW; END IF;
  IF OLD.status = 'finished' THEN RETURN NEW; END IF;

  UPDATE tournament_pairings SET
    winner_id = CASE
      WHEN NEW.winner = 'attacker' THEN NEW.attacker_id
      WHEN NEW.winner = 'defender' THEN NEW.defender_id
      ELSE NULL END,
    result = CASE
      WHEN NEW.winner = 'attacker' THEN
        CASE WHEN player_1_side = 'attacker' THEN 'player_1' ELSE 'player_2' END
      WHEN NEW.winner = 'defender' THEN
        CASE WHEN player_1_side = 'defender' THEN 'player_1' ELSE 'player_2' END
      ELSE 'draw' END
  WHERE id = NEW.tournament_pairing_id;

  RETURN NEW;
END; $$;

CREATE TRIGGER trg_update_tournament_pairing
  AFTER UPDATE ON games FOR EACH ROW
  EXECUTE FUNCTION update_tournament_pairing_result();

-- ---------------------------------------------------------------------------
-- Trigger: check if round is complete, call Edge Function
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION check_tournament_round_complete()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER AS $$
DECLARE
  t_id UUID; t_round INT; pending INT;
  edge_fn_url TEXT; service_key TEXT;
BEGIN
  IF NEW.tournament_id IS NULL THEN RETURN NEW; END IF;
  IF NEW.status <> 'finished' THEN RETURN NEW; END IF;
  IF OLD.status = 'finished' THEN RETURN NEW; END IF;

  t_id := NEW.tournament_id;
  SELECT current_round INTO t_round FROM tournaments WHERE id = t_id;

  SELECT count(*) INTO pending
  FROM tournament_pairings tp JOIN games g ON g.id = tp.game_id
  WHERE tp.tournament_id = t_id AND tp.round_number = t_round
    AND g.status <> 'finished';

  IF pending = 0 THEN
    SELECT decrypted_secret INTO edge_fn_url
      FROM vault.decrypted_secrets WHERE name = 'tournament_advance_fn_url';
    SELECT decrypted_secret INTO service_key
      FROM vault.decrypted_secrets WHERE name = 'service_role_key';
    IF edge_fn_url IS NOT NULL AND service_key IS NOT NULL THEN
      PERFORM net.http_post(
        url := edge_fn_url,
        headers := jsonb_build_object(
          'Content-Type', 'application/json',
          'Authorization', 'Bearer ' || service_key),
        body := jsonb_build_object('tournament_id', t_id));
    END IF;
  END IF;
  RETURN NEW;
END; $$;

CREATE TRIGGER trg_tournament_round_check
  AFTER UPDATE ON games FOR EACH ROW
  EXECUTE FUNCTION check_tournament_round_complete();

-- ---------------------------------------------------------------------------
-- Cron: forfeit handling
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION check_tournament_forfeits() RETURNS void AS $$
BEGIN
  WITH forfeitable AS (
    SELECT tp.id AS pairing_id, g.id AS game_id
    FROM tournament_pairings tp
    JOIN games g ON g.id = tp.game_id
    JOIN tournaments t ON t.id = tp.tournament_id
    WHERE tp.result IS NULL AND g.status = 'active'
      AND t.status = 'active'
      AND g.played_at + (t.forfeit_timeout_minutes * interval '1 minute') < now()
  )
  UPDATE games SET status = 'finished', result_desc = 'Forfeit (tournament timeout)',
    winner = NULL, total_moves = COALESCE(total_moves, 0)
  FROM forfeitable WHERE games.id = forfeitable.game_id;
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_cron') THEN
    PERFORM cron.schedule('check-tournament-forfeits', '60 seconds',
      'SELECT check_tournament_forfeits()');
  END IF;
END $$;
