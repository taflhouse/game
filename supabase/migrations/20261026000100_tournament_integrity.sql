-- ============================================================================
-- Tournament integrity: server-side join/leave/cancel, forfeits, reliable
-- round advancement, and the round interval.
-- ============================================================================

-- ---------------------------------------------------------------------------
-- Lock down direct writes. Joining, leaving and cancelling go through the
-- SECURITY DEFINER functions below; everything else is the edge function
-- (service role).
-- ---------------------------------------------------------------------------

DROP POLICY IF EXISTS "Authenticated users can join" ON tournament_players;
REVOKE INSERT, UPDATE ON tournament_players FROM authenticated;

DROP POLICY IF EXISTS "Organizer can update own tournament" ON tournaments;
REVOKE UPDATE ON tournaments FROM authenticated;

-- ---------------------------------------------------------------------------
-- join_tournament: registration open, room left, invite code for private
-- tournaments, and the player's real username.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION join_tournament(p_tournament_id UUID, p_invite_code TEXT DEFAULT NULL)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  t tournaments;
  uname TEXT;
  player_count INT;
BEGIN
  IF auth.uid() IS NULL THEN
    RAISE EXCEPTION 'Please sign in to join.';
  END IF;

  -- Row lock serialises concurrent joins so max_players can't be overshot.
  SELECT * INTO t FROM tournaments WHERE id = p_tournament_id FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Tournament not found.';
  END IF;
  IF t.status <> 'registration' THEN
    RAISE EXCEPTION 'Registration for this tournament is closed.';
  END IF;
  IF t.is_private AND t.organizer_id <> auth.uid()
     AND (p_invite_code IS NULL OR p_invite_code IS DISTINCT FROM t.invite_code) THEN
    RAISE EXCEPTION 'This tournament is private. Use its invite link to join.';
  END IF;
  IF EXISTS (SELECT 1 FROM tournament_players
             WHERE tournament_id = t.id AND player_id = auth.uid()) THEN
    RETURN;
  END IF;
  IF t.max_players IS NOT NULL THEN
    SELECT count(*) INTO player_count FROM tournament_players WHERE tournament_id = t.id;
    IF player_count >= t.max_players THEN
      RAISE EXCEPTION 'This tournament is full.';
    END IF;
  END IF;

  SELECT username INTO uname FROM profiles WHERE id = auth.uid();
  IF uname IS NULL THEN
    RAISE EXCEPTION 'Choose a username before joining.';
  END IF;

  INSERT INTO tournament_players (tournament_id, player_id, player_name)
  VALUES (t.id, auth.uid(), uname);
END; $$;

-- ---------------------------------------------------------------------------
-- leave_tournament: drop out during registration, or withdraw while active
-- (the current game is forfeited and the player is skipped in later rounds).
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION leave_tournament(p_tournament_id UUID)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  t tournaments;
BEGIN
  IF auth.uid() IS NULL THEN
    RAISE EXCEPTION 'Please sign in.';
  END IF;

  SELECT * INTO t FROM tournaments WHERE id = p_tournament_id FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Tournament not found.';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM tournament_players
                 WHERE tournament_id = t.id AND player_id = auth.uid()) THEN
    RAISE EXCEPTION 'You are not in this tournament.';
  END IF;

  IF t.status = 'registration' THEN
    DELETE FROM tournament_players
    WHERE tournament_id = t.id AND player_id = auth.uid();
  ELSIF t.status = 'active' THEN
    UPDATE tournament_players SET is_active = false
    WHERE tournament_id = t.id AND player_id = auth.uid();

    UPDATE games SET
      status = 'finished',
      result_desc = 'Forfeit (withdrew from tournament)',
      winner = CASE WHEN attacker_id = auth.uid() THEN 'defender' ELSE 'attacker' END
    WHERE tournament_id = t.id AND status = 'active'
      AND auth.uid() IN (attacker_id, defender_id);
  ELSE
    RAISE EXCEPTION 'This tournament is already over.';
  END IF;
END; $$;

-- ---------------------------------------------------------------------------
-- cancel_tournament: organizer only, before it finishes.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION cancel_tournament(p_tournament_id UUID)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  t tournaments;
BEGIN
  SELECT * INTO t FROM tournaments WHERE id = p_tournament_id FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Tournament not found.';
  END IF;
  IF t.organizer_id IS DISTINCT FROM auth.uid() THEN
    RAISE EXCEPTION 'Only the organizer can cancel this tournament.';
  END IF;
  IF t.status NOT IN ('registration', 'active') THEN
    RAISE EXCEPTION 'This tournament is already over.';
  END IF;

  UPDATE tournaments SET status = 'cancelled', finished_at = now(), next_round_at = NULL
  WHERE id = t.id;

  UPDATE games SET status = 'cancelled'
  WHERE tournament_id = t.id AND status IN ('waiting', 'active');
END; $$;

REVOKE ALL ON FUNCTION join_tournament(UUID, TEXT) FROM PUBLIC;
REVOKE ALL ON FUNCTION leave_tournament(UUID) FROM PUBLIC;
REVOKE ALL ON FUNCTION cancel_tournament(UUID) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION join_tournament(UUID, TEXT) TO authenticated;
GRANT EXECUTE ON FUNCTION leave_tournament(UUID) TO authenticated;
GRANT EXECUTE ON FUNCTION cancel_tournament(UUID) TO authenticated;

-- ---------------------------------------------------------------------------
-- Players can write their own game rows, so stop them cancelling a game in a
-- live tournament: the pairing would never get a result and the round would
-- never complete.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION prevent_tournament_game_cancel()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
  IF NEW.tournament_id IS NOT NULL AND NEW.status = 'cancelled'
     AND OLD.status IS DISTINCT FROM 'cancelled'
     AND EXISTS (SELECT 1 FROM tournaments
                 WHERE id = NEW.tournament_id AND status = 'active') THEN
    RAISE EXCEPTION 'Tournament games cannot be cancelled. Resign instead.';
  END IF;
  RETURN NEW;
END; $$;

DROP TRIGGER IF EXISTS trg_prevent_tournament_game_cancel ON games;
CREATE TRIGGER trg_prevent_tournament_game_cancel
  BEFORE UPDATE ON games FOR EACH ROW
  EXECUTE FUNCTION prevent_tournament_game_cancel();

-- ---------------------------------------------------------------------------
-- Pairing results: timeouts and forfeits count as forfeit_1 / forfeit_2.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_tournament_pairing_result()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  is_forfeit BOOLEAN;
BEGIN
  IF NEW.tournament_pairing_id IS NULL THEN RETURN NEW; END IF;
  IF NEW.status <> 'finished' THEN RETURN NEW; END IF;
  IF OLD.status = 'finished' THEN RETURN NEW; END IF;

  is_forfeit := NEW.winner IS NOT NULL
    AND (NEW.result_desc ILIKE '%on time%' OR NEW.result_desc ILIKE 'forfeit%');

  UPDATE tournament_pairings SET
    winner_id = CASE
      WHEN NEW.winner = 'attacker' THEN NEW.attacker_id
      WHEN NEW.winner = 'defender' THEN NEW.defender_id
      ELSE NULL END,
    result = CASE
      WHEN NEW.winner IS NULL THEN 'draw'
      -- player_1 won
      WHEN (NEW.winner = player_1_side) THEN
        CASE WHEN is_forfeit THEN 'forfeit_2' ELSE 'player_1' END
      -- player_2 won
      ELSE
        CASE WHEN is_forfeit THEN 'forfeit_1' ELSE 'player_2' END
      END
  WHERE id = NEW.tournament_pairing_id;

  RETURN NEW;
END; $$;

-- ---------------------------------------------------------------------------
-- Calling the edge function, shared by the trigger and the cron job.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION request_tournament_advance(t_id UUID)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  edge_fn_url TEXT;
  service_key TEXT;
BEGIN
  SELECT decrypted_secret INTO edge_fn_url
    FROM vault.decrypted_secrets WHERE name = 'tournament_advance_fn_url';
  SELECT decrypted_secret INTO service_key
    FROM vault.decrypted_secrets WHERE name = 'service_role_key';
  IF edge_fn_url IS NULL OR service_key IS NULL THEN
    RAISE WARNING 'tournament_advance_fn_url / service_role_key missing from Vault';
    RETURN;
  END IF;
  PERFORM net.http_post(
    url := edge_fn_url,
    headers := jsonb_build_object(
      'Content-Type', 'application/json',
      'Authorization', 'Bearer ' || service_key),
    body := jsonb_build_object('tournament_id', t_id));
END; $$;

CREATE OR REPLACE FUNCTION check_tournament_round_complete()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  t_round INT;
  pending INT;
BEGIN
  IF NEW.tournament_id IS NULL THEN RETURN NEW; END IF;
  IF NEW.status <> 'finished' THEN RETURN NEW; END IF;
  IF OLD.status = 'finished' THEN RETURN NEW; END IF;

  SELECT current_round INTO t_round FROM tournaments
  WHERE id = NEW.tournament_id AND status = 'active';
  IF t_round IS NULL THEN RETURN NEW; END IF;

  SELECT count(*) INTO pending
  FROM tournament_pairings tp JOIN games g ON g.id = tp.game_id
  WHERE tp.tournament_id = NEW.tournament_id AND tp.round_number = t_round
    AND g.status <> 'finished';

  IF pending = 0 THEN
    PERFORM request_tournament_advance(NEW.tournament_id);
  END IF;
  RETURN NEW;
END; $$;

-- ---------------------------------------------------------------------------
-- Cron: forfeit players who stop moving, then advance any tournament whose
-- round is complete. The advance sweep also covers rounds the trigger missed
-- (two games finishing in concurrent transactions each see the other as
-- unfinished) and starts rounds once round_interval_minutes has elapsed.
-- The edge function is idempotent, so repeat requests are harmless.
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION check_tournament_forfeits() RETURNS void
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE
  t_id UUID;
BEGIN
  -- The player to move has been idle longer than the tournament allows.
  -- Daily games with a running move deadline are left to check_game_timeouts.
  UPDATE games g SET
    status = 'finished',
    result_desc = CASE g.current_turn
      WHEN 'attacker' THEN 'Forfeit (attacker did not move in time)'
      ELSE 'Forfeit (defender did not move in time)' END,
    winner = CASE g.current_turn WHEN 'attacker' THEN 'defender' ELSE 'attacker' END
  FROM tournament_pairings tp, tournaments t
  WHERE tp.game_id = g.id AND t.id = tp.tournament_id
    AND tp.result IS NULL AND g.status = 'active' AND t.status = 'active'
    AND NOT (g.time_control = 'daily' AND g.move_deadline IS NOT NULL)
    AND COALESCE(g.last_move_at, g.played_at)
        + (t.forfeit_timeout_minutes * interval '1 minute') < now();

  FOR t_id IN
    SELECT t.id FROM tournaments t
    WHERE t.status = 'active'
      AND (t.next_round_at IS NULL OR t.next_round_at <= now())
      AND EXISTS (SELECT 1 FROM tournament_pairings tp
                  WHERE tp.tournament_id = t.id AND tp.round_number = t.current_round)
      AND NOT EXISTS (SELECT 1 FROM tournament_pairings tp
                      WHERE tp.tournament_id = t.id AND tp.round_number = t.current_round
                        AND tp.result IS NULL)
  LOOP
    PERFORM request_tournament_advance(t_id);
  END LOOP;
END; $$;

-- ---------------------------------------------------------------------------
-- check_game_timeouts cancels untimed games nobody has moved in after two
-- hours. A cancelled tournament game never gets a pairing result, which
-- stalls the round, so leave tournament games to check_tournament_forfeits.
-- ---------------------------------------------------------------------------

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
    AND tournament_id IS NULL
    AND played_at < now() - interval '2 hours';
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;
