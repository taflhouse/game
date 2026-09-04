-- Push notification when an opponent joins a waiting game.
--
-- notify_push_on_move() returns early unless the moves array changed, so a join
-- (which only touches status/attacker_id/defender_id) never produced a
-- notification. Realtime was the sole delivery path for that event, and it has
-- no replay: a creator whose channel had dropped stayed on "Waiting for
-- opponent..." until they reloaded the page.

CREATE OR REPLACE FUNCTION notify_push_on_join()
RETURNS trigger
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
  waiter_id UUID;
  joiner_name TEXT;
  game_variant TEXT;
  has_subs BOOLEAN;
  edge_fn_url TEXT;
  service_key TEXT;
BEGIN
  -- Only the waiting -> active transition.
  IF OLD.status <> 'waiting' OR NEW.status <> 'active' THEN
    RETURN NEW;
  END IF;
  IF NEW.attacker_id IS NULL OR NEW.defender_id IS NULL THEN
    RETURN NEW;
  END IF;

  -- The creator is whichever seat was already filled before the join.
  IF OLD.attacker_id IS NOT NULL THEN
    waiter_id := OLD.attacker_id;
    joiner_name := COALESCE(NULLIF(NEW.defender_name, ''), 'An opponent');
  ELSIF OLD.defender_id IS NOT NULL THEN
    waiter_id := OLD.defender_id;
    joiner_name := COALESCE(NULLIF(NEW.attacker_name, ''), 'An opponent');
  ELSE
    RETURN NEW;
  END IF;

  -- Don't notify someone who joined their own game from a second tab.
  IF waiter_id = NEW.attacker_id AND waiter_id = NEW.defender_id THEN
    RETURN NEW;
  END IF;

  game_variant := COALESCE(NEW.variant, 'Hnefatafl');

  SELECT EXISTS(
    SELECT 1 FROM push_subscriptions WHERE user_id = waiter_id
  ) INTO has_subs;

  IF NOT has_subs THEN
    RETURN NEW;
  END IF;

  SELECT decrypted_secret INTO edge_fn_url
    FROM vault.decrypted_secrets WHERE name = 'push_edge_fn_url';
  SELECT decrypted_secret INTO service_key
    FROM vault.decrypted_secrets WHERE name = 'service_role_key';

  IF edge_fn_url IS NULL OR service_key IS NULL THEN
    RETURN NEW;
  END IF;

  PERFORM net.http_post(
    url := edge_fn_url,
    headers := jsonb_build_object(
      'Content-Type', 'application/json',
      'Authorization', 'Bearer ' || service_key
    ),
    body := jsonb_build_object(
      'user_id', waiter_id,
      'game_id', NEW.id,
      'mover_name', joiner_name,
      'variant', game_variant,
      'event', 'join'
    )
  );

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_push_on_join ON games;
CREATE TRIGGER trg_push_on_join
  AFTER UPDATE ON games
  FOR EACH ROW
  EXECUTE FUNCTION notify_push_on_join();
