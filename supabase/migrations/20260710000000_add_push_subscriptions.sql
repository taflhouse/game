-- Push subscription storage for Web Push notifications.
CREATE TABLE push_subscriptions (
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  user_id UUID NOT NULL REFERENCES auth.users(id) ON DELETE CASCADE,
  endpoint TEXT NOT NULL,
  p256dh TEXT NOT NULL,
  auth TEXT NOT NULL,
  subscription_json JSONB NOT NULL,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now(),
  UNIQUE(user_id, endpoint)
);

ALTER TABLE push_subscriptions ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can view own subscriptions"
  ON push_subscriptions FOR SELECT
  USING (auth.uid() = user_id);

CREATE POLICY "Users can insert own subscriptions"
  ON push_subscriptions FOR INSERT
  WITH CHECK (auth.uid() = user_id);

CREATE POLICY "Users can update own subscriptions"
  ON push_subscriptions FOR UPDATE
  USING (auth.uid() = user_id);

CREATE POLICY "Users can delete own subscriptions"
  ON push_subscriptions FOR DELETE
  USING (auth.uid() = user_id);

GRANT SELECT, INSERT, UPDATE, DELETE ON push_subscriptions TO anon, authenticated, service_role;

-- Enable pg_net for HTTP calls from triggers.
CREATE EXTENSION IF NOT EXISTS pg_net WITH SCHEMA extensions;

-- Trigger function: notify opponent via Edge Function when a move is made.
CREATE OR REPLACE FUNCTION notify_push_on_move()
RETURNS trigger
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
  opponent_id UUID;
  mover_name TEXT;
  game_variant TEXT;
  has_subs BOOLEAN;
  edge_fn_url TEXT;
  service_key TEXT;
BEGIN
  -- Only fire for active multiplayer games where the moves array changed.
  IF NEW.status <> 'active' THEN
    RETURN NEW;
  END IF;
  IF NEW.attacker_id IS NULL OR NEW.defender_id IS NULL THEN
    RETURN NEW;
  END IF;
  IF NEW.moves IS NOT DISTINCT FROM OLD.moves THEN
    RETURN NEW;
  END IF;

  -- current_turn now points at the player who needs to move (the opponent).
  IF NEW.current_turn = 'attacker' THEN
    opponent_id := NEW.attacker_id;
    mover_name := COALESCE(NEW.defender_name, 'Opponent');
  ELSE
    opponent_id := NEW.defender_id;
    mover_name := COALESCE(NEW.attacker_name, 'Opponent');
  END IF;

  game_variant := COALESCE(NEW.variant, 'Hnefatafl');

  -- Quick check: does the opponent have any push subscriptions?
  SELECT EXISTS(
    SELECT 1 FROM push_subscriptions WHERE user_id = opponent_id
  ) INTO has_subs;

  IF NOT has_subs THEN
    RETURN NEW;
  END IF;

  -- Read the Edge Function URL and service role key from Vault.
  SELECT decrypted_secret INTO edge_fn_url
    FROM vault.decrypted_secrets WHERE name = 'push_edge_fn_url';
  SELECT decrypted_secret INTO service_key
    FROM vault.decrypted_secrets WHERE name = 'service_role_key';

  IF edge_fn_url IS NULL OR service_key IS NULL THEN
    RETURN NEW;
  END IF;

  -- Fire-and-forget HTTP POST to the Edge Function.
  PERFORM net.http_post(
    url := edge_fn_url,
    headers := jsonb_build_object(
      'Content-Type', 'application/json',
      'Authorization', 'Bearer ' || service_key
    ),
    body := jsonb_build_object(
      'user_id', opponent_id,
      'game_id', NEW.id,
      'mover_name', mover_name,
      'variant', game_variant
    )
  );

  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_push_on_move
  AFTER UPDATE ON games
  FOR EACH ROW
  EXECUTE FUNCTION notify_push_on_move();
