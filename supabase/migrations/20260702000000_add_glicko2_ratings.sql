-- Glicko-2 player ratings
-- Adds rating columns to profiles, is_rated flag to games,
-- a rating_history audit trail, and the update_ratings() PL/pgSQL function.

-- ---------------------------------------------------------------------------
-- 1. Schema changes
-- ---------------------------------------------------------------------------

-- Rating columns on profiles
ALTER TABLE profiles ADD COLUMN IF NOT EXISTS rating      DOUBLE PRECISION NOT NULL DEFAULT 1500.0;
ALTER TABLE profiles ADD COLUMN IF NOT EXISTS rating_rd   DOUBLE PRECISION NOT NULL DEFAULT 350.0;
ALTER TABLE profiles ADD COLUMN IF NOT EXISTS rating_vol  DOUBLE PRECISION NOT NULL DEFAULT 0.06;
ALTER TABLE profiles ADD COLUMN IF NOT EXISTS games_rated INTEGER          NOT NULL DEFAULT 0;

-- Rated flag on games (rated by default)
ALTER TABLE games ADD COLUMN IF NOT EXISTS is_rated BOOLEAN NOT NULL DEFAULT true;

-- Rating audit trail
CREATE TABLE IF NOT EXISTS rating_history (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id         UUID NOT NULL REFERENCES auth.users(id) ON DELETE CASCADE,
  game_id         UUID NOT NULL REFERENCES games(id) ON DELETE CASCADE,
  rating_before   DOUBLE PRECISION NOT NULL,
  rd_before       DOUBLE PRECISION NOT NULL,
  vol_before      DOUBLE PRECISION NOT NULL,
  rating_after    DOUBLE PRECISION NOT NULL,
  rd_after        DOUBLE PRECISION NOT NULL,
  vol_after       DOUBLE PRECISION NOT NULL,
  opponent_rating DOUBLE PRECISION NOT NULL,
  opponent_rd     DOUBLE PRECISION NOT NULL,
  score           DOUBLE PRECISION NOT NULL,  -- 1.0 / 0.5 / 0.0
  time_control    TEXT,       -- for future per-pool splitting
  variant         TEXT,       -- for future per-pool splitting
  played_side     TEXT NOT NULL,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(user_id, game_id)
);

-- RLS on rating_history
ALTER TABLE rating_history ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Users can view their own rating history"
  ON rating_history FOR SELECT
  USING (auth.uid() = user_id);

CREATE POLICY "Users can view opponent rating history for their games"
  ON rating_history FOR SELECT
  USING (game_id IN (
    SELECT id FROM games
    WHERE attacker_id = auth.uid() OR defender_id = auth.uid()
  ));

-- Grant access (profiles columns are already readable via existing RLS)
GRANT SELECT ON rating_history TO authenticated;
GRANT SELECT ON rating_history TO anon;

-- ---------------------------------------------------------------------------
-- 2. Glicko-2 update_ratings(p_game_id) function
-- ---------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_ratings(p_game_id UUID)
RETURNS void
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  v_game        RECORD;
  v_atk         RECORD;
  v_def         RECORD;
  -- Glicko-2 constants
  c_tau         CONSTANT DOUBLE PRECISION := 0.5;
  c_glicko2_scale CONSTANT DOUBLE PRECISION := 173.7178;
  c_epsilon     CONSTANT DOUBLE PRECISION := 0.000001;
  -- Working variables for attacker
  a_mu          DOUBLE PRECISION;
  a_phi         DOUBLE PRECISION;
  a_sigma       DOUBLE PRECISION;
  a_score       DOUBLE PRECISION;
  a_opp_mu      DOUBLE PRECISION;
  a_opp_phi     DOUBLE PRECISION;
  -- Working variables for defender
  d_mu          DOUBLE PRECISION;
  d_phi         DOUBLE PRECISION;
  d_sigma       DOUBLE PRECISION;
  d_score       DOUBLE PRECISION;
  d_opp_mu      DOUBLE PRECISION;
  d_opp_phi     DOUBLE PRECISION;
  -- Computation temps
  v_g           DOUBLE PRECISION;
  v_ee          DOUBLE PRECISION;
  v_v           DOUBLE PRECISION;
  v_delta       DOUBLE PRECISION;
  v_ln_sig2     DOUBLE PRECISION;
  v_il_a        DOUBLE PRECISION;
  v_il_b        DOUBLE PRECISION;
  v_il_c        DOUBLE PRECISION;
  v_fa          DOUBLE PRECISION;
  v_fb          DOUBLE PRECISION;
  v_fc          DOUBLE PRECISION;
  v_new_sigma   DOUBLE PRECISION;
  v_phi_star    DOUBLE PRECISION;
  v_new_phi     DOUBLE PRECISION;
  v_new_mu      DOUBLE PRECISION;
  -- Final Glicko-1 scale results
  a_new_rating  DOUBLE PRECISION;
  a_new_rd      DOUBLE PRECISION;
  a_new_vol     DOUBLE PRECISION;
  d_new_rating  DOUBLE PRECISION;
  d_new_rd      DOUBLE PRECISION;
  d_new_vol     DOUBLE PRECISION;
BEGIN
  -- 1. Load and validate game
  SELECT * INTO v_game FROM games WHERE id = p_game_id;
  IF NOT FOUND THEN RETURN; END IF;
  IF v_game.status != 'finished' THEN RETURN; END IF;
  IF v_game.is_rated IS NOT TRUE THEN RETURN; END IF;
  IF v_game.game_mode != 'multiplayer' THEN RETURN; END IF;
  IF v_game.attacker_id IS NULL OR v_game.defender_id IS NULL THEN RETURN; END IF;
  IF v_game.total_moves < 4 THEN RETURN; END IF;

  -- Only rate games between two signed-in (non-anonymous) players
  IF EXISTS (SELECT 1 FROM auth.users WHERE id = v_game.attacker_id
             AND raw_app_meta_data->>'provider' = 'anonymous') THEN RETURN; END IF;
  IF EXISTS (SELECT 1 FROM auth.users WHERE id = v_game.defender_id
             AND raw_app_meta_data->>'provider' = 'anonymous') THEN RETURN; END IF;

  -- 2. Idempotency check
  IF EXISTS (SELECT 1 FROM rating_history WHERE game_id = p_game_id LIMIT 1) THEN
    RETURN;
  END IF;

  -- 3. Load player profiles
  SELECT * INTO v_atk FROM profiles WHERE id = v_game.attacker_id;
  IF NOT FOUND THEN RETURN; END IF;
  SELECT * INTO v_def FROM profiles WHERE id = v_game.defender_id;
  IF NOT FOUND THEN RETURN; END IF;

  -- 4. Determine scores
  IF v_game.winner = 'attacker' THEN
    a_score := 1.0;
    d_score := 0.0;
  ELSIF v_game.winner = 'defender' THEN
    a_score := 0.0;
    d_score := 1.0;
  ELSE
    -- Draw
    a_score := 0.5;
    d_score := 0.5;
  END IF;

  -- 5. Convert to Glicko-2 scale
  a_mu    := (v_atk.rating - 1500.0) / c_glicko2_scale;
  a_phi   := v_atk.rating_rd / c_glicko2_scale;
  a_sigma := v_atk.rating_vol;
  d_mu    := (v_def.rating - 1500.0) / c_glicko2_scale;
  d_phi   := v_def.rating_rd / c_glicko2_scale;
  d_sigma := v_def.rating_vol;

  -- =========================================================================
  -- 6. Compute new rating for ATTACKER (opponent = defender)
  -- =========================================================================
  a_opp_mu  := d_mu;
  a_opp_phi := d_phi;

  -- g(phi)
  v_g := 1.0 / sqrt(1.0 + 3.0 * a_opp_phi * a_opp_phi / (pi() * pi()));
  -- E(mu, mu_j, phi_j)
  v_ee := 1.0 / (1.0 + exp(-v_g * (a_mu - a_opp_mu)));
  -- v (estimated variance)
  v_v := 1.0 / (v_g * v_g * v_ee * (1.0 - v_ee));
  -- delta
  v_delta := v_v * v_g * (a_score - v_ee);

  -- Iterative algorithm to find new volatility (Illinois algorithm)
  v_ln_sig2 := ln(a_sigma * a_sigma);

  IF v_delta * v_delta > a_phi * a_phi + v_v THEN
    v_il_b := ln(v_delta * v_delta - a_phi * a_phi - v_v);
  ELSE
    -- Find k such that f(a - k*tau) < 0
    v_il_b := v_ln_sig2 - c_tau;
    WHILE (exp(v_il_b) * (v_delta * v_delta - a_phi * a_phi - v_v - exp(v_il_b)))
          / (2.0 * (a_phi * a_phi + v_v + exp(v_il_b)) * (a_phi * a_phi + v_v + exp(v_il_b)))
          - (v_il_b - v_ln_sig2) / (c_tau * c_tau) >= 0 LOOP
      v_il_b := v_il_b - c_tau;
    END LOOP;
  END IF;

  v_il_a := v_ln_sig2;
  v_fa := (exp(v_il_a) * (v_delta * v_delta - a_phi * a_phi - v_v - exp(v_il_a)))
          / (2.0 * (a_phi * a_phi + v_v + exp(v_il_a)) * (a_phi * a_phi + v_v + exp(v_il_a)))
          - (v_il_a - v_ln_sig2) / (c_tau * c_tau);
  v_fb := (exp(v_il_b) * (v_delta * v_delta - a_phi * a_phi - v_v - exp(v_il_b)))
          / (2.0 * (a_phi * a_phi + v_v + exp(v_il_b)) * (a_phi * a_phi + v_v + exp(v_il_b)))
          - (v_il_b - v_ln_sig2) / (c_tau * c_tau);

  WHILE abs(v_il_b - v_il_a) > c_epsilon LOOP
    v_il_c := v_il_a + (v_il_a - v_il_b) * v_fa / (v_fb - v_fa);
    v_fc := (exp(v_il_c) * (v_delta * v_delta - a_phi * a_phi - v_v - exp(v_il_c)))
            / (2.0 * (a_phi * a_phi + v_v + exp(v_il_c)) * (a_phi * a_phi + v_v + exp(v_il_c)))
            - (v_il_c - v_ln_sig2) / (c_tau * c_tau);
    IF v_fc * v_fb <= 0 THEN
      v_il_a := v_il_b;
      v_fa := v_fb;
    ELSE
      v_fa := v_fa / 2.0;
    END IF;
    v_il_b := v_il_c;
    v_fb := v_fc;
  END LOOP;

  a_new_vol := exp(v_il_b / 2.0);
  v_phi_star := sqrt(a_phi * a_phi + a_new_vol * a_new_vol);
  v_new_phi := 1.0 / sqrt(1.0 / (v_phi_star * v_phi_star) + 1.0 / v_v);
  v_new_mu := a_mu + v_new_phi * v_new_phi * v_g * (a_score - v_ee);

  -- Convert back to Glicko-1 scale
  a_new_rating := v_new_mu * c_glicko2_scale + 1500.0;
  a_new_rd     := v_new_phi * c_glicko2_scale;

  -- =========================================================================
  -- 7. Compute new rating for DEFENDER (opponent = attacker)
  -- =========================================================================
  d_opp_mu  := a_mu;  -- use original (pre-update) values
  d_opp_phi := a_phi;

  v_g := 1.0 / sqrt(1.0 + 3.0 * d_opp_phi * d_opp_phi / (pi() * pi()));
  v_ee := 1.0 / (1.0 + exp(-v_g * (d_mu - d_opp_mu)));
  v_v := 1.0 / (v_g * v_g * v_ee * (1.0 - v_ee));
  v_delta := v_v * v_g * (d_score - v_ee);

  v_ln_sig2 := ln(d_sigma * d_sigma);

  IF v_delta * v_delta > d_phi * d_phi + v_v THEN
    v_il_b := ln(v_delta * v_delta - d_phi * d_phi - v_v);
  ELSE
    v_il_b := v_ln_sig2 - c_tau;
    WHILE (exp(v_il_b) * (v_delta * v_delta - d_phi * d_phi - v_v - exp(v_il_b)))
          / (2.0 * (d_phi * d_phi + v_v + exp(v_il_b)) * (d_phi * d_phi + v_v + exp(v_il_b)))
          - (v_il_b - v_ln_sig2) / (c_tau * c_tau) >= 0 LOOP
      v_il_b := v_il_b - c_tau;
    END LOOP;
  END IF;

  v_il_a := v_ln_sig2;
  v_fa := (exp(v_il_a) * (v_delta * v_delta - d_phi * d_phi - v_v - exp(v_il_a)))
          / (2.0 * (d_phi * d_phi + v_v + exp(v_il_a)) * (d_phi * d_phi + v_v + exp(v_il_a)))
          - (v_il_a - v_ln_sig2) / (c_tau * c_tau);
  v_fb := (exp(v_il_b) * (v_delta * v_delta - d_phi * d_phi - v_v - exp(v_il_b)))
          / (2.0 * (d_phi * d_phi + v_v + exp(v_il_b)) * (d_phi * d_phi + v_v + exp(v_il_b)))
          - (v_il_b - v_ln_sig2) / (c_tau * c_tau);

  WHILE abs(v_il_b - v_il_a) > c_epsilon LOOP
    v_il_c := v_il_a + (v_il_a - v_il_b) * v_fa / (v_fb - v_fa);
    v_fc := (exp(v_il_c) * (v_delta * v_delta - d_phi * d_phi - v_v - exp(v_il_c)))
            / (2.0 * (d_phi * d_phi + v_v + exp(v_il_c)) * (d_phi * d_phi + v_v + exp(v_il_c)))
            - (v_il_c - v_ln_sig2) / (c_tau * c_tau);
    IF v_fc * v_fb <= 0 THEN
      v_il_a := v_il_b;
      v_fa := v_fb;
    ELSE
      v_fa := v_fa / 2.0;
    END IF;
    v_il_b := v_il_c;
    v_fb := v_fc;
  END LOOP;

  d_new_vol := exp(v_il_b / 2.0);
  v_phi_star := sqrt(d_phi * d_phi + d_new_vol * d_new_vol);
  v_new_phi := 1.0 / sqrt(1.0 / (v_phi_star * v_phi_star) + 1.0 / v_v);
  v_new_mu := d_mu + v_new_phi * v_new_phi * v_g * (d_score - v_ee);

  d_new_rating := v_new_mu * c_glicko2_scale + 1500.0;
  d_new_rd     := v_new_phi * c_glicko2_scale;

  -- =========================================================================
  -- 8. Write results
  -- =========================================================================

  -- Rating history for attacker
  INSERT INTO rating_history
    (user_id, game_id, rating_before, rd_before, vol_before,
     rating_after, rd_after, vol_after,
     opponent_rating, opponent_rd, score,
     time_control, variant, played_side)
  VALUES
    (v_game.attacker_id, p_game_id,
     v_atk.rating, v_atk.rating_rd, v_atk.rating_vol,
     a_new_rating, a_new_rd, a_new_vol,
     v_def.rating, v_def.rating_rd, a_score,
     v_game.time_control, v_game.variant, 'attacker');

  -- Rating history for defender
  INSERT INTO rating_history
    (user_id, game_id, rating_before, rd_before, vol_before,
     rating_after, rd_after, vol_after,
     opponent_rating, opponent_rd, score,
     time_control, variant, played_side)
  VALUES
    (v_game.defender_id, p_game_id,
     v_def.rating, v_def.rating_rd, v_def.rating_vol,
     d_new_rating, d_new_rd, d_new_vol,
     v_atk.rating, v_atk.rating_rd, d_score,
     v_game.time_control, v_game.variant, 'defender');

  -- Update profiles
  UPDATE profiles SET
    rating = a_new_rating,
    rating_rd = a_new_rd,
    rating_vol = a_new_vol,
    games_rated = games_rated + 1
  WHERE id = v_game.attacker_id;

  UPDATE profiles SET
    rating = d_new_rating,
    rating_rd = d_new_rd,
    rating_vol = d_new_vol,
    games_rated = games_rated + 1
  WHERE id = v_game.defender_id;

END;
$$;

-- Grant execute to authenticated users (function runs as SECURITY DEFINER)
GRANT EXECUTE ON FUNCTION update_ratings(UUID) TO authenticated;
