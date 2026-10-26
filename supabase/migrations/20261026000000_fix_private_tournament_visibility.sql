-- The original SELECT policy included `invite_code IS NOT NULL`, which is true
-- for every private tournament, so all of them were readable by anyone. Its
-- participant check also compared tp.tournament_id to tp.id (the unqualified
-- `id` bound to tournament_players), so it never matched.
DROP POLICY IF EXISTS "Anyone can view public tournaments" ON tournaments;

CREATE POLICY "Anyone can view public tournaments"
  ON tournaments FOR SELECT
  USING (is_private = false OR organizer_id = auth.uid()
         OR EXISTS (SELECT 1 FROM tournament_players tp
                    WHERE tp.tournament_id = tournaments.id
                      AND tp.player_id = auth.uid()));

-- Knowing the invite code is what grants access to a private tournament, so
-- look it up through a definer function that only matches the exact code.
CREATE OR REPLACE FUNCTION get_tournament_by_invite_code(p_code TEXT)
RETURNS SETOF tournaments
LANGUAGE sql STABLE SECURITY DEFINER
SET search_path = public
AS $$
  SELECT * FROM tournaments WHERE invite_code = p_code AND p_code <> '';
$$;

REVOKE ALL ON FUNCTION get_tournament_by_invite_code(TEXT) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION get_tournament_by_invite_code(TEXT) TO anon, authenticated;
