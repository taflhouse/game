import "@supabase/functions-js/edge-runtime.d.ts";
import { createClient, SupabaseClient } from "@supabase/supabase-js";

const supabaseUrl = Deno.env.get("SUPABASE_URL")!;
const serviceRoleKey = Deno.env.get("SUPABASE_SERVICE_ROLE_KEY")!;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface Tournament {
  id: string;
  format: string;
  variant: string;
  time_control: string | null;
  time_per_player_ms: number | null;
  time_per_move_seconds: number | null;
  is_rated: boolean;
  status: string;
  current_round: number;
  total_rounds: number | null;
  round_interval_minutes: number;
}

interface TournamentPlayer {
  id: string;
  player_id: string;
  player_name: string;
  score: number;
  wins: number;
  losses: number;
  draws: number;
  buchholz: number;
  games_played: number;
  attacker_wins: number;
  attacker_losses: number;
  attacker_draws: number;
  defender_wins: number;
  defender_losses: number;
  defender_draws: number;
  is_active: boolean;
  seed: number | null;
}

interface Pairing {
  player_1_id: string;
  player_2_id: string | null; // null = bye
  player_1_side: "attacker" | "defender";
}

interface PairingRow {
  id: string;
  tournament_id: string;
  round_number: number;
  player_1_id: string;
  player_2_id: string | null;
  player_1_side: string;
  game_id: string | null;
  winner_id: string | null;
  result: string | null;
}

// ---------------------------------------------------------------------------
// Pairing algorithms
// ---------------------------------------------------------------------------

function computeTotalRounds(format: string, playerCount: number): number {
  switch (format) {
    case "swiss":
      return Math.ceil(Math.log2(Math.max(playerCount, 2)));
    case "round_robin":
      return playerCount % 2 === 0 ? playerCount - 1 : playerCount;
    case "single_elimination":
      return Math.ceil(Math.log2(Math.max(playerCount, 2)));
    default:
      return Math.ceil(Math.log2(Math.max(playerCount, 2)));
  }
}

/** Count how many times a player has been attacker in prior pairings */
function countAttackerAssignments(
  playerId: string,
  priorPairings: PairingRow[]
): number {
  let count = 0;
  for (const p of priorPairings) {
    if (p.player_1_id === playerId && p.player_1_side === "attacker") count++;
    if (p.player_2_id === playerId && p.player_1_side === "defender") count++;
  }
  return count;
}

/** Determine sides for a pairing based on side alternation */
function assignSides(
  p1: string,
  p2: string,
  priorPairings: PairingRow[]
): "attacker" | "defender" {
  const p1Attacks = countAttackerAssignments(p1, priorPairings);
  const p2Attacks = countAttackerAssignments(p2, priorPairings);
  if (p1Attacks < p2Attacks) return "attacker"; // p1 gets attacker
  if (p1Attacks > p2Attacks) return "defender"; // p1 gets defender
  return p1 < p2 ? "attacker" : "defender"; // deterministic tiebreak
}

/** Swiss pairing: sort by score, pair adjacent, avoid rematches */
function swissPairings(
  players: TournamentPlayer[],
  priorPairings: PairingRow[]
): Pairing[] {
  const active = players.filter((p) => p.is_active);
  const sorted = [...active].sort((a, b) => {
    if (b.score !== a.score) return b.score - a.score;
    return b.buchholz - a.buchholz;
  });

  const paired = new Set<string>();
  const pairings: Pairing[] = [];

  // Track who has played whom
  const opponents = new Map<string, Set<string>>();
  for (const p of priorPairings) {
    if (!p.player_2_id) continue;
    if (!opponents.has(p.player_1_id))
      opponents.set(p.player_1_id, new Set());
    if (!opponents.has(p.player_2_id))
      opponents.set(p.player_2_id, new Set());
    opponents.get(p.player_1_id)!.add(p.player_2_id);
    opponents.get(p.player_2_id)!.add(p.player_1_id);
  }

  // Track who has had a bye
  const hadBye = new Set<string>();
  for (const p of priorPairings) {
    if (!p.player_2_id) hadBye.add(p.player_1_id);
  }

  for (let i = 0; i < sorted.length; i++) {
    const p1 = sorted[i];
    if (paired.has(p1.player_id)) continue;

    let matched = false;
    for (let j = i + 1; j < sorted.length; j++) {
      const p2 = sorted[j];
      if (paired.has(p2.player_id)) continue;
      // Skip if already played each other (if possible)
      const prevOpps = opponents.get(p1.player_id);
      if (prevOpps?.has(p2.player_id)) continue;

      const side = assignSides(p1.player_id, p2.player_id, priorPairings);
      pairings.push({
        player_1_id: p1.player_id,
        player_2_id: p2.player_id,
        player_1_side: side,
      });
      paired.add(p1.player_id);
      paired.add(p2.player_id);
      matched = true;
      break;
    }

    if (!matched) {
      // Try again without rematch restriction
      for (let j = i + 1; j < sorted.length; j++) {
        const p2 = sorted[j];
        if (paired.has(p2.player_id)) continue;
        const side = assignSides(p1.player_id, p2.player_id, priorPairings);
        pairings.push({
          player_1_id: p1.player_id,
          player_2_id: p2.player_id,
          player_1_side: side,
        });
        paired.add(p1.player_id);
        paired.add(p2.player_id);
        matched = true;
        break;
      }
    }

    if (!matched) {
      // Give bye — score += 1.0 (handled in standings update)
      pairings.push({
        player_1_id: p1.player_id,
        player_2_id: null,
        player_1_side: "attacker",
      });
      paired.add(p1.player_id);
    }
  }

  return pairings;
}

/** Round-robin pairing using the circle method */
function roundRobinPairings(
  players: TournamentPlayer[],
  roundNumber: number,
  priorPairings: PairingRow[]
): Pairing[] {
  const active = players.filter((p) => p.is_active);
  const ids = active.map((p) => p.player_id);

  // Add a dummy for odd count
  const hasDummy = ids.length % 2 !== 0;
  if (hasDummy) ids.push("__bye__");

  const n = ids.length;
  const pairings: Pairing[] = [];

  // Circle method: fix player 0, rotate rest
  // Round r (1-indexed): rotation = r - 1
  const rot = roundNumber - 1;
  const fixed = ids[0];
  const rest = ids.slice(1);
  const rotated: string[] = [];
  for (let i = 0; i < rest.length; i++) {
    rotated.push(rest[(i + rot) % rest.length]);
  }
  const all = [fixed, ...rotated];

  for (let i = 0; i < n / 2; i++) {
    const p1 = all[i];
    const p2 = all[n - 1 - i];

    if (p1 === "__bye__") {
      pairings.push({
        player_1_id: p2,
        player_2_id: null,
        player_1_side: "attacker",
      });
    } else if (p2 === "__bye__") {
      pairings.push({
        player_1_id: p1,
        player_2_id: null,
        player_1_side: "attacker",
      });
    } else {
      const side = assignSides(p1, p2, priorPairings);
      pairings.push({
        player_1_id: p1,
        player_2_id: p2,
        player_1_side: side,
      });
    }
  }

  return pairings;
}

/** Single elimination pairing */
function eliminationPairings(
  players: TournamentPlayer[],
  roundNumber: number,
  priorPairings: PairingRow[]
): Pairing[] {
  if (roundNumber === 1) {
    // Seed by registration order (seed field) or player_id
    const active = players
      .filter((p) => p.is_active)
      .sort((a, b) => (a.seed ?? 999) - (b.seed ?? 999));

    // Pad to power of 2
    const n = active.length;
    const target = Math.pow(2, Math.ceil(Math.log2(Math.max(n, 2))));
    const pairings: Pairing[] = [];

    for (let i = 0; i < target / 2; i++) {
      const p1 = active[i];
      const p2Idx = target - 1 - i;
      const p2 = p2Idx < n ? active[p2Idx] : null;

      if (!p1) continue;

      if (!p2) {
        // Bye for p1
        pairings.push({
          player_1_id: p1.player_id,
          player_2_id: null,
          player_1_side: "attacker",
        });
      } else {
        const side = assignSides(
          p1.player_id,
          p2.player_id,
          priorPairings
        );
        pairings.push({
          player_1_id: p1.player_id,
          player_2_id: p2.player_id,
          player_1_side: side,
        });
      }
    }

    return pairings;
  }

  // Subsequent rounds: winners from previous round advance
  const prevRound = priorPairings.filter(
    (p) => p.round_number === roundNumber - 1
  );
  const winners: string[] = [];

  for (const p of prevRound) {
    if (p.result === "bye" || !p.player_2_id) {
      winners.push(p.player_1_id);
    } else if (p.winner_id) {
      winners.push(p.winner_id);
    } else if (p.result === "draw") {
      // In elimination, draws shouldn't happen, but default to player_1
      winners.push(p.player_1_id);
    }
  }

  const pairings: Pairing[] = [];
  for (let i = 0; i < winners.length; i += 2) {
    if (i + 1 < winners.length) {
      const side = assignSides(winners[i], winners[i + 1], priorPairings);
      pairings.push({
        player_1_id: winners[i],
        player_2_id: winners[i + 1],
        player_1_side: side,
      });
    } else {
      // Odd winner gets bye
      pairings.push({
        player_1_id: winners[i],
        player_2_id: null,
        player_1_side: "attacker",
      });
    }
  }

  return pairings;
}

function generatePairings(
  tournament: Tournament,
  players: TournamentPlayer[],
  roundNumber: number,
  priorPairings: PairingRow[]
): Pairing[] {
  switch (tournament.format) {
    case "swiss":
      return swissPairings(players, priorPairings);
    case "round_robin":
      return roundRobinPairings(players, roundNumber, priorPairings);
    case "single_elimination":
      return eliminationPairings(players, roundNumber, priorPairings);
    default:
      return swissPairings(players, priorPairings);
  }
}

// ---------------------------------------------------------------------------
// Standings update
// ---------------------------------------------------------------------------

async function updateStandings(
  supabase: SupabaseClient,
  tournamentId: string,
  players: TournamentPlayer[],
  allPairings: PairingRow[]
) {
  const playerMap = new Map(players.map((p) => [p.player_id, { ...p }]));

  // Reset all stats
  for (const p of playerMap.values()) {
    p.score = 0;
    p.wins = 0;
    p.losses = 0;
    p.draws = 0;
    p.games_played = 0;
    p.attacker_wins = 0;
    p.attacker_losses = 0;
    p.attacker_draws = 0;
    p.defender_wins = 0;
    p.defender_losses = 0;
    p.defender_draws = 0;
  }

  // Recompute from all pairings
  for (const pairing of allPairings) {
    if (!pairing.result) continue;

    const p1 = playerMap.get(pairing.player_1_id);
    if (!p1) continue;

    if (pairing.result === "bye") {
      p1.score += 1;
      continue;
    }

    const p2 = pairing.player_2_id
      ? playerMap.get(pairing.player_2_id)
      : null;

    const p1IsAttacker = pairing.player_1_side === "attacker";

    if (
      pairing.result === "player_1" ||
      pairing.result === "forfeit_2"
    ) {
      p1.wins++;
      p1.score += 1;
      p1.games_played++;
      if (p1IsAttacker) p1.attacker_wins++;
      else p1.defender_wins++;

      if (p2) {
        p2.losses++;
        p2.games_played++;
        if (p1IsAttacker) p2.defender_losses++;
        else p2.attacker_losses++;
      }
    } else if (
      pairing.result === "player_2" ||
      pairing.result === "forfeit_1"
    ) {
      p1.losses++;
      p1.games_played++;
      if (p1IsAttacker) p1.attacker_losses++;
      else p1.defender_losses++;

      if (p2) {
        p2.wins++;
        p2.score += 1;
        p2.games_played++;
        if (p1IsAttacker) p2.defender_wins++;
        else p2.attacker_wins++;
      }
    } else if (pairing.result === "draw") {
      p1.draws++;
      p1.score += 0.5;
      p1.games_played++;
      if (p1IsAttacker) p1.attacker_draws++;
      else p1.defender_draws++;

      if (p2) {
        p2.draws++;
        p2.score += 0.5;
        p2.games_played++;
        if (p1IsAttacker) p2.defender_draws++;
        else p2.attacker_draws++;
      }
    }
  }

  // Compute Buchholz (sum of opponents' scores)
  for (const p of playerMap.values()) {
    let buchholz = 0;
    for (const pairing of allPairings) {
      if (!pairing.result || pairing.result === "bye") continue;
      let oppId: string | null = null;
      if (pairing.player_1_id === p.player_id) oppId = pairing.player_2_id;
      if (pairing.player_2_id === p.player_id) oppId = pairing.player_1_id;
      if (oppId) {
        const opp = playerMap.get(oppId);
        if (opp) buchholz += opp.score;
      }
    }
    p.buchholz = buchholz;
  }

  // Write updates
  for (const p of playerMap.values()) {
    await supabase
      .from("tournament_players")
      .update({
        score: p.score,
        wins: p.wins,
        losses: p.losses,
        draws: p.draws,
        buchholz: p.buchholz,
        games_played: p.games_played,
        attacker_wins: p.attacker_wins,
        attacker_losses: p.attacker_losses,
        attacker_draws: p.attacker_draws,
        defender_wins: p.defender_wins,
        defender_losses: p.defender_losses,
        defender_draws: p.defender_draws,
      })
      .eq("tournament_id", tournamentId)
      .eq("player_id", p.player_id);
  }
}

// ---------------------------------------------------------------------------
// Game creation
// ---------------------------------------------------------------------------

async function createGamesForPairings(
  supabase: SupabaseClient,
  tournament: Tournament,
  pairings: Pairing[],
  players: TournamentPlayer[],
  roundNumber: number
): Promise<{ pairingId: string; gameId: string | null }[]> {
  const playerMap = new Map(players.map((p) => [p.player_id, p]));
  const results: { pairingId: string; gameId: string | null }[] = [];

  for (let i = 0; i < pairings.length; i++) {
    const pairing = pairings[i];
    const pairingId = crypto.randomUUID();

    if (!pairing.player_2_id) {
      // Bye — no game needed
      const { error } = await supabase.from("tournament_pairings").insert({
        id: pairingId,
        tournament_id: tournament.id,
        round_number: roundNumber,
        pairing_order: i,
        player_1_id: pairing.player_1_id,
        player_2_id: null,
        player_1_side: pairing.player_1_side,
        game_id: null,
        result: "bye",
      });
      if (error) console.error("Error inserting bye pairing:", error);
      results.push({ pairingId, gameId: null });
      continue;
    }

    const p1 = playerMap.get(pairing.player_1_id);
    const p2 = playerMap.get(pairing.player_2_id);
    if (!p1 || !p2) continue;

    const attackerId =
      pairing.player_1_side === "attacker"
        ? pairing.player_1_id
        : pairing.player_2_id;
    const defenderId =
      pairing.player_1_side === "defender"
        ? pairing.player_1_id
        : pairing.player_2_id;
    const attackerName =
      pairing.player_1_side === "attacker" ? p1.player_name : p2.player_name;
    const defenderName =
      pairing.player_1_side === "defender" ? p1.player_name : p2.player_name;

    const gameId = crypto.randomUUID();
    const gameRow: Record<string, unknown> = {
      id: gameId,
      variant: tournament.variant,
      status: "active",
      game_mode: "multiplayer",
      attacker_id: attackerId,
      defender_id: defenderId,
      attacker_name: attackerName,
      defender_name: defenderName,
      current_turn: "attacker",
      moves: [],
      result_desc: "in_progress",
      is_public: true,
      is_rated: tournament.is_rated,
      tournament_id: tournament.id,
      tournament_pairing_id: pairingId,
      total_moves: 0,
    };

    // Add time control fields
    if (tournament.time_control === "blitz" && tournament.time_per_player_ms) {
      gameRow.time_control = "blitz";
      gameRow.time_per_player_ms = tournament.time_per_player_ms;
      gameRow.attacker_time_remaining_ms = tournament.time_per_player_ms;
      gameRow.defender_time_remaining_ms = tournament.time_per_player_ms;
    } else if (
      tournament.time_control === "daily" &&
      tournament.time_per_move_seconds
    ) {
      gameRow.time_control = "daily";
      gameRow.time_per_move_seconds = tournament.time_per_move_seconds;
    }

    const { error: gameError } = await supabase.from("games").insert(gameRow);
    if (gameError) {
      console.error("Error creating game:", gameError);
      continue;
    }

    const { error: pairingError } = await supabase
      .from("tournament_pairings")
      .insert({
        id: pairingId,
        tournament_id: tournament.id,
        round_number: roundNumber,
        pairing_order: i,
        player_1_id: pairing.player_1_id,
        player_2_id: pairing.player_2_id,
        player_1_side: pairing.player_1_side,
        game_id: gameId,
      });
    if (pairingError) console.error("Error inserting pairing:", pairingError);

    results.push({ pairingId, gameId });
  }

  return results;
}

// ---------------------------------------------------------------------------
// Push notifications for new round
// ---------------------------------------------------------------------------

async function notifyPlayers(
  supabase: SupabaseClient,
  tournament: Tournament,
  pairings: Pairing[],
  roundNumber: number
) {
  const pushFnUrl = Deno.env.get("SUPABASE_URL") + "/functions/v1/send-push";
  const playerIds = new Set<string>();
  for (const p of pairings) {
    playerIds.add(p.player_1_id);
    if (p.player_2_id) playerIds.add(p.player_2_id);
  }

  for (const userId of playerIds) {
    try {
      await fetch(pushFnUrl, {
        method: "POST",
        headers: {
          Authorization: `Bearer ${serviceRoleKey}`,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          user_id: userId,
          game_id: tournament.id,
          mover_name: tournament.variant,
          variant: `Tournament round ${roundNumber} started`,
        }),
      });
    } catch (e) {
      console.error("Push notification error:", e);
    }
  }
}

// ---------------------------------------------------------------------------
// Main handler
// ---------------------------------------------------------------------------

Deno.serve(async (req) => {
  // Auth: accept both service_role key and authenticated user tokens
  const authHeader = req.headers.get("Authorization");
  if (!authHeader?.startsWith("Bearer ")) {
    return new Response("Unauthorized", { status: 401 });
  }

  const token = authHeader.replace("Bearer ", "");
  const supabase = createClient(supabaseUrl, serviceRoleKey);

  let body: { tournament_id: string; action?: string };
  try {
    body = await req.json();
  } catch {
    return new Response("Invalid JSON body", { status: 400 });
  }

  const { tournament_id, action } = body;
  if (!tournament_id) {
    return new Response("Missing tournament_id", { status: 400 });
  }

  // Fetch tournament
  const { data: tournament, error: tErr } = await supabase
    .from("tournaments")
    .select("*")
    .eq("id", tournament_id)
    .single();

  if (tErr || !tournament) {
    return Response.json({ error: "Tournament not found" }, { status: 404 });
  }

  // Fetch players
  const { data: players } = await supabase
    .from("tournament_players")
    .select("*")
    .eq("tournament_id", tournament_id);

  if (!players || players.length < 2) {
    return Response.json(
      { error: "Not enough players" },
      { status: 400 }
    );
  }

  // Fetch all pairings
  const { data: allPairings } = await supabase
    .from("tournament_pairings")
    .select("*")
    .eq("tournament_id", tournament_id)
    .order("round_number", { ascending: true });

  const priorPairings: PairingRow[] = allPairings ?? [];

  // ---------------------------------------------------------------------------
  // START tournament
  // ---------------------------------------------------------------------------
  if (action === "start" && tournament.status === "registration") {
    const totalRounds = computeTotalRounds(tournament.format, players.length);
    const roundNumber = 1;

    // Assign seeds if not already set
    for (let i = 0; i < players.length; i++) {
      if (players[i].seed === null) {
        await supabase
          .from("tournament_players")
          .update({ seed: i + 1 })
          .eq("id", players[i].id);
        players[i].seed = i + 1;
      }
    }

    // Generate round 1 pairings
    const pairings = generatePairings(
      tournament,
      players,
      roundNumber,
      []
    );

    // Create games
    await createGamesForPairings(
      supabase,
      tournament,
      pairings,
      players,
      roundNumber
    );

    // Update tournament status
    await supabase
      .from("tournaments")
      .update({
        status: "active",
        current_round: roundNumber,
        total_rounds: totalRounds,
        started_at: new Date().toISOString(),
      })
      .eq("id", tournament_id);

    // Notify players
    await notifyPlayers(supabase, tournament, pairings, roundNumber);

    return Response.json({
      status: "started",
      round: roundNumber,
      total_rounds: totalRounds,
      pairings: pairings.length,
    });
  }

  // ---------------------------------------------------------------------------
  // ADVANCE to next round (called by trigger when all games in current round finish)
  // ---------------------------------------------------------------------------
  if (tournament.status === "active") {
    // Refresh pairings with results
    const { data: freshPairings } = await supabase
      .from("tournament_pairings")
      .select("*")
      .eq("tournament_id", tournament_id)
      .order("round_number", { ascending: true });

    const currentPairings: PairingRow[] = freshPairings ?? [];

    // Update standings from all completed pairings
    await updateStandings(supabase, tournament_id, players, currentPairings);

    // Re-fetch players with updated scores
    const { data: updatedPlayers } = await supabase
      .from("tournament_players")
      .select("*")
      .eq("tournament_id", tournament_id);

    const currentRound = tournament.current_round;
    const totalRounds = tournament.total_rounds ?? 0;

    if (currentRound >= totalRounds) {
      // Tournament is finished
      await supabase
        .from("tournaments")
        .update({
          status: "finished",
          finished_at: new Date().toISOString(),
        })
        .eq("id", tournament_id);

      return Response.json({ status: "finished", final_round: currentRound });
    }

    // Generate next round
    const nextRound = currentRound + 1;
    const pairings = generatePairings(
      tournament,
      updatedPlayers ?? players,
      nextRound,
      currentPairings
    );

    await createGamesForPairings(
      supabase,
      tournament,
      pairings,
      updatedPlayers ?? players,
      nextRound
    );

    await supabase
      .from("tournaments")
      .update({ current_round: nextRound })
      .eq("id", tournament_id);

    // Notify players
    await notifyPlayers(supabase, tournament, pairings, nextRound);

    return Response.json({
      status: "advanced",
      round: nextRound,
      pairings: pairings.length,
    });
  }

  return Response.json({ status: "no_action", tournament_status: tournament.status });
});
