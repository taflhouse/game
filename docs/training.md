# Training: expanding /learn beyond the tutorial

## Decision

`/learn` stops being a single ladder of lessons and becomes a hub over six
tracks: **Lessons**, **Review**, **Tactics**, **Endgames**, **Openings**, and
**Library**. The existing tutorial keeps working unchanged and becomes one track
among several.

Two of today's lessons — `reading-eval` and `ratings` — taught taflhouse.com,
not tafl. **They are removed** (`app/App/Tutorial/Lessons/Advanced.hs`), leaving
the lesson ladder purely about the game and Advanced with a single lesson, Exit
Forts. What they covered belongs where a player meets it: the eval bar explains
itself in Review, and the rating system belongs on the profile screen, not in a
lesson someone has to finish before the tutorial reads as complete.

## What exists today

`app/App/Tutorial/` is a Miso sub-component (mounted at
`app/App/View.hs:126`) holding eight lessons across three modules — five
Beginner, four Intermediate, one Advanced, after the two removals above. A lesson is a
`TutorialLesson` — an initial board plus a list of `TutorialStep`s — and each
step has one of three kinds (`app/App/Tutorial/Lessons/Types.hs:36`):

```haskell
data StepKind
  = InfoStep
  | MoveStep (Maybe [Coords]) (Maybe [Coords]) (Maybe MoveAction)
  | ChallengeStep (GameState -> Bool) (Maybe MoveAction)
```

Two things already in the tree do most of the work for the new tracks:

- **`ChallengeStep` is already a puzzle engine.** A success predicate over a
  `GameState` with an optional opponent reply is exactly what a tactics puzzle
  needs. What is missing is a runner around it — retry, reveal, streak — not an
  engine.
- **`App/Replay/` already reconstructs games.** `replayMoves`
  (`app/App/Route.hs:159`) folds `games.moves` into `[GameState]`, and the
  replay view scores the current position with `Tafl.AI.evaluate`
  (`src/Tafl/AI.hs:121`) to drive the eval bar. Game review is mostly a matter
  of walking that list and interpreting it.

Progress is a list of completed lesson IDs in `localStorage`
(`app/App/Tutorial/Update.hs:44`, `app/App/FFI.hs:342`).

Games are stored with the full move list and, for AI games, the AI's side and
depth (`supabase/migrations/20260621001400_create_games_table.sql`). Since
`20260622000000_public_game_recaps.sql` the SELECT policy is
`USING (true)` — every game is readable by anyone, so aggregate queries over the
game corpus need no `SECURITY DEFINER` wrapper.

## Shared plumbing

Build this before any individual track, so six features do not each invent their
own storage and routing.

### 1. `Track` replaces `TutorialModule`

`TutorialModule` is doing double duty as both the grouping label and the URL
segment (`/learn/beginner/<id>` via `moduleSlug`, `app/App/Route.hs:186`).
Generalise it rather than adding a parallel concept:

```haskell
data Track
  = LessonTrack LessonModule   -- Beginner | Intermediate | Advanced
  | TacticsTrack
  | EndgameTrack
  | OpeningsTrack
  | LibraryTrack
  | ReviewTrack
```

`moduleSlug` becomes `trackSlug`, and `LearnRoute` grows a track-index route so
`/learn` is the hub, `/learn/tactics` is a track, and
`/learn/tactics/<puzzle-id>` is one item. `parseRoute` already splits on the
first `/` after `learn/` and keeps only the tail; that logic becomes a real
two-segment parse.

### 2. `learning_progress` table

`localStorage` is fine for ten booleans and wrong for puzzle ratings, streaks,
and per-game review state — none of which should vanish when a player switches
devices, given they already have accounts.

```sql
CREATE TABLE learning_progress (
  user_id     UUID NOT NULL REFERENCES auth.users(id) ON DELETE CASCADE,
  track       TEXT NOT NULL,   -- 'lessons' | 'tactics' | 'endgames' | ...
  item_id     TEXT NOT NULL,   -- lesson id, puzzle id, game uuid
  status      TEXT NOT NULL,   -- 'completed' | 'failed' | 'seen'
  attempts    INTEGER NOT NULL DEFAULT 1,
  data        JSONB,           -- track-specific: solve time, review payload
  updated_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id, track, item_id)
);
```

RLS mirrors `rating_history` (`20260702000000_add_glicko2_ratings.sql`): owner
reads and writes their own rows only. Signed-out players keep using
`localStorage`; on sign-in, merge the local set up into the table once.

### 3. `src/Tafl/Review.hs` — the analysis pass

This belongs in the pure engine library, not in `app/`. It has no IO and no FFI,
so it is testable under `cabal.project.test` — which matters, because the move
grading is the part most likely to be quietly wrong.

```haskell
data MoveGrade = Best | Good | Inaccuracy | Mistake | Blunder

data KeyMoment = KeyMoment
  { kmPly        :: Int
  , kmPlayed     :: MoveAction
  , kmBetter     :: Maybe MoveAction
  , kmEvalBefore :: Int
  , kmEvalAfter  :: Int
  , kmGrade      :: MoveGrade
  }

reviewGame :: AiConfig -> GameState -> [MoveAction] -> GameReview
```

The algorithm, in two passes:

1. **Cheap pass.** `evaluate` every state in the replay. This is one call per
   ply and gives the eval curve for free.
2. **Confirm pass.** Take only the plies whose eval swung hardest against the
   side that moved — cap it, say the worst ten — and run `bestMove` at depth 3–4
   on the position *before* the move. Grade the played move only if the search
   finds something meaningfully better. If it does not, the swing was forced and
   gets no label.

The confirm pass is not optional. `evaluate` is a shallow heuristic tuned to
order a search — material with a defender premium, king-to-corner distance, king
exposure, centre control. It is good enough to steer alpha-beta and not good
enough to tell a human they blundered. Grading straight off the eval delta will
produce confident nonsense in quiet positions, which is worse than saying
nothing. Gating on a real search also bounds the cost: ten searches, not sixty.

**The pass must not block the UI.** WASM here is single-threaded, so a
sixty-ply analysis in one `update` freezes the board. Chunk it the way the
tutorial already chunks its auto-advance countdown — `scheduleTick`
(`app/App/Tutorial/Update.hs:383`) yields via `threadDelay` inside `withSink`
and re-enters through an action. Analyse one candidate ply per tick, show a
progress bar, and cache the finished `GameReview` into
`learning_progress.data` so a game is analysed once.

## The tracks

### Lessons — existing

No change beyond the two removals above and re-parenting `TutorialModule` under
`Track`. Advanced now holds only Exit Forts, so this track wants new advanced
content of its own — that is a content task, not a structural one, and Tactics
and Endgames may absorb most of what would have gone there. The wizard-then-browse behaviour in
`viewLessonSelect` (`app/App/Tutorial/View.hs:33`) stays; it just becomes the
Lessons track's own index rather than the whole `/learn` screen.

### Review — your games against the computer

The flagship. Every AI game is already stored with its full move list, so the
entire back catalogue is reviewable on day one with nothing new captured.

- **Entry points:** a "Review" button on each row of `/your-games`, and a Review
  track index listing analysed and unanalysed games.
- **The screen:** the existing replay board, plus an eval graph across the whole
  game with the key moments marked. Clicking a marker jumps the replay to that
  ply. Below it, three to five annotated moments — what was played, what was
  better, and what changed.
- **The summary:** one paragraph derived from the review, not from a language
  model. The shape is mechanical: which side held the advantage and for how
  long, the ply where it flipped, and the single largest swing. Something like
  *"You were winning from move 12 to move 34. The game turned when the king's
  route to the north-west corner closed on move 35."*
- **Reuse:** `Replay/Model.hs` gains the review payload and a selected-moment
  index; `Replay/View.hs` gains the graph and the annotation list. The analysis
  itself is `Tafl.Review`, shared with Tactics.

### Tactics — puzzles from real positions

Same runner as lessons, different framing: one position, one goal, retry until
solved, reveal on request, then straight to the next. A puzzle is a
`ChallengeStep` with metadata:

```haskell
data Puzzle = Puzzle
  { pzId       :: MisoString
  , pzVariant  :: BoardVariant
  , pzBoard    :: Board
  , pzSide     :: Side
  , pzGoal     :: PuzzleGoal
  , pzTheme    :: PuzzleTheme  -- Sandwich | ShieldWall | KingHunt | Escape | Block
  , pzSource   :: Maybe MisoString  -- game uuid + ply, when mined
  }
```

**Where positions come from** is the real question, and the answer unifies with
Review: **mine them from played games.** The confirm pass already finds
positions where one move swings the evaluation hard and a search identifies a
better move — that is the definition of a tactics puzzle. Seed the bank with
roughly twenty hand-authored positions in the style of the current lessons, then
grow it from the corpus. Mined candidates need a filter before they are shown:
the solution should be unique (second-best move materially worse) and the
position should be reachable-looking, not a blowout.

Difficulty comes later. Once there are enough puzzles, per-puzzle and per-player
ratings are the obvious ordering, and the Glicko-2 machinery already in the
codebase is the natural model to copy.

### Endgames — puzzles with a different goal type

Same runner, same storage, different bank and different win conditions. "Find
the move" is the wrong frame for an endgame; the goals are:

```haskell
data PuzzleGoal
  = FindMove (GameState -> Bool)      -- tactics
  | AchieveWithin Int (GameState -> Bool)  -- "escape in 4"
  | SurviveFor Int                    -- "hold the draw for 10 moves"
```

`AchieveWithin` and `SurviveFor` need the runner to track a move budget and to
play a real opponent — `Tafl.AI.bestMove` rather than the tutorial's scripted
`autoResponse`. That is the only genuinely new engine wiring in the whole plan,
and it is small.

Content: king-and-few-defenders escapes, attacker conversions with a material
edge, exit-fort constructions (`src/Tafl/Game/Fort.hs` already validates them),
and shield-wall finishes.

### Openings

The weakest of the six as originally scoped, for a reason worth stating: the app
supports nine variants, and opening knowledge does not transfer across board
sizes and corner rules. Written tafl opening theory effectively covers
Copenhagen 11x11, with a little for Tablut and Brandubh. **Scope the track to
those three and skip the rest** rather than shipping thin content across nine
boards.

Two versions, and the second is the one worth building:

1. *Static lines.* Named openings as a chain of `MoveStep`s with scripted
   replies. Cheap, and mostly a content-writing exercise.
2. *Explorer from the corpus.* First N plies of every stored game, aggregated by
   position: how often each continuation was played and how each side scored.
   Since games are world-readable this is a plain aggregate — a view or an RPC
   over `moves`, keyed by the `Tafl.Symmetry` canonical hash so transpositions
   and mirrored lines collapse into one node. That hashing already exists for
   repetition draws and is exactly the right key here.

The explorer is more useful *and* self-maintaining, but it needs a real game
corpus to be interesting, which is why this track goes last.

### Library — curated readings

The cheapest item: a route and a data module of links with, for each, a
one-line note on why it is worth reading and which part of the game it covers.
Curation, not a bookmark dump.

Two rules: every link gets fetched and checked before it ships, and entries
carry the track they support (`tactics`, `endgames`, `openings`) so a puzzle
screen can point at the reading that explains the theme.

## Build order

1. **Shared plumbing** — `Track`, routing, `learning_progress`, the `/learn` hub
   shell. Nothing user-visible ships until step 2, but everything after depends
   on it.
2. **Review** — highest payoff, most infrastructure already present, and it
   produces `Tafl.Review`, which Tactics then reuses.
3. **Tactics** — puzzle runner over `ChallengeStep`, hand-authored seed bank,
   then mining from the corpus using the pass built in step 2.
4. **Endgames** — same runner plus the move-budget goal types and a live AI
   opponent.
5. **Library** — independent of everything; can be slotted in at any point.
6. **Openings** — last, once the corpus is worth aggregating.

## Open questions

- **Puzzle rating.** Reuse Glicko-2 for puzzles, or start with fixed difficulty
  tiers and add rating once the bank is large enough for it to mean anything?
- **Review for multiplayer games.** Nothing in `Tafl.Review` is AI-specific, so
  it works on multiplayer games too. Worth exposing immediately, or held back
  until the grading is trusted?
- **Analysis budget on mobile.** Depth 3–4 across ten positions is fine on a
  laptop. Needs measuring on a phone before the confirm pass depth is fixed.
- **Advanced content.** With `reading-eval` and `ratings` gone, Advanced is a
  single lesson. Does it get new lessons, or does it fold into Tactics and
  Endgames once those exist?
