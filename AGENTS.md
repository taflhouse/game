# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Dev

Requires Nix with flakes enabled. All WASM build commands run inside `nix develop .#wasm`.

```bash
# Full build (inside nix wasm shell)
nix develop .#wasm --command make        # build, copy static assets, substitute env vars
nix develop .#wasm --command make optim  # wasm-opt + wasm-tools strip

# Dev workflow (starts Supabase, builds, watches for changes)
./run.sh

# Or manually:
make db-start          # start local Supabase (needs Docker/colima)
make db-reset          # apply migrations + seed
make build             # compile WASM, assemble public/
make serve             # http-server public on :8080
```

Environment variables `SUPABASE_URL` and `SUPABASE_KEY` must be set before `make build` — they get substituted into `public/index.js` via sed. Local values are in `.envrc`.

```bash
# Run tests (uses cabal.project.test to avoid WASM-only dependencies)
cabal test --project-file=cabal.project.test
```

## Architecture

Haskell compiled to WASM via GHC WASM backend, running entirely in the browser. No backend server — Supabase handles auth, database, and realtime.

### Layers

**`src/Tafl/`** — Pure game engine library. No IO, no FFI. Core function is `act :: GameState -> MoveAction -> GameState`.

- `Types.hs` — `Piece`, `Side`, `Coords`, `MoveAction`, `Board` (= `Vector (Vector Piece)`), `GameState`, `RuleSet`, `GameResult`
- `Game.hs` — `initialState`, `act`, `isGameOver` (wires together all modules)
- `Move.hs` — Ray-walk movement generation, `getPossibleActions`, `isActionPossible`
- `Capture.hs` — Sandwich captures, shield wall detection
- `Fort.hs` — Exit fort validation via DFS/BFS
- `Surround.hs` — Defender surrounding detection via flood-fill
- `Symmetry.hs` — D4 board hashing (8 transforms → canonical key) for repetition draws
- `AI.hs` — Minimax with alpha-beta pruning, iterative deepening, node budget. `bestMove :: AiConfig -> GameState -> Maybe MoveAction`
- `Board.hs` — 9 predefined board layouts
- `Rules.hs` — `BoardVariant` enum, `RuleSet` defaults per variant

**`app/Main.hs`** — Miso SPA (single ~1500-line file). Model/View/Update architecture.

- `Model` has ~40 fields covering game state, auth, multiplayer, UI state
- Manual `Eq` instance (skips `JSVal` fields like `Channel`)
- Uses `Miso.JSON` for JSON (not `Data.Aeson`) — the library's types mirror Aeson but are distinct
- Orphan `ToJSON`/`FromJSON` instances for `Coords`, `MoveAction`, `Side`, `GameResult` using `Miso.JSON` are defined at the top of Main.hs (the library uses `Data.Aeson` instances in `Types.hs`)

**`static/index.js`** — JS interop layer. This is the source file; `public/index.js` is generated during build with env var substitution.

- Initializes `@supabase/supabase-js` client (loaded from CDN)
- Exposes `globalThis` functions called from Haskell via FFI: `runSupabase`, `runSupabaseSelect`, `runSupabaseUpdate`, `subscribePostgresChanges`, `removeChannel`, `getSupabaseSession`, `generateUUID`, `playMoveSound`, etc.
- Loads WASM via `@bjorn3/browser_wasi_shim` and calls `hs_start`

### Supabase Integration

All Supabase calls go through the `supabase-miso` library (at `github.com/heath/supabase-miso`), which provides typed Haskell wrappers around the JS functions in `static/index.js`.

- **Auth**: `Supabase.Miso.Auth` — anonymous sign-in, email/password, session management
- **Database**: `Supabase.Miso.Database` — `insert`, `selectWithFilters`, `updateTable` with filter builders (`eq`, `neq`)
- **Realtime**: `Supabase.Miso.Realtime` — `subscribeToTable` for Postgres Changes, `removeChannel`

### Multiplayer Flow

Uses Supabase Realtime (Postgres Changes), not WebSockets. Both players read/write game state directly in the `games` table.

1. Creator inserts a `games` row with status='waiting' and subscribes to Realtime on that row
2. Joiner finds the game by invite code, updates it to status='active', and subscribes
3. Moves are applied optimistically in the local model, then written to DB via `updateTable`
4. Realtime delivers the opponent's moves; echo suppression compares move list lengths
5. Game state is reconstructed from the move list using `foldl act initialState moves`

### Dependencies (via `source-repository-package` in `cabal.project`)

- `miso` — Haskell frontend framework (Elm-like MVU)
- `supabase-miso` — Supabase Haskell bindings (auth, database, realtime)
- `miso-ui` — UI component library

For local development, create `cabal.project.local` (gitignored) to override with filesystem paths.

## Deployment

GitHub Actions on push to `main` (production) or `staging` (staging):
1. Supabase CLI pushes migrations via transaction pooler (`--db-url`)
2. Nix builds WASM app
3. Wrangler deploys to Cloudflare Workers

Secrets: `PROD_DATABASE_URL`, `STAGING_DATABASE_URL`, `PROD_SUPABASE_URL`, `STAGING_SUPABASE_URL`, `PROD_SUPABASE_KEY`, `STAGING_SUPABASE_KEY`, `CLOUDFLARE_API_TOKEN`, `CLOUDFLARE_ACCOUNT_ID`
