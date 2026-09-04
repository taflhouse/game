#!/usr/bin/env bash
set -euo pipefail

SESSION="taflhouse"
NIX="nix --extra-experimental-features"
NIXFLAGS="nix-command flakes"
# The wasm devShell points DEVELOPER_DIR at a nix-store Apple SDK, which breaks
# /usr/bin/git's internal xcrun lookup ("error: tool 'git' not found").
# Re-overriding it after --command restores the real Xcode CLT.
DEV="env DEVELOPER_DIR=/Library/Developer/CommandLineTools"

FULL=0
for arg in "$@"; do
  case "$arg" in
    --full)  FULL=1 ;;
    -h|--help)
      echo "usage: ./run.sh [--full]"
      echo "  --full   clean rebuild plus wasm-opt (slow; matches a deploy build)"
      exit 0 ;;
    *) echo "unknown option: $arg (try --help)" >&2; exit 1 ;;
  esac
done

step() { echo; echo "==> $*"; }

# --- Environment ------------------------------------------------------------
# These get substituted into public/index.js by `make build`. Unset means the
# app silently ships with an empty Supabase URL and every call fails, so check
# rather than let it through.
step "Loading environment"
if [ -f .env.local ]; then
  set -a; source .env.local; set +a
  echo "    from .env.local"
elif [ -f .envrc ]; then
  set -a; source .envrc; set +a
  echo "    from .envrc"
else
  echo "    no .env.local or .envrc found" >&2
fi

missing=()
for var in SUPABASE_URL SUPABASE_KEY; do
  [ -n "${!var:-}" ] || missing+=("$var")
done
if [ ${#missing[@]} -gt 0 ]; then
  echo "Error: ${missing[*]} unset. The build would bake empty values into" >&2
  echo "public/index.js and every Supabase call would fail." >&2
  exit 1
fi
echo "    SUPABASE_URL=${SUPABASE_URL}"

# --- Supabase ---------------------------------------------------------------
step "Checking Docker"
if ! docker info &>/dev/null; then
  echo "Error: Docker is not running. Start it with: colima start" >&2
  exit 1
fi
echo "    running"

step "Checking local Supabase"
# Ask Docker, not the CLI. There's no package.json here, so `npx supabase
# status` re-downloads the CLI from npm on every run - and with its output
# swallowed, npx's "Ok to proceed?" prompt hangs invisibly.
if docker ps --format '{{.Names}}' | grep -q '^supabase_db_'; then
  echo "    already running"
else
  echo "    not running, starting it (slow on a cold start)"
  # --yes so npx never stops to ask; output left visible on purpose.
  npx --yes supabase start
fi

# --- Build ------------------------------------------------------------------
# Default is `make build`: compile, assemble public/, substitute env vars.
# Plain `make` is `clean update build optim`, which wipes dist-newstyle and runs
# wasm-opt over the whole module - minutes, and pointless for a dev loop.
if [ "$FULL" -eq 1 ]; then
  step "Full rebuild (clean + wasm-opt)"
  TARGET=""
else
  step "Building frontend (incremental; use --full for a clean optimised build)"
  TARGET="build"
fi
$NIX "$NIXFLAGS" develop .#wasm --command $DEV make $TARGET

DIR="$(cd "$(dirname "$0")" && pwd)"

# --- tmux -------------------------------------------------------------------
step "Starting tmux session '$SESSION'"
tmux kill-session -t "$SESSION" 2>/dev/null || true

# Pane 0: the static server.
tmux new-session -d -s "$SESSION" -c "$DIR" \
  "bash -c 'echo \"==> Frontend: http://localhost:8080  (Ctrl-C to stop)\" && \
   $NIX \"$NIXFLAGS\" develop .#wasm --command \
   http-server public -p 8080 -c-1; exec bash'"

# Pane 1: a shell already inside the wasm devShell with the env loaded, so
# rebuilding after an edit is just `make build` rather than the full
# nix develop incantation. fswatch isn't installed, so this is manual.
tmux split-window -t "$SESSION" -v -c "$DIR" \
  "bash -c 'set -a; source .env.local 2>/dev/null || source .envrc; set +a; \
   echo \"==> Dev shell. After editing app/ src/ static/, run:  make build\"; \
   echo \"    then hard-reload the browser (Cmd-Shift-R).\"; \
   $NIX \"$NIXFLAGS\" develop .#wasm --command $DEV bash'"

tmux select-layout -t "$SESSION" even-vertical
tmux select-pane -t "$SESSION":0.1

cat <<EOF

==> Running in tmux session '$SESSION'
    Frontend:  http://localhost:8080
    Supabase:  http://localhost:54323 (Studio)
    Mail:      http://localhost:54324 (Mailpit)

    Top pane    static server
    Bottom pane wasm dev shell - run 'make build' here after edits

    tmux: switch panes Ctrl-b o | detach Ctrl-b d | reattach: tmux attach -t $SESSION

EOF

if [ -n "${TMUX:-}" ]; then
  tmux switch-client -t "$SESSION"
else
  tmux attach -t "$SESSION"
fi
