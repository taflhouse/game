#!/usr/bin/env bash
set -euo pipefail

SESSION="taflhouse"

# Kill existing session if any
tmux kill-session -t "$SESSION" 2>/dev/null || true

# Source local environment
if [ -f .env.local ]; then
  set -a; source .env.local; set +a
fi

# Ensure Docker is running (needed for local Supabase)
if ! docker info &>/dev/null; then
  echo "Error: Docker is not running. Start it with: colima start"
  exit 1
fi

# Start local Supabase if not already running
if ! npx supabase status &>/dev/null; then
  echo "==> Starting local Supabase..."
  npx supabase start
else
  echo "==> Local Supabase already running"
fi

# Initial build
echo "==> Building frontend..."
nix --extra-experimental-features 'nix-command flakes' develop .#wasm --command make

DIR="$(cd "$(dirname "$0")" && pwd)"

# Create tmux session — frontend server (top pane)
tmux new-session -d -s "$SESSION" -c "$DIR" \
  "bash -c 'echo \"==> Frontend: http://localhost:8080\" && \
   nix --extra-experimental-features \"nix-command flakes\" develop .#wasm --command \
   npx http-server public -p 8080 -c-1 --proxy \"http://localhost:8080?\"; exec bash'"

# Watch & rebuild on source changes (middle pane)
tmux split-window -t "$SESSION" -v -c "$DIR" \
  "bash -c 'source .env.local 2>/dev/null; \
   echo \"==> Watching for changes (app/ src/ static/)...\" && \
   fswatch -o -l 2 -e dist-newstyle -e public -e \"\\.o$\" -e \"\\.hi$\" --include \"\\.hs$\" --include \"\\.js$\" --include \"\\.html$\" --include \"\\.css$\" app/ src/ static/ | while read _; do \
     echo \"==> Rebuilding...\"; \
     nix --extra-experimental-features \"nix-command flakes\" develop .#wasm --command make \
       && echo \"==> Build complete\" || echo \"==> Build failed\"; \
   done; exec bash'"

# WebSocket server (bottom pane)
tmux split-window -t "$SESSION" -v -c "$DIR" \
  "bash -c 'source .env.local 2>/dev/null; \
   echo \"==> WS server: localhost:\${PORT:-3000}\" && \
   make serve-server; exec bash'"

# Even out pane heights
tmux select-layout -t "$SESSION" even-vertical

# Focus the top pane
tmux select-pane -t "$SESSION":0.0

echo ""
echo "==> All services running in tmux session '$SESSION'"
echo "    Frontend:  http://localhost:8080"
echo "    WS Server: localhost:${PORT:-3000}"
echo "    Supabase:  http://localhost:54323 (Studio)"
echo "    Watch:     auto-rebuilds on app/ src/ static/ changes"
echo ""

# If already in tmux, switch to the session; otherwise attach
if [ -n "${TMUX:-}" ]; then
  tmux switch-client -t "$SESSION"
else
  tmux attach -t "$SESSION"
fi
