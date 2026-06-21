#!/usr/bin/env bash
set -euo pipefail

NIX="nix --extra-experimental-features 'nix-command flakes'"

echo "==> Building..."
eval "$NIX develop .#wasm --command make"

echo "==> Serving on http://localhost:8080"
eval "$NIX develop .#wasm --command npx http-server public -p 8080 -c-1 --proxy http://localhost:8080?"
