# Taflhouse

A browser-based [Hnefatafl](https://en.wikipedia.org/wiki/Tafl_games) (Viking Chess) game built with [Haskell](https://www.haskell.org/) and the [Miso](https://github.com/dmjio/miso) framework. Compiles to WebAssembly and runs entirely in the browser.

## Features

- **Game engine** — pure Haskell implementation of Hnefatafl rules including movement, captures, shield walls, exit forts, surrounding, and draw detection across 9 historical board variants.
- **AI opponent** — minimax search with alpha-beta pruning, configurable difficulty levels.
- **Online multiplayer** — real-time games via Supabase Realtime. Game state persists in the database; page refreshes and reconnections work naturally.
- **Accounts** — anonymous or email-based auth via Supabase. Player profiles with display names.
- **UI** — interactive SVG board with sound effects, move history, and SPA routing.

## Board Variants

| Variant | Size |
|---|---|
| Brandubh | 7×7 |
| Tablut | 9×9 |
| Copenhagen | 11×11 |
| Line | 11×11 |
| Tawlbwrdd | 11×11 |
| Lewis | 11×11 |
| Parlett | 13×13 |
| Damien Walker | 15×15 |
| Alea Evangelii | 19×19 |

## Prerequisites

```bash
# Install Nix
curl -L https://nixos.org/nix/install | sh

# Enable flakes
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

## Local Development

```bash
# Start local Supabase
make db-start
make db-reset

# Build and serve
nix develop .#wasm --command make
make serve
```

Open http://localhost:8080.

## Deployment

Pushes to `main` deploy to production; pushes to `staging` deploy to staging. Both go through GitHub Actions which runs Supabase migrations and deploys to Cloudflare Workers.

## License

BSD3
