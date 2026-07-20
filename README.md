# Taflhouse

A browser-based [Hnefatafl](https://en.wikipedia.org/wiki/Tafl_games) game built with [Haskell](https://www.haskell.org/) and the [Miso](https://github.com/dmjio/miso) framework; compiles to WebAssembly and runs entirely in the browser.

## Features

- **Game engine** — pure Haskell implementation of Hnefatafl rules including movement, captures, shield walls, exit forts, surrounding, and draw detection across 5 board variants.
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
| Parlett | 13×13 |
| Damien Walker | 15×15 |

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

## Push Notifications

Multiplayer games use Web Push to notify players when their opponent moves, even if the browser tab is closed. The flow:

1. A move is written to the `games` table.
2. A Postgres trigger (`notify_push_on_move`) fires and calls a Supabase Edge Function via `pg_net`.
3. The Edge Function reads the opponent's push subscriptions and sends encrypted Web Push messages using VAPID.
4. The browser's Service Worker (`sw.js`) receives the push event and shows a notification (unless the game tab is already focused).

### VAPID Keys

Web Push requires a VAPID key pair to identify the application to push services (Google, Mozilla, Apple). Generate one once and use it across all environments:

```bash
npx web-push generate-vapid-keys
```

This outputs a public key and a private key. Keep the private key secret.

### Local Setup

1. **`.envrc`** — set the public key so it gets baked into the JS bundle at build time:

   ```
   export VAPID_PUBLIC_KEY="<your public key>"
   ```

2. **`supabase/.env.local`** — set secrets for the Edge Function runtime:

   ```
   VAPID_PUBLIC_KEY=<your public key>
   VAPID_PRIVATE_KEY=<your private key>
   VAPID_SUBJECT=mailto:hello@taflhouse.com
   ```

3. **Vault secrets** — the Postgres trigger reads the Edge Function URL and service role key from Vault. After `make db-reset`, run in the SQL Editor (`http://localhost:54323`):

   ```sql
   SELECT vault.create_secret(
     'http://localhost:54321/functions/v1/send-push',
     'push_edge_fn_url'
   );
   SELECT vault.create_secret(
     '<local service_role key from supabase status>',
     'service_role_key'
   );
   ```

4. **Start Supabase** — `make db-start` serves the Edge Function automatically (no separate `supabase functions serve` needed). Rebuild the app with `make` so the VAPID key gets substituted into `public/index.js`.

### Production / Staging Setup

**GitHub Actions secrets** (set in repo settings):

| Secret | Description |
|--------|-------------|
| `PROD_VAPID_PUBLIC_KEY` | VAPID public key |
| `STAGING_VAPID_PUBLIC_KEY` | VAPID public key |
| `SUPABASE_ACCESS_TOKEN` | Personal access token from [supabase.com/dashboard/account/tokens](https://supabase.com/dashboard/account/tokens), used by `supabase functions deploy` |
| `PROD_PROJECT_REF` | Production project ref (the subdomain of `supabase.co`) |
| `STAGING_PROJECT_REF` | Staging project ref |

**Supabase Edge Function secrets** (per environment):

```bash
supabase secrets set \
  VAPID_PUBLIC_KEY="<public key>" \
  VAPID_PRIVATE_KEY="<private key>" \
  VAPID_SUBJECT="mailto:hello@taflhouse.com" \
  --project-ref <project-ref>
```

**Vault secrets** (run against each environment's database):

```sql
SELECT vault.create_secret(
  'https://<project-ref>.supabase.co/functions/v1/send-push',
  'push_edge_fn_url'
);
SELECT vault.create_secret(
  '<service_role_key>',
  'service_role_key'
);
```

The service role key is in the Supabase dashboard under Settings > API.

## Deployment

Pushes to `main` deploy to production; pushes to `staging` deploy to staging. Both go through GitHub Actions which runs Supabase migrations, deploys Edge Functions, and deploys to Cloudflare Workers.

## License

BSD3
