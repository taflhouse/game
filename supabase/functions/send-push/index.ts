import "@supabase/functions-js/edge-runtime.d.ts";
import { createClient } from "@supabase/supabase-js";

const supabaseUrl = Deno.env.get("SUPABASE_URL")!;
const serviceRoleKey = Deno.env.get("SUPABASE_SERVICE_ROLE_KEY")!;
const vapidPublicKey = Deno.env.get("VAPID_PUBLIC_KEY")!;
const vapidPrivateKey = Deno.env.get("VAPID_PRIVATE_KEY")!;
const vapidSubject =
  Deno.env.get("VAPID_SUBJECT") || "mailto:hello@taflhouse.com";

// ---------------------------------------------------------------------------
// Base64url helpers
// ---------------------------------------------------------------------------

function b64url(buf: ArrayBuffer | Uint8Array): string {
  const bytes = buf instanceof Uint8Array ? buf : new Uint8Array(buf);
  let s = "";
  for (const b of bytes) s += String.fromCharCode(b);
  return btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function unb64url(s: string): Uint8Array {
  s = s.replace(/-/g, "+").replace(/_/g, "/");
  s += "=".repeat((4 - (s.length % 4)) % 4);
  const bin = atob(s);
  const arr = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
  return arr;
}

// ---------------------------------------------------------------------------
// VAPID JWT (ES256)
// ---------------------------------------------------------------------------

async function vapidAuthHeader(endpoint: string): Promise<string> {
  const pub = unb64url(vapidPublicKey); // 65 bytes: 0x04 || x || y

  const key = await crypto.subtle.importKey(
    "jwk",
    {
      kty: "EC",
      crv: "P-256",
      x: b64url(pub.slice(1, 33)),
      y: b64url(pub.slice(33, 65)),
      d: vapidPrivateKey,
    },
    { name: "ECDSA", namedCurve: "P-256" },
    false,
    ["sign"],
  );

  const header = b64url(
    new TextEncoder().encode(JSON.stringify({ typ: "JWT", alg: "ES256" })),
  );
  const payload = b64url(
    new TextEncoder().encode(
      JSON.stringify({
        aud: new URL(endpoint).origin,
        exp: Math.floor(Date.now() / 1000) + 12 * 3600,
        sub: vapidSubject,
      }),
    ),
  );

  const unsigned = `${header}.${payload}`;
  // Web Crypto ECDSA returns raw r||s (64 bytes for P-256)
  const sig = await crypto.subtle.sign(
    { name: "ECDSA", hash: "SHA-256" },
    key,
    new TextEncoder().encode(unsigned),
  );

  return `vapid t=${unsigned}.${b64url(sig)}, k=${vapidPublicKey}`;
}

// ---------------------------------------------------------------------------
// Web Push payload encryption (RFC 8291, aes128gcm)
// ---------------------------------------------------------------------------

async function encrypt(
  plaintext: Uint8Array,
  subscriberPub: Uint8Array,
  authSecret: Uint8Array,
): Promise<Uint8Array> {
  const subKey = await crypto.subtle.importKey(
    "raw",
    subscriberPub,
    { name: "ECDH", namedCurve: "P-256" },
    false,
    [],
  );

  const eph = await crypto.subtle.generateKey(
    { name: "ECDH", namedCurve: "P-256" },
    true,
    ["deriveBits"],
  );

  const sharedBits = await crypto.subtle.deriveBits(
    { name: "ECDH", public: subKey },
    eph.privateKey,
    256,
  );

  const ephPub = new Uint8Array(
    await crypto.subtle.exportKey("raw", eph.publicKey),
  );

  // IKM via HKDF: salt=authSecret, ikm=sharedSecret,
  //   info = "WebPush: info\0" || subscriber_pub(65) || ephemeral_pub(65)
  const ikmInfo = new Uint8Array(
    new TextEncoder().encode("WebPush: info\0").length + 65 + 65,
  );
  const prefix = new TextEncoder().encode("WebPush: info\0");
  ikmInfo.set(prefix);
  ikmInfo.set(subscriberPub, prefix.length);
  ikmInfo.set(ephPub, prefix.length + 65);

  const ikmKey = await crypto.subtle.importKey(
    "raw",
    sharedBits,
    { name: "HKDF" },
    false,
    ["deriveBits"],
  );
  const prk = await crypto.subtle.deriveBits(
    { name: "HKDF", hash: "SHA-256", salt: authSecret, info: ikmInfo },
    ikmKey,
    256,
  );

  const salt = crypto.getRandomValues(new Uint8Array(16));
  const prkKey = await crypto.subtle.importKey(
    "raw",
    prk,
    { name: "HKDF" },
    false,
    ["deriveBits"],
  );

  const cekBits = await crypto.subtle.deriveBits(
    {
      name: "HKDF",
      hash: "SHA-256",
      salt,
      info: new TextEncoder().encode("Content-Encoding: aes128gcm\0"),
    },
    prkKey,
    128,
  );
  const nonce = new Uint8Array(
    await crypto.subtle.deriveBits(
      {
        name: "HKDF",
        hash: "SHA-256",
        salt,
        info: new TextEncoder().encode("Content-Encoding: nonce\0"),
      },
      prkKey,
      96,
    ),
  );

  // Pad: plaintext || 0x02 (final record delimiter)
  const padded = new Uint8Array(plaintext.length + 1);
  padded.set(plaintext);
  padded[plaintext.length] = 2;

  const cek = await crypto.subtle.importKey(
    "raw",
    cekBits,
    { name: "AES-GCM" },
    false,
    ["encrypt"],
  );
  const ciphertext = new Uint8Array(
    await crypto.subtle.encrypt(
      { name: "AES-GCM", iv: nonce, tagLength: 128 },
      cek,
      padded,
    ),
  );

  // aes128gcm header: salt(16) || rs(4, big-endian) || idlen(1) || keyid(65)
  const header = new Uint8Array(86);
  header.set(salt);
  new DataView(header.buffer).setUint32(16, 4096, false);
  header[20] = 65;
  header.set(ephPub, 21);

  const body = new Uint8Array(header.length + ciphertext.length);
  body.set(header);
  body.set(ciphertext, header.length);
  return body;
}

// ---------------------------------------------------------------------------
// Send a single push notification
// ---------------------------------------------------------------------------

async function sendPush(
  sub: { endpoint: string; keys: { p256dh: string; auth: string } },
  payload: string,
): Promise<Response> {
  const body = await encrypt(
    new TextEncoder().encode(payload),
    unb64url(sub.keys.p256dh),
    unb64url(sub.keys.auth),
  );

  return fetch(sub.endpoint, {
    method: "POST",
    headers: {
      Authorization: await vapidAuthHeader(sub.endpoint),
      "Content-Encoding": "aes128gcm",
      "Content-Type": "application/octet-stream",
      TTL: "86400",
      Urgency: "normal",
    },
    body,
  });
}

// ---------------------------------------------------------------------------
// Edge Function handler
// ---------------------------------------------------------------------------

Deno.serve(async (req) => {
  const authHeader = req.headers.get("Authorization");
  if (authHeader !== `Bearer ${serviceRoleKey}`) {
    return new Response("Unauthorized", { status: 401 });
  }

  const { user_id, game_id, mover_name, variant } = await req.json();
  if (!user_id || !game_id) {
    return new Response("Missing user_id or game_id", { status: 400 });
  }

  const supabase = createClient(supabaseUrl, serviceRoleKey);

  const { data: subs, error } = await supabase
    .from("push_subscriptions")
    .select("id, subscription_json")
    .eq("user_id", user_id);

  if (error || !subs || subs.length === 0) {
    return Response.json({ sent: 0, total: 0 });
  }

  const payload = JSON.stringify({
    title: `${mover_name} made a move`,
    body: `It's your turn in ${variant}!`,
    game_id,
  });

  let sent = 0;
  const staleIds: string[] = [];
  const errors: { endpoint: string; status?: number; body?: string; error?: string }[] = [];

  for (const sub of subs) {
    try {
      const res = await sendPush(sub.subscription_json, payload);
      if (res.ok || res.status === 201) {
        sent++;
      } else if (res.status === 410 || res.status === 404) {
        staleIds.push(sub.id);
      } else {
        const body = await res.text().catch(() => "");
        const ep = sub.subscription_json?.endpoint || "unknown";
        console.error(`Push failed: status=${res.status} endpoint=${ep} body=${body}`);
        errors.push({ endpoint: ep, status: res.status, body });
      }
    } catch (e) {
      const ep = sub.subscription_json?.endpoint || "unknown";
      const msg = e instanceof Error ? e.message : String(e);
      console.error(`Push exception: endpoint=${ep} error=${msg}`);
      errors.push({ endpoint: ep, error: msg });
    }
  }

  if (staleIds.length > 0) {
    await supabase
      .from("push_subscriptions")
      .delete()
      .in("id", staleIds);
  }

  return Response.json({ sent, total: subs.length, errors });
});
