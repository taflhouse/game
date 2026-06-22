# Environment: source .env.local, .env.staging, or .envrc before running
ENV ?= local

.PHONY: update build optim serve clean db-start db-stop db-reset

all: clean update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build
	rm -rf public
	cp -r static public
	sed -i'' -e "s|__SUPABASE_URL__|$${SUPABASE_URL}|g" public/index.js
	sed -i'' -e "s|__SUPABASE_KEY__|$${SUPABASE_KEY}|g" public/index.js
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

# --- Local Supabase ---

db-start:
	npx supabase start

db-stop:
	npx supabase stop

db-reset:
	npx supabase db reset

# --- Staging ---

staging-push:
	npx supabase db push --linked

clean:
	rm -rf dist-newstyle public
