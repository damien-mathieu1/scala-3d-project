# Scala 3D Apps

A Scala.js + Three.js project featuring a 3D **Blackjack** game and an image viewer.
The Blackjack engine is written in a functional style: immutable `GameState`, the
`cats` `State` monad for game actions, a small `IO` monad, and typeclasses (`Show`, `Eq`).

## Features

The Blackjack game implements a full ruleset:

- Hit, Stand, Double Down
- **Split** (pairs of equal value, resplit up to 4 hands, double-after-split)
- **Insurance** (offered when the dealer shows an Ace, pays 2:1)
- **Surrender** (forfeit a fresh hand for half the bet)
- Blackjack pays 3:2, dealer stands on 17, interactive chip betting

## Prerequisites

- **JDK** 11+ (21 is fine)
- **sbt** 1.x
- **Node.js** + npm (used by `scalajs-bundler` to bundle Three.js)

## Build

The project is compiled to JavaScript and bundled with its npm dependencies via
webpack. Build the bundle with:

```bash
NODE_OPTIONS=--openssl-legacy-provider sbt fastOptJS::webpack
```

> ⚠️ The `NODE_OPTIONS=--openssl-legacy-provider` flag is **required** on modern
> Node. Without it the webpack 4 build fails with
> `error:0308010C:digital envelope routines::unsupported`.

This produces:

```
target/scala-3.7.0/scalajs-bundler/main/image3d-fastopt-bundle.js
```

`index.html` loads `./image3d-fastopt-bundle.js` from the repo root, so copy the
freshly built bundle there:

```bash
cp target/scala-3.7.0/scalajs-bundler/main/image3d-fastopt-bundle.js .
```

## Run

Serve the project root over HTTP and open it in a browser:

```bash
npx serve -l 8080
```

Then open <http://localhost:8080>.

Opening `index.html` directly via `file://` may also work since the app is a single
bundle, but a local HTTP server is recommended.

## One-liner (build + copy + serve)

```bash
NODE_OPTIONS=--openssl-legacy-provider sbt fastOptJS::webpack \
  && cp target/scala-3.7.0/scalajs-bundler/main/image3d-fastopt-bundle.js . \
  && npx serve -l 8080
```

## Compile only (type-check, no bundle)

```bash
sbt compile
```

## Project layout

```
src/main/scala/
  Main.scala              # entry point, scene + mode switching
  model/                  # Card, Hand, GameState (ADTs, scoring, split rules)
  rules/Blackjack.scala   # game actions via the cats State monad
  effects/                # IO monad, deck shuffling
  modes/                  # Blackjack + image viewer UI (Three.js boundary)
  render/                 # 3D meshes, textures, animation
index.html                # markup, styles, button controls
build.sbt                 # Scala.js + bundler config
```
