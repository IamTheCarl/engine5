# Agent Instructions — Engine 5

## Setup

- Dev environment: `direnv` + Nix flakes (`.envrc` runs `use flake`). Run `direnv allow` or enter via `nix develop`.
- Bevy is a **forked submodule** at `crates/bevy` (`git@github.com:IamTheCarl/bevy.git`). Run `git submodule update --init --recursive` after cloning.
- PNG assets use Git LFS (`*.png filter=lfs` in `.gitattributes`).
- `cargo run` to launch. `publish.sh` builds a release binary for Linux (`--release --no-default-features`).
- Feature `fast-compile` (default) enables Bevy dynamic linking. Disable with `--no-default-features`.

## Commands

| Command | Notes |
|---|---|
| `cargo run` | Launch game |
| `cargo run --no-default-features` | Static Bevy linking |
| `ENGINE5_STARTUP_SCRIPT=assets/scripts/foo.e5s cargo run` | Run a console script at startup |
| `test_scripts/lone_host.sh` | Single-instance multiplayer test |
| `test_scripts/multiplayer.sh` | Two-instance client+server test |
| `cargo build` / `cargo clippy` / `cargo fmt` | Standard Rust tooling |

No test framework is configured. The only "tests" are the shell scripts above.

## Architecture

- **Entry point**: `src/main.rs` — creates `App`, adds `DefaultPlugins` (with custom asset watching and exit behavior), `ConsolePlugin`, `ConsoleScripts`, then `Engine5` plugin.
- **`Engine5` plugin** (main.rs:61) — sets up logging, config dirs, states, then adds: `WorldPlugin`, `DebugLinesPlugin`, `DefaultNavigationPlugins`, `UserInterface`, `ConfigPlugin`, `MultiplayerPlugin`.
- **States**: `GameState` (MainMenu → InGame → FatalError → ShuttingDown).
- **World persistence**: `sled` key-value database per world. Entities are serialized via `SpatialEntity` trait implemented by `#[entity_serialization]` proc macro.

### Module layout

| Module | Purpose |
|---|---|
| `src/world/` | World management, terrain, physics, spatial entity storage |
| `src/world/terrain/` | Block/chunk system, terrain generation (OscillatingHills) |
| `src/world/spatial_entities/` | Entity storage (sled-backed), serialization, entity types (player, global terrain) |
| `src/multiplayer/` | `renet`-based networking; host and client contexts |
| `src/ui/` | Bevy UI with navigation; main menu, pause menu, settings, fatal error display |
| `src/config/` | YAML config files (controls, graphics, player info); auto-save on change |
| `src/console_scripts.rs` | `.e5s` script files — line-by-line console commands parsed via `shlex` |

### Proc macros (`crates/proc_macros`)

- `#[entity_serialization(type_id = N, marker = Foo)]` — generates `SpatialEntity` trait impl for entity archetypes. Supports `bootstrap`, `#[get = ...]`, and `#[query = ...]` field attributes.
- `#[ref_serializable]` — derives `Serialize`/`Deserialize` plus a `FooRef` variant with borrowed fields. Supports `#[owned_box]` and `#[borrowed_ref]` field attributes for zero-copy serialization of large data.

## Gotchas

- **No `LogPlugin`** — logging is custom (`src/logging.rs`) with a `tracing_subscriber` pipeline that also pipes to the in-game console.
- **`dbg!()` calls remain in production code** (main.rs, logging.rs, world/mod.rs). Do not remove them without asking.
- **Config files** live in a project-specific config directory (created by `config::file_paths::create_folders()`). They auto-save when changed (`config.is_changed()`).
- **`world/` and `config/` directories are gitignored** — they contain runtime data.
- **`src/logging.rs:43-45`** — the author left a note about tracing_subscriber difficulty; the `ConsoleSubscriber` layer is hand-written and not a complete implementation.
- **Multiplayer channels**: entity updates are unreliable; terrain updates are reliable-ordered with 300ms resend.
- **`exit_condition: DontExit`** — Bevy's default window-close exit is disabled; shutdown is handled via `GameState::ShuttingDown` → `AppExit`.
