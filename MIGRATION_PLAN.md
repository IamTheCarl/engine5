# Migration Plan: Bevy 0.11 → 0.18.1

## Overview

A **7.5 major version jump** (0.11→0.18.1) affecting ~30+ source files. The migration goes through intermediate versions: 0.11 → 0.12 → 0.13 → 0.14 → 0.15 → 0.16 → 0.17 → 0.18.

## Current State

| Dependency | Current | Target |
|---|---|---|
| **Bevy** | 0.11 (forked at `crates/bevy`) | 0.18.1 (crates.io) |
| `bevy_prototype_debug_lines` | 0.11.1 | `bevy_gizmos` 0.18 |
| `bevy-ui-navigation` | 0.31.0 | 0.17.0 |
| `bevy_console` | 0.8.0 | 0.17.1 |
| `renet` | 0.0.13 | 1.2.0 |
| `renet_visualizer` | 0.0.6 | **DROP** (no 0.18 version) |

---

## Phase 1: Dependencies

### 1.1 Cargo.toml Changes

**Remove the patch section** entirely (drop Bevy fork):
```toml
[patch.crates-io]
# bevy = { path = "crates/bevy" }  ← REMOVE
```

**Update version pins**:
- `bevy` → `0.18` with `serialize` feature
- `bevy_prototype_debug_lines` → replace with `bevy_gizmos = "0.18"`
- `bevy-ui-navigation` → `0.17.0`
- `bevy_console` → `0.17.1`
- `renet` → `1.2.0` (check API changes)
- `renet_visualizer` → remove

### 1.2 Debug Lines → Gizmos

`bevy_gizmos` provides `Gizmos::line(start, end, color)` as a direct replacement. Update imports and usage in:
- `src/world/physics/terrain_to_terrain.rs`
- `src/world/physics/ray_cast_with_terrain.rs`
- `src/world/physics/cylinder_to_cylinder.rs`
- `src/world/physics/cylinder_to_terrain.rs`
- `src/world/spatial_entities/mod.rs`
- `src/main.rs`

---

## Phase 2: Import Path Changes

| Old | New | Files |
|---|---|---|
| `bevy::utils::synccell::SyncCell` | `bevy::platform::cell::SyncCell` | `logging.rs:9`, `ui/mod.rs:2` |
| `bevy::utils::HashSet` | `std::collections::HashSet` | `config/controls.rs` |
| `bevy::utils::Subscriber` | `tracing::Subscriber` | `logging.rs:10` |
| `bevy::render::mesh::*` | `bevy::mesh::*` | `world/terrain/mod.rs` |
| `bevy::render::texture::Volume` | `bevy::image::Volume` | `world/terrain/mod.rs` |

---

## Phase 3: ECS API Changes

### 3.1 `add_state` → `init_state`
```rust
// OLD: app.add_state::<GameState>();
// NEW: app.init_state::<GameState>();
```
Files: `main.rs:68`, `world/mod.rs:217`

### 3.2 `Pipe` → `pipe()`
```rust
// OLD: system_a.pipe(system_b)
// NEW: pipe(system_a, system_b)
```
Files: `logging.rs:124`, `ui/mod.rs:19`, `world/mod.rs:222`, `world/terrain/mod.rs:1655,1667`

### 3.3 `#[component(storage = "SparseSet")]` → `#[component(storage = SparseSet)]`
Remove quotes around `SparseSet`. Files: `world/terrain/mod.rs:1613`, `world/spatial_entities/storage.rs:31,35,39,43`, `multiplayer/host.rs:443,452`

### 3.4 `States` → `State` derive
```rust
// OLD: #[derive(..., States)]
// NEW: #[derive(..., State)]
```
Files: `main.rs:43`

### 3.5 `clear_children`/`remove_children` → `detach_*`
Check all files for these renamed methods.

---

## Phase 4: Material/Rendering (TerrainMaterial)

### 4.1 Remove `AsBindGroupShaderType` impl (Bevy 0.17+)
In Bevy 0.17+, `AsBindGroupShaderType` is **removed**. The uniform is auto-generated from `#[uniform]` attributes. **Delete** `impl AsBindGroupShaderType<StandardMaterialUniform> for TerrainMaterial` (`terrain/mod.rs:1022-1088`). Simplify `#[uniform(0, StandardMaterialUniform)]` to `#[uniform(0)]`.

### 4.2 `MaterialPlugin` fields → trait methods (Bevy 0.18)
Add to `impl Material for TerrainMaterial`:
```rust
fn enable_prepass() -> bool { false }
fn enable_shadows() -> bool { false }
```

### 4.3 `Image::reinterpret_stacked_2d_as_array()` returns `Result`
Add `.unwrap()` or error handling at `terrain/mod.rs:984,1229,1343`.

### 4.4 Module reorganizations
- `bevy::render::mesh` → `bevy::mesh`
- `bevy::render::texture::Volume` → `bevy::image::Volume`

---

## Phase 5: Console Logging (`src/logging.rs`) — **Highest Risk**

### 5.1 tracing imports
```rust
// OLD: bevy::utils::tracing::{field::Field, Subscriber}
// NEW: tracing::{field::Field, Subscriber}
// (bevy no longer re-exports tracing the same way)
```

### 5.2 `bevy::utils::tracing::subscriber::set_global_default`
```rust
// OLD: bevy::utils::tracing::subscriber::set_global_default(subscriber)
// NEW: tracing::subscriber::set_global_default(subscriber)
```

### 5.3 `ConsoleSet::ConsoleUI`
Verify the enum variant still exists in `bevy_console` 0.17.1.

### 5.4 `PrintConsoleLine` / `ConsoleReceiver` internals
The internal `receiver` field access pattern (`receiver.receiver.get()`) may have changed. Check `bevy_console` 0.17.1 source for the new API.

### 5.5 `ConsoleSubscriber` Layer implementation
The `Layer<S>` trait for `tracing_subscriber` may have signature changes. The `on_event` method context parameter (`Context<'_, S>`) may have changed. This likely needs a careful rewrite.

---

## Phase 6: UI

### 6.1 `bevy_ui_navigation` 0.17.0
Verify these still exist with compatible signatures:
- `NavMarkerPropagationPlugin`
- `DefaultNavigationPlugins`
- `NavRequestSystem`, `NavEvent`, `NavRequest`
- `MenuBuilder`, `MenuSetting`, `NavMarker`
- `Focusable`, `Focused`, `FocusState`
- `InputMapping`

### 6.2 `BorderRadius` (0.18)
No longer a Component — now a field on `Node`. Check for Component usage.

### 6.3 `LineHeight` (0.18)
Now a separate Component (was previously part of `TextFont`). Check `numaric_input.rs`.

---

## Phase 7: Multiplayer (`renet` 0.0.13 → 1.2.0)

### 7.1 API Changes to Review
Key types in `src/multiplayer/` that may differ:
- `RenetServer`, `RenetClient`
- `NetcodeServerTransport`, `NetcodeClientTransport`
- `ServerAuthentication`, `ServerConfig`, `ClientAuthentication`
- `NETCODE_USER_DATA_BYTES`
- `ServerEvent::ClientConnected`, `ServerEvent::ClientDisconnected`

**Action**: Read the renet 1.2.0 changelog. The transport layer API changed significantly between 0.0.x and 1.x.

---

## Phase 8: Proc Macros (`crates/proc_macros`)

Verify these generated paths are still valid:
- `bevy::prelude::Mut<'_, T>`
- `bevy::prelude::Entity`
- `bevy::ecs::system::EntityCommands`

These should be unchanged in 0.18.

---

## Phase 9: Other

### 9.1 `ExitCondition`
Verify `bevy::window::ExitCondition` still exists in 0.18.

### 9.2 `PrimaryWindow`
Verify `bevy::window::PrimaryWindow` still exists.

### 9.3 `bevy_console` 0.17.1
Verify: `ConsolePlugin`, `ConsoleConfiguration`, `ConsoleCommand<T>`, `AddConsoleCommand`, `ConsoleOpen`, `ConsoleSet`, `PrintConsoleLine`, `ConsoleReceiver`.

---

## Execution Order

1. **Backup**: `git commit -am "pre-migration"`
2. **Drop Bevy submodule**, update `Cargo.toml`
3. **`cargo update`**
4. **Fix imports** (Phase 2) — smallest changes first
5. **Fix ECS APIs** (Phase 3)
6. **Fix materials/shaders** (Phase 4)
7. **Fix UI** (Phase 6)
8. **Fix gizmos** (Phase 1.2)
9. **Fix logging** (Phase 5) — most complex, do last
10. **Fix networking** (Phase 7)
11. **Build and iterate** — expect many compilation errors
12. **Test**: run `test_scripts/multiplayer.sh`

---

## Risk Matrix

| Area | Risk | Effort |
|---|---|---|
| Console logging rewrite | **High** | Medium |
| TerrainMaterial shader changes | **High** | Medium |
| `renet` 0.0.13 → 1.2.0 API | Medium | Medium |
| `bevy_ui_navigation` compat | Medium | Low |
| Proc macro generated code | Low | Low |
| World data (sled) compat | Medium | Low |
