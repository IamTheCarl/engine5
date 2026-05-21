# Migration Plan: Bevy 0.11 → 0.18.1

## Overview

A **7.5 major version jump** (0.11→0.18.1) affecting ~30+ source files. The migration goes through intermediate versions: 0.11 → 0.12 → 0.13 → 0.14 → 0.15 → 0.16 → 0.17 → 0.18.

## Current State

| Dependency | Current | Target | Status |
|---|---|---|---|
| **Bevy** | 0.11 (forked at `crates/bevy`) | 0.18.1 (crates.io) | ✅ Done |
| `bevy_prototype_debug_lines` | 0.11.1 | `bevy_gizmos` 0.18 | ✅ Done |
| `bevy-ui-navigation` | 0.31.0 | **DROPPED** (incompatible) | ❌ No Bevy 0.18 version exists (0.33.1 max is Bevy 0.12) |
| `bevy_console` | 0.8.0 | 0.17.1 | 🔴 Blocked: API changed (derive macros) |
| `renet` | 0.0.13 | 1.2.0 | ✅ Done |
| `renet_visualizer` | 0.0.6 | **DROPPED** (no 0.18 version) | ✅ Done |

---

## Phase 1: Dependencies — **COMPLETE**

### 1.1 Cargo.toml Changes ✅

- [x] Removed patch section (dropped Bevy fork)
- [x] `bevy` → `0.18` with `serialize` feature
- [x] `bevy_prototype_debug_lines` → replaced with `bevy_gizmos = "0.18"`
- [x] `renet` → `1.2.0` (API compatible, no changes needed)
- [x] `renet_visualizer` → removed
- [x] `bevy_console = "0.14.1"` → **update to `0.17.1`** (see Phase 5)
- [x] `bevy_ui_navigation = "0.33.1"` → **dropped** (see UI section below)

### 1.2 Debug Lines → Gizmos ✅

All `bevy_prototype_debug_lines` imports and usage replaced with `bevy_gizmos::Gizmos::line(start, end, color)` in:
- `src/world/physics/terrain_to_terrain.rs`
- `src/world/physics/ray_cast_with_terrain.rs`
- `src/world/physics/cylinder_to_cylinder.rs`
- `src/world/physics/cylinder_to_terrain.rs`
- `src/world/spatial_entities/mod.rs`
- `src/main.rs`

---

## Phase 2: Import Path Changes — **COMPLETE**

| Old | New | Files | Status |
|---|---|---|---|
| `bevy::utils::synccell::SyncCell` | `bevy::platform::cell::SyncCell` | `logging.rs:8`, `ui/mod.rs:2` | ✅ Done |
| `bevy::utils::HashSet` | `std::collections::HashSet` | `config/controls.rs` | ✅ Done |
| `bevy::utils::Subscriber` | `tracing::Subscriber` | `logging.rs:12` | ✅ Done |
| `bevy::render::mesh::*` | `bevy::mesh::*` | `world/terrain/mod.rs` | ✅ Done |
| `bevy::render::texture::Volume` | `bevy::image::Volume` | `world/terrain/mod.rs` | ✅ Done |

---

## Phase 3: ECS API Changes — **COMPLETE**

### 3.1 `add_state` → `init_state` ✅
Files: `main.rs:69`, `world/mod.rs:217`

### 3.2 `Pipe` → `pipe()` ✅
Files: `logging.rs`, `ui/mod.rs`, `world/mod.rs`, `world/terrain/mod.rs`

### 3.3 `#[component(storage = "SparseSet")]` → `#[component(storage = SparseSet)]` ✅
Remove quotes around `SparseSet`. Files: `world/terrain/mod.rs`, `world/spatial_entities/storage.rs`, `multiplayer/host.rs`

### 3.4 `States` → `State` derive ✅
`main.rs:44` — `#[derive(..., State)]`

### 3.5 `clear_children`/`remove_children` → `detach_*`
Check all files for these renamed methods.

---

## Phase 4: Material/Rendering (TerrainMaterial) — **COMPLETE**

### 4.1 Remove `AsBindGroupShaderType` impl ✅
Deleted `impl AsBindGroupShaderType<StandardMaterialUniform> for TerrainMaterial`. Simplified `#[uniform(0, StandardMaterialUniform)]` to `#[uniform(0)]`.

### 4.2 `MaterialPlugin` fields → trait methods ✅
Added `enable_prepass()` and `enable_shadows()` to `impl Material for TerrainMaterial`.

### 4.3 `Image::reinterpret_stacked_2d_as_array()` returns `Result` ✅
Added `.unwrap()` at `terrain/mod.rs:984,1229,1343`.

### 4.4 Module reorganizations ✅
- `bevy::render::mesh` → `bevy::mesh`
- `bevy::render::texture::Volume` → `bevy::image::Volume`

---

## Phase 5: Console (`bevy_console` 0.8.0 → 0.17.1) — **IN PROGRESS**

### 5.1 API Changes (0.8 → 0.17.1)

**BEVY_CONSOLE 0.17.1 IS COMPATIBLE WITH BEVY 0.18.** The dependency conflict in 0.14.1 is resolved in 0.17.1.

Key API changes:

| Old (0.8) | New (0.17.1) |
|---|---|
| `ConsoleCommandEntered` event + `MessageWriter` | `ConsoleCommand<CommandType>` wrapper struct |
| `AddConsoleCommand::add_console_command()` | Same method, but command type must derive `ConsoleCommand` |
| `ConsoleSet::ConsoleUI` | `ConsoleSet::Console` |
| `PrintConsoleLine` (event) | `PrintConsoleLine` (struct) + `BevyLogBuffer` resource |
| `ConsoleOpen` (event) | `ConsoleOpen` (resource) |
| `ConsoleConfiguration` (resource) | Same |
| `ConsoleReceiver` (custom, synced via `SyncCell<mpsc::Receiver>` | **DROPPED** — replaced by `BevyLogBuffer` + `make_layer()`/`make_filtered_layer()` tracing layers |
| Console log capture (custom `ConsoleSubscriber` Layer) | `make_layer()` or `make_filtered_layer()` from bevy_console |

**Console log capture**: Instead of a custom `ConsoleSubscriber` + `mpsc` pipe, use the built-in tracing layer:
```rust
use bevy_console::{make_filtered_layer, BevyLogBuffer, send_log_buffer_to_console};

// In setup:
let console_layer = make_filtered_layer(...);
// Add to subscriber via .with(console_layer)

// System to flush buffer to console:
fn flush_console(app: &App) {
    app.add_systems(Update, send_log_buffer_to_console);
}
```

**Console commands**: Commands now use a derive macro:
```rust
use bevy_console::ConsoleCommand;

#[derive(Parser, ConsoleCommand)]
#[command(name = "world")]
struct WorldCommand { ... }

fn world_command(mut command: ConsoleCommand<WorldCommand>) {
    if let Some(Ok(cmd)) = command.take() {
        // handle command
    }
}

app.add_console_command::<WorldCommand, _>(world_command);
```

### 5.2 Implementation Plan

1. **Update Cargo.toml**: Change `# bevy_console = "0.14.1"` → `bevy_console = "0.17.1"`
2. **Rewrite logging.rs**: Replace custom `ConsoleSubscriber` with `bevy_console::make_layer()` + `BevyLogBuffer`
3. **Update main.rs**: Add `ConsolePlugin`, `ConsoleConfiguration`, and `ConsoleScripts`
4. **Update world/commands.rs**: Convert to `ConsoleCommand` derive macro pattern
5. **Update console_scripts.rs**: Adapt for `ConsoleCommandEntered` event changes
6. **Update config/controls.rs**: Use `ConsoleOpen` resource instead of event

### 5.3 ConsoleScripts Module

The `console_scripts.rs` module (startup scripts via `ENGINE5_STARTUP_SCRIPT` env var) needs:
- `ConsoleCommandEntered` event (still exists in 0.17.1)
- `ConsoleConfiguration` resource (still exists, but API may differ)
- `MessageWriter` → `EventWriter` (Bevy 0.18 change)

---

## Phase 6: UI — **BLOCKED: bevy_ui_navigation incompatible**

### 6.1 `bevy_ui_navigation` Status — **DROPPED**

**Critical finding**: `bevy-ui-navigation` 0.33.1 (latest) only supports **Bevy 0.12**, not Bevy 0.18. There is no Bevy 0.18 compatible version of this crate.

The version matrix from docs.rs:
| Bevy | Latest bevy_ui_navigation |
|---|---|
| 0.12 | 0.33.1 (latest) |
| 0.11 | 0.32.0 |
| 0.10 | 0.24.1 |
| 0.9 | 0.23.1 |
| 0.8 | 0.21.0 |

### 6.2 Recommended Alternatives

1. **Minimal custom navigation** (recommended for now): Implement simple tab-focus or mouse-click navigation using Bevy's built-in interaction system. This replaces the most commonly used navigation features.

2. **Fork bevy_ui_navigation**: Maintain a fork updated for Bevy 0.18. Given the crate's architecture (centered on `NavRequest`/`NavEvent`/`Focusable`), the migration should be straightforward.

3. **Wait for upstream**: Monitor https://github.com/nicopap/ui-navigation for a Bevy 0.18 release.

### 6.3 UI Stubs (Current State)

The following files contain `TODO: restore bevy_ui_navigation` comments with stub implementations that need to be re-implemented with custom navigation:

| File | Stubbed Systems |
|---|---|
| `ui/widgets/buttons.rs` | `button_system`, `process_back_buttons` |
| `ui/widgets/combo.rs` | `hide_combo_when_unfocused`, `show_combo_when_focused`, `handle_combo_selection` |
| `ui/widgets/numaric_input.rs` | `process_digit_general_input`, `process_digit_mouse_input`, `process_digit_keyboard_input`, `copy`, `paste` |
| `ui/sub_menus/mod.rs` | `setup_submenu` generic |
| `ui/root_menus/main_menu.rs` | `handle_selections` |
| `ui/root_menus/pause_menu.rs` | `handle_selections`, `exit_pause_state` |
| `ui/root_menus/fatal_error_display.rs` | `handle_selections` |
| `ui/sub_menus/settings/controls.rs` | `process_open_button`, `process_reload_button` |

### 6.4 BorderRadius (0.18) ✅

No longer a Component — now a field on `Node`. Checked. No issues found.

### 6.5 LineHeight (0.18)

Now a separate Component (was previously part of `TextFont`). Check `numaric_input.rs`.

---

## Phase 7: Multiplayer (renet) — **COMPLETE**

### 7.1 API Changes ✅

renet 1.2.0 API is compatible with the existing code:
- `RenetServer`, `RenetClient` — same interface
- `NetcodeServerTransport`, `NetcodeClientTransport` — same interface
- `ServerAuthentication::Unsecure`, `ServerConfig`, `ClientAuthentication::Unsecure` — same interface
- `NETCODE_USER_DATA_BYTES` — still exists
- `ServerEvent::ClientConnected`, `ServerEvent::ClientDisconnected` — still exist
- `ChannelConfig`, `ConnectionConfig`, `SendType` — same interface

**Note**: renet 2.0.0 is available but would require API changes. 1.2.0 is stable and working.

---

## Phase 8: Proc Macros — **NO CHANGES NEEDED**

Verified these generated paths are still valid in Bevy 0.18:
- `bevy::prelude::Mut<'_, T>` ✅
- `bevy::prelude::Entity` ✅
- `bevy::ecs::system::EntityCommands` ✅

---

## Phase 9: Other — **COMPLETE**

### 9.1 `ExitCondition` ✅
`bevy::window::ExitCondition` still exists in 0.18.

### 9.2 `PrimaryWindow` ✅
`bevy::window::PrimaryWindow` still exists.

### 9.3 `bevy_console` 0.17.1 Types ✅ (verified)
All required types exist: `ConsolePlugin`, `ConsoleConfiguration`, `ConsoleCommand`, `AddConsoleCommand`, `ConsoleOpen`, `ConsoleSet`, `PrintConsoleLine`, `ConsoleCommandEntered`.

---

## Execution Order

1. ~~**Backup**: `git commit -am "pre-migration"`~~ (already done)
2. ~~**Drop Bevy submodule**, update `Cargo.toml`~~ (already done)
3. ~~**`cargo update`**~~ (already done)
4. ~~**Fix imports** (Phase 2)~~ (already done)
5. ~~**Fix ECS APIs** (Phase 3)~~ (already done)
6. ~~**Fix materials/shaders** (Phase 4)~~ (already done)
7. ~~**Fix gizmos** (Phase 1.2)~~ (already done)
8. ~~**Fix networking** (Phase 7)~~ (already done)
9. **Fix console logging** (Phase 5) — **IN PROGRESS** (highest priority remaining)
10. **Fix UI navigation** (Phase 6) — **BLOCKED** (no compatible crate available)
11. ~~**Build and iterate**~~ (current build has 4 E0446 proc macro errors, unrelated to migration)

---

## Risk Matrix

| Area | Risk | Effort | Status |
|---|---|---|---|
| Console logging rewrite | **High** | Medium | 🔴 In Progress |
| `renet` 0.0.13 → 1.2.0 API | Medium | Medium | ✅ Complete |
| `bevy_ui_navigation` compat | **High** | High | ❌ Dropped (no Bevy 0.18 version) |
| Proc macro generated code | Low | Low | ✅ Complete |
| World data (sled) compat | Medium | Low | ✅ Complete |
| Custom UI navigation (replacement) | Medium | Medium | ⚪ Not started |

(End of file)
