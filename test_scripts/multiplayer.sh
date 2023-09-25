#!/bin/bash
# Spawns an engine instance in multiplayer mode and another in client mode.
# They will connect to each other.

ENGINE5_STARTUP_SCRIPT="assets/scripts/multiplayer_server.e5s" cargo run &
ENGINE5_STARTUP_SCRIPT="assets/scripts/multiplayer_client.e5s" cargo run
