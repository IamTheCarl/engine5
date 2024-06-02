{ pkgs ? import <nixpkgs> { } }:
let
  rust-overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/stable.tar.gz");
  pkgs = import <nixpkgs> { overlays = [ rust-overlay ]; };
  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [
      "rust-src"
      "rust-analyzer"
      "rustfmt"
      "clippy"
    ];
    targets = [
      "x86_64-unknown-linux-gnu"
    ];
  };
  rust_platform = pkgs.makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };
in
pkgs.mkShell {
  buildInputs = [
    rust
    rust_platform.bindgenHook
    pkgs.crate2nix
    pkgs.pkg-config
    pkgs.alsa-lib
    pkgs.udev
    pkgs.wayland
    pkgs.libxkbcommon
    pkgs.vulkan-loader
  ];

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.libxkbcommon
    pkgs.vulkan-loader
  ];

  # Set environment variables
  shellHook = ''
    export OPENSSL_NO_VENDOR=1
  '';
}
