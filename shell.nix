{ pkgs ? import <nixpkgs> {} }:

# Dev shell for dungeon-haskell. Mirrors the packages that stack.yaml's
# `nix.packages` list pulls in for builds, plus haskell-language-server
# so VSCode's Haskell extension works out of the box via direnv.
#
# GHC version (ghc9103 = 9.10.3) must match the snapshot in stack.yaml
# so HLS speaks the same compiler as stack does.

let
  hsPkgs = pkgs.haskell.packages.ghc9103;
in
pkgs.mkShell {
  name = "dungeon-haskell-shell";

  buildInputs = [
    # Haskell toolchain
    hsPkgs.ghc
    hsPkgs.haskell-language-server
    hsPkgs.hspec-discover
    pkgs.stack
    pkgs.cabal-install

    # Native deps required by the cabal file (SDL2 via proteaaudio-sdl,
    # zlib via http-client, gmp for GHC RTS). Keep in sync with
    # stack.yaml's nix.packages.
    pkgs.SDL2
    pkgs.pkg-config
    pkgs.zlib
    pkgs.gmp
    pkgs.gcc
    pkgs.git
    pkgs.lefthook
    pkgs.hlint
  ];

  # Stack detects IN_NIX_SHELL and skips its own nested nix-shell
  # re-exec, so `stack build` / `stack repl` inside this shell use the
  # GHC we provide here instead of spawning a second shell.
  shellHook = ''
    export LANG=en_US.UTF-8
    lefthook install
  '';
}
