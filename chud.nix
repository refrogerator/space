# { pkgs ? import <nixpkgs> { } }:
# pkgs.rustPlatform.buildRustPackage rec {
#     pname = "space";
#     version = "0.1";
#     cargoLock.lockFile = ./Cargo.lock;
#     src = pkgs.lib.cleanSource ./.;
#     buildInputs = [ pkgs.SDL2 ];
# }
let
  pkgs = import <nixpkgs> { };
in
  pkgs.stdenv.mkDerivation {
    name = "space";

    src = ./.;

    buildInputs = with pkgs; [
      rustc cargo SDL2
    ];
    buildPhase = ''
      cargo build --release
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp target/release/space $out/bin/space
    '';
  }
