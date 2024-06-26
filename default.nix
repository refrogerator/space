{ pkgs ? import <nixpkgs> { } }:
pkgs.rustPlatform.buildRustPackage rec {
  pname = "space";
  version = "0.1";
  cargoLock.lockFile = ./Cargo.lock;
  src = pkgs.lib.cleanSource ./.;
  buildInputs = [ pkgs.SDL2 ];
  postBuild = ''
    mkdir $out/share/space/ -p
    cp -r res $out/share/space
    mkdir $out/share/applications/ -p
    cp res/Space.desktop $out/share/applications
  '';
}
