# Nix Shell for engiadina-pwa
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "elm-prismicio";

  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    nodejs-12_x
  ];
}
