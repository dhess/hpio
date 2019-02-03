let
  nixpkgs = (import ./nix/lib.nix).nixpkgs;

in

{ pkgs ? nixpkgs {} }:

with pkgs.lib;

let

  self = foldl'
    (prev: overlay: prev // (overlay (pkgs // self) (pkgs // prev)))
    {} (map import (import ./nix/overlays.nix));

in
{
  inherit (self) haskellPackages;
  overlays.haskellPackages = import ./.;
}
