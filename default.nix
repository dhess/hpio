let

  localLib = (import ./nix/lib.nix);
  defaultPkgs = localLib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  inherit (localLib.dhess-lib-nix) lib;
  localOverlays = import ./nix/overlays.nix;
  self = lib.customisation.composeOverlays (lib.singleton localOverlays) pkgs;

in
{
  inherit (self) haskellPackages;
  overlays.all = localOverlays;
}
