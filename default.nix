let

  localLib = (import ./nix/lib.nix);
  defaultPkgs = localLib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  inherit (localLib.dhess-lib-nix) lib;
  localOverlays = import ./nix/overlays.nix;
  hpioNix = nix/pkgs/hpio.nix;
  self = lib.customisation.composeOverlays (lib.singleton localOverlays) pkgs;

in
{
  # haskellPackages with the local hpio package.
  inherit (self) haskellPackages;

  # The path to the local hpio.nix, in case you want to make your own.
  inherit hpioNix;

  overlays.all = localOverlays;
}
