let

  lib = (import nix/lib);
  defaultPkgs = lib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  hpioOverlays = self: super:
    lib.customisation.composeOverlays lib.overlays super;
  hpioOverlaysMaintainer = self: super:
    lib.customisation.composeOverlays lib.maintainerOverlays super;
  hpioNix = nix/pkgs/hpio.nix;
  hpioNixMaintainer = nix/pkgs/hpio-maintainer.nix;

  hpioPkgs = lib.customisation.composeOverlays (lib.singleton hpioOverlays) pkgs;
  hpioPkgsMaintainer = lib.customisation.composeOverlays (lib.singleton hpioOverlaysMaintainer) pkgs;

in
{
  # haskellPackages with the local hpio package. Note that this
  # package set builds hpio *without* maintainer tests.
  inherit (hpioPkgs) haskellPackages;

  # The path to the local hpio.nix, in case you want to make your own.
  inherit hpioNix hpioNixMaintainer;

  # Same as the above, except with the hpio package in maintainer
  # mode.
  maintainer = {
    inherit (hpioPkgsMaintainer) haskellPackages;
    hpioNix = hpioNixMaintainer;
  };

  # Overlays for the hpio package in both modes.
  overlays.hpio = hpioOverlays;
  overlays.hpioMaintainer = hpioOverlaysMaintainer;
}
