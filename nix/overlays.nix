self: super:

let

  localLib = import ./lib;
  inherit (localLib) dhess-lib-nix;
  inherit (dhess-lib-nix) lib;

  overlays = [
    dhess-lib-nix.overlays.all
    (import overlays/haskell-overrides.nix)
    ];

in
lib.customisation.composeOverlays overlays super
