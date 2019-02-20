self: super:

let

  lib = import ./lib;

  overlays = [
    lib.inputOverlays
    (import overlays/haskell-overrides.nix)
    ];

in
lib.customisation.composeOverlays overlays super
