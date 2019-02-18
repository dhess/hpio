self: super:

let

  localLib = import ./lib.nix;
  inherit (localLib) dhess-lib-nix;
  inherit (dhess-lib-nix) lib;

  overlays =
    (lib.singleton dhess-lib-nix.overlays.all)
    ++
    (map import [
      ./overlays/haskell-lib.nix
      ./overlays/haskell-overrides-next.nix
    ]);

in
lib.customisation.composeOverlays overlays super
