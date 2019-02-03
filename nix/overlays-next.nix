let

  localLib = import ./lib.nix;

in
[
  localLib.fetchNixPkgsLibQuixoftic
  ./overlays/haskell-lib.nix
  ./overlays/haskell-overrides-next.nix
]
