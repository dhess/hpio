let

  # From https://github.com/input-output-hk/iohk-ops/blob/e6f1ae95cdbfdd5c213aa0b9a1ef67150febc503/lib.nix
  
  fixedDhessLibNix =
  let
    try = builtins.tryEval <dhess_lib_nix>;
  in
    if try.success
      then builtins.trace "Using <dhess_lib_nix>" try.value
      else (import ./fetch-github.nix) { jsonSpec = builtins.readFile ./dhess-lib-nix-src.json; };

  dhess-lib-nix = (import fixedDhessLibNix) {};
  inherit (dhess-lib-nix) lib haskell;
  inherit (lib.fetchers) fixedNixpkgs;
  inherit (lib.dhess-lib-nix) nixpkgs;


  ## Ignore local files that shouldn't contribute to the Nix hash.
  ## Ideally this would be based on the cabal sdist contents, but
  ## that's not easily do-able at the moment.

  filterSourceLocal = name: type: let baseName = baseNameOf (toString name); in ! (
    type == "directory" && (
      baseName == "scripts"
    ) ||
    type != "directory" && (
      baseName == "Makefile"
    )
  );
  cleanSourceLocal = src: lib.sources.cleanSourceWith { filter = filterSourceLocal; inherit src; };
  myCleanSource = src: cleanSourceLocal (lib.sources.cleanSourceAllExtraneous src);


  ## Haskell package combinators.

  withLocalHpio = hp: (haskell.lib.properExtend hp (self: super: (
    {
      hpio = lib.sources.cleanPackage myCleanSource (super.callPackage ../pkgs/hpio.nix {});
    }
  )));
  withLocalHpioMaintainer = hp: (haskell.lib.properExtend hp (self: super: (
    {
      hpio = lib.sources.cleanPackage myCleanSource (super.callPackage ../pkgs/hpio-maintainer.nix {});
    }
  )));

  overlays = lib.singleton (import ../overlays/haskell-overrides.nix);
  maintainerOverlays = lib.singleton (import ../overlays/haskell-overrides-maintainer.nix);

in lib //
{
  inherit fixedNixpkgs;
  inherit nixpkgs;
  inherit withLocalHpio withLocalHpioMaintainer;
  inherit overlays maintainerOverlays;
}
