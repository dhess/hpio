{ pkgs }:

let

  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) properExtend;


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

  withOurHpio = hp: (properExtend hp (self: super: (
    {
      hpio = lib.sources.cleanPackage myCleanSource (super.callPackage ../../pkgs/hpio.nix {});
      hpioHlint = lib.sources.cleanPackage myCleanSource (super.callPackage ../../pkgs/hpio-hlint.nix {});
    }
  )));

in
{
  inherit withOurHpio;
}
