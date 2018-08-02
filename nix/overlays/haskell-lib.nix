self: super:

let

  inherit (self) haskell lib;


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
  cleanSourceLocal = src: lib.cleanSourceWith { filter = filterSourceLocal; inherit src; };

  myCleanSource = src: cleanSourceLocal (lib.cleanSourceAllExtraneous src);


  ## Haskell package combinators.

  withOurHpio = hp: (hp.extend (self: super: (
    {
      hpio = lib.cleanPackage myCleanSource (super.callCabal2nix "hpio" ../../. {});
    }
  )));

  withOurHpioHlint = hp: (hp.extend (self: super: (
    {
      hpio = lib.cleanPackage myCleanSource (super.callCabal2nix "hpio" ../../. { extraCabal2nixOptions = "--flag test-hlint"; });
    }
  )));

in
{
  inherit withOurHpio withOurHpioHlint;
}
