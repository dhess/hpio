self: super:

let

  inherit (self) haskell lib;


  ## Ignore local files that shouldn't contribute to the Nix hash.
  ## Ideally this would be based on the cabal sdist contents, but
  ## that's not easily do-able at the moment.

  # Pretty sure that filtering Nix files from the source hash is the
  # right thing to do. They're obviously already evaluated when a
  # nix-build command is executed, so if *what they evaluate* changes
  # they'll cause a rebuild anyway, as they should; while cosmetic
  # changes (comments, formatting, etc.) won't.

  filterNix = name: type: let baseName = baseNameOf (toString name); in ! (
    type != "directory" && lib.hasSuffix ".nix" baseName
  );
  cleanNix = src: lib.cleanSourceWith { filter = filterNix; inherit src; };

  filterOther = name: type: let baseName = baseNameOf (toString name); in ! (
    type == "directory" && (
      baseName == ".cabal-sandbox" ||
      baseName == ".stack-work"    ||
      baseName == "dist"           ||
      baseName == "dist-newstyle"  ||
      baseName == "scripts"
    ) ||
    type != "directory" && (
      baseName == ".gitignore"           ||
      baseName == ".dir-locals.el"       ||
      baseName == "cabal.sandbox.config" ||
      baseName == "Makefile"
    )
  );
  cleanOther = src: lib.cleanSourceWith { filter = filterOther; inherit src; };

  cleanPackage = pkg: (pkg.overrideAttrs (oldAttrs: {
    src = cleanOther (cleanNix (lib.cleanSource oldAttrs.src));
  }));


  ## Haskell package combinators.

  withOurHpio = hpioPkgPath: hp: (hp.extend (self: super: (
    with haskell.lib;
    {
      hpio = cleanPackage (super.callPackage hpioPkgPath {});
    }
  )));

  # For most of these package sets, especially the older LTSes, we
  # don't want to waste time with Haddock generation. In theory this
  # could reduce cache re-use, but there's no Hydra for these
  # Stackage LTS package sets, anyway; and once they've been built
  # once, they're very unlikely to change, anyway.

  dontHaddock = hp: (hp.extend (self: super: (
    with haskell.lib;
    {
      mkDerivation = args: super.mkDerivation (args // {
        doHaddock = false;
      });
    }
  )));

in
{
  inherit withOurHpio dontHaddock;
}
