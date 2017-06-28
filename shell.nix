{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, base-compat, bytestring
      , containers, directory, doctest, exceptions, filepath, hlint
      , hspec, mtl, mtl-compat, optparse-applicative, QuickCheck, stdenv
      , text, transformers, transformers-compat, unix, unix-bytestring
      }:
      mkDerivation {
        pname = "hpio";
        version = "0.8.0.10";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base-compat bytestring containers directory exceptions
          filepath mtl mtl-compat QuickCheck text transformers
          transformers-compat unix unix-bytestring
        ];
        executableHaskellDepends = [
          async base base-compat exceptions mtl mtl-compat
          optparse-applicative transformers transformers-compat
        ];
        testHaskellDepends = [
          async base base-compat bytestring containers directory doctest
          exceptions filepath hlint hspec mtl mtl-compat QuickCheck text
          transformers transformers-compat unix unix-bytestring
        ];
        homepage = "https://github.com/quixoftic/hpio";
        description = "Monads for GPIO in Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  # Test with optparse-applicative-0.13.0.0.
  # modifiedHaskellPackages = haskellPackages.override {
  #     overrides = self: super: {
  #       optparse-applicative = self.optparse-applicative_0_13_0_0;
  #     };
  # };
  #drv = modifiedHaskellPackages.callPackage f {};
  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
