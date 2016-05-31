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
        version = "0.8.0.1";
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
        homepage = "https://github.com/dhess/hpio";
        description = "Monads for GPIO in Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
