{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, base-compat, bytestring
      , containers, directory, doctest, exceptions, filepath, hlint
      , hspec, monad-control, monad-logger, mtl, mtl-compat
      , optparse-applicative, protolude, QuickCheck, stdenv, text
      , transformers, transformers-base, transformers-compat, unix
      , unix-bytestring
      }:
      mkDerivation {
        pname = "hpio";
        version = "0.9.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base-compat bytestring containers directory exceptions
          filepath monad-control monad-logger mtl mtl-compat protolude
          QuickCheck text transformers transformers-base transformers-compat
          unix unix-bytestring
        ];
        executableHaskellDepends = [
          async base base-compat exceptions mtl mtl-compat
          optparse-applicative protolude text transformers
          transformers-compat
        ];
        testHaskellDepends = [
          base base-compat containers directory doctest exceptions filepath
          hlint hspec protolude QuickCheck
        ];
        homepage = "https://github.com/quixoftic/hpio#readme";
        description = "Monads for GPIO in Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
