{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, errors
      , exceptions, filepath, free, hspec, mtl, optparse-applicative
      , QuickCheck, stdenv, strict, text, transformers
      }:
      mkDerivation {
        pname = "gpio";
        version = "0.5.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers directory errors exceptions filepath free mtl
          QuickCheck strict text transformers
        ];
        executableHaskellDepends = [
          base errors mtl optparse-applicative transformers
        ];
        testHaskellDepends = [
          base containers directory errors exceptions filepath free hspec mtl
          QuickCheck strict text transformers
        ];
        homepage = "https://github.com/dhess/gpio";
        description = "Control GPIO pins";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
