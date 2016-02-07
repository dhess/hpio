{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , exceptions, filepath, free, hspec, inline-c, mtl
      , optparse-applicative, QuickCheck, stdenv, text, transformers
      , unix, unix-bytestring
      }:
      mkDerivation {
        pname = "gpio";
        version = "0.5.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring containers directory exceptions filepath free
          inline-c mtl QuickCheck text transformers unix unix-bytestring
        ];
        executableHaskellDepends = [
          base mtl optparse-applicative transformers
        ];
        testHaskellDepends = [
          base bytestring containers directory exceptions filepath free hspec
          inline-c mtl QuickCheck text transformers unix unix-bytestring
        ];
        homepage = "https://github.com/dhess/gpio";
        description = "Monads for GPIO in Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
