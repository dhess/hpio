{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, errors
      , exceptions, filepath, free, hspec, mtl, stdenv, strict, text
      , transformers
      }:
      mkDerivation {
        pname = "gpio";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers directory errors exceptions filepath free mtl
          strict text transformers
        ];
        testHaskellDepends = [
          base containers directory errors exceptions filepath free hspec mtl
          strict text transformers
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
