# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "test - configure and build the package, then run the tests"
	@echo "build - configure and build the package"
	@echo "configure - configure the package"
	@echo "clean - remove all targets"
	@echo "help - this message"

build:	configure
	@echo "*** Building the package"
	cabal build

configure: hpio.cabal shell.nix default.nix
	@echo "*** Configuring the package"
	cabal configure

shell.nix: hpio.cabal
	@echo "*** Generating shell.nix"
	cabal2nix --shell ./. > shell.nix

default.nix: hpio.cabal
	@echo "*** Generating default.nix"
	cabal2nix ./. > default.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean
