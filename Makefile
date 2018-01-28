# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.


nix-build: nix
	nix-build nix/jobsets/release.nix -A hpio

doc:	test
	@echo "*** Generating docs"
	cabal haddock --hyperlink-source

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "(Default is 'nix-build')"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    nix-build - Run nix-build on all release.nix targets"
	@echo "    test      - configure and build the package, then run the tests"
	@echo "    build     - configure and build the package"
	@echo "    configure - configure the package"
	@echo
	@echo "Stack/Nix:"
	@echo
	@echo "The following targets build and test the package with Stack, using the"
	@echo "given version of Stackage LTS as configured by the file stack-<target>.yaml."
	@echo
	@echo "    lts   [build all supported LTS targets]"
	@echo "    lts-10"
	@echo "    lts-9"
	@echo "    lts-7"
	@echo "    lts-6"
	@echo "    lts-3"
	@echo "    lts-2 [Note: does not work on macOS]"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

nix-stack = nix-shell -p stack-env zlib libiconv ncurses --run 'stack test --stack-yaml $(1)'

lts:    lts-10 lts-9 lts-7 lts-6 lts-3 lts-2

lts-10: nix
	$(call nix-stack,stack.yaml)

lts-9:  nix
	$(call nix-stack,stack-lts-9.yaml)

lts-7:  nix
	$(call nix-stack,stack-lts-7.yaml)

lts-6:  nix
	$(call nix-stack,stack-lts-6.yaml)

lts-3:  nix
	$(call nix-stack,stack-lts-3.yaml)

lts-2:  nix
	$(call nix-stack,stack-lts-2.yaml)

sdist:	check
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: hpio.cabal nix/pkgs/hpio.nix
	@echo "*** Configuring the package"
	cabal configure

nix nix/pkgs/hpio.nix: hpio.cabal
	@echo "*** Generating pkgs/hpio.nix"
	cd nix/pkgs && cabal2nix ../../. > hpio.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean nix
