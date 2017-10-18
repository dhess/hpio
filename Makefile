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
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
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
	@echo "    lts-9"
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

sdist:	check
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: hpio.cabal nix/hpio.nix
	@echo "*** Configuring the package"
	cabal configure

nix-stack = nix-shell -p stack-$(1)-env --run 'stack test --nix --nix-packages "zlib binutils gcc" --stack-yaml $(2)'

lts:	lts-9 lts-6 lts-3 lts-2

lts-9: 	hpio.cabal nix/hpio.nix
	$(call nix-stack,lts-9,stack.yaml)

# Currently disabled, as Nix no longer supports GHC 8.0.1 out of the
# box.

#lts-7: 	hpio.cabal nix/hpio.nix
#	$(call nix-stack,lts-7,stack-lts-7.yaml)

lts-6: 	hpio.cabal nix/hpio.nix
	$(call nix-stack,lts-6,stack-lts-6.yaml)

lts-3: 	hpio.cabal nix/hpio.nix
	$(call nix-stack,lts-3,stack-lts-3.yaml)

lts-2: 	hpio.cabal nix/hpio.nix
	$(call nix-stack,lts-2,stack-lts-2.yaml)

nix/hpio.nix: hpio.cabal
	@echo "*** Generating nix/hpio.nix"
	cabal2nix ./. > nix/hpio.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean
