## 0.9.0.1 (2017-10-16)

Fixes:
  - Better Nix packaging.
  - Add a few missing files to extra-source-files.

## 0.9.0.0 (2017-10-02)

Changes:
  - This project now uses Protolude (0.2.*). I believe the only visible
    API change is that SysfsException's string type is now Text,
    instead of String.
  - Add monad and transformer instances for transformers-base,
    monad-control, and monad-logger monads.
  - Use DefaultSignatures and ConstraintKinds to simplify the
    implementation.
  - Use hpack to generate the project's Cabal file.

Fixes:
  - Add some new compile-time warnings.
  - Remove redundant Functor and Applicative constraints.
  - Update Travis-CI config to test against more recent Stackage LTS
    and GHC releases.
  - Remove most of the Travis-CI macOS jobs; they're really slow.
  - Get all supported GHC/Stackage LTS versions to build again.

## 0.8.0.10 (2017-06-28)

Fixes:
  - Bump QuickCheck bounds.
  - Bump optparse-applicative bounds.
  - Stack: update to lts-8.20.

## 0.8.0.9 (2017-05-24)

Fixes:
  - Fix project URLs.

## 0.8.0.8 (2017-05-24)

No changes; copyright for the project has been assigned to Quixoftic, LLC.

## 0.8.0.7 (2017-02-21)

Fixes:
  - Fix hlint tests.
  - Note GHC 8.0.2 compatibility in cabal file.

## 0.8.0.6 (2017-01-25)

Fixes:
  - Remove hspec upper bounds.

## 0.8.0.5 (2017-01-11)

Fixes:
  - Bump directory bounds.

## 0.8.0.4 (2016-10-26)

Fixes:
  - Support for optparse-applicative 0.13.x.
  - Bump hspec bounds.

## 0.8.0.3 (2016-07-15)

Fixes:
  - Bump QuickCheck bounds.

## 0.8.0.2 (2016-06-08)

Fixes:
  - Fix Stackage doctests issue (#48).

## 0.8.0.1 (2016-05-30)

Fixes:
  - Fix sdist problem with test files (#47)

## 0.8.0.0 (2016-05-27)

Initial release.
