## 1.0.4

_Andreas Abel, 2022-02-07_

  - Add `Eq` instance for `HsModule`.

## 1.0.3.2

_Andreas Abel, 2022-02-07_

  - Version tested with GHC 7.0 - 9.2.
  - Silence warning `incomplete-uni-patterns` in module `ParseMonad` for GHC >= 9.2.
  - Cosmetic documentation changes.

## 1.0.3.1 Revision 5

  - Support happy-1.21.

## 1.0.3.1 Revision 4

  - Support GHC 9.2 & base-4.16.

## 1.0.3.1 Revision 3

  - Support GHC 9.0 & base-4.15.

## 1.0.3.0/1 Revision 2

  - Support happy-1.20.

## 1.0.3.1

_Herbert Valerio Riedel, 2019-11-09_

  - Version for GHC 8.8 and 8.10.

## 1.0.3.0

_Herbert Valerio Riedel, 2018-03-13_

  - Version for GHC 7.10 - 8.6.
    (Dropped support for GHC versions prior to GHC 7.0.)

  - Add support for `MonadFail` & `Semigroup` proposals by
    adding respective instances for `P` and `Lex`.

  - Remove `-O2` from `ghc-options`.

## 1.0.2.0

_Herbert Valerio Riedel, 2015-01-24_

  - Add support for GHC 7.10 & base-4.8.

  - Add missing `Functor` & `Applicative` instances for `P` and `Lex`
    monads needed for AMP compatibility.
