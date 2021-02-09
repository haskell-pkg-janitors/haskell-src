## 1.0.3.1 Revision 3

  - Support GHC 9.0 & base-4.15.

## 1.0.3.0/1 Revision 2

  - Support happy-1.20.

## 1.0.3.1

  - Version for GHC 8.8 and 8.10.

## 1.0.3.0

  - Version for GHC 7.10 - 8.6.
    (Dropped support for GHC versions prior to GHC 7.0.)

  - Add support for `MonadFail` & `Semigroup` proposals by
    adding respective instances for `P` and `Lex`.

  - Remove `-O2` from `ghc-options`.

## 1.0.2.0

  - Add support for GHC 7.10 & base-4.8

  - Add missing `Functor` & `Applicative` instances for `P` and `Lex`
    monads needed for AMP compatibility.
