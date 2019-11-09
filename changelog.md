### 1.0.3.1

 - GHC 8.8.1/base-4.13 only compatibility release to support `MonadFail`

## 1.0.3.0

 - Add support for `MonadFail` & `Semigroup` proposals by
   adding respective instances for `P` and `Lex`.

 - Drop support for GHC versions prior to GHC 7.0.

 - Remove `-O2` from `ghc-options`.

## 1.0.2.0

 - Add support for GHC 7.10 & base-4.8)

 - Add missing `Functor` & `Applicative` instances for `P` and `Lex`
   monads needed for AMP compatibility.
