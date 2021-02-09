Making a new revision on hackage
================================

For example, these were the steps to make revision 3 of 1.0.3.1,
bumping the upper bound on base to < 4.16 for GHC 9.0.1:

* On branch `master`:

  - `haskell-src.cabal`:
    1. bump `base` dependency's upper bound to `< 4.16`
    2. update `tested-with` field to include `GHC == 8.10.4` and `GHC == 9.0.1`

  - `changelog.md`:
    add entry for __1.0.3.1 Revision 3__

  - `attic/haskell-src-1.0.3.1-r3.cabal`
    1. create this file as a copy of `attic/haskell-src-1.0.3.1-r2.cabal`
    2. increase `x-revision` to 3
    3. bump `base` dependency's upper bound to `< 4.16`
    2. update `tested-with` field to include `GHC == 8.10.4` and `GHC == 9.0.1`

  - commit all that
  - push (or do this in the very end)

* Perform the revision on hackage:

  - review the revision:
    ```
    hackage-cli push-cabal attic/haskell-src-1.0.3.1-r3.cabal
    ```

  - make the revision:
    ```
    hackage-cli push-cabal --publish attic/haskell-src-1.0.3.1-r3.cabal
    ```

* On branch `1.0.3._`:

  - pull revision from hackage
    ```
    hackage-cli sync-cabal haskell-src.cabal
    ```
  - commit this
  - tag this commit
    ```
    git tag v1.0.3.1-r3 -m "haskell-src 1.0.3.1 Revision 3"
    git push
    git push --tags
    ```
