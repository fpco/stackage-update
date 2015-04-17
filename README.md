## stackage-update

This package provides an executable, `stackage-update`, which provides the same
functionality as `cabal update` (it updates your local package index). However,
instead of downloading the entire package index as a compressed tarball over
insecure HTTP, it uses `git` to incrementally update your package list, and
downloads over secure HTTPS.

It has minimal Haskell library dependencies (all dependencies are shipped with
GHC itself) and only requires that the `git` executable be available on the
PATH. It builds on top of the
[all-cabal-files](https://github.com/commercialhaskell/all-cabal-files)
repository.

### Usage

Install from Hackage as usual with:

```
cabal update
cabal install stackage-update
```

From then on, simply run `stackage-update` instead of `cabal update`.

### Why stackage?

You may be wondering why this tool is called `stackage-update`, when in fact
the functionality is useful outside of [the Stackage
project](https://www.stackage.org/) itself. The reason is that the naming
allows it to play nicely with the other Stackage command line tooling.
Concretely, that means that if you have stackage-cli installed, stackage-update
works as a plugin. However, you can certainly use `stackage-update` on its own
without any other tooling or dependencies on the Stackage project.
