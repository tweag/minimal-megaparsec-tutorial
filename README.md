
### Development instruction

Install the different tools (you need to have ghcup available) locally:

```
mkdir -p bin/{cabal,ghc,hls}
# Instal cabal, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
ghcup install cabal --isolate $(pwd)/bin/cabal
# Populate GHC, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
# Note that GHC's version number is also in .github/workflows/haskell.yml
ghcup install ghc 9.10.1 --isolate $(pwd)/bin/ghc
ghcup install hls 2.9.0.1 --isolate $(pwd)/bin/hls
```

Because there is a [cabal.project](./cabal.project) file that pins the set of packages to a specific
timestamp, this project is highly reproducible.

Then build:

```shell
cabal update # Only required the first time
cabal build all
```

To run tests:

```shell
cabal test all
```

To test your code interactively, you probably want to do: `cabal repl Lib`
