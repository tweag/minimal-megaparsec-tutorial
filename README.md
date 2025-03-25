[![Lint](https://github.com/tweag/minimal-megaparsec-tutorial/actions/workflows/lint.yml/badge.svg)](https://github.com/tweag/minimal-megaparsec-tutorial/actions/workflows/lint.yml)
[![Build and test](https://github.com/tweag/minimal-megaparsec-tutorial/actions/workflows/build.yml/badge.svg)](https://github.com/tweag/minimal-megaparsec-tutorial/actions/workflows/build.yml)

## minimal-megaparsec-tutorial

Welcome 👋 This is the repository accompanying the [minimal megaparsec tutorial](TODO) blog post from Tweag's website.
It can be forked to obtain a complete setup for experimenting with the
[megaparsec](https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec.html) library.

Comments and improvements welcome!

## Development instructions

Install the different tools (you need to have [ghcup](https://www.haskell.org/ghcup/)
and [direnv](https://direnv.net/) installed) locally:

```
mkdir -p bin/{cabal,ghc,hls,ormolu}
# All paths below match paths in .envrc, so update both places at the same time.
# cabal and GHC's version numbers are also in .github/workflows/haskell.yml
ghcup install cabal 3.12.1.0 --isolate $(pwd)/bin/cabal
ghcup install ghc 9.10.1 --isolate $(pwd)/bin/ghc
ghcup install hls 2.9.0.1 --isolate $(pwd)/bin/hls

# Note that ormolu's version number is also in .github.workflows/lint.yml
wget https://github.com/tweag/ormolu/releases/download/0.8.0.0/ormolu-x86_64-linux.zip -O bin/ormolu/ormolu.zip
unzip bin/ormolu/ormolu.zip -d bin/ormolu/
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
cabal test min-mega-test
```

To test your code interactively, you probably want to do: `cabal repl Lib`
