JEST
========

An Information Flow Monitor Inliner for JavaScript Web Applications. Read [Andrey Chudnov's PhD dissertation](https://www.cs.stevens.edu/~naumann/pub/ChudnovDissertation.pdf) for an overview and rationale for the design and implementation.

Build instructions
------------------

Currently the `stack` tool (http://haskellstack.org) is a prerequisite for building. Follow the instructions to set up the tool and your environment. Then issue `stack build` in the project directory.

<!-- You can also build without `stack`. You would need GHC (at least 7.4.2) and cabal-install (at least 1.18). To perform a full sanboxed (recommended) build issue the following commands: -->

<!-- ``` -->
<!-- cabal update -->
<!-- cabal sandbox init -->
<!-- cabal install --enable-tests --only-dependencies -->
<!-- cabal build -->
<!-- ``` -->

Development setup
-----------------

Use your favorite tools for developing in Haskell. For JavaScript I use Facebook Flow (http://github.com/facebook/flow).
