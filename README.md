# Fae: a functional alternative to Ethereum

Fae is a smart contract system and blockchain definition with equivalent power
to Ethereum but with the following additional benefits:

  - Transaction execution is parallelizable and each network node need only
    execute transactions it wants.

  - There is no virtual machine, but rather a minimal execution environment that
    can run an existing, general-purpose programming language.

  - There is no native currency, only a simple token awarded to the participants
    that mine blocks.

  - Scarcity policy is left to the discretion of each smart contract.

We provide an [implementation](src/) of this system in Haskell, for which we have included the [haddocks](https://consensys.github.io/Fae/).  Prose documentation for various design aspects of Fae can be found in the [fae-documents](https://github.com/ConsenSys) repository.  As of version 0.9.9.9, there are [docker images](https://hub.docker.com/r/ryancreich/fae/tags/) for the two executables `faeServer` and `postTX` that comprise a playground environment; contact me for access as the repository is private.
