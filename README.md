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

We provide an [implementation](src/) of this system in Haskell, for which we have included the [haddocks](https://consensys.github.io/Fae/); if you have
permission, you can also read [the white
paper](https://consensys.quip.com/QsIxAHHGHz6W/Functional-principles-of-contract-design),
which explains the rationale for the design from legal, algorithmic, and
security perspectives.
