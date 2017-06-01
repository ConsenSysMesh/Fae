# Fae: a functional alternative to Ethereum

Fae is a smart contract system and blockchain definition with equivalent power
to Ethereum but with the following additional benefits:

  - It is inherently scalable in that a participant's storage requirements need
    not grow at the same speed as the blockchain.

  - Smart contracts are immutable and stateless, preventing errors of reentrancy.

  - Immutability and statelessness equate to a UTXO-based blockchain.

  - There is no virtual machine, but rather a minimal execution environment that
    can run an existing, general-purpose programming language.

  - There is no native currency, only a simple token awarded to the participants
    that mine blocks.

  - Scarcity policy is left to the discretion of each smart contract.

We provide a formal specification of the system in [docs/Specification.md] and
an implementation in Haskell in the `src` subdirectory.
