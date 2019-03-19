# Fae, a functional smart contract system

Fae is a smart contract system and blockchain definition with equivalent power
to Ethereum but with the following additional benefits:

  - Transaction execution is parallelizable and each network node need only
    execute transactions it wants.

  - There is no virtual machine, but rather a minimal execution environment that
    can run an existing, general-purpose programming language.

  - There is no native currency, only a simple token awarded to the participants
    that mine blocks.

  - Scarcity policy is left to the discretion of each smart contract.

We provide an [implementation](src/) of this system in Haskell, for which we have included the [haddocks](https://consensys.github.io/Fae/).  Prose documentation for various design aspects of Fae can be found in the [documentation](documentation) directory as HTML files.  As of version 2.0.0.0, there are [docker images](https://cloud.docker.com/u/teamfae/repository/list) for the two executables `faeServer` and `postTX` that comprise a playground environment.

For the curious, the name "Fae" is inspired by an element of the setting of Patrick Rothfuss' *The Kingkiller Chronicles*: an otherworldly realm that periodically verges on the natural world.  This and other metaphorical parallels suggest the relationship between Fae and the physical world, or Fae and Ethereum.  A more direct comparison lies in the acronym "Fae: functional alternative to Ethereum".

