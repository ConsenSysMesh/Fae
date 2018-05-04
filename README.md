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

We provide an [implementation](src/) of this system in Haskell, for which we have included the [haddocks](https://consensys.github.io/Fae/).  Prose documentation for various design aspects of Fae can be found in the [fae-documents](https://github.com/ConsenSys) repository.  As of version 0.9.9.9, there are [docker images](https://hub.docker.com/r/ryancreich/fae/tags/) for the two executables `faeServer` and `postTX` that comprise a playground environment; contact me for access as the repository is private.

For the curious, the name "Fae" is inspired by an element of the setting of Patrick Rothfuss' *The Kingkiller Chronicles*: an otherworldly realm that periodically verges on the natural world.  This and other metaphorical parallels suggest the relationship between Fae and the physical world, or Fae and Ethereum.  A more direct comparison lies in the acronym "Fae: functional alternative to Ethereum".

# Getting Started

## macOS

#### Install the Haskell compiler

Use Homebrew to install & upgrade [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/):
```
brew install haskell-stack
```

#### Build & start the Fae server

``` 
stack build
```

Create a working directory and start the server:
``` 
mkdir tmp && cd tmp
stack exec faeServer
```

Open your browser to localhost at port 27182:
``` 
http://localhost:27182/
```

You should see a similar response from the server, like so:
``` 
self: 293e4db228f06efe10e8a38c7caf0895eaad7972b6449160cd08bb1cf78b771c
```

