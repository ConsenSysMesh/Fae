# Formal specification of the Fae system

As described in [the README](../README.md), Fae is a functional-style smart
contract system and blockchain.  This document defines its behavior.  Haskell
code samples are provided to clarify the discussion but are not to be taken
literally.

## Blockchain

Fae follows the typical structure of a blockchain in that the actionable data is
in the form of *transactions*, which are sequenced into *blocks* whose right to
exist is somehow established by network consensus.  Here, we do not deal with
the consensus aspect, as this is entirely separate from the actual system.

### Blocks

A block in this system is, essentially, just a list of transactions.  It must
also be wrapped in some sort of token establishing its right to exist, but we do
not deal with that structure, which is part of the consensus protocol.
Likewise, it must point somehow to its parent block, but this is not an
interesting functional detail.  Thus, the payload of the block is a data type
containing only Fae-specific values, represented in Haskell as:

```hs
type EntryID -- some kind of hash
type TXID -- some kind of hash

newtype Block =
  Block
  {
    txs :: [TXID]
  }
```

For `EntryID`, see [Storage](#storage).

Via the (unmentioned) parent pointer, each block determines a sequence, or
*blockchain*, back to the genesis block; each block therefore implicitly has a
*block number* denoting the length of this sequence, and the concatenation of
the sequence's `blockTXs` entries establishes a linear order on transactions.
The *block size*, the length of `txs`, is limited by one more than the
previously largest block size, with the effective size of the genesis block set
by the implementation.  Combined with the transaction limit (see
[Transactions](#transactions)) this ensures that a variety of authors may find
space for their transactions in any block.

#### Block execution

A block is executed by sequentially calling its transactions following the
sequence of all previous transactions in the blockchain.  These calls are
evaluated *lazily*, which entails that any portion of a transaction is actually
fully executed only when its result (return value or contracts created) is
required.  This means that, for a participant that never examines any of these
results, very little code is ever executed, and in general, a participant will
only execute that code that directly impacts the results it is interested in.
In this manner, the Fae system is automatically highly scalable and
parallelizable.

### Transactions

Transactions are not included in blocks directly.  Rather, they are disseminated
separately throughout the network, and block creators assemble the ones they
have received into new blocks referencing them just by ID.  Participants who
execute transactions in a block therefore must look up the entries in its
`txs` list in their own cache.

A transaction is the following data type:

```hs
type Code
type Signature -- Some cryptographic signature type

data Transaction =
  Transaction
  {
    source :: Text,
    calls :: [Text],
    creates :: [Text],
    extra :: [(Text, Text)],
    signature :: Signature
  }
```

The `source` is executed as described in [Smart Contracts](#smart-contracts).
It must define a value `tx`, whose type is analogous to a `Contract` taking no
arguments (see [Storage](#storage)).  The contract's return value may be used by
the implementation as a kind of return status for the transaction.

The `calls` and `creates` fields specify the exact entry IDs that are,
respectively, called and created by the transaction contract.  These IDs are
used to construct the actual `Contract` that is called in the transaction.  The
purpose of this stricture is to prevent the transaction author from maliciously
embedding expensive code in the definition of the entry IDs.  These two fields
must be given as lists of *literals* in the transaction as it is transmitted
throughout the network.

The `extra` field is an associative list each entry of which contains module
names and associated files (e.g. Haskell interface files) facilitating the
exposure of an API for the products of the transaction in that language; see
[Source modules](#source-modules).

The `signature` includes all of the other fields.  The `Signature` type must
support retrieval of the public key from a signature; the one corresponding to
the `signature` is called the *transaction key*.  The implementation should
establish constraints on the acceptable transaction keys that make it difficult
for a bad actor to possess too many at one time, so that the following
restriction has force.

To prevent abuse, the number of transactions in any block that may be signed by
the same transaction key is limited to one more than the number of blocks
already in the chain that contain a transaction signed by that key.

No two transactions in the chain may have the same transaction ID.

### Block rewards

Depending on the consensus algorithm employed by a Fae implementation, it may
make sense to incentivize network nodes to participate in it by providing a
reward for doing so.  For example, coinbase payments in Bitcoin and Ethereum.
Since Fae does not have a native currency, it implements this practice in an
open-ended manner.

According to the policy of the implementation, special *reward* transactions may
be placed in the `txs` list.  These transactions are like any other except that
their `tx` contract takes one argument, an escrow ID (see [Escrow](#escrow)) for
a special type `Reward`, which is defined to have just one value, which is an
opaque token.  During execution, this escrow ID is valid and refers to an escrow
account containing a `Reward` value.  Thus, custom currencies may provide funds
in exchange for a `Reward`, social contracts may offer privileges, and so on.

### Source modules

The `extra` field of a transaction may enclose various source code files as
modules according to the module system of the ambient programming language.
These modules should be stored locally so that any transaction may import them;
the module path should be some combination of the transaction ID, for
disambiguation, and the provided module name.  To prevent abuse, the system may
impose size limits on these modules.

## Smart contracts

Fae takes the ecumenical perspective that *everything* is a "smart contract",
including transactions.  It provides very little policy on its own; rather, each
type of value is responsible, though its contract code, for enforcing such
properties as double-spending limitations and identity verification.  The Fae
system simply provides the contract author with the necessary environmental data
to write these policies.

### Storage

Smart contracts in Fae operate on a notional *storage* consiting of the
following three parts:

  1. [*Transactions.*](#transactions) Each transaction is stored under its
     transaction ID.  The corresponding value is the transaction return value,
     or an exception, if it threw one.  Transactions are read-only; although
     transactions are provided as contracts, these contracts may not be
     re-executed.

  2. [*Contracts.*](#contracts) All the contracts created in the course of
     executing a transaction are stored by their contract IDs.  The contract
     is the value corresponding to its key; each contract has a mutable state
     and, therefore, potentially different return values upon invocation.

  3. [*Escrows.*](#escrow) Contracts may programmatically open escrow
     accounts, which are stored globally by their escrow ID.  Escrow IDs may
     not be supplied as literals but only passed around as runtime values,
     which ensures that lazy contract execution is not forced by closing an
     escrow account, as the contracts that reference it must have already been
     executed anyway.

Transactions are simply a special kind of contract, and are evaluated as soon as
they are sequenced into the blockchain.  When any contract is evaluated (as
opposed to executed; see [Block execution](#block-execution)), its input
contracts are updated lazily and its output contracts created lazily in storage.
The execution of return values, output contracts, escrow openings and closings,
and accumulator updates (see below) are linked, in that each one forces all the
others.  If during the course of this execution any exception occurs, the escrow
changes and accumulator update are discarded, and the return values and output
contracts are replaced by the raised exception.

### Contracts

A contract (and a transaction) is a function that takes an argument and computes
a return value, possibly with the side effect of invoking or creating other
contracts or escrows.  

The contracts (but not the escrows) that it invokes must be enumerated with
their arguments at the time of the caller's creation; likewise, the contracts
that it creates must be enumerated, though their contents may be programmatic.
This restriction is imposed to ensure that contracts may be executed lazily:

  - For output contracts, it is necessary to know exactly the contract IDs that
    are created, even for transactions that are not otherwise executed, so that
    their existence is known to later transactions that invoke them.
    
  - For input contracts, not only the existence of an update to a particular
    contract must be known in advance, but also the *nonexistence* of updates to
    other contracts, so that future transactions may selectively execute just
    those contracts in their dependency tree.

    There is also a security detail.  The arguments cannot be computed by the
    calling contract because to allow untrusted computation to intervene in
    contract invocation exposes the entire system to crippling denial-of-service
    attacks where transactions spuriously invoke contracts via a nonterminating
    computation, thus blocking them from ever being used by another transaction.

Contracts have an internal state called the *accumulator* that they may
update.  When a contract is invoked, the following sequence is executed:

  1. Each of its input contracts is invoked with its supplied argument and the
     return values made available.  The tuple of the argument, the accumulator,
     and these return values is called the *data*.

  2. Each of its output contracts is created as a lazy function of the data.

  3. The return value is computed as a lazy function of the data.

  4. The accumulator is recomputed as a lazy function of the data.

Each of these lazy functions is part of the contract definition.

#### Contract philosophy

These restrictions mean that one should design contracts according to the
following principles:

  - *Single responsibility.* Contracts are effectively functions from their
    argument to their return value; the input contracts are merely "advisory".
    Chained evaluation of other contracts is severely limited, and therefore,
    contracts should really be considered as *contracts*, namely, agreements to
    provide particular results for a particular request.  The role of the input
    contracts is limited to obtaining various rights and then building a derived
    asset from them.

  - *Pull rewards.* Since contracts cannot invoke a computed set of other
    contracts, it is not possible to create a contract that, say, accepts
    several contract IDs in successive calls and then invokes them when it is
    "full".  Instead, one should view these dependent invocations as *rewards*,
    and these rewards should be provided via the return values of the original
    calls, dependent on the contract becoming full before they can be claimed.
    Effectively, the contract is a complex valued asset to which various parties
    obtain rights as it is called and, possibly, its state changes.

  - *No library contracts.* If in the above scenario the dependent invocations
    are truly required as function calls and not rights to an asset, the
    contracts thus invoked are misconceived: their true purpose is actually as
    *library functions*.  This kind of function should be part of a source
    module instead; functions in a source module are no less trustworthy than
    contracts, since the modules are synchronized just like the storage state,
    and can therefore be understood as a kind of meta-contract that conveys no
    value.

### Escrow

Fae provides an "internal escrow" facility to enable private or scarce values to
be handled in contract code.  At any time various internal escrow accounts may
be open, each with an *escrow ID* that contains the type of escrowed value but
*not* the value itself.    

  - An escrow account can be created in the course of computing a contract's
    return value, producing an escrow ID.  Passing this ID around via contract
    or function arguments is the only way it can be communicated; escrow IDs
    cannot be computed or supplied as literals, and escrows cannot be called
    like contracts.

  - Each escrow account has a *private value*, its actual contents, and a
    *public value*.  Regardless of access control, the account may be *peeked
    at* to obtain the public value.

  - An escrow account is created with an *access control token*, whose presence
    proves that the caller is authorized to *close* the escrow account.  Closing
    an escrow account returns its private value and invalidates its escrow ID.

  - Escrow IDs are typed to include public, private, and token types, so that
    functions may express precisely through the type of the escrow ID what kind
    of escrowed value they accept.

## Virtual machine

The Fae system does not operate within a virtual machine, unlike other smart
contract systems.  Contract code is compiled and executed natively; the
guarantees of the contract system are provided by the Fae runtime that curates
this execution.

Fae also does not impose any restrictions on computational resource usage.  This
is possible because of the lazy evaluation of transactions (see [Block
execution](#block-execution)), which means that no one contract (or transaction)
can force any participant to waste their time executing it.  Needlessly
expensive contracts will be shunned by other participants, and any participant
is free to impose its own, local restrictions on execution time and space for
the purposes of retrieving specific transaction results.  This does not affect
the synchronicity of the storage across the entire network.

