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

Smart contracts in Fae occupy positions in a filesystem-like storage: each has a
unique hash identifier called the *basename* that is contained in a *path*,
which specifies the call stack leading to the creation of the basenamed
contract.  The pair of the path and basename is the *contract ID*; the path
components are themselves contract IDs.  

Transactions are simply contracts at the top level of the storage; each output
of a contract is placed with its basename under the path of its parent; and each
input initiates a new path component at its contract ID.  As contracts,
transations are treated specially and are not themselves stored, but rather just
their return value, so the transaction cannot be replayed.

Immediately upon evaluation (as opposed to [execution](#block-execution)) of any
contract, the path components of its inputs are created and the inputs
themselves are lazily evaluated.  Upon complete execution of the contract, its
outputs are assigned basenames and stored under them.

Contracts themselves, as stored under their contract ID, contain two pieces of
mutable data: the escrows and the state.  These are updated in-memory during
contract execution and the results stored back under the same contract ID once
execution is complete.  Contract execution also produces a return value, but
this is not stored, as it may contain private information.

If, during the course of contract execution, an exception occurs, then it
follows that no outputs are created and none of the mutable data is changed.
Exceptions cannot be caught in a contract but rather terminate all contract
executions in the path above the contract that first raised them.

### Contracts

A contract (and a transaction) is a function that takes an argument and computes
a return value, possibly with the side effect of invoking or creating other
contracts or escrows.  A contract may maintain a nontrivial mutable state, and
therefore the executions of these contracts must be sequenced in order for the
state updates to avoid race conditions, which entails some restrictions on their
operation for security reasons.

The contracts (but not the escrows) that it invokes must be enumerated with
their arguments at the time of the caller's creation; by contrast, output
contracts can be created programmatically during execution.  This restriction is
imposed to ensure that contracts may be executed lazily, for the following
reason.

Both the existence of an update to a particular contract, as well as the
*nonexistence* of updates to other contracts, must be known in advance, so that
future transactions may selectively execute just those contracts in their
dependency tree.  The entire information necessary for resolving state changes
to input contracts must be known *without* having to execute irrelevant and
possibly untrusted code.

There is also a security detail.  The arguments cannot be computed by the
calling contract because to allow untrusted computation to intervene in contract
invocation exposes the entire system to crippling denial-of-service attacks
where transactions spuriously invoke contracts via a nonterminating computation,
thus blocking them from ever being used by another transaction.

Because a contract cannot directly accept variable arguments, some operations
must span several transactions in which the first calls some contracts to obtain
resources, then creates outputs that use these resources in their own inputs,
and so on.  The outputs must, of course, be called in later transactions.  This
scheme has the aspect of a "confirmation" mechanism that proves that the caller
does, in fact, possess the resources before attempting to use them.  Possession
is demonstrated by *complete and successful* execution of the contract, and the
burden of this execution must fall on the transaction that attempts to obtain
the resource, and not on the contract that will receive it.

### Escrow

Fae provides an "internal escrow" facility to enable private or scarce values to
be handled in contract code.  At any time various internal escrow accounts may
be open, each with an *escrow ID* that contains the type of escrowed value but
*not* the value itself.    

  - Escrow accounts are private to the contract that created them, and escrow
    IDs cannot be (meaningfully) directly passed as arguments to input
    contracts.

  - Each escrow account has a *private value*, its actual contents, and a
    *public value*.  Regardless of access control, the account may be *peeked
    at* to obtain the public value.

  - An escrow account is created with an *access control token*, whose presence
    proves that the caller is authorized to *close* the escrow account.  Closing
    an escrow account returns its private value and invalidates its escrow ID.

  - Escrow IDs are typed to include public, private, and token types, so that
    functions may express precisely through the type of the escrow ID what kind
    of escrowed value they accept.

Although escrows are contract-specific, there is a facility to transfer their
contents from a contract to one of its outputs.  Doing so causes the parent
contract to close its escrow and the output contract to open a new one with the
same contents.  Thus, responsibility for the private value contained in the
escrow rests with the contract that owns the escrow ID.

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

### The lambda contract calculus

The structure of a Fae blockchain is essentially that of the lambda calculus,
i.e. that of functional programming.  At the highest level of abstraction, the
continuing operation of a Fae client is simply applying multivariate functions
to previously constructed values and constructing other values that feed into
the next iteration of this cycle.  This structure is adapted to the adversarial
situation of a blockchain by assigning *ownership* to some of the intermediate
values; these are the contracts.  Such values can only be used by proving
ownership via supplied arguments to the contracts.  Once this is done, those
values can be passed to the function body of the calling contract, which in turn
produces another value (the one owned by the calling contract) and possibly
several other owned values, the output contracts.

Ideally, a self-contained fragment of a Fae blockchain could be created by
writing simply a normal functional program, annotating those expressions that
should receive ownership.  As it is impractical to create a new language for
this purpose, the present structure serves the same purpose.
