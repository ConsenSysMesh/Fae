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

Smart contracts in Fae operate on a notional *storage* with the following basic
properties.

  - The storage is a filesystem-like tree, each of whose *entries* bears a
    textual *label* and possibly also a `Contract` value.  The sequence of
    labels leading to an entry is its *entry ID*, by which it may be looked up.
    The top-level entry IDs are transaction IDs, and the full entry ID with this
    top level omitted is the entry's *relative path*.

  - A `Contract` type consists of a *result function*, a *combining function*,
    an *evaluation spec*, and an *accumulator value*.  The accumulation
    mechanism allows contracts to maintain a persistent state.  When called with
    an argument, the following occurs:

    - The evaluation spec, which is a function of the argument and the
      accumulator, is executed.  This evaluates any number of other contracts
      (given by entry ID) with various derived arguments.

    - The result of this execution is a structure containing the return values
      of the called contracts.  The combining function is applied to the
      argument, this structure, and the accumulator value, the result of which
      is the new accumulator value.

    - The result function is applied to the accumulator value, evaluating to a
      structure specifying the return value and any new contracts to be created.
      These contracts are given by their paths relative to the relative path of
      the entry containing the called contract. 

  - The storage state is only committed once each transaction terminates.
    During execution, exceptions may occur as ordinary programming errors or
    invalid contract calls.  If that happens, all storage changes are voided and
    the transaction terminates immediately.

Entries have the following structure.  The careful wrapping of functions into
fields of the various data types is important because it forces the contract to
specify *at creation* the precise entry IDs it calls and creates.  This allows
lazy evaluation to avoid forcing the expensive thunks created by the various
functions when traversing the storage tree (see [Block
execution](#block-execution)).

```hs
type Entry = Contracts () Identity

data Contracts accumType m =
  Entry
  {
    contractM :: accumType -> Maybe (m Contract),
    contracts :: Map Text (Contracts accumType)
  }

data Contract =
  forall argType accumType valType.
  Entry
  {
    result :: ContractResult accumType valType,
    combine :: ReturnValues -> argType -> accumType -> accumType,
    evalSpec :: EvalSpec argType accumType,
    accum :: accumType
  }

data ContractResult accumType valType =
  ContractResult
  {
    returnValue :: accumType -> Fae valType,
    newContracts :: Contracts accumType Fae
  }

data ReturnValues =
  ReturnValues
  {
    valM :: forall b. Maybe b,
    subVals :: Map Text ReturnValues
  }

data EvalSpec argType =
  EvalSpec
  {
    argM :: forall a. argType -> accumType -> Maybe a,
    subEvals :: Map Text EvalSpec
  }
```

### Escrow

Fae provides an "internal escrow" facility to enable private or scarce values to
be handled in contract code.  At any time various internal escrow accounts may
be open, each with an *escrow ID* that contains the type of escrowed value but
*not* the value itself.    

  - An escrow account can be created in any `Fae` code (which only occurs in a
    `ContractResult`; see above).  It is an error for an account to remain open
    when a transaction completes.  Escrow IDs are only valid during the
    transaction in which they are created.

  - An escrow account is created with an *access control token*, whose presence
    proves that the caller is authorized to close the escrow account.  An escrow
    account may be *closed* with a token of this type, returning its *private
    value* and invalidating its escrow ID.

  - Each escrow account has a *private value*, its actual contents, and a
    *public value*.  Regardless of access control, the account may be *peeked
    at* to obtain the public value.

```hs
type EscrowID tokType privType pubType

data Escrow tokType privType pubType =
  Escrow
  {
    private :: privType,
    public :: pubType
  }
```

## Virtual machine

The Fae system does not operate within a virtual machine, unlike other smart
contract systems.  Contract code is compiled and executed natively; the
guarantees of the contract system are provided by the Fae runtime that curates
this execution.

Fae also does not impose any restrictions on computational resource usage.  This
is possible because of the lazy evaluation of transactions (see [Block
execution](#block-execution)), which means that no one transaction can force any
participant to waste their time executing it.  Needlessly expensive transactions
will be shunned by other participants, and any participant is free to impose its
own, local restrictions on execution time and space for the purposes of
retrieving specific transaction results.  This does not affect the synchronicity
of the storage across the entire network.

