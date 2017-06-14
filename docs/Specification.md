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
newtype Fee = Fee Integer

data Block =
  Block
  {
    reward :: EntryID,
    txCredit :: Fee,
    txs :: [TXID],
    facets :: [Facet]
  }
```

For `EntryID`, see [Storage](#storage); for `Fee`, see [Fees](#fees).  The
`Facet` data is another simple type (the significance of which is explained in
[Facets](#facets)):

```hs
type FacetID

data Facet =
  Facet
  {
    fee :: Fee,
    facetID :: FacetID,
    depends :: [FacetID]
  }
```

Via the (unmentioned) parent pointer, each block determines a sequence, or
*blockchain*, back to the genesis block; each block therefore implicitly has a
*block number* denoting the length of this sequence, and the concatenation of
the sequence's `blockTXs` entries establishes a linear order on transactions.
The *block size*, the length of `txs`, is defined to be one more than the
previously largest block size, with the effective size of the genesis block set
by the implementation.  Combined with the transaction limit (see
[Transactions](#transactions)) this ensures that a variety of authors may find
space for their transactions in any block.

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
    extra :: [(Text, Text)],
    signature :: Signature
  }
```

The `source` is executed as described in [Smart Contracts](#smart-contracts).
It must define a function `tx`, which takes no arguments (except for reward
transactions) and is allowed to return any type.  The implementation may make
these return values available externally for informational purposes.

The `extra` field is an associative list each entry of which contains module
names and associated files (e.g. Haskell interface files) facilitating the
exposure of an API for the products of the transaction in that language; see
[Source modules](#source-modules).

The `signature` includes both of the other fields.  The `Signature` type must
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
their `tx` function takes one argument, an escrow ID (see [Escrow](#escrow)) for
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
impose size limits on these modules and should also defer storing them
permanently until the transaction itself is known to execute without error.

## Smart contracts

Fae takes the ecumenical perspective that *everything* is a "smart contract".
It provides very little policy on its own; rather, each type of value is
responsible, though its contract code, for enforcing such properties as
double-spending limitations and identity verification.  The Fae system simply
provides the contract author with the necessary environmental data to write
these policies.

### Storage

Smart contracts in Fae operate on a notional *storage* with the following basic
properties.

  - The storage consists entirely of *entries* indexed by their *entry ID*.
    Each entry contains a function of one argument by which it may be evaluated,
    called its *contract*.

  - Entries are almost entirely immutable, being subject only to creation,
    deletion, and argument accumulation.  The latter means that each time the
    entry is called, the argument is combined (using a function provided at
    entry creation) with all previous ones and the result passed to the contract
    function.  This allows a limited form of persistent state, but only for pure
    values, as the Fae API may not be used during accumulation.

  - During entry evaluation, a value may be returned normally, or it may be
    returned as an argument to the `spend` function, which terminates evaluation
    immediately, returning the value and deleting the entry (see [API](#api)).

  - Upon creation, each entry is given a *facet* (see [Facets](#facets))
    restricting when it may be evaluated.  

  - The storage state is only committed once each transaction terminates.
    During execution, exceptions may occur, either from ordinary programming
    errors or computational resource exhaustion.  If that happens, all storage
    changes except for Fee account creation and deletion are voided and the
    transaction terminates immediately (see [Fees](#fees)).

Entries have the following structure:

```hs
data Entry argType accumType valType =
  Entry
  {
    contract :: accumType -> valType,
    combine :: argType -> accumType -> accumType,
    accum :: accumType,
    facet :: FacetID
  }
```

### Escrow

Fae provides an "internal escrow" facility to enable private or scarce values to
be handled in contract code.  At any time various internal escrow accounts may
be open, each with an *escrow ID* that contains the type of escrowed value but
*not* the value itself.    

  - An escrow account can be created in any code.  If an account remains open
    when a transaction terminates, a new entry is automatically created to store
    its value.  It accepts as an argument the signature of the entry ID with the
    transaction key, and returns a new escrow ID for the old account.  The new
    entry's label is the original escrow ID.

  - Escrow IDs are only valid during the transaction in which they are created.

  - An escrow account is created with an *access control token*, which functions
    similarly to a contract's argument except that the token's value is not used
    and not accumulated; only its presence is required.  An escrow account may
    be *closed* with a token of this type, returning its *private value* and
    invalidating its escrow ID.

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

### Facets

Each entry in Fae's storage is created with an immutable `facet` field, which is
a `FacetID`.  Facets have a simple tree structure determined upon their creation
(see [Blocks](#blocks)), where a new facet may *depend* on any existing facets.
Correspondingly, contract execution proceeds through *facet states*, which
restricts the storage operations in the following way:

  - There always exists a facet, which we call facet 0, with no dependencies and
    which is the initial facet for all executions.

  - New entries are created with the same facet as the current facet state.

  - An existing entry may only be evaluated if their facet is the same as the
    current facet state.

  - The facet state may change during *top-level transaction execution* to any
    facet that depends on the current one.

These rules ensure that a network participant may choose to ignore all but a few
interesting facets and their dependencies, and still maintain a fully verifiable
chain of transactions proving the value and expenditure status of any entry in
its storage.

### Fees

Fae allows contract execution to be limited by fees paid in a restricted
currency we call Fee.  A Fee entry, when spent, returns an escrow ID to an
account with no access allowed at all; thus, Fee can only be used for fees, as
follows.

Each facet has an associated fee denominated in the Fee currency, such that:

  - Facet 0, which cannot be explicitly entered during execution, has a fee and
    implicit up-front payment equal to the block's transaction credit.  It is
    only through the refund of this implicit payment that new Fee values are
    obtained.

  - When entering a new facet, an escrow ID must be supplied for a Fee escrow
    account with at least the facet fee as its balance.  This escrow account is
    not closed unless an exception occurs (see below) and can therefore be used
    for subsequent facet changes.

  - If an exception occurs or the computational resources used (see
    [Virtual machine](#virtual-machine)) exceed the facet fee, then the up-front 
    fee's escrow account is closed, the entire facet fee is lost from the
    up-front payment, and the remainder placed immediately in escrow before
    execution terminates (see [Storage](#storage)).

  - Given two facets, one depending on the other, and their fees, the ratio of
    the fees must not exceed a constant value set by the implementation.  This
    discourages the use of a single facet for complex transactions and
    encourages eager use of faceting to enable increasingly specialized
    contracts.

### Virtual machine

Fae does not have a virtual machine like Ethereum, since its execution model
leverages the capabilities of an existing programming language.  However, it
does have a concept of computational resources.  For the moment (this can easily
change) they are measured by compiling the source to the LLVM IR and creating a
custom compiler pass that counts opcodes.  Note that LLVM IR itself is not valid
code for a transaction or contract, because its type system is too weak to
ensure safety of escrowed values; i.e. one can bitcast or type pun a specially
constructed byte array to a private type and place it in escrow to obtain a
forbidden value.  However, any other language having a sufficiently strong type
system, supporting compilation to LLVM IR, and having bindings to the Fae API
can in principle be allowed in contracts.

### API

The following operations are available during execution; other utility functions
may also be provided by the implementation.  Many of the functions return
optional values, which are taken to mean that the value is void if the
conditions of the function's specification are violated.

  - **create:** takes a contract function, an accumulating function, and an
    initial accumulated value as argument and returns an the ID of a new
    entry as per [Storage](#storage).  By default this entry has an empty label;
    see `label` for how to change this.

  - **evaluate:** when called with an entry ID for an existing entry in the
    current facet, and an appropriate argument, evaluates the entry on that
    argument and returns the result.

  - **escrow:** when called with a token value and the pair of a "private"
    value and a function from its type to some other "public" one, creates a new
    escrow account with the indicated access token and private and public values.

  - **peek:** when called with a valid escrow ID, returns the public value in
    escrow.

  - **close:** when called with a valid escrow ID and token of the correct type,
    returns its private value and closes the account.

  - **facet:** when called with the pair of a facet ID and escrow ID for a Fee
    value as an argument, changes the current facet state to that one, if the
    current state is a dependency of the new facet and the Fee balance exceeds
    the facet fee.

  - **signer:** the transaction key of the current transaction.

  - **follow:** takes a transaction ID as argument, returning an optional
    associative array mapping the labels given to each `create` call in the
    transaction to the resulting entry IDs.

  - **label:** this combinator function takes a string, the *label prefix*, and
    any Fae action, returning a new Fae action in which every `create` call made
    in the inner action has the label prefix prepended to its label.

