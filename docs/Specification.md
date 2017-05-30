= Formal specification of the Fae system =

As described in [../README.md], Fae is a functional-style smart contract system
and blockchain.  This document defines its behavior.

== Blockchain ==

Fae follows the typical structure of a blockchain in that the actionable data is
in the form of *transactions*, which are sequenced into *blocks* whose right to
exist is somehow established by network consensus.  Here, we do not deal with
the consensus aspect, as this is entirely separate from the actual system.

=== Blocks ===

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

For `EntryID`, see [#storage]; for `Fee`, see [#fees].  The `Facet` data is
another simple type (the significance of which is explained in [#facets]):

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
[#transactions]) this ensures that a variety of authors may find space for their
transactions in any block.

=== Block rewards ===

Depending on the consensus algorithm employed by a Fae implementation, it may
make sense to incentivize network nodes to participate in it by providing a
reward for doing so.  For example, coinbase payments in Bitcoin and Ethereum.
Since Fae does not have a native currency, it implements this practice in an
open-ended manner.

According to the policy of the implementation, special *reward* transactions may
be placed in the `txs` list.  These transactions are like any other except that
their `tx` function takes one argument, an escrow ID (see [#escrow]) for a
special type `Reward`, which is defined to have just one value, which is an
opaque token.  During execution, this escrow ID is valid and refers to an escrow
account containing a `Reward` value.  Thus, custom currencies may provide funds
in exchange for a `Reward`, social contracts may offer privileges, and so on.

=== Transactions ===

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

The `source` is executed as described in [#smart contracts].  It must define a
function `tx`, which takes no arguments (except for reward transactions) and is
allowed to return any type.  The implementation may make these return values
available externally for informational purposes.

The `extra` field is an associative list each entry of which contains filenames
and associated files (e.g. Haskell interface files) facilitating the exposure of
an API for the products of the transaction in that language.

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

== Smart contracts ==

Fae takes the ecumenical perspective that *everything* is a "smart contract".
It provides very little policy on its own; rather, each type of value is
responsible, though its contract code, for enforcing such properties as
double-spending limitations and identity verification.  The Fae system simply
provides the contract author with the necessary environmental data to write
these policies.

=== Storage ===

Smart contracts in Fae operate on a notional *storage* with the following basic
properties:

  - The storage consists entirely of *entries* indexed by their *entry ID*.
    Each entry contains a function of one argument by which it may be evaluated
    (see [#execution]).

  - Entries contain a mutable field, `escrow`, which stores the value currently
    in escrow, as described in [#execution], and the facet in which it was
    originally returned.

  - Entries contain a mutable field, `userIDs`, which determines which other
    entries are allowed to close their escrow account.  If there are no
    restrictions, then the entry is considered to be *spent*; otherwise, it is
    *unspent*.

Entries have the following structure, whose additional fields are explained in
the following sections.

```hs
data AccessList = All | Only [EntryID]

data Entry argType privType pubType =
  Entry
  {
    contract :: argType -> privType,
    public :: privType -> pubType,
    userIDs :: AccessList,
    escrow :: Maybe (privType, FacetID),
    facet :: FacetID
  }
```

=== Escrow ===

Fae provides an "internal escrow" facility for dealing with unspent quantities.
At any time various internal escrow accounts may be open, each with an *escrow
ID* that contains the type of escrowed value but *not* the value itself.    

  - According to the limitations of the `userIDs` field, this escrow account may
    be *closed*, returning its contents.  

  - Regardless of limitations, the escrow may be *peeked at*, which does not
    close the account but also applies the `public` function of the entry to the
    value in it.

  - If, somehow, the same entry is twice evaluated unspent without closing its
    first escrow, the old value is replaced with the new.  

  - An entry's escrow ID can be obtained from the entry ID in the entry's facet
    (see [#facets]).

  - Other internal escrow accounts may exist independently of storage entries,
    for example the `Reward` escrow from [#blockrewards].  Fae provides several
    methods for applying harmless structural functions to escrow values, placing
    the results in anonymous internal escrow.  Since these escrow accounts are
    not associated with entries, they do not persist after the end of the current
    transaction.

=== Execution ===

The execution policy concerns both transaction execution and contract
fulfillment.  When an entry is looked up by its ID, it must be fully evaluated,
and this evaluation is equivalent to executing its `contract` function on the
given argument.  Execution has the following specification:

  - During entry evaluation, a function `access` is available, taking one
    argument and setting `userIDs` to that argument for the currently-evaluated
    contract.  Thus, contracts may establish their own spending policy.

  - When an entry is evaluated, the actual data type returned depends on whether
    the entry has been spent.  If not, then the `contract` field's return value
    is placed in escrow, and the escrow ID is returned; if it has, then this
    return value has the function in the entry's `public` field applied and the
    result returned as-is.  Contracts requiring unspent values as arguments may
    therefore take escrow IDs of the correct types, ensuring that those values
    may not be spent twice.

  - Execution may throw exceptions and thus exit early.  These exceptions cannot
    be caught within contract code but, rather, percolate up through the entire
    call stack and terminate the transaction execution.

=== Facets ===

Each entry in Fae's storage is created with an immutable `facet` field, which is
a `FacetID`.  Facets have a simple tree structure determined upon their creation
(see [#blocks]), where a new facet may *depend* on any existing facets.
Correspondingly, contract execution proceeds through *facet states*, which
restricts the storage operations in the following way:

  - There always exists a facet, which we call facet 0, with no dependencies and
    which is the initial facet for all executions.

  - New entries may only be created in the same facet as the current facet
    states.

  - An existing entry may be evaluated only if its facet is the current facet
    state.

  - An entry's escrow ID may only be obtained in the same facet that the escrow
    contents were returned.

  - The facet state may change during contract execution only to a facet that
    depends on the current one.

These rules ensure that a network participant may choose to ignore all but a few
interesting facets and their dependencies, and still maintain a fully verifiable
chain of transactions proving the value and expenditure status of any entry in
its storage.

=== Fees ===

Fae allows contract execution to be limited by fees paid in a restricted
currency we call Fee.  Unspent Fee entries have an empty `userIDs` access list,
and therefore can *only* be spent on fees, as described below.

Each facet has an associated fee denominated in the Fee currency, such that:

  - When entering a new facet, an escrow ID must be supplied for a Fee escrow
    account with at least the facet fee as its balance, as well as a boolean
    function taking two arguments, the first of which is an entry ID; this is
    the *authentication function*.  

  - The authentication function is used to create *unspent* entries containing
    Fee values.  Such an entry's contract function takes one argument and, when
    evaluated, applies the authentication function to that argument and the
    entry ID, throwing an exception if the result is `False`.

  - Facet 0, which cannot be explicitly entered during execution, has a fee and
    implicit up-front payment equal to the block's transaction credit.  Its
    authentication function is "signature validation of the entry ID against
    the transaction key". (See [#transactions].)

  - If the computational resources used exceed the facet fee, then the entire
    facet fee is lost, and computation terminates with an exception.  Before
    termination, if the up-front fee was greater than the facet fee, the
    remainder is placed in a new entry in the facet that was entered, using
    the authentication function.

  - Otherwise, the entire fee is refunded to a new entry in the facet that was
    entered, using the authentication function.

=== API ===

The following operations are available during execution; other utility functions
may also be provided by the implementation.  If any constraints in the
descriptions are violated, or if there is a type error, an exception is thrown.

  - **create:** when called with arguments appropriate to the `userIDs`,
    `contract` and `public` fields of an `Entry`, creates a new entry in storage
    in the current facet, having these arguments for those fields.  It returns
    the ID of this new entry.

  - **evaluate:** when called with an entry ID for an existing entry in the
    current facet, and an appropriate argument, evaluates the entry on that
    argument as described in [#execution] and returns the result.

  - **escrow:** when called with an entry ID for an existing entry whose
    associated facet is the current facet, returns the escrow ID for that entry.

  - **access:** when called with arguments appropriate to a `userIDs` field, sets
    that field in the entry currently being evaluated if the current facet is
    the same as the entry facet.  Throws an exception in top-level transaction
    code.

  - **signer:** the transaction key of the current transaction.

  - **peek:** when called with a valid escrow ID, returns the `public`
    version of the value in escrow.

  - **close:** when called with a valid escrow ID, if the currently evaluated
    entry is in the `userIDs` field of the entry that was evaluated into escrow,
    returns the `private` version of the value in escrow and closes the account.

  - **facet:** when called with a facet ID as an argument, changes the current
    facet state to that one, if the current state is a dependency of the new
    facet.  Returns nothing.

  - **throw:** manually throws an exception.

