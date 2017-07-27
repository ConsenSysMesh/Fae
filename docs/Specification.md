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
type ContractID

data Transaction =
  Transaction
  {
    source :: Text,
    inputs :: [(ContractID, Text)],
    extra :: [(Text, Text)],
    signature :: Signature
  }
```

The `source` is executed as described in [Smart Contracts](#smart-contracts).
It must define a value `tx`, whose type is a contract taking no arguments and
maintaining no state.  The contract's return value may be used by the
implementation as a kind of return status for the transaction.

The `inputs` specifies the exact contracts and their arguments that are called
by the transaction; this is the only place that contracts may be called.  The
purpose of this stricture is to prevent the transaction author from maliciously
embedding expensive code in the definition of the entry IDs.  This field must be
given as a list of *literals* in the transaction as it is transmitted throughout
the network.

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
their `tx` contract is run with an additional "input", placed after all the
others, which is an escrow ID (see [Escrow](#escrow)) for a special type
`Reward`, which is defined to have just one value, which is an opaque token.
During execution, this escrow ID is valid and refers to an escrow account
containing a `Reward` value.  Thus, custom currencies may provide funds in
exchange for a `Reward`, social contracts may offer privileges, and so on.

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

Smart contracts in Fae are organized in a three-level storage hierarchy.  The top
level is indexed by transaction IDs, each one containing an object with three
components:

  - The *inputs*, referenced by contract ID;
  - The *direct outputs*, namely, the outputs created by the code of the
    transaction itself, listed in order of creation;
  - The *return value*.

Each input ID stores its own list of direct outputs, which are those created by
the invocation of the input by the transaction.  The various lists of direct
outputs contain the contracts themselves, as descrived in
[Contracts](#contracts).  The return value of the transaction is whatever the
transaction code evaluates to eventually, stored as a dynamically typed value.
Transaction return values may not be referenced from within Fae, and are
intended to be informational.

Note that although transactions are abstractly considered as contracts, they
store the return value and not the contract code, because the code may contain
sensitive computations that must not be replayed; by contrast, actual contracts
(stored as some direct output) store the code and not the return value, because
the code is intended to be called repeatedly, and the latest return value may be
sensitive data that must not be made public.

Each contract is identified by its *contract id*, which is the list of *path
components* leading to the direct output containing the contract.  Thus, every
contract is a two- or three-level path, consisting of a transaction id,
optionally an input contract ID, and a sequence number in the list of direct
outputs.  The optional second component may be abbreviated by its hash to avoid
creating indefinitely long contract IDs.

When a transaction is executed, its input contracts are immediately evaluated
with their given arguments and their entries in the hierarchy populated with
their outputs, in the order that the inputs were given.  The outputs of any
transaction or contract are created dynamically during execution and their
hierarchy entries are created after execution is complete.

Contracts themselves, as stored under their contract ID, contain a piece of
mutable data that is updated in-memory during contract execution and the results
stored back under the same contract ID once execution is complete.

If, during the course of contract execution, an exception occurs, then it
follows that no outputs are created and none of the mutable data is changed.
The execution may not be caught, but terminates the entire transaction that
called the contract as input; its return value is therefore replaced by an
exceptional value that may be caught by any query of transaction return values.

### Contracts

A contract is a function taking one argument and evaluating to a single return
value, maintaining an author-defined state.

Since contracts are stateful, their execution must be sequenced to avoid race
conditions.  This is the primary reason that the format for input contract
arguments is so restricted, because if a contract's evaluation is locked up by a
nonterminating computation, the contract is forever unusable thereafter.

Both the existence of an update to a particular contract, as well as the
*nonexistence* of updates to other contracts, must be known without executing
any user code, so that future transactions may selectively execute just those
contracts in their dependency tree.  The entire information necessary for
resolving state changes to input contracts must be known *without* having to
execute irrelevant and possibly untrusted code.

### Escrow

Fae provides an "escrow" facility to securely pass private values between
contracts.  An escrow account is simply a contract stored within another
contract, which, when evaluated, is deleted.  Escrows are therefore not
maintained as part of the persistent contract state but are specific to a single
contract execution.  Evaluating an escrow is called "closing" it.

Contracts (including escrows) can return escrow contracts to the caller; these
escrow contracts can themselves be endowed with escrows, which are only
available while the containing escrow contract is being evaluated.  It is an
error for any contract evaluation to complete with escrows still open.

Escrows are identified by their *escrow ID*, which is a typed identifier that
records the signature of the contract function.  This allows library functions
in [source modules](#source-modules) to take escrow IDs of a particular
signature in order to manipulate private values as part of the library.

This feature enables complex economic activity:

  - A scarce value can be passed around in contract code by wrapping it in an
    escrow taking as its argument a private token type.  This value can then
    only be directly manipulated by library functions to which the creator of
    the value provided the token.

  - A contract can function as a storefront by using complex escrow contracts to
    clear sales.  The storefront contract accepts an argument identifying the
    product the caller wishes to purchase, and returns an escrow contract to
    sell it.  This escrow contract takes a value in some currency (likely
    itself an escrow), checks it against the price, and if correct, returns a
    simple escrow for the product.  If the payment is insufficient, the selling
    escrow may throw an exception, causing the entire transaction to be rolled
    back, so that the product returns to the storefront.  Since it is also an
    error if the selling escrow is never closed, the buyer cannot maliciously
    sit on the product without either buying or returning it. 

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
values can be passed to the function body of the calling transaction, which in
turn produces another value (the one owned by the calling contract) and possibly
several other owned values, the output contracts.

Ideally, a self-contained fragment of a Fae blockchain could be created by
writing simply a normal functional program, annotating those expressions that
should receive ownership.  As it is impractical to create a new language for
this purpose, the present structure serves the same purpose.

