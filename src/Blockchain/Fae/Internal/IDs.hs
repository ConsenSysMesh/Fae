module Blockchain.Fae.Internal.IDs where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens hiding (from, to)

import Control.Applicative
import Control.DeepSeq

import qualified Data.Serialize as Ser
import Data.Traversable
import Data.Typeable
import Data.Void

import GHC.Generics

import Numeric.Natural

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

{- Types -}

-- | This identifier locates a contract in storage.  It is not intended to
-- be used in contract code, as indeed, a contract can never be called
-- explicitly but only as a transaction input, for which there is a special
-- syntax outside Haskell.
data ContractID =
  JustTransaction TransactionID |
  TransactionOutput TransactionID Int |
  InputOutput TransactionID ShortContractID Int
  deriving (Show, Generic)

-- | The hash of a 'ContractID', useful for abbreviating what would
-- otherwise be unboundedly long chains of contracts that are outputs of
-- contracts that are outputs of ... that are outputs of some long-ago
-- transaction.
newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Serialize)

type TransactionID = ShortContractID -- ^ For simplicity
type BlockID = Digest -- ^ For simplicity

type EntryID = Digest

-- | This identifier locates an escrow.  Escrow IDs are assigned when the
-- escrow is first created and are guaranteed to be globally unique and
-- immutable.  Each escrow ID is valid only within a contract or other
-- escrow that actually holds the escrow, and the type parameters must
-- correspond to the escrow's actual argument and value types.  Escrow IDs
-- may be constructed by the 'newEscrow' function or from string literals.
-- However, they should appear type-correct in contract signatures to
-- formally verify that the contract receives and returns a particular kind
-- of opaque value, e.g. a currency.

-- The last two constructors identify an escrow that is called
-- "transactionally".  Transactional escrow calls are supplied with their
-- argument, then evaluated /in the context of the caller/ when they are
-- returned from a contract.  So a transactional escrow call can be given
-- an escrow ID as an argument, referring to an escrow that is only valid
-- in the contract to which it is returned.  This is how payments are
-- accepted.
data EscrowID argType valType = 
  EscrowID { entID :: EntryID } |
  TXEscrowIn { entID :: EntryID, eArg :: argType } |
  TXEscrowOut { entID :: EntryID, eVal :: valType }
  deriving (Generic)
-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.
data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

{- Typeclasses -}

-- | A map of escrow IDs that preserves input and output types, regardless
-- of what they are.
type EscrowIDMap f =
  forall argType valType. 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  EscrowID argType valType -> f (EscrowID argType valType)

-- | The type of a traversal by an 'EscrowIDMap', used in 'HasEscrowIDs'.
type EscrowIDTraversal a = forall f. (Applicative f) => EscrowIDMap f -> a -> f a

-- | Every contract must accept arguments and return values in this class.
-- The returned traversal /must/ contain, in any order, the IDs of every
-- escrow upon which the type 'a' depends for its value.  These escrows
-- will be transferred along with a value of type 'a' whenever it is
-- returned from a contract.  One usually need not define this class
-- explicitly, as suitable general instances are given.
class HasEscrowIDs a where
  traverseEscrowIDs :: EscrowIDTraversal a
  default 
    traverseEscrowIDs :: 
      (Generic a, GHasEscrowIDs (Rep a)) => 
      EscrowIDTraversal a
  traverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

class GHasEscrowIDs f where
  gTraverseEscrowIDs :: EscrowIDTraversal (f p)

{- Instances -}

instance (NFData argType, NFData valType) => NFData (EscrowID argType valType)

instance Serialize ContractID
instance Digestible ContractID
instance Digestible ShortContractID

instance Read ShortContractID where
  readsPrec _ = fmap (_1 %~ ShortContractID) . readsPrec 0

instance Show ShortContractID where
  show (ShortContractID dig) = show dig

instance Read (EscrowID argType valType) where
  readsPrec _ = fmap (_1 %~ EscrowID) . readsPrec 0

instance Show (EscrowID argType valType) where
  show = show . entID
  
instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  traverseEscrowIDs _ = pure

instance {-# OVERLAPPABLE #-} 
  (Traversable f, HasEscrowIDs a) => HasEscrowIDs (f a) where

  traverseEscrowIDs g = traverse (traverseEscrowIDs g)

instance HasEscrowIDs Void 
instance (HasEscrowIDs a) => HasEscrowIDs (Maybe a)
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (Either a b)
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b)

instance 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  HasEscrowIDs (EscrowID argType valType) where

  -- Not 'id'; we need to specialize the forall.
  traverseEscrowIDs f x = f x

-- Boring Generic boilerplate

instance GHasEscrowIDs V1 where
  gTraverseEscrowIDs _ = pure

instance GHasEscrowIDs U1 where
  gTraverseEscrowIDs _ = pure

instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :+: g) where
  gTraverseEscrowIDs h = \case
    L1 x -> L1 <$> gTraverseEscrowIDs h x
    R1 x -> R1 <$> gTraverseEscrowIDs h x

instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :*: g) where
  gTraverseEscrowIDs h (x :*: y) = 
    liftA2 (:*:) (gTraverseEscrowIDs h x) (gTraverseEscrowIDs h y)

instance (HasEscrowIDs c) => GHasEscrowIDs (K1 i c) where
  gTraverseEscrowIDs f (K1 x) = K1 <$> traverseEscrowIDs f x

instance (GHasEscrowIDs f) => GHasEscrowIDs (M1 i t f) where
  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs g x

{- Functions -}

-- | Take the hash of a contract ID.
shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

-- | Request an escrow transaction with the given argument.  An escrow
-- transaction is a deferred escrow call that is actually performed /after/
-- the result of `escrowTX` is passed in an argument or returned as
-- a value, in the context of the newly initiated transaction, contract, or
-- escrow execution.
--
-- This is the mechanism by which a contract may accept programmatic values
-- as arguments: these values, which should be `Read`able, can be passed as
-- literals to the contract as a transaction input, then applied to an
-- escrow transaction that is executed in the calling transaction.  Any
-- escrow IDs contained in these arguments that are valid in the calling
-- transaction can therefore denote value passed to the escrow transaction.
-- The escrow can then scrutinize these values, perhaps creating new
-- contracts to provide them to the (human) creator of the escrow, in
-- exchange for some goods.
--
-- The calling transaction cannot inject its own code (via escrows) into
-- the input contract that makes the escrow transaction, and it cannot
-- interject its own code between the input's return and the actual escrow
-- call.
escrowTX :: EscrowID argType valType -> argType -> EscrowID argType valType
escrowTX = TXEscrowIn . entID

-- | Get the result of an escrow transaction.  This function is intended to
-- be used after the escrow ID is returned and the escrow transaction
-- performed; the ID will be available either as an escrow argument or
-- a return value, depending on how it was passed, and this function
-- extracts the result of the transaction from that ID.
escrowTXResult :: EscrowID argType valType -> valType
escrowTXResult (TXEscrowOut _ x) = x
-- This is to avoid having to import Exceptions, which would be a cyclical
-- dependency
escrowTXResult _ = error "NotTXEscrowOut"

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue

