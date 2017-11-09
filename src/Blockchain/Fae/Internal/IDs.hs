{- |
Module: Blockchain.Fae.Internal.IDs
Description: Identifier types and associated functions
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

There are several identifier types in Fae: two kinds of contract ID (one detailed, one convenient but lossy), transaction IDs, and escrow IDs.  In addition, the escrow IDs in particular have a lot of ritual surrounding them.
-}
{-# LANGUAGE DataKinds #-}
module Blockchain.Fae.Internal.IDs where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens hiding (from, to)

import Control.Applicative
import Control.DeepSeq

import Control.Monad.Writer

import Data.Char
import Data.Functor.Identity
import qualified Data.Serialize as Ser
import Data.List
import Data.String
import Data.Traversable
import Data.Typeable
import Data.Void

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

import Text.ParserCombinators.ReadP

-- * Types

-- | This identifier locates a contract in storage.  It is not intended to
-- be used in contract code, as indeed, a contract can never be called
-- explicitly but only as a transaction input, for which there is a special
-- syntax outside Haskell.
data ContractID =
  -- | Technically, a transaction is a type of contract, but this kind of
  -- contract ID is basically only for errors.
  JustTransaction TransactionID |
  -- | One way contracts can be created is by being directly output by
  -- a transaction.  The outputs are indexed from 0 in order of creation.
  TransactionOutput TransactionID Int |
  -- | The other way contracts can be created is by being output during the
  -- execution of one of its input contract calls.  The outputs are indexed
  -- from 0 in order of creation, with the indexing specific to each input
  -- contract.
  InputOutput TransactionID ShortContractID Int
  deriving (Read, Show, Generic)

-- | The hash of a 'ContractID', useful for abbreviating what would
-- otherwise be unboundedly long chains of contracts that are outputs of
-- contracts that are outputs of ... that are outputs of some long-ago
-- transaction.
newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Serialize, IsString, NFData)

-- | For simplicity
type TransactionID = ShortContractID
-- | For simplicity
type BlockID = Digest

-- | For simplicity
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

-- The second and third constructors identify an escrow that is called
-- "transactionally".  Transactional escrow calls are supplied with their
-- argument, then evaluated /in the context of the caller/ when they are
-- returned from a contract.  So a transactional escrow call can be given
-- an escrow ID as an argument, referring to an escrow that is only valid
-- in the contract to which it is returned.  This is how payments are
-- accepted.
data EscrowID argType valType = 
  -- | A basic escrow ID, just referring to an entry in the Map of escrows.
  EscrowID { entID :: EntryID } |
  -- | Describes a transactional escrow call with its argument.
  TXEscrowIn { entID :: EntryID, eArg :: argType } |
  -- | Describes the result of a transactional escrow call.
  TXEscrowOut { entID :: EntryID, eVal :: valType } |
  -- | Locates an escrow from within a contract's output, using syntax
  -- similar to iterated record locators.
  EscrowLocator { path :: [String] }
  deriving (Generic)
-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.
data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

-- | Exceptions for ID-related errors.
data IDException =
  BadInputEscrow String |
  NotEscrowOut EntryID |
  UnresolvedEscrowLocator [String]
  deriving (Typeable, Show)

-- | A map of escrow IDs that preserves input and output types, regardless
-- of what they are.
type EscrowIDMap f =
  forall argType valType. 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  EscrowID argType valType -> f (EscrowID argType valType)

-- | The same as 'EscrowIDMap', but also takes an escrow locator.
type IndexedEscrowIDMap f =
  forall argType valType. 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  [String] -> EscrowID argType valType -> f (EscrowID argType valType)

-- | The type of a traversal by an 'EscrowIDMap', used in 'HasEscrowIDs'.
-- Note that because the kind of traversal map that is allowed is subject
-- to some Fae-specific constraints, this is a little different from
-- a @lens@ 'Traversal'.
type EscrowIDTraversal a = 
  forall f. (Monad f) => EscrowIDMap f -> a -> f a

-- | The type of a traversal by an 'IndexedEscrowIDMap', used in 'HasEscrowIDs'.
-- Note that because the kind of traversal map that is allowed is subject
-- to some Fae-specific constraints, this is a little different from
-- a @lens@ 'IndexedTraversal'.
type IndexedEscrowIDTraversal a = 
  forall f. (Monad f) => IndexedEscrowIDMap f -> a -> f a

-- * Typeclasses

-- | Every contract must accept arguments and return values in this class.
-- The returned traversal /must/ contain, in any order, the IDs of every
-- escrow upon which the type 'a' depends for its value.  These escrows
-- will be transferred along with a value of type 'a' whenever it is
-- returned from a contract.  
--
-- Although this class must be instantiated for any user-defined types used
-- in contracts, we do not export the class members, so that only the
-- default instance may be used.
class HasEscrowIDs a where
  -- | Like 'traverse' from 'Traversable', except that it only covers the
  -- escrow IDs, which may be of heterogeneous types.
  traverseEscrowIDs :: EscrowIDTraversal a
  traverseEscrowIDs f = iTraverseEscrowIDs (const f)

  -- | Traverses the escrow IDs, providing the traversal function with the
  -- path of each one.
  iTraverseEscrowIDs :: IndexedEscrowIDTraversal a
  default 
    iTraverseEscrowIDs :: 
      (Generic a, GHasEscrowIDs (Rep a)) => 
      IndexedEscrowIDTraversal a
  iTraverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

-- | Generic backend for 'HasEscrowIDs'
class GHasEscrowIDs f where
  gTraverseEscrowIDs :: IndexedEscrowIDTraversal (f p)

-- * Instances

-- | Of course
instance Exception IDException

-- | We want to force escrow IDs before they leave a contract, so that any
-- malicious values fall on the creator and not the recipient.
instance (NFData argType, NFData valType) => NFData (EscrowID argType valType)

-- | Just so we can get 'Digestible'
instance Serialize ContractID
-- | So we can get a 'ShortContractID' from a regular one.
instance Digestible ContractID
-- | For forcing parsed inputs before they go to the interpreter.
instance NFData ContractID

--instance Digestible ShortContractID

-- | 'ShortContractID's and, by extension, 'TransactionIDs', are read as
-- the digests they wrap.
instance Read ShortContractID where
  readsPrec _ = fmap (_1 %~ ShortContractID) . readsPrec 0

-- | 'ShortContractID's and, by extension, 'TransactionIDs', show as hex
-- strings.  This should be inverse to the 'Read' instance.
instance Show ShortContractID where
  show (ShortContractID dig) = show dig

-- | Escrow IDs should only be read from input contract arguments.  Since
-- the only escrow IDs that are valid in the input contract calls are the
-- ones returned by previous calls in the same transaction, they can all be
-- located by descending into those return values.  This is much more
-- stable and convenient than describing the escrow IDs as hex strings.
instance Read (EscrowID argType valType) where
  readsPrec s = readP_to_S $ EscrowLocator <$> sepBy1 pathStr pathSep  
    where  
      pathStr = munch1 (not . (\c -> c == '.' || isSpace c))
      pathSep = skipSpaces >> char '.' >> skipSpaces

-- | In addition to displaying the contents of the escrow ID, 'show' should
-- also display the /type/, which is potentially an important diagnostic.
instance (Typeable argType, Typeable valType) => Show (EscrowID argType valType) where
  show eID@EscrowLocator{..} = 
    "EscrowLocator " ++ 
    intercalate "." path ++
    " :: " ++ show (typeOf eID)
  show eID = show (entID eID) ++ " :: " ++ show (typeOf eID)
  
-- | This default instance allows casual transaction authors to use basic,
-- non-escrow-backed types without any hoopla.
instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  iTraverseEscrowIDs _ = pure

-- | This is just natural, though it can probably be covered in most
-- practical cases by the 'Generic' instance, if probably slower.
instance {-# OVERLAPPABLE #-} 
  (Traversable f, HasEscrowIDs a) => HasEscrowIDs (f a) where

  iTraverseEscrowIDs g = traverse (iTraverseEscrowIDs g)

-- | Escrow IDs, of course, contain themselves.  A tricky special case is
-- that the transactional variants contain escrows in their argument or
-- value as well.
instance 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  HasEscrowIDs (EscrowID argType valType) where

  iTraverseEscrowIDs f TXEscrowIn{..} = do
    eArg' <- iTraverseEscrowIDs f eArg
    f [] $ TXEscrowIn{entID, eArg = eArg'}
  iTraverseEscrowIDs f TXEscrowOut{..} = do
    eVal' <- iTraverseEscrowIDs f eVal
    f [] $ TXEscrowOut{entID, eVal = eVal'}
  -- Not point-free; we need to specialize the forall.
  iTraverseEscrowIDs f eID = f [] eID

-- | Default instance.
instance HasEscrowIDs Void 
-- | Default instance.
instance (HasEscrowIDs a) => HasEscrowIDs (Maybe a)
-- | Default instance.
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (Either a b)
-- | Default instance.
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b)

-- Boring Generic boilerplate

-- | Empty types have no escrow IDs to apply the traversal function to.
instance GHasEscrowIDs V1 where
  gTraverseEscrowIDs _ = pure

-- | Constructors with no records have no escrow IDs to traverse.
instance GHasEscrowIDs U1 where
  gTraverseEscrowIDs _ = pure

-- | In a sum type, you traverse each alternative.
instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :+: g) where
  gTraverseEscrowIDs h = \case
    L1 x -> L1 <$> gTraverseEscrowIDs h x
    R1 x -> R1 <$> gTraverseEscrowIDs h x

-- | In a product type, you traverse both halves.
instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :*: g) where
  gTraverseEscrowIDs h (x :*: y) = 
    liftA2 (:*:) (gTraverseEscrowIDs h x) (gTraverseEscrowIDs h y)

-- | Recurse into nested types.
instance (HasEscrowIDs c) => GHasEscrowIDs (K1 i c) where
  gTraverseEscrowIDs f (K1 x) = K1 <$> iTraverseEscrowIDs f x

-- | Type names are ignored for the escrow path.
instance 
  (GHasEscrowIDs f) => 
  GHasEscrowIDs (M1 D (MetaData n m p nt) f) where

  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs g x 

-- | Constructor names are part of the path, so the locator can select
-- between different variants of a sum type.
instance 
  (GHasEscrowIDs f, KnownSymbol n) => 
  GHasEscrowIDs (M1 C (MetaCons n x s) f) where

  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs (g . (name :)) x where
    name = symbolVal (Proxy @n)

-- | Record selector names are part of the path.
instance 
  (GHasEscrowIDs f, KnownSymbol n) => 
  GHasEscrowIDs (M1 S (MetaSel (Just n) su ss ds) f) where

  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs (g . (name :)) x where
    name = symbolVal (Proxy @n)

-- | Fields without record selectors are transparent to the path.  This
-- makes it easy to deal with single-field constructors, but creates an
-- ambiguity for things like tuples.  This ambiguity shows up as an error
-- in the lookup function.
instance 
  (GHasEscrowIDs f) => 
  GHasEscrowIDs (M1 S (MetaSel Nothing su ss ds) f) where

  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs g x 

-- * Functions

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
escrowTX EscrowLocator{..} = throw $ UnresolvedEscrowLocator path
escrowTX eID = TXEscrowIn $ entID eID

-- | Get the result of an escrow transaction.  This function is intended to
-- be used after the escrow ID is returned and the escrow transaction
-- performed; the ID will be available either as an escrow argument or
-- a return value, depending on how it was passed, and this function
-- extracts the result of the transaction from that ID.
escrowTXResult :: EscrowID argType valType -> valType
escrowTXResult TXEscrowOut{..} = eVal
escrowTXResult EscrowLocator{..} = throw $ UnresolvedEscrowLocator path
escrowTXResult eID = throw $ NotEscrowOut $ entID eID

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue

-- | Combs through an escrow-backed value, given another one where all the
-- paths live, and replaces each escrow locator with the actual escrow ID.
resolveEscrowLocators :: (HasEscrowIDs a, HasEscrowIDs b) => a -> b -> b
resolveEscrowLocators input x = 
  runIdentity $ traverseEscrowIDs (Identity . resolveEscrowLocator input) x

-- | Resolves a single escrow locator.
resolveEscrowLocator :: 
  (HasEscrowIDs a) => 
  a -> EscrowID argType valType -> EscrowID argType valType
resolveEscrowLocator input EscrowLocator{..} = locateEscrow input path
resolveEscrowLocator _ eID = eID

-- | Resolves an escrow path.
locateEscrow :: 
  (HasEscrowIDs a) => 
  a -> [String] -> EscrowID argType valType
locateEscrow input path =
  case execWriter $ iTraverseEscrowIDs (selectEscrowLocator path) input of
    [eID] -> EscrowID eID
    _ -> throw $ UnresolvedEscrowLocator path

-- | The indexed traversal function that combs through the reference value
-- in search of the desired path.
selectEscrowLocator :: 
  [String] -> IndexedEscrowIDMap (Writer [EntryID])
selectEscrowLocator path1 path2 EscrowLocator{..} 
  | path1 == path2 = throw $ UnresolvedEscrowLocator path
selectEscrowLocator _ _ eID@EscrowLocator{..} = return eID
selectEscrowLocator path path' eID 
  | path == path' = tell [entID eID] >> return eID
  | otherwise = return eID

