{- |
Module: Blockchain.Fae.Internal.IDs.Types
Description: Identifier types
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

There are several identifier types in Fae: two kinds of contract ID (one
detailed, one convenient but lossy), transaction IDs, escrow IDs, and
version IDs.
-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.IDs.Types where

import Blockchain.Fae.Internal.Crypto
import Common.Lens
import Control.DeepSeq
import Data.Map (Map)
import Data.Serialize
import Data.String
import GHC.Generics
import Text.ParserCombinators.ReadP

-- | This identifier locates a contract in storage.  It is not intended to
-- be used in contract code, as indeed, a contract can never be called
-- explicitly but only as a transaction input, for which there is a special
-- syntax outside Haskell.
data ContractID =
  -- | One way contracts can be created is by being directly output by
  -- a transaction.  The outputs are indexed from 0 in order of creation.
  TransactionOutput TransactionID Int |
  -- | The other way contracts can be created is by being output during the
  -- execution of one of its input contract calls.  The outputs are indexed
  -- from 0 in order of creation, with the indexing specific to each input
  -- contract.
  InputOutput TransactionID ShortContractID Int |
  -- | Contracts can be given with an optional /nonce/, denoting how many
  -- times they have been called.  This infix constructor is for
  -- convenience.  
  ContractID :# Int
  deriving (Read, Show, Generic, Eq, Ord)

-- | The hash of a 'ContractID', useful for abbreviating what would
-- otherwise be unboundedly long chains of contracts that are outputs of
-- contracts that are outputs of ... that are outputs of some long-ago
-- transaction.
newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Serialize, IsString, Generic, NFData)

-- | Transactions can have many named signatories, which are available in
-- all contract code.  It has to be a newtype so that we don't need to
-- import more modules in the interpreter to get 'Map'.
newtype Signers = Signers { getSigners :: Map String PublicKey }
  deriving (Serialize, NFData)

-- | For simplicity
type TransactionID = ShortContractID
-- | For simplicity
type BlockID = Digest

-- | For simplicity
type EntryID = Digest
-- | For convenience
newtype VersionID = VersionID Digest 
  deriving (Generic, Eq, Ord, Serialize, NFData)

-- | This identifier locates an escrow.  Escrow IDs are assigned when the
-- escrow is first created and are guaranteed to be globally unique and
-- immutable.  Each escrow ID is valid only within a contract or other
-- escrow that actually holds the escrow, which must have been created with
-- a "name" type matching the phantom type parameter.  Escrow IDs may only
-- be constructed by the 'newEscrow' function; in contract calls, they can
-- also be referenced by version (see "Versions").  However, they should
-- appear type-correct in contract signatures to formally verify that the
-- contract receives and returns a particular kind of opaque value, e.g.
-- a currency.
newtype EscrowID name = EscrowID { entID :: EntryID }
  deriving (NFData)

-- Instances

-- | Just so we can get 'Digestible'
instance Serialize ContractID
-- | So we can get a 'ShortContractID' from a regular one.
instance Digestible ContractID
-- | -
instance NFData ContractID

-- | 'ShortContractID's and, by extension, 'TransactionIDs', are read as
-- the digests they wrap.
instance Read ShortContractID where
  readsPrec n = fmap (_1 %~ ShortContractID) . readsPrec n

-- | 'ShortContractID's and, by extension, 'TransactionIDs', show as hex
-- strings.  This should be inverse to the 'Read' instance.
instance Show ShortContractID where
  show (ShortContractID dig) = show dig

-- | -
instance Read VersionID where
  readsPrec n = map (_1 %~ VersionID) . readsPrec n

-- | -
instance Show VersionID where
  show (VersionID ver) = show ver

-- * Template Haskell

makeLenses ''Signers

-- * Functions

-- | The transaction ID of the "genesis transaction"
nullID :: TransactionID
nullID = ShortContractID nullDigest

