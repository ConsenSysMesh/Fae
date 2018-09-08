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

import Data.List
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
  ContractID
  {
    parentTransaction :: TransactionID,
    transactionPart :: TransactionPart,
    creationIndex :: Int,
    contractNonce :: Nonce
  }
  deriving (Eq, Ord, Generic)

-- | Contracts may be created either in the transaction body or in the body
-- of a previous contract called by the transaction.
data TransactionPart = Body | InputCall Int
  deriving (Read, Show, Eq, Ord, Generic)

-- | A contract ID can be specified without a nonce, meaning that whatever
-- the current version of the contract is should be used.
data Nonce = Current | Nonce Int
  deriving (Read, Show, Eq, Ord, Generic)
                         --
-- | Transactions can have many named signatories, which are available in
-- all contract code.  It has to be a newtype so that we don't need to
-- import more modules in the interpreter to get 'Map'.
newtype Signers = Signers { getSigners :: Map String PublicKey }
  deriving (Serialize, NFData)

-- | Contract calls may also declare local renaming of signatories, which
-- this records in the structure @newName -> oldName@.
newtype Renames = Renames { getRenames :: Map String String }
  deriving (Serialize, NFData) 

-- | For simplicity
type ShortContractID = Digest
-- | For simplicity
type TransactionID = Digest
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

-- | There are various times a contract ID needs to be serialized: as part
-- of a transaction, or as part of an exported return value.
instance Serialize ContractID
-- | -
instance Digestible ContractID
-- | -
instance NFData ContractID
-- | The 'Read' instance, being a parser, belongs in 'PostTX.Parser', but
-- a 'Show' instance is valuable as part of the core library.  This prints
-- a contract ID as a "path" `txID/txPart/index/nonce`.
instance Show ContractID where
  show ContractID{..} = intercalate "/" $ 
    [
      show parentTransaction, 
      show transactionPart, 
      show creationIndex, 
      show contractNonce
    ]

-- | -
instance Serialize TransactionPart
-- | -
instance NFData TransactionPart

-- | -
instance Serialize Nonce
-- | -
instance NFData Nonce

-- | -
instance Read VersionID where
  readsPrec n = map (_1 %~ VersionID) . readsPrec n

-- | -
instance Show VersionID where
  show (VersionID ver) = show ver

-- | Useful for debugging
instance Show (EscrowID name) where
  show = show . entID

-- * Template Haskell

makeLenses ''Signers
makeLenses ''Renames

-- * Functions

-- | The transaction ID of the "genesis transaction"
nullID :: TransactionID
nullID = nullDigest

shorten :: ContractID -> ShortContractID
shorten = digest

hasNonce :: ContractID -> Bool
hasNonce ContractID{..} =
  case contractNonce of
    Current -> False
    _ -> True
