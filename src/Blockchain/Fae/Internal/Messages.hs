{- |
Module: Blockchain.Fae.Internal.MEssages
Description: Message types for Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The structure of blocks and transactions as they are transmitted, plus
cryptography.
-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Messages where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.TX

import Common.Lens

import Control.Monad

import qualified Data.ByteString.Char8 as C8

import qualified Data.Map as Map
import Data.Map (Map)

import GHC.Generics

-- * Types

-- | The transaction message as transmitted.  This does not include the
-- full module files, but only their "previews", containing sufficient
-- information for the client to decide if the memory cost of requesting
-- the module is acceptable, and to request it if so.
data TXMessage a =
  TXMessage
  {
    salt :: a, -- ^ Needs to be the first field
    mainModulePreview :: ModulePreview,
    otherModulePreviews :: Map String ModulePreview,
    inputCalls :: Inputs,
    fallbackFunctions :: [String],
    signatures :: Map String (PublicKey, Maybe Signature)
  }
  deriving (Generic)

-- | The digest uniquely identifies the module, and the size indicates how
-- heavy it is.
data ModulePreview =
  ModulePreview
  {
    moduleDigest :: Digest,
    moduleSize :: Integer
  }
  deriving (Generic)

-- | The actual contents of a module file
type Module = C8.ByteString
-- | Named modules
type ModuleMap = Map String Module

{- Instances -}

-- | -
instance (Serialize a) => Serialize (TXMessage a)
-- | -
instance (Serialize a) => Digestible (TXMessage a)

-- | -
instance Serialize ModulePreview
-- | -
instance Digestible ModulePreview

-- * Template Haskell

makeLenses ''TXMessage
makeLenses ''ModulePreview

-- * Functions

-- | Gets the "base" transaction message without validation
unsignedTXMessage :: TXMessage a -> TXMessage a
unsignedTXMessage = over _signatures $ fmap (_2 .~ Nothing)

-- | Adds a single signature, overwriting one that's already there
signTXMessage :: 
  (Serialize a) => String -> PrivateKey -> TXMessage a -> Maybe (TXMessage a)
signTXMessage name privKey txm = do
  p <- Map.lookup name $ signatures txm
  let p' = p & _2 ?~ sig (sign utxm privKey)
  return (txm & _signatures . at name ?~ p')

  where utxm = unsignedTXMessage txm
  
-- | Validates a message (all signatures present, correct, and the right
-- identity) and returns the base message
unsignTXMessage :: (Serialize a) => TXMessage a -> Maybe (TXMessage a)
unsignTXMessage txm = do
  checked <- traverse (uncurry checkSignature) $ signatures txm 
  guard $ and checked
  return utxm

  where 
    checkSignature pubKey sigM = (pubKey ==) <$> signerKeyM sigM 
    signerKeyM sigM = (Signed utxm <$> sigM) >>= unsign
    utxm = unsignedTXMessage txm

-- | The transaction ID is its hash.  This has to be the hash of the
-- 'TXMessage' structure, rather than 'TX', because only the former
-- contains complete information identifying the transaction uniquely.  The
-- hash is taken of the unsigned message, without validating the
-- signatures.
getTXID :: (Serialize a) => TXMessage a -> TransactionID
getTXID = digest . unsignedTXMessage

-- | Extracts the portion of the transaction that is useful for
-- constructing the transaction call.  Modules must be placed in the
-- appropriate directory structure by the client.
txMessageToTX :: (Serialize a) => Bool -> TXMessage a -> Bool -> Maybe TX
txMessageToTX isReward txm@TXMessage{..} unchecked = do
  TXMessage{..} <- unsign txm
  let 
    txID = getTXID txm
    pubKeys = Signers $ fst <$> signatures
    fallback = fallbackFunctions
    inputs = inputCalls
  return TX{..}
  where unsign | unchecked = Just . unsignedTXMessage
               | otherwise = unsignTXMessage

-- | Checks the hashes of the received module files against the ones
-- promised in the transaction.  This does /not/ validate the modules as
-- Haskell source code.
validateModules :: Module -> ModuleMap -> TXMessage a -> Bool
validateModules mainModule otherModules TXMessage{..} =
  validateModule mainModulePreview mainModule &&
    Map.keys otherModules == Map.keys otherModulePreviews &&
    and (Map.intersectionWith validateModule otherModulePreviews otherModules)
  where
    validateModule ModulePreview{..} file = digest file == moduleDigest

