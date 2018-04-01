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
data TXMessage =
  TXMessage
  {
    mainModulePreview :: ModulePreview,
    otherModulePreviews :: Map String ModulePreview,
    inputCalls :: Inputs,
    fallbackFunctions :: [String],
    signatures :: Map String (Either PublicKey Signature),
    salt :: String
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
instance Serialize TXMessage
-- | -
instance Digestible TXMessage

-- | -
instance Serialize ModulePreview
-- | -
instance Digestible ModulePreview

-- * Template Haskell

makeLenses ''TXMessage
makeLenses ''ModulePreview

-- * Functions

-- | Gets the "base" transaction message without validation
unsignedTXMessage :: TXMessage -> TXMessage
unsignedTXMessage = over _signatures $ fmap (either (Left . id) (Left . getSigner))

-- | Adds a single signature, overwriting one that's already there
signTXMessage :: String -> PrivateKey -> TXMessage -> TXMessage
signTXMessage name privKey txm = txm & _signatures . at name ?~ Right sig where
  Signed{sig} = sign (unsignedTXMessage txm) privKey
  
-- | Validates a message (all signatures present, correct, and the right
-- identity) and returns the base message
unsignTXMessage :: TXMessage -> Maybe TXMessage
unsignTXMessage txm = do
  signedPubKeys <- mapM getTXSigner $ signatures txm
  guard $ not (Map.null signedPubKeys) && signedPubKeys == actualPubKeys
  return utxm
  where
    getTXSigner = either (const Nothing) (fmap Left . unsign . Signed utxm)
    actualPubKeys = signatures utxm
    utxm = unsignedTXMessage txm

-- | The transaction ID is its hash.  This has to be the hash of the
-- 'TXMessage' structure, rather than 'TX', because only the former
-- contains complete information identifying the transaction uniquely.  The
-- hash is taken of the unsigned message, without validating the
-- signatures.
getTXID :: TXMessage -> TransactionID
getTXID = ShortContractID . digest . unsignedTXMessage

-- | Extracts the portion of the transaction that is useful for
-- constructing the transaction call.  Modules must be placed in the
-- appropriate directory structure by the client.
txMessageToTX :: TXMessage -> Maybe TX
txMessageToTX txm = do
  TXMessage{..} <- unsignTXMessage txm
  let 
    txID = getTXID txm
    -- This is guaranteed to match because we have used 'unsignTXMessage',
    -- so every signer is now 'Left'.
    pubKeys = Signers $ fmap (\(Left pk) -> pk) signatures
    fallback = fallbackFunctions
    inputs = inputCalls
  return TX{..}

-- | Checks the hashes of the received module files against the ones
-- promised in the transaction.  This does /not/ validate the modules as
-- Haskell source code.
validateModules :: Module -> ModuleMap -> TXMessage -> Bool
validateModules mainModule otherModules TXMessage{..} =
  validateModule mainModulePreview mainModule &&
    Map.keys otherModules == Map.keys otherModulePreviews &&
    and (Map.intersectionWith validateModule otherModulePreviews otherModules)
  where
    validateModule ModulePreview{..} file = digest file == moduleDigest

