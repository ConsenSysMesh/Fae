{-# LANGUAGE Unsafe #-}
{- |
Module: Blockchain.Fae.FrontEnd
Description: The API for implementors of a Fae front-end
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

If you are writing a Fae client, this module is for you.  It exposes the functions for running blocks and transactions, and for showing the storage.
-}
module Blockchain.Fae.FrontEnd
  (
    -- * Messages
    module Blockchain.Fae.Internal.Messages,
    -- * Interpreting transactions
    module Blockchain.Fae.Internal.TX,
    -- * Running transactions without interpreting
    module Blockchain.Fae.Internal.Transaction,
    -- * Fae storage types and storage access
    module Blockchain.Fae.Internal.Storage,
    -- * Types and Instances for Clients
    module Blockchain.Fae.Internal.TXSummary,
    -- * Cryptography types and functions
    module Blockchain.Fae.Internal.Crypto,
    -- * Fae exceptions
    module Blockchain.Fae.Internal.Exceptions,
    -- * Fae ID types
    module Blockchain.Fae.Internal.IDs.Types
  ) where

import Blockchain.Fae.Internal.Crypto hiding
  (
    Serialize, PassFail, PartialSerialize,
    compareSerialize, putPartialSerialize, 
    getPartialSerialize, readsPrecSer,
    EdPublicKey
  )
import Blockchain.Fae.Internal.Exceptions hiding (unsafeIsDefined)
import Blockchain.Fae.Internal.IDs hiding
  (
    GHasEscrowIDs,
    defaultTraverseEscrowIDs
  )
import Blockchain.Fae.Internal.IDs.Types
import Blockchain.Fae.Internal.Messages hiding
  (
    unsignedTXMessage, unsignTXMessage
  )
import Blockchain.Fae.Internal.TXSummary
import Blockchain.Fae.Internal.Storage hiding 
  (
    hoistFaeStorage, nonceAt, checkNonce, nonceSetter,
    listToOutputs, emptyOutputs, combineIOV, combineO
  )
import Blockchain.Fae.Internal.Transaction hiding
  (
    TXStorageM, doTX, doFallback, runInputContracts, 
    runInputContract, runContract, hoistFaeContractNaught
  )
import Blockchain.Fae.Internal.TX

