{- |
Module: Blockchain.Fae.FrontEnd
Description: The API for implementors of a Fae front-end
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

If you are writing a Fae client, this module is for you.  It exposes the functions for running blocks and transactions, and for showing the storage.
-}
module Blockchain.Fae.FrontEnd
  (
    module Blockchain.Fae.Internal.Block,
    module Blockchain.Fae.Internal.TX,
    -- * Transaction execution
    -- | The function here is potentially useful for executing
    -- a transaction that, for some reason, wasn't obtained by
    -- interpretation.
    runTransaction,
    -- * Storage
    Storage, StorageT(..),
    TransactionEntry, TransactionEntryT(..), 
    Outputs, OutputsT(..), 
    InputOutputs, InputOutputsT(..), 
    InputOutputVersions, InputOutputVersionsT(..),
    FaeStorage
  ) where

import Blockchain.Fae.Internal.Block
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.TX

