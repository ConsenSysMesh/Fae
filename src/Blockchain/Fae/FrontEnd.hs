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
    -- * Interpreting transactions
    module Blockchain.Fae.Internal.TX,
    -- * Running transactions without interpreting
    module Blockchain.Fae.Internal.Transaction,
    -- * Running a block of transactions
    module Blockchain.Fae.Internal.Block,
    -- * Fae storage types and storage access
    module Blockchain.Fae.Internal.Storage,
    showTransaction,
    -- * Cryptography types and functions
    module Blockchain.Fae.Internal.Crypto,
    -- * Specialized 'NFData' instances
    module Blockchain.Fae.Internal.NFData,
    -- * Fae exceptions
    module Blockchain.Fae.Internal.Exceptions,
    -- * Fae ID types
    module Blockchain.Fae.Internal.IDs.Types
  ) where

import Blockchain.Fae.Internal.Block
import Blockchain.Fae.Internal.Crypto hiding
  (compareSerialize, putPartialSerialize, getPartialSerialize, readsPrecSer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs.Types
import Blockchain.Fae.Internal.NFData hiding (GNFData)
import Blockchain.Fae.Internal.PrettyFae (showTransaction)
import Blockchain.Fae.Internal.Storage hiding 
  (nonceAt, checkNonce, nonceSetter, intMapList)
import Blockchain.Fae.Internal.Transaction hiding
  (doTX, doFallback, runInputContracts, runInputContract)
import Blockchain.Fae.Internal.TX

