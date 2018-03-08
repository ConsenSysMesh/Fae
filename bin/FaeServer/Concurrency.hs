module FaeServer.Concurrency where

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.ByteString.Char8 as C8

import Data.Map (Map)

-- | All the pieces of data that are required to execute a transaction in
-- the dedicated interpreter thread.
data TXExecData =
  TXExecData
  {
    mainFile :: C8.ByteString,
    modules :: Map String C8.ByteString,
    parentM :: Maybe TransactionID,
    lazy :: Bool,
    fake :: Bool,
    reward :: Bool,
    tx :: TX,
    resultVar :: TMVar String,
    callerTID :: ThreadId
  } |
  View 
  {
    txID :: TransactionID,
    parentM :: Maybe TransactionID,
    resultVar :: TMVar String,
    callerTID :: ThreadId
  }

-- | Communications channel with the interpreter thread
type TXQueue = TQueue TXExecData


