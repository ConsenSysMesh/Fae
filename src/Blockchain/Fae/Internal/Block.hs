module Blockchain.Fae.Internal.Block where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.TX

import Data.Sequence (Seq)

import GHC.Generics

{- Types -}

data Block =
  Block
  {
    rewardTransactions :: [TX],
    transactions :: [TX]
  }
  deriving (Generic)

{- Instances -}

instance Serialize Block
instance Digestible Block

{- Functions -}

runBlock :: Block -> FaeInterpret ()
runBlock Block{..} = do
  mapM_ (interpretTX True) rewardTransactions
  mapM_ (interpretTX False) transactions

