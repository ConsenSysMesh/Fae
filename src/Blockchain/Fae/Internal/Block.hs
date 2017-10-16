module Blockchain.Fae.Internal.Block where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Interpreter
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Data.Sequence (Seq)

{- Types -}

data Block =
  Block
  {
    rewardTransactions :: [TransactionID],
    transactions :: [TransactionID]
  }
  deriving (Generic)

{- Instances -}

instance Serialize Block
instance Digestible Block

{- Functions -}
runBlock :: Block -> FaeStorage ()
runBlock Block{..} = interpretTXs $
  map (,True) (moduleName rewardTransactions) ++
  map (,False) (moduleName transactions)
  where fileName txID = "Transactions.TX" ++ show txID
