module TwoPartyTX7 where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (ContractID(..), ShortContractID(..), TransactionID, Digestible, digest)
import Blockchain.Fae.Contracts

import Data.Void

import qualified TwoPartyTX1 as TX1 (txID)
import qualified TwoPartyTX2 as TX2 (txID)
import TwoPartyCommon

txID :: TransactionID
txID = ShortContractID $ digest (7 :: Int)

pubKey :: PublicKey
pubKey = pubKey2

inputs :: [(ContractID, String)]
inputs = 
  [
    (TransactionOutput TX1.txID 0, "True"),
    (TransactionOutput TX2.txID 0, "()")
  ]

body = claimTX
