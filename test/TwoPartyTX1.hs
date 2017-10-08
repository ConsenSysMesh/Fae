module TwoPartyTX1 where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (ShortContractID(..), TransactionID, Digestible, digest)
import Blockchain.Fae.Contracts

import Data.Void

import TwoPartyCommon

txID :: TransactionID
txID = ShortContractID $ digest (1 :: Int)

pubKey :: PublicKey
pubKey = pubKey1

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = twoPartySwap pubKey1 pubKey2
