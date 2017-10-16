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
    (TransactionOutput TX1.txID 0, "Get"),
    (TransactionOutput TX2.txID 0, "(660352baa787b9a3cc193e0eae81a3c79111123f4c5b19f9593213a820d4b0f6,0)")
  ]

body :: Transaction (Maybe TwoPartyEscrow, EscrowID TwoPartyEscrow String) String
body = return . escrowTXResult . snd
