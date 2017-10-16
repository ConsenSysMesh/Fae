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
    (TransactionOutput TX2.txID 0, "3de621824d53e0fa11d38d0bc12b7328105a481d82ef857492a6e67eccea6aa2")
  ]

body :: Transaction (Maybe TwoPartyEscrow, EscrowID TwoPartyEscrow String) String
body = return . escrowTXResult . snd
