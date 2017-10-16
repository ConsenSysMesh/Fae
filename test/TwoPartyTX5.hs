module TwoPartyTX5 where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (ContractID(..), ShortContractID(..), TransactionID, Digestible, digest)
import Blockchain.Fae.Contracts

import Data.Void

import qualified TwoPartyTX1 as TX1 (txID)
import TwoPartyCommon

txID :: TransactionID
txID = ShortContractID $ digest (5 :: Int)

pubKey :: PublicKey
pubKey = pubKey2

inputs :: [(ContractID, String)]
inputs = [(TransactionOutput TX1.txID 0, "Yes")]

body :: Transaction (Maybe TwoPartyEscrow) ()
body _ = return ()
