module Contract1TX1 where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (
    ShortContractID(..), TransactionID, Digestible, 
    digest, public, unsafeNewPrivateKey
  )

import Data.Void

txID :: TransactionID
txID = ShortContractID $ digest (1 :: Int)

pubKey :: PublicKey
pubKey = k where Just k = public $ unsafeNewPrivateKey

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = newContract [] c
  where
    c :: Contract String String
    c = spend

