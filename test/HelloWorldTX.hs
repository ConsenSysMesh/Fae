module HelloWorldTX where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (
    ShortContractID(..), TransactionID, Digestible, 
    digest, public, unsafeNewPrivateKey
  )

import Data.Void

instance Digestible ()

txID :: TransactionID
txID = ShortContractID $ digest ()

pubKey :: PublicKey
pubKey = k where Just k = public $ unsafeNewPrivateKey

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void String
body _ = return "Hello, World!"

