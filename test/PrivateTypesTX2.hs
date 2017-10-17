{-# LANGUAGE DeriveGeneric #-}
module PrivateTypesTX2 where

import Blockchain.Fae
import Blockchain.Fae.Internal

data T = T deriving (Generic)

instance NFData T
instance HasEscrowIDs T

txID :: TransactionID
txID = ShortContractID $ digest (1 :: Int)

pubKey :: PublicKey
pubKey = k where Just k = public $ unsafeNewPrivateKey

inputs :: [(ContractID, String)]
inputs = [(TransactionOutput (ShortContractID (digest (0 :: Int))) 0, "()")]

body :: Transaction (EscrowID T ()) ()
body eID = useEscrow eID T
