{-# LANGUAGE DeriveGeneric #-}
module PrivateTypesTX1 where

import Blockchain.Fae
import Blockchain.Fae.Internal

data T = T deriving (Generic)

instance NFData T
instance HasEscrowIDs T

txID :: TransactionID
txID = ShortContractID $ digest (0 :: Int)

pubKey :: PublicKey
pubKey = k where Just k = public $ unsafeNewPrivateKey

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = newContract [] $ \() -> do
  eID <- newEscrow [] $ \T -> spend ()
  spend eID
