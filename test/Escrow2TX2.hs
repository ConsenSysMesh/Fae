module Escrow2TX2 where

import Blockchain.Fae
import Blockchain.Fae.Internal 
  (
    ContractID(..),
    ShortContractID(..), TransactionID, Digestible, 
    digest, public, unsafeNewPrivateKey
  )

import Data.Void

import qualified Escrow2TX1 as TX1 (txID)

txID :: TransactionID
txID = ShortContractID $ digest (2 :: Int)

pubKey :: PublicKey
pubKey = k where Just k = public $ unsafeNewPrivateKey

inputs :: [(ContractID, String)]
inputs = [(TransactionOutput TX1.txID 0, "\"Hello, world!\"")]

body :: Transaction (EscrowID () String) String
body = flip useEscrow ()
