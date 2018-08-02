import Blockchain.Fae.Contracts

data Secret = Secret deriving (Generic, Show)

body :: Transaction Void PublicKey
body _ = do
  deposit Secret "self"
  signer "self"
