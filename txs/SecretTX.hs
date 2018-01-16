import Blockchain.Fae.Contracts

import Secret

body :: Transaction Void PublicKey
body _ = do
  deposit Secret "self"
  signer "self"
