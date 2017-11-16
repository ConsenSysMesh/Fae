import Blockchain.Fae.Contracts

body :: Transaction Void ()
body _ = twoPartySwap x y where
  x = "Hello from A!" :: String
  y = "Hello from B!" :: String
