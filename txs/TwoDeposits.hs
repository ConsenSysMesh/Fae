import Blockchain.Fae.Contracts

body :: Transaction Void ()
body _ = do
  deposit True "me"
  deposit False "you"
