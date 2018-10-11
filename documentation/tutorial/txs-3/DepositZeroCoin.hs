import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: FaeTX ()
body = do
  zID <- zero @Coin
  deposit zID "self"
