import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Reward -> Coin -> FaeTX ()
body rID oldCoin = do
  coin <- reward rID
  newCoin <- add oldCoin coin
  deposit newCoin "self"
