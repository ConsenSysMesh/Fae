import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (Reward, Coin) ()
body (rID, oldCoin) = do
  coin <- reward rID
  newCoin <- add oldCoin coin
  deposit newCoin "self"
