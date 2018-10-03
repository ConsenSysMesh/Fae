import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Reward -> Coin -> FaeTX (Valuation Coin)
body rToken coin = do
  rCoin <- reward rToken
  coin' <- add rCoin coin
  val <- value coin'
  deposit coin' "self"
  return val
