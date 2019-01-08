import Auction
import Blockchain.Fae.Currency

body :: FaeTX ()
body = do
  let price = 1 :: Valuation Coin
  let numBids = 3
  auction ("You won!" :: String) price numBids

