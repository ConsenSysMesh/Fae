import Auction
import Blockchain.Fae.Currency

body :: Transaction Void ()
body _ = do
  let price = 1 :: Valuation Coin
  let numBids = 3
  auction ("You won!" :: String) price numBids

