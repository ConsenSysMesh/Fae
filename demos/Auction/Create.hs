import Auction
import Blockchain.Fae.Currency

type Auction = Auction.Auction

body :: Transaction Void ()
body _ = do
  let price = 1 :: Valuation Coin
  let numBids = 3
  auction ("You won!" :: String) price numBids

