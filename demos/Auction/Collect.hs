import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

import Blockchain.Fae.Transactions.TX$aucTX.Auction

body :: AuctionResult Coin String -> FaeTX String
body (Remit c) = do
  deposit c "self"
  return "Collected"
body _ = error "unexpected result"
