import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

import Blockchain.Fae.Transactions.TX$aucTX.Auction

body :: AuctionResult Coin String -> FaeTX String
body BidAccepted = return "Bid accepted"
body (Prize s) = return s
body _ = error "unexpected response"

