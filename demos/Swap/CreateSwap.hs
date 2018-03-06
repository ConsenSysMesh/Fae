import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (RewardEscrowID, Coin) ()
body (rwd, coin) = twoPartySwap rwd coin

