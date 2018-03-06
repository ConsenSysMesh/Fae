import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction Coin ()
body coin = twoPartySwap ("Hi Bob!" :: String) coin

