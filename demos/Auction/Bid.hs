import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (Coin, Maybe (Either Coin String)) String
body (_, Nothing) = return ""
body (_, Just (Right s)) = return s
