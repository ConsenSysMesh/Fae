import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (Maybe (Either Coin String)) String
body (Just (Left c)) = do
  deposit c "self"
  return "Withdrew"
body (Just (Right s)) = return s
body Nothing = return ""
