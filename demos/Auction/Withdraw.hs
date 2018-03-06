import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (Maybe (Either (Versioned Coin) (Versioned String))) String
body (Just (Left (Versioned c))) = do
  deposit c "self"
  return "Withdrew"
body (Just (Right (Versioned s))) = return s
body Nothing = return ""
