import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction (Coin, Maybe (Either (Versioned Coin) (Versioned String))) String
body (_, Nothing) = return ""
body (_, Just (Right (Versioned s))) = return s
