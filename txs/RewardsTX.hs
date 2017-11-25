import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction RewardEscrowID Integer
body rID = do
  eID <- reward rID
  v <- value eID
  s <- signer "self"
  signOver eID s
  return $ toInteger v
