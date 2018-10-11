import Blockchain.Fae.Contracts (deposit)
import Blockchain.Fae.Transactions.TX$installTXID.Nametag

body :: Nametag -> Nametag -> FaeTX (String, String)
body nt1 nt2 = do
  message1 <- checkNametag nt1
  message2 <- checkNametag nt2
  deposit nt1 "person2"
  deposit nt2 "person1"
  return (message1, message2)
