import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Reward -> FaeTX ()
body rID = do
  coin <- reward rID
  deposit coin "self"
