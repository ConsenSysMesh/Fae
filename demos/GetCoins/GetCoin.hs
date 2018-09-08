import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction Reward ()
body rID = do
  coin <- reward rID
  deposit coin "self"
