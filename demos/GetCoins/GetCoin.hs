import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: Transaction RewardEscrowID ()
body rID = do
  coin <- reward rID
  deposit coin "self"
