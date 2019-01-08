import Blockchain.Fae.Contracts
import Blockchain.Fae.Transactions.TX$nametagID.Nametag

body :: Reward -> FaeTX ()
body rwd = do
  nt <- getNametag rwd "Ryan Reich"
  deposit nt "self"
