import Blockchain.Fae.Contracts
import Nametag

body :: Reward -> FaeTX ()
body rwd = do
  nt <- getNametag rwd "Ryan Reich"
  deposit nt "self"
