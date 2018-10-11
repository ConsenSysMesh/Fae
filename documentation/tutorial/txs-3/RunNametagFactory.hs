import Blockchain.Fae.Contracts
import Blockchain.Fae.Transactions.TX$txID

body :: Reward -> EscrowID GetNametag -> FaeTX ()
body rwd ntFactory = do
  nt <- useEscrow [] ntFactory rwd
  deposit nt "self"
