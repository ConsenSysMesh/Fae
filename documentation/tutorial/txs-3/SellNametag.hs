import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency
import Blockchain.Fae.Transactions.TX$installTX

body :: Reward -> EscrowID GetNametag -> FaeTX (Valuation Coin)
body rwd ntFactory = do
  nametag <- useEscrow [] ntFactory rwd
  seller <- signer "name"
  sell nametag price seller
  return price
  
  where price = 3
