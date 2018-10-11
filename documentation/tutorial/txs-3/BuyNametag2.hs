import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency
import Blockchain.Fae.Transactions.TX$installTX.Nametag

body :: Coin -> EscrowID (Sell Nametag Coin) -> FaeTX (Valuation Coin, String)
body payment salesEscrow = do
  (nametag, changeM) <- useEscrow [] salesEscrow payment
  message <- checkNametag nametag
  deposit nametag "person1"
  changeVal <- changeDeposited changeM
  return (changeVal, message)
  
changeDeposited :: Maybe Coin -> FaeTX (Valuation Coin)
changeDeposited Nothing = return 0
changeDeposited (Just changeCoin) = do
  val <- value changeCoin
  deposit changeCoin "person1"
  return val
