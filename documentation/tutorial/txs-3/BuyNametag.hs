import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency
import Blockchain.Fae.Transactions.TX$installTX.Nametag

import Data.Maybe (fromMaybe)

body :: Coin -> Nametag -> FaeTX (Valuation Coin, String)
body payment nametag = do
  message <- checkNametag nametag
  changedMay <- change payment 3

  let err = error "Nametag price is 3 coins"
      (priceCoin, changeMay) = fromMaybe err changedMay

  deposit nametag "person1"
  deposit priceCoin "person2"

  changeVal <- case changeMay of
    Nothing -> return 0
    Just changeCoin -> do
      val <- value changeCoin
      deposit changeCoin "person1"
      return val

  return (changeVal, message)
