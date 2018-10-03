import Blockchain.Fae.Currency
import Data.Maybe

body :: Coin -> FaeTX (Valuation Coin, Valuation Coin)
body coin = do
  resultPairM <- change coin 2
  case resultPairM of
    Nothing -> do
      coinVal <- value coin
      return (0, coinVal)
    Just (result, changeM) -> do
      resultVal <- value result
      changeValM <- traverse value changeM
      return (resultVal, fromMaybe 0 changeValM)
