import Blockchain.Fae.Currency
import Data.Maybe
import Prelude hiding (round)

body :: Coin -> FaeTX (Valuation Coin, Valuation Coin)
body c = do
  (c', rM) <- round c 3
  roundVal <- value c'
  remainderVal <- fromMaybe (return 0) $ value <$> rM
  return (roundVal, remainderVal)
