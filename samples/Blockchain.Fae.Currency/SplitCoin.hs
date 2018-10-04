import Blockchain.Fae.Currency
import Data.Maybe

body :: Coin -> FaeTX ([Valuation Coin], Valuation Coin)
body coin = do
  (pieces, remainderM) <- split coin [3,2,2]
  values <- traverse value pieces
  remainder <- fromMaybe 0 <$> traverse value remainderM
  return (values, remainder)
