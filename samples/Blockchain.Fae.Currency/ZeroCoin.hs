import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency
import Control.Monad

body :: FaeTX ()
body = newContract @(Contract () Coin) $ 
  \_ -> forever $ zero >>= release
