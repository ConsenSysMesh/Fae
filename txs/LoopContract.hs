import Control.Monad

body :: Transaction Void ()
body _ = do
  newContract [] $ \() -> forever @_ @_ @() (return ()) >> spend ()
  newContract [] $ \() -> newContract [] (\() -> spend ()) >> spend ()
