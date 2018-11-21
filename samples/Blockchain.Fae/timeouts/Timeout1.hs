import Control.Monad

body :: FaeTX ()
body = do
  newContract @(Contract () ()) $ \() -> forever (return 0) >> spend ()
  newContract @(Contract () ()) $ feedback $ const $ release () 
