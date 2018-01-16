import Control.Monad

body :: Transaction Void ()
body _ = do
  key1 <- signer "person1"
  key2 <- signer "person2"
  newContract [] $ \() -> forever $ release (key1, key2)
