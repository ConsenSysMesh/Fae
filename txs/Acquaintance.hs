import Control.Monad

body :: Transaction Void ()
body _ = do
  key1 <- signer "person1"
  key2 <- signer "person2"
  newContract $ C key1 key2

data C = C PublicKey PublicKey deriving (Generic)

instance ContractName C where
  type ArgType C = ()
  type ValType C = (PublicKey, PublicKey)
  theContract (C key1 key2) = \() -> forever $ release (key1, key2)
