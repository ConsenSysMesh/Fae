import Control.Monad

body :: Transaction Void ()
body _ = newContract C

data C = C deriving (Generic)

instance ContractName C where
  type ArgType C = ()
  type ValType C = String
  theContract C = \() -> forever $ do
    newContract C1
    newContract C2
    release ("Created 2 outputs")

data C1 = C1 deriving (Generic)

instance ContractName C1 where
  type ArgType C1 = ()
  type ValType C1 = String
  theContract C1 = \() -> forever $ release ("Output 0")

data C2 = C2 deriving (Generic)

instance ContractName C2 where
  type ArgType C2 = ()
  type ValType C2 = String
  theContract C2 = \() -> spend ("Output 1" :: String)

