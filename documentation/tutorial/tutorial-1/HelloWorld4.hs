import Control.Monad (forever)

body :: FaeTX ()
body = newContract (EchoForever "Hello, world!")

data EchoForever = EchoForever String deriving (Generic)

instance ContractName EchoForever where
  type ArgType EchoForever = ()
  type ValType EchoForever = String
  theContract (EchoForever s) = \_ -> forever (release s)
