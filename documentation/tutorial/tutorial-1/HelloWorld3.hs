body :: FaeTX ()
body = newContract (Echo2 "Hello, world!" "Goodbye!")

data Echo2 = Echo2 String String deriving (Generic)

instance ContractName Echo2 where
  type ArgType Echo2 = ()
  type ValType Echo2 = String
  theContract (Echo2 s1 s2) = \_ -> do
    release s1
    spend s2
