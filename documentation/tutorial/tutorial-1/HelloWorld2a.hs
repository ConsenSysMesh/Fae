body :: FaeTX ()
body = newContract (Echo "Hello, world!")

data Echo = Echo String deriving (Generic)

instance ContractName Echo where
  type ArgType Echo = ()
  type ValType Echo = String
  theContract (Echo s) = \_ -> spend s
