body :: FaeTX ()
body = newContract C

data C = C deriving (Generic)
data E' = E' deriving (Generic)

instance ContractName C where
  type ArgType C = ()
  type ValType C = EscrowID E'
  theContract C () = newEscrow E' >>= spend

instance ContractName E' where
  type ArgType E' = Int
  type ValType E' = Int
  theContract E' n = spend $ n + 1

