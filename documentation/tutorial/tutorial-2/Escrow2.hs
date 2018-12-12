import Blockchain.Fae.Transactions.TX$txID

body :: EscrowID EType -> FaeTX ()
body eID = newContract (Spend eID)

data Spend a = Spend a deriving (Generic)

instance (ContractVal a) => ContractName (Spend a) where
  type ArgType (Spend a) = ()
  type ValType (Spend a) = a
  theContract (Spend x) = \_ -> spend x
