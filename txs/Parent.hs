body :: Transaction Void ()
body _ = newContract ParentContract

data ParentContract = ParentContract deriving (Generic)

instance ContractName ParentContract where
  type ArgType ParentContract = ()
  type ValType ParentContract = String
  theContract ParentContract = \() -> spend "Called"

