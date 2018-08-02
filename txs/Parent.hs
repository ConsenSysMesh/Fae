body :: Transaction Void ()
body _ = newContract [] ParentContract

data ParentContract = ParentContract

instance ContractName ParentContract where
  type ArgType ParentContract = ()
  type ValType ParentContract = String
  theContract ParentContract = \() -> spend "Called"

