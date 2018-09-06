body :: Transaction Void ()
body _ = do
  newContract VersionC1
  newContract VersionC2

data VersionC1 = VersionC1 deriving (Generic)
data VersionC2 = VersionC2 deriving (Generic)

instance ContractName VersionC1 where
  type ArgType VersionC1 = ()
  type ValType VersionC1 = Versioned String
  theContract VersionC1 = \() -> spend $ Versioned ("Hello, world!" :: String)

instance ContractName VersionC2 where
  type ArgType VersionC2 = Versioned String
  type ValType VersionC2 = String
  theContract VersionC2 = spend . getVersioned

