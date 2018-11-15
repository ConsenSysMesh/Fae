import Control.Monad

import Blockchain.Fae.Transactions.TX$txID

body :: FaeTX Int
body = do
  eID <- newEscrow E
  eID' <- newEscrow E'
  useEscrow ["txMaterial" <=| "theMaterial", "newMaterial" *<- eID'] eID 1

data E = E deriving (Generic)

instance ContractName E where
  type ArgType E = Int
  type ValType E = Int
  theContract E n = do
    txEID <- material "txMaterial"
    newEID <- material "newMaterial"
    useEscrow @E' [] txEID n >>= useEscrow @E' [] newEID >>= spend

