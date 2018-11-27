import Blockchain.Fae.Transactions.TX$txID
import Control.Monad (unless)

body :: EscrowID EType -> FaeTX ()
body eID = do
 us <- signer "self"
 newContract (Restricted us eID)

data Restricted a = Restricted PublicKey a deriving (Generic)

instance (ContractVal a) => ContractName (Restricted a) where
  type ArgType (Restricted a) = ()
  type ValType (Restricted a) = a
  theContract (Restricted us x) = \_ -> do
    them <- signer "self"
    unless (us == them) (error "Wrong sender")
    spend x
