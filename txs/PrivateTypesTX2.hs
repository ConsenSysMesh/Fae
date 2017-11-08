import Blockchain.Fae
import Blockchain.Fae.Internal

data T = T deriving (Generic)

instance NFData T
instance HasEscrowIDs T

--inputs :: [(ContractID, String)]
--inputs = [(TransactionOutput (ShortContractID (digest (0 :: Int))) 0, "()")]

body :: Transaction (EscrowID T ()) ()
body eID = useEscrow eID T
