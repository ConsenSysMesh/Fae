import Blockchain.Fae
import Blockchain.Fae.Internal

data T = T deriving (Generic)

instance NFData T
instance HasEscrowIDs T

body :: Transaction Void ()
body _ = newContract [] $ \() -> do
  eID <- newEscrow [] $ \T -> spend ()
  spend eID
