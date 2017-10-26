inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = newContract [] c
  where
    c :: Contract String (EscrowID () String)
    c s = do
      eID <- newEscrow [] $ \() -> spend s
      spend eID

