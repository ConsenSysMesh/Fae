body :: FaeTX ()
body = newContract @CName $ \() -> do
  eID <- newEscrow @EName $ const $ spend "Hello!"
  spend eID

type EName = Contract () String
type CName = Contract () (EscrowID EName)
