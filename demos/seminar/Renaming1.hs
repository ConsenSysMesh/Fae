-- Create a valuable (escrow) and deposit it with contract A
-- Also create contract B that can have that valuable transfered to it
body :: FaeTX ()
body = do
  eID <- newEscrow $ Val 0
  newContract $ C eID -- Owns eID, when called, calls the escrow and returns it

data Val = Val Integer deriving (Generic)

instance ContractName Val where
  type ArgType Val = ()
  type ValType Val = (Integer, PublicKey)
  theContract (Val n) = \() -> do
    key <- signer "valRole"
    _ <- release (n, key)
    theContract (Val $ n + 1) ()

data C = C (EscrowID Val) deriving (Generic)

instance ContractName C where
  type ArgType C = ()
  type ValType C = (Integer, PublicKey, EscrowID Val)
  theContract (C eID) = \() -> do
    (n, key) <- useEscrow ["valRole" <-| "cRole"] eID ()
    spend (n, key, eID)

