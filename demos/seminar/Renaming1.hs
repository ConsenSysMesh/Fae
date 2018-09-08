-- Create a valuable (escrow) and deposit it with contract A
-- Also create contract B that can have that valuable transfered to it
body :: Transaction Void ()
body _ = do
  eID <- newEscrow $ Val 0
  newContract $ C1 eID -- Owns eID, when called, calls the escrow and returns it
  newContract C2 -- When called with an escrow ID, calls it


data Val = Val Integer deriving (Generic)

instance ContractName Val where
  type ArgType Val = ()
  type ValType Val = (Integer, PublicKey)
  theContract (Val n) = \() -> do
    key <- signer "valRole"
    _ <- release (n, key)
    theContract (Val $ n + 1) ()

data C1 = C1 (EscrowID Val) deriving (Generic)

instance ContractName C1 where
  type ArgType C1 = ()
  type ValType C1 = (Integer, PublicKey, EscrowID Val)
  theContract (C1 eID) = \() -> do
    (n, key) <- useEscrow ["valRole" <-| "c1Role"] eID ()
    spend (n, key, eID)

data C2 = C2 deriving (Generic)

instance ContractName C2 where
  type ArgType C2 = Versioned (EscrowID Val)
  type ValType C2 = (Integer, PublicKey)
  theContract C2 = \(Versioned eID) -> do
    (n, key) <- useEscrow ["valRole" <-| "c2Role"] eID ()
    spend (n, key)
