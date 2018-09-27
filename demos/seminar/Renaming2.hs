import Blockchain.Fae.Transactions.TX$txID

body :: (Integer, PublicKey, EscrowID Val) -> FaeTX String
body (n1, k1, cE) = do
  eID <- newEscrow E
  (n2, k2) <- useEscrow ["eRole" <-| "tom"] eID cE
  return $
    "C: escrow saw " ++ show k1 ++ ", returned " ++ show n1 ++ "; " ++
    "E: escrow saw " ++ show k2 ++ ", returned " ++ show n2

data E = E deriving (Generic)

instance ContractName E where
  type ArgType E = EscrowID Val
  type ValType E = (Integer, PublicKey)
  theContract E = \eID -> do
    (n, key) <- useEscrow ["valRole" <-| "eRole"] eID ()
    spend (n, key)

