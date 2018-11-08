body :: FaeTX Int
body = do
  payments <- materials @(EscrowID (Contract () Int))
  sum <$> mapM (\eID -> useEscrow [] eID ()) payments
