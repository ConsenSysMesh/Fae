body :: EscrowID (Contract () Int) -> FaeTX Int
body eID0 = do
  eID <- material @(EscrowID (Contract () Int)) "payment" 
  eID' <- material @(EscrowID (Contract () Int)) "payment'" 
  n <- useEscrow [] eID ()
  n' <- useEscrow [] eID' ()
  n0 <- useEscrow [] eID0 ()
  return $ n + n' + n0
