body :: FaeTX Int
body = do
  eID <- material @(EscrowID (Contract () Int)) "payment" 
  useEscrow [] eID ()
