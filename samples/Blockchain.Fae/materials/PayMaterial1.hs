body :: FaeTX ()
body = newContract @_ @(Contract () (EscrowID (Contract () Int))) $
  \_ -> newEscrow (\_ -> spend 1) >>= spend

