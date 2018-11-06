body :: FaeTX ()
body = do
  newContract @(Contract () (EscrowID (Contract () Int))) $
    \_ -> newEscrow (\_ -> traverse release [1 ..] >> spend 0) >>= spend
  newContract @(Contract () (EscrowID (Contract () Int))) $
    let f _ = do
          eID <- material "eID"
          _ <- useEscrow [] eID ()
          release eID
          f ()
    in f
