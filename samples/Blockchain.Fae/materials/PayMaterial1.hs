body :: FaeTX ()
body = do
  newContract @(Contract () (EscrowID (Contract () Int))) $
    feedback $ \_ -> newEscrow (\_ -> traverse release [1 ..] >> spend 0) >>= release
  newContract @(Contract () (EscrowID (Contract () Int))) $
    let f _ = do
          eID <- material "eID"
          _ <- useEscrow [] eID ()
          release eID
          f ()
    in f
