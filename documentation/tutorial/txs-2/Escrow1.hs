type EType = Contract () String

body :: FaeTX ()
body = newContract c where
  c :: Contract String (EscrowID EType)
  c name = do
    eID <- newEscrow e 
    spend eID
    
    where
      e :: EType
      e _ = spend ("Property of: " ++ name)
