import Blockchain.Fae.Currency

body :: 
  Transaction (Maybe (Either (Versioned String) (Versioned Coin))) String
body Nothing = error "Swap is incomplete"
body (Just (Left (Versioned s))) = return s
body (Just (Right (Versioned coin))) = return "Got coin"
