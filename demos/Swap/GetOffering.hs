import Blockchain.Fae.Currency

body :: 
  Transaction (Maybe (Either (Versioned RewardEscrowID) (Versioned Coin))) String
body Nothing = error "Swap is incomplete"
body (Just (Left (Versioned !rwd))) = return "Got reward"
body (Just (Right (Versioned !coin))) = return "Got coin"
