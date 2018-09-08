import Blockchain.Fae.Transactions.TX$txID

body :: 
  Transaction ((Integer, PublicKey, EscrowID Val), (Integer, PublicKey)) String
body ((n1, k1, _), (n2, k2)) = return $ 
  "C1: escrow saw " ++ show k1 ++ ", returned " ++ show n1 ++ "; " ++
  "C2: escrow saw " ++ show k2 ++ ", returned " ++ show n2
