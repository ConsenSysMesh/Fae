import Blockchain.Fae.Contracts

inputs :: [(ContractID, String)]
inputs = 
  [
    (TransactionOutput "4eb29f4262087e9a890524a5a381942276e2447702b8b9979a4c84202479b1d6" 0, "Get"),
    (TransactionOutput "6dfe98a7e0bbdeefe162c080a94b04b05967883dd38aac1e25b3ae6a4d40a578" 0, "b3872a4769a840fca05a713bf38cdf5ffae4ee06765f279f7598716121957ca8")
  ]

body :: Transaction (Maybe TwoPartyEscrow, EscrowID TwoPartyEscrow String) String
body = return . escrowTXResult . snd
