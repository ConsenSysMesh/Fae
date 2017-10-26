import Blockchain.Fae.Contracts

inputs :: [(ContractID, String)]
inputs = 
  [
    (TransactionOutput "4eb29f4262087e9a890524a5a381942276e2447702b8b9979a4c84202479b1d6" 0, "Get"),
    (TransactionOutput "446e7b3da26764eefcf865b82ff1dd4d05b3aec263ad50e93faca30bd40786a3" 0, "a97b5063335764b735fe063f4427a4272e9f3e5e20884b38f413828fdfd54350")
  ]

body :: Transaction (Maybe TwoPartyEscrow, EscrowID TwoPartyEscrow String) String
body = return . escrowTXResult . snd
