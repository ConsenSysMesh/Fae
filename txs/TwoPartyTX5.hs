import Blockchain.Fae.Contracts

inputs :: [(ContractID, String)]
inputs = [(TransactionOutput "4eb29f4262087e9a890524a5a381942276e2447702b8b9979a4c84202479b1d6" 0, "Yes")]

body :: Transaction (Maybe TwoPartyEscrow) ()
body _ = return ()
