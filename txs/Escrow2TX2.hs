inputs :: [(ContractID, String)]
inputs = [(TransactionOutput "4eb29f4262087e9a890524a5a381942276e2447702b8b9979a4c84202479b1d6" 0, "\"Hello, world!\"")]

body :: Transaction (EscrowID () String) String
body = flip useEscrow ()
