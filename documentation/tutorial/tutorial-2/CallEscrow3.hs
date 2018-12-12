import Blockchain.Fae.Transactions.TX$tx1ID

body :: EscrowID EType -> FaeTX String
body eID = useEscrow [] eID ()
