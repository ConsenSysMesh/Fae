import Blockchain.Fae.Transactions.TX$txID

body :: EscrowID EType -> FaeTX String
body eID = useEscrow [] eID ()
