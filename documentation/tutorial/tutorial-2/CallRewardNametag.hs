import Blockchain.Fae.Transactions.TX$txID.Nametag

body :: Nametag -> FaeTX String
body = checkNametag
