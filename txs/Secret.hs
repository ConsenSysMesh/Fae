import Blockchain.Fae.Contracts

data Secret = Secret deriving (Generic, Show)

body :: FaeTX ()
body = deposit Secret "self"
