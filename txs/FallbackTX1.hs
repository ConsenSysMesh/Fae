import Blockchain.Fae.Contracts

body :: FaeTX ()
body = error "TX error"

fb :: FaeTX ()
fb = deposit ("Fallback" :: String) "self"
