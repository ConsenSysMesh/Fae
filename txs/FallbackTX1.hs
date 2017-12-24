import Blockchain.Fae.Contracts

body :: Transaction Void ()
body _ = error "TX error"

fb :: Transaction Void ()
fb _ = deposit ("Fallback" :: String) "self"
