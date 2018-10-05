body :: EscrowID (Contract () String) -> FaeTX String
body eID = useEscrow [] eID ()
