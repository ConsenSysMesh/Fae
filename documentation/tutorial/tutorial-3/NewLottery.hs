import Lottery

body :: FaeTX ()
body = do
  owner <- signer "self"
  newContract $ Lottery 5 owner
