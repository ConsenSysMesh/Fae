module Secret (Secret, SecretID) where

import Blockchain.Fae

data Secret = Secret deriving (Generic, Show)

type SecretID = EscrowID () Secret
