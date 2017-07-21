module Blockchain.Fae
  (
    -- * Fae
    Fae,
    -- * Contract
    Contract, ContractID,
    inputContract, outputContract, inputValue,
    -- * Escrow
    EscrowID, PublicEscrowID, PrivateEscrowID,
    escrow, peek, close,
    -- * Transaction
    Transaction
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Escrow
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads

