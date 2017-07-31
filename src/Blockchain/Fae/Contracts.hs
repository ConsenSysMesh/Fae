module Blockchain.Fae.Contracts where

import Blockchain.Fae
import Blockchain.Fae.Crypto
import Blockchain.Fae.Currency

import Control.Exception
import Control.Monad

import Control.Monad.Reader.Class

import Data.Typeable

import Numeric.Natural

signOver :: 
  forall tok val.
  (Typeable tok, Typeable val) => 
  EscrowID tok val -> PublicKey -> AnyFae ContractID
signOver eID recip = 
  outputContract @() @(Escrow tok val) () [SomeEscrowID eID] $ do
    who <- sender
    when (who /= recip) $ throw $ SignOver who recip
    spend
    eID <- inputValue 0
    transferEscrow eID

vendor :: 
  (Currency tok coin, Typeable tok', Typeable val) =>
  EscrowID tok' val -> Natural -> PublicKey -> 
  AnyFae (PrivateEscrow (EscrowID tok coin) (Escrow tok' val))
vendor itemID price seller =
  returnEscrow [SomeEscrowID itemID] $ do
    paymentID <- ask
    payment <- value paymentID
    (exactID, changeID) <- do
      changeM <- change paymentID price
      return $ case changeM of
        Nothing -> throw $ Vendor payment price
        Just x -> x
    changeVal <- value changeID
    buyer <- sender
    when (changeVal > 0) $ signOver changeID buyer >> return ()
    _ <- signOver exactID seller

    spend
    itemID <- inputValue 0
    transferEscrow itemID

data ContractsError =
  SignOver PublicKey PublicKey |
  Vendor Natural Natural
  deriving (Typeable, Show)

instance Exception ContractsError

