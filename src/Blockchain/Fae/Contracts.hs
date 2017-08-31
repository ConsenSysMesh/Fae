module Blockchain.Fae.Contracts 
  (
    TwoPartyToken, twoPartySwap--, offerA2, offerB2
  )
  where

import Blockchain.Fae
import Blockchain.Fae.Crypto
import Blockchain.Fae.Currency

import Control.Exception
import Control.Monad
import Control.Monad.State.Class

import Control.Monad.Reader.Class

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Typeable

import Numeric.Natural

newtype TwoPartyToken = TwoPartyToken Bool
data TwoPartyState =
  Undecided Tristate |
  Decided (Bool -> TwoPartyToken)
data Tristate = One Bool | Neither

twoPartyChoice :: Bool -> Bool -> TwoPartyState -> TwoPartyState
twoPartyChoice False _ _ = Decided coerce 
twoPartyChoice _ isA (Undecided Neither) = Undecided (One isA)
twoPartyChoice _ isA (Undecided (One wasA))
  | isA /= wasA = Decided $ coerce . not
twoPartyChoice _ _ s = s

offer2 :: 
  forall argType valType.
  (Typeable argType, Typeable valType) => 
  Bool -> EscrowID argType valType -> ShortContractID -> AnyFae ContractID
offer2 isA eID dealID = outputContract () [SomeEscrowID eID] [dealID] $ do
  -- Fae TwoPartyToken () (EscrowID argType valType)
  token <- coerce <$> ask @TwoPartyToken
  unless (token == isA) $ throw WrongParty
  spend
  transferEscrow =<< inputValue @(EscrowID argType valType) 0

twoPartySwap :: PublicKey -> PublicKey -> AnyFae ContractID
twoPartySwap partyA partyB | partyA /= partyB = 
  outputContract (Undecided Neither) [] [] $ do
    -- Fae Bool TwoPartyState (Maybe TwoPartyToken)
    party <- sender
    let
      isA = party == partyA
      isB = party == partyB
    unless (isA || isB) $ throw NotAParty
    choice <- ask
    modify $ twoPartyChoice choice isA
    dealState <- get
    return $ case dealState of
      Undecided _ -> Nothing
      Decided f -> Just $ f isA

-- signOver :: 
--   forall tok val.
--   (Typeable tok, Typeable val) => 
--   EscrowID tok val -> PublicKey -> AnyFae ContractID
-- signOver eID recip = 
--   outputContract @() @(Escrow tok val) () [SomeEscrowID eID] $ do
--     who <- sender
--     when (who /= recip) $ throw $ SignOver who recip
--     spend
--     eID <- inputValue 0
--     transferEscrow eID
-- 
-- vendor :: 
--   (Currency tok coin, Typeable tok', Typeable val) =>
--   EscrowID tok' val -> Natural -> PublicKey -> 
--   AnyFae (PrivateEscrow (EscrowID tok coin) (Escrow tok' val))
-- vendor itemID price seller =
--   returnEscrow [SomeEscrowID itemID] $ do
--     paymentID <- ask
--     payment <- value paymentID
--     (exactID, changeID) <- do
--       changeM <- change paymentID price
--       return $ case changeM of
--         Nothing -> throw $ Vendor payment price
--         Just x -> x
--     changeVal <- value changeID
--     buyer <- sender
--     when (changeVal > 0) $ signOver changeID buyer >> return ()
--     _ <- signOver exactID seller
-- 
--     spend
--     itemID <- inputValue 0
--     transferEscrow itemID

data ContractsError =
  WrongParty | NotAParty
  deriving (Typeable, Show)

instance Exception ContractsError

