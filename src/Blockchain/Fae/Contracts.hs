module Blockchain.Fae.Contracts 
  (
    TwoParties(..), TwoPartyToken, offer2, twoPartySwap
  )
  where

import Blockchain.Fae
import Blockchain.Fae.Crypto
import Blockchain.Fae.Currency

import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class

import Control.Monad.Reader.Class

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Typeable

import Numeric.Natural

data TwoParties = A | B deriving (Eq)
newtype TwoPartyToken = TwoPartyToken TwoParties

offer2 :: 
  forall a.
  (HasEscrowIDs a, Typeable a) => 
  TwoParties -> a -> ShortContractID -> AnyFae ()
offer2 party x dealID = newContract (getEscrowIDs x) [dealID] c where
  c :: Contract TwoPartyToken (Maybe a)
  c (TwoPartyToken party')
    | party == party' = spend $ Just x
    | otherwise = release Nothing >>= c

data TwoPartyState =
  Undecided Tristate |
  Decided (TwoParties -> TwoPartyToken)
data Tristate = One TwoParties | Neither

switchParty :: TwoParties -> TwoParties
switchParty A = B
switchParty B = A

twoPartyChoice :: Bool -> TwoParties -> TwoPartyState -> TwoPartyState
twoPartyChoice False _ _ = Decided $ TwoPartyToken
twoPartyChoice _ party (Undecided Neither) = Undecided (One party)
twoPartyChoice _ party (Undecided (One oldParty))
  | party /= oldParty = Decided $ TwoPartyToken . switchParty
twoPartyChoice _ _ s = s

twoPartySwap :: PublicKey -> PublicKey -> AnyFae ()
twoPartySwap partyA partyB | partyA /= partyB = newContract [] [] c where
  c :: Contract Bool (Maybe TwoPartyToken)
  c choice = flip evalStateT (Undecided Neither) $ do
    partyKey <- lift sender
    let
      isA = partyKey == partyA
      isB = partyKey == partyB
      party = if isA then A else B
    unless (isA || isB) $ throw NotAParty
    modify $ twoPartyChoice choice party
    dealState <- get
    lift $ do
      nextChoice <- release $ case dealState of
        Undecided _ -> Nothing
        Decided f -> Just $ f party
      c nextChoice

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

