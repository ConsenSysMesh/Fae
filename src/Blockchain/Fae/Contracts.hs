module Blockchain.Fae.Contracts 
  (
    TwoParties(..), TwoPartyToken, offer2, twoPartySwap
  )
  where

import Blockchain.Fae
import Blockchain.Fae.Currency

import Control.Exception
import Control.Monad
import Control.Monad.State

import Control.Monad.Reader.Class

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Typeable

import Numeric.Natural

{- Two-party swap -}

data TwoParties = A | B deriving (Eq)
newtype TwoPartyToken = TwoPartyToken TwoParties

offer2 :: 
  forall a m. (HasEscrowIDs a, Typeable a, MonadTX m) => 
  TwoParties -> a -> ShortContractID -> m ()
offer2 party x dealID = newContract [bearer x] [dealID] c where
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

twoPartySwap :: (MonadTX m) => PublicKey -> PublicKey -> m ()
twoPartySwap partyA partyB 
  | partyA /= partyB = newContract [] [] $ flip evalStateT (Undecided Neither) . c
  where
    c choice = do
      partyKey <- sender
      let
        isA = partyKey == partyA
        isB = partyKey == partyB
        party = if isA then A else B
      unless (isA || isB) $ throw NotAParty
      modify $ twoPartyChoice choice party
      dealState <- get
      nextChoice <- release $ case dealState of
        Undecided _ -> Nothing
        Decided f -> Just $ f party
      c nextChoice

{- Vendor -}

vendor :: 
  (HasEscrowIDs a, Typeable a, Currency tok coin, MonadTX m) =>
  a -> Natural -> PublicKey -> 
  m (EscrowID (EscrowID tok coin) (a, EscrowID tok coin))
vendor x price seller = newEscrow [bearer x] $ \payment -> do
  changeM <- change payment price
  let (cost, remit) = fromMaybe (throw NotEnough) changeM
  signOver cost seller
  spend (x, remit)

signOver ::
  (HasEscrowIDs a, Typeable a, MonadTX m) =>
  a -> PublicKey -> m ()
signOver x owner = newContract [bearer x] [] $ \() -> do
  who <- sender
  unless (owner == who) $ throw NotOwner
  spend x

data ContractsError =
  WrongParty | NotAParty |
  NotEnough | NotOwner
  deriving (Typeable, Show)

instance Exception ContractsError

