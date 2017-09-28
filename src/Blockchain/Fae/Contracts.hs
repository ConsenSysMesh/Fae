{- |
Module: Blockchain.Fae.Contracts
Description: A contract library for Fae
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides some contracts that would seem to be of very common and general utility in a smart contract economy.
-}
module Blockchain.Fae.Contracts 
  (
    -- * Two-party swap
    -- $twopartyswap
    TwoParties(..), TwoPartyToken, offer2, twoPartySwap,
    -- * Direct selling
    -- $vendor
    vendor, redeem,
    -- * Possession
    -- $possession
    signOver
  )
  where

import Debug.Trace

import Blockchain.Fae
import Blockchain.Fae.Currency

import Control.Exception
import Control.Monad
import Control.Monad.State

import Control.Monad.Reader.Class

import Data.Bool

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Typeable

import GHC.Generics

import Numeric.Natural

-- $twopartyswap
-- The two-party swap is a trio of contracts that mediates a deal between
-- two parties that make their decisions outside of Fae.  It is the Fae
-- implementation of the ransom payment: each party puts forward its
-- offering (hostage or money, respectively) and scrutinizes the other's
-- offering.  If both agree, the swap occurs; otherwise, the deal is off.

-- | Abstract tokens denoting the two parties.
data TwoParties = A | B deriving (Eq, Generic, Show)
-- | An opaque token indicating whose offering the bearer is entitled to.
newtype TwoPartyToken = TwoPartyToken TwoParties deriving (Generic)

instance HasEscrowIDs TwoParties
instance HasEscrowIDs TwoPartyToken    

-- | The first argument is the party claimed by the caller; the second
-- argument is the escrow-backed value they offer; the third argument is
-- the contract ID of the two-party swap contract mediating this deal,
-- which must of course already have been created.  The value is placed
-- into a new contract accepting a 'TwoPartyToken' matching the caller's
-- party as argument, and spending the contained value when successful.
offer2 :: 
  forall a m. (HasEscrowIDs a, Typeable a, MonadTX m) => 
  TwoParties -> a -> ShortContractID -> m ()
offer2 party x dealID = newContract [bearer x] c where
  c :: Contract () (PrivateEscrowID TwoPartyToken a)
  c _ = redeem x $ \(TwoPartyToken party') -> party == party'

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

type TwoPartyM argType valType = StateT TwoPartyState (Fae argType valType)

-- | The two arguments are the public keys (as in the 'sender' function) of
-- parties A and B respectively.  A new contract is created that accepts
-- a Bool argument, which marks the choice of the party that calls it
-- (other senders are rejected).  Once both parties agree or either one
-- disagrees, a subsequent call will return the 'TwoPartyToken' unlocking
-- either the opposite party's offering (if the former) or one's own
-- offering (if the latter, to reclaim the property).
twoPartySwap :: (MonadTX m) => PublicKey -> PublicKey -> m ()
twoPartySwap partyA partyB 
  | partyA /= partyB = newContract [] $ flip evalStateT (Undecided Neither) . c
  where
    c :: ContractT (TwoPartyM Bool (Maybe TwoPartyToken)) Bool (Maybe TwoPartyToken)
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

-- $vendor
-- Direct sale contracts are simplifications of the two-party swap in which
-- the seller delivers the product automatically, allowing Fae-powered
-- stores.

-- | An essential type synonym for keeping things neat
type VendorT tok coin a = 
  PrivateEscrowID (EscrowID tok coin) (a, EscrowID tok coin)

-- | The first argument is an escrow-backed value to sell; the second is
-- its price; the third is the seller's public key.  A new escrow is
-- created containing the value, which accepts a specific currency's value
-- as an argument and, if it meets the price, returns the value and also
-- change.  The price of the value is signed over to the seller as a new
-- contract.
vendor :: 
  (
    HasEscrowIDs a, Typeable a, Currency tok coin, 
    MonadContract argType (VendorT tok coin a) m
  ) =>
  a -> Natural -> PublicKey -> 
  m (WithEscrows (VendorT tok coin a))
vendor x price seller = do
  eID <- newEscrow [bearer x] $ \payment -> do
    changeM <- change payment price
    let (cost, remit) = fromMaybe (throw NotEnough) changeM
    signOver cost seller
    spend (x, remit)
  private eID

-- | The first argument is the product to sell; the second is a validation
-- function for the token type that this contract accepts.  A new escrow is
-- created that accepts the token and, if it is valid, returns the product.
redeem ::
  (
    HasEscrowIDs a, HasEscrowIDs b, Typeable a, Typeable b,
    MonadContract argType (PrivateEscrowID b a) m
  ) =>
  a -> (b -> Bool) -> m (WithEscrows (PrivateEscrowID b a))
redeem x f = do
  eID <- newEscrow [bearer x] $ bool (throw BadToken) (spend x) . f
  private eID

-- $possession
-- A possession contract is simply one that marks a value as being owned by
-- a particular cryptographically-identified entity.

-- | The first argument is a value to assign possession; the second is the
-- public key of the recipient.  A new contract is created that takes no
-- arguments (that is, takes '()') and checks that the sender is the owner,
-- in which case it returns the value.
signOver ::
  (HasEscrowIDs a, Typeable a, MonadTX m) =>
  a -> PublicKey -> m ()
signOver x owner = newContract [bearer x] $ \() -> do
  who <- sender
  unless (owner == who) $ throw NotOwner
  spend x

data ContractsError =
  WrongParty | NotAParty |
  NotEnough | NotOwner |
  BadToken
  deriving (Typeable, Show)

instance Exception ContractsError

