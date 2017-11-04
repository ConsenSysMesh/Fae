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
    TwoParties(..), TwoPartyToken, Token, TwoPartyEscrow, TwoPartyVote(..), 
    offer2, twoPartySwap,
    -- * Direct selling
    -- $vendor
    sell, redeem,
    -- * Possession
    -- $possession
    signOver
  )
  where

import Blockchain.Fae
import Blockchain.Fae.Currency hiding (Token)

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
-- | A private type controlling the escrow containing
-- a `TwoPartyToken`.                        
data Token = Token deriving (Generic)
-- | The actual "payout" from a two-party swap.  It can be used to collect
-- one of the offerings.
type TwoPartyEscrow = EscrowID Token TwoPartyToken

instance NFData TwoParties
instance NFData TwoPartyToken

instance HasEscrowIDs TwoParties
instance HasEscrowIDs TwoPartyToken    

instance NFData Token

-- | The first argument is the party claimed by the caller; the second
-- argument is the escrow-backed value they offer.  The value is placed
-- into a new contract accepting a 'TwoPartyEscrow' containing
-- a 'TwoPartyToken' matching the caller's party as argument, and spending
-- the contained value as an escrow transaction.
offer2 :: 
  (HasEscrowIDs a, Typeable a, MonadTX m, NFData a) => 
  TwoParties -> a -> m ()
offer2 party x = newContract [bearer x] $ \tokID -> do
  eID <- newEscrow [bearer x] $ \tokID -> do
    TwoPartyToken party' <- useEscrow tokID Token
    when (party /= party') $ throw WrongParty
    spend x
  spend $ escrowTX eID tokID

data TwoPartyState =
  Undecided Tristate |
  Decided (TwoParties -> TwoPartyToken)
data Tristate = One TwoParties | Neither
-- | Argument to the two-party swap.  'Yes' and 'No' are actual votes on
-- the swap; 'Get' requests the token if the swap is complete.
data TwoPartyVote = Yes | No | Get deriving (Read, Show, Generic)

instance NFData TwoPartyVote
instance ReadInput TwoPartyVote

switchParty :: TwoParties -> TwoParties
switchParty A = B
switchParty B = A

twoPartyChoice :: TwoPartyVote -> TwoParties -> TwoPartyState -> TwoPartyState
twoPartyChoice No _ _ = Decided $ TwoPartyToken
twoPartyChoice Yes party (Undecided Neither) = Undecided (One party)
twoPartyChoice Yes party (Undecided (One oldParty))
  | party /= oldParty = Decided $ TwoPartyToken . switchParty
twoPartyChoice _ _ s = s

type TwoPartyM argType valType = StateT TwoPartyState (Fae argType valType)
type TwoPartyC a v = ContractT (TwoPartyM a (Maybe v)) a (Maybe v)

-- | The two arguments are the public keys (as in the 'sender' function) of
-- parties A and B respectively.  A new contract is created that accepts
-- a 'TwoPartyVote' argument, which marks the choice of the party that
-- calls it (other senders are rejected).  Once both parties agree or
-- either one disagrees, a subsequent call will return the 'TwoPartyEscrow'
-- unlocking either the opposite party's offering (if the former) or one's
-- own offering (if the latter, to reclaim the property).
twoPartySwap :: (MonadTX m) => PublicKey -> PublicKey -> m ()
twoPartySwap partyA partyB 
  | partyA /= partyB = newContract [] $ flip evalStateT (Undecided Neither) . c
  | otherwise = throw OnlyOneParty
  where
    c :: TwoPartyC TwoPartyVote TwoPartyEscrow
    c choice = do
      partyKey <- sender
      let
        isA = partyKey == partyA
        isB = partyKey == partyB
        party = if isA then A else B
      unless (isA || isB) $ throw NotAParty
      modify $ twoPartyChoice choice party
      dealState <- get
      ret <- case dealState of
        Undecided _ -> return Nothing
        Decided f -> 
          case choice of
            Get -> Just <$> newEscrow [] (\Token -> spend $ f party)
            _ -> return Nothing
      nextChoice <- release ret
      c nextChoice

-- $vendor
-- Direct sale contracts are simplifications of the two-party swap in which
-- the seller delivers the product automatically, allowing Fae-powered
-- stores.

-- | The first argument is an escrow-backed value to sell; the second is
-- its price; the third is the seller's public key.  A new escrow is
-- created containing the value, which accepts a specific currency's value
-- as an argument and, if it meets the price, returns the value and also
-- change.  The price of the value is signed over to the seller as a new
-- contract.
sell :: 
  (
    HasEscrowIDs a, Typeable a, 
    NFData tok, NFData coin, NFData a, 
    Currency tok coin, MonadTX m
  ) =>
  a -> Valuation tok coin -> PublicKey -> m ()
sell x price seller = newContract [] $ \payID -> do
  eID <- newEscrow [bearer x] $ \payment -> do
    changeM <- change payment price
    let (cost, remit) = fromMaybe (throw NotEnough) changeM
    signOver cost seller
    spend (x, remit)
  spend $ escrowTX eID payID

-- | The first argument is the product to sell; the second is a validation
-- function for the token type that this contract accepts.  A new escrow is
-- created that accepts the token and, if it is valid, returns the product.
redeem ::
  (
    HasEscrowIDs a, HasEscrowIDs b, 
    Typeable a, Typeable b, ReadInput b,
    NFData a, NFData b,
    MonadTX m
  ) =>
  a -> (b -> Fae b a Bool) -> m ()
redeem x f = newContract [] $ \tok -> do
  eID <- newEscrow [bearer x] $ \token -> do
    val <- f tok
    bool (throw BadToken) (spend x) val
  spend $ escrowTX eID tok

-- $possession
-- A possession contract is simply one that marks a value as being owned by
-- a particular cryptographically-identified entity.

-- | The first argument is a value to assign possession; the second is the
-- public key of the recipient.  A new contract is created that takes no
-- arguments (that is, takes '()') and checks that the sender is the owner,
-- in which case it returns the value.
signOver ::
  (HasEscrowIDs a, Typeable a, NFData a, MonadTX m) =>
  a -> PublicKey -> m ()
signOver x owner = newContract [bearer x] $ \() -> do
  who <- sender
  unless (owner == who) $ throw NotOwner
  spend x

data ContractsError =
  WrongParty | NotAParty | OnlyOneParty |
  NotEnough | NotOwner |
  BadToken
  deriving (Typeable, Show)

instance Exception ContractsError

