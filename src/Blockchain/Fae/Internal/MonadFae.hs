{- |
Module: Blockchain.Fae.Internal.MonadFae
Description: The Fae API
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides a clean, minimal API for contract authoring.  The 'MonadTX' and 'MonadContract' classes exist to facilitate using monad transformers on top of the base 'Fae' monad when defining contracts.
-}
{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.MonadFae where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class

import Data.Dynamic
import Data.Maybe
import Data.Sequence (Seq)
import Data.Typeable

import qualified Data.Map as Map

-- * Types

-- | This wrapper is necessary because 'Fae' and 'FaeTX' are monads that
-- contract authors can actually use, and so we need to carefully limit the
-- capabilities they are allowed.
newtype FaeM s a = Fae { getFae :: FaeContract s a }
  deriving (Functor, Applicative, Monad)
-- | The monad for multi-stage contracts taking an 'argType' and returning
-- a 'valType'.  These may not be transactions.
type Fae argType valType = FaeM (FaeRequest argType valType)
-- | The monad for single-stage contracts, i.e. transactions.
type FaeTX = FaeM Naught
-- | This is the type to use when defining a new contract.  Its argument is
-- the argument given in the first call to this contract; all subsequent
-- calls pass in their arguments via 'release'.
type Contract argType valType = ContractT (Fae argType valType) argType valType

-- * Type classes

-- |
-- Instances of this class have access to the full Fae API, allowing them
-- to define multi-stage contracts.  As for 'MonadTX', these instances must
-- have their own evaluation function to get down to the base 'Fae' monad.
-- Notably, 'Transaction's are /not/ written in a 'MonadContract', because
-- they are one-shot.
class 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadTX m) => 
  MonadContract argType valType m | m -> argType valType where

  -- | Injects the Fae contract API into 'm'.
  liftContract :: Fae argType valType a -> m a

-- |
-- Instances of this class may use the bulk of the Fae API; namely, they
-- may hold value via escrows and create new contracts.  Such monads are
-- intended to be defined by a transformer stack on top of 'FaeTX', hence
-- 'liftTX'; to be useful, they must have a function that "evaluates" them
-- down to 'Fae' or 'FaeTX', so they may define new transactions or
-- contracts.
class (Monad m) => MonadTX m where
  -- | Injects the Fae transaction API into 'm'.
  liftTX :: FaeTX a -> m a

{- Instances -}

-- | An @UndecidableInstance@ for sure
instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadContract argType valType m, Monad (t m)) =>
  MonadContract argType valType (t m) where

  liftContract = lift . liftContract

-- | An @UndecidableInstance@ for sure
instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadTX m, Monad (t m)) => 
  MonadTX (t m) where

  liftTX = lift . liftTX

-- | Of course
instance 
  (HasEscrowIDs argType, HasEscrowIDs valType) => 
  MonadContract argType valType (Fae argType valType) where

  liftContract = id

-- | A little backwards, since 'FaeContract' lies below rather than above
-- 'FaeTX', but they are actually isomorphic, and we need to start the
-- monad stack down here so that we can derive @forall s. (Functor s) =>
-- MonadTX (FaeM s)@. 
instance (Functor s) => MonadTX (FaeContract s) where
  liftTX (Fae xM) = mapSuspension (const undefined) xM

-- | Of course
deriving instance (Functor s) => MonadTX (FaeM s)

-- * Functions

-- | This function is like 'return' but also ensures that the returned
-- value is passed with its backing escrows, maintaining its value.  Once
-- a contract terminates with a 'spend', it is removed from storage.
spend :: 
  (HasEscrowIDs valType, MonadContract argType' valType' m) => 
  valType -> m (WithEscrows valType)
spend = liftTX . Fae . internalSpend 

-- | This function is used in an expression like @nextArg <- release
-- currentValue@ to suspend the current contract, simultaneously
-- `spend`ing an intermediate value, and awaiting its next call to
-- continue with the arg that was passed.
release :: 
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release x = liftContract $ Fae $ do
  req <- internalSpend x
  escrows <- get
  suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
    put escrows -- makeInternalT wipes out the ongoing escrows
    _escrowMap %= Map.union inputEscrows
    return y

-- | Calls the given escrow by ID as a function.  This causes the escrow to
-- be updated, and possibly deleted if it terminated with a `spend`.
useEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    MonadTX m
  ) =>
  EscrowID argType valType -> argType -> m valType
useEscrow EscrowID{..} x = liftTX $ Fae $ do
  (fAbs, ver) <- 
    use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  let ConcreteContract f = unmakeAbstractEscrow fAbs
  (gConcM, y) <- f x
  txID <- view _thisTXID
  -- We hash with the transaction ID so that each new version reflects how
  -- it was created.  If the transaction is a known quantity, then this
  -- ensures that the version accurately reflects its effects and not those
  -- of some other, hidden, transaction.
  let newVer = digest (ver, txID)
  _escrowMap . at entID .= ((,newVer) . makeEscrowAbstract <$> gConcM)
  return y

-- | Registers a contract as a new escrow, returning its ID.  The argument
-- is a list of Haskell values marked as "bearers" of escrow-backed Fae
-- value; their backing escrows are transferred into the new escrow, so
-- that the bearer is no longer valuable in the contract that calls this
-- function.  The returned ID is completely determined by the contract in
-- which the escrow is created.
newEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType, 
    MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m (EscrowID argType valType)
newEscrow eIDs f = liftTX $ Fae $ do
  entID <- use _nextID
  -- modifies nextID
  cAbs <- makeEscrowAbstract . makeConcrete <$> makeInternalT eIDs (getFae . f)
  _escrowMap %= Map.insert entID (cAbs, entID) -- Initial version is entry ID
  return $ EscrowID entID

-- | Registers a contract publicly, with the same interface as 'newEscrow'.
-- Public contracts may only be called as transaction inputs with string
-- literal arguments, which are parsed into the actual 'argType' by the
-- provided 'Read' instance.
newContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType, 
    Versionable argType, Versionable valType,
    Typeable argType, Typeable valType, 
    Read argType, MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m ()
newContract eIDs f = liftTX $ Fae $ do
  cAbs <- makeContractAbstract . makeConcrete <$> makeInternalT eIDs (getFae . f)
  tell [cAbs]

-- | Looks up a named signatory, maybe. 
lookupSigner :: (MonadTX m) => String -> m (Maybe PublicKey)
lookupSigner s = liftTX $ Fae $ view $ _txSigners . at s

-- | Looks up a named signatory, or throws if not found.
signer :: (MonadTX m) => String -> m PublicKey
signer s = fromMaybe (throw $ MissingSigner s) <$> lookupSigner s

