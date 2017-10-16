{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.MonadFae where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class

import Data.Dynamic
import Data.Sequence (Seq)
import Data.Typeable

import qualified Data.Map as Map

{- Typeclasses -}

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
  liftFae :: Fae argType valType a -> m a

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

  liftFae = lift . liftFae

-- | An @UndecidableInstance@ for sure
instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadTX m, Monad (t m)) => 
  MonadTX (t m) where

  liftTX = lift . liftTX

instance 
  (HasEscrowIDs argType, HasEscrowIDs valType) => 
  MonadContract argType valType (Fae argType valType) where

  liftFae = id

instance (Functor s) => MonadTX (FaeContract s) where
  liftTX (Fae xM) = mapSuspension (const undefined) xM

deriving instance (Functor s) => MonadTX (FaeM s)

{- Functions -}

-- | This function is used in an expression like @nextArg <- release
-- currentValue@ to suspend the current contract, simultaneously
-- releasing an intermediate value, and awaiting its next call to
-- continue with the arg that was passed.
release :: 
  (HasEscrowIDs valType, NFData valType, MonadContract argType valType m) => 
  valType -> m argType
release x = liftFae $ Fae $ do
  req <- internalSpend x
  suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
    lift $ modify $ Map.union inputEscrows
    runTXEscrows y

-- | This function is like 'return' but also ensures that the returned
-- value is passed with its backing escrows, maintaining its value.  Once
-- a contract terminates with a 'spend', it is removed from storage.
spend :: 
  (HasEscrowIDs valType, NFData valType, MonadTX m) => 
  valType -> m (WithEscrows valType)
spend = liftTX . Fae . internalSpend 

-- | Calls the given escrow by ID as a function.
useEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    MonadTX m
  ) =>
  EscrowID argType valType -> argType -> m valType
useEscrow eID arg = liftTX $ Fae $ do
  val <- internalUseEscrow (entID eID) arg
  runTXEscrows val

-- | Registers a contract as a new escrow, returning its ID.  The first
-- argument is a list of Haskell values marked as "bearers" of
-- escrow-backed Fae value; their backing escrows are transferred into the new
-- escrow, so that the bearer is no longer valuable in the contract that
-- calls this function.
newEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType, 
    NFData argType, NFData valType,
    MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m (EscrowID argType valType)
newEscrow eIDs f = liftTX $ Fae $ do
  cAbs <- makeEscrow eIDs f
  eID@(EscrowID entID) <- nextEscrowID
  modify $ Map.insert entID cAbs
  return eID

nextEscrowID :: (Functor s) => FaeContract s (EscrowID argType valType)
nextEscrowID = lift $ lift $ Wrapped $ do
  eID <- get
  modify digest
  return $ EscrowID eID

-- | Registers a contract publicly.  The first argument is the same as for
-- 'newEscrow'.  The second argument is a list of short contract IDs for
-- contracts that are "trusted" by the new one.  These contracts must
-- already exist and the decision to trust them is entirely on the author
-- of the new contract, most likely by manual scrutiny.  The new contract
-- may be called in a transaction with an argument that is the return value
-- of one of its trusted contracts; otherwise, its argument must be
-- literal.
newContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType, 
    Read argType, Typeable valType, 
    NFData argType, NFData valType,
    MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m ()
newContract eIDs f = liftTX $ Fae $ do
  cAbs <- makeContract eIDs f
  lift $ tell [cAbs]

-- | Gives the public key that signed the current transaction.
sender :: (MonadTX m) => m PublicKey
sender = liftTX $ Fae $ lift $ lift $ Wrapped ask

