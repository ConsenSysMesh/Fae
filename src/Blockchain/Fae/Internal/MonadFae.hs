{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.MonadFae where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class

import Data.Dynamic
import Data.Sequence (Seq)
import Data.Typeable

import qualified Data.Map as Map

{- Typeclasses -}

class 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadTX m) => 
  MonadContract argType valType m | m -> argType valType where

  liftFae :: Fae argType valType a -> m a
  release :: valType -> m argType

class (Monad m) => MonadTX m where
  liftTX :: FaeTX a -> m a
  spend :: 
    (HasEscrowIDs valType) => 
    valType -> m (WithEscrows valType)
  useEscrow :: 
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    EscrowID argType valType -> argType -> m valType
  newEscrow :: 
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    [BearsValue] -> Contract argType valType -> m (EscrowID argType valType)
  newContract ::
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    [BearsValue] -> [ShortContractID] -> Contract argType valType -> m ()

{- Instances -}

instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadContract argType valType m, Monad (t m)) =>
  MonadContract argType valType (t m) where

  liftFae = lift . liftFae
  release = lift . release

instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadTX m, Monad (t m)) => 
  MonadTX (t m) where

  liftTX = lift . liftTX
  spend = lift . spend
  useEscrow eID arg = lift $ useEscrow eID arg
  newEscrow xs c = lift $ newEscrow xs c
  newContract xs trusts c = lift $ newContract xs trusts c

instance MonadContract argType valType (Fae argType valType) where
  liftFae = id
  release x = Fae $ do
    req <- spend x
    suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
      lift $ modify $ Map.union inputEscrows
      return y

instance (Functor s) => MonadTX (FaeContract s) where
  liftTX (Fae xM) = mapSuspension (const undefined) xM

  spend = internalSpend 

  useEscrow (EscrowID eID) x = do
    fAbs <- use $ at eID . defaultLens (throw $ BadEscrowID eID)
    let ConcreteContract f = unmakeAbstract fAbs
    (gAbsM, y) <- f x
    at eID .= gAbsM
    return y

  newEscrow eIDs f = do
    cAbs <- makeContract eIDs f
    eID <- lift $ lift $ Wrapped $ do
      eID <- get
      _2 += 1
      return eID
    modify $ Map.insert eID cAbs
    return $ EscrowID eID

  newContract eIDs trusts f = do
    cAbs <- makeContract eIDs f
    lift $ lift $ Wrapped $ 
      tell [Trusted cAbs trusts]

deriving instance (Functor s) => MonadTX (FaeM s)

{- Functions -}

sender :: (MonadTX m) => m PublicKey
sender = liftTX $ Fae $ lift $ lift $ Wrapped ask

