module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto 
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Monads

import Control.Monad.Fix
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Trans

import Data.Dynamic
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Proxy

class (Typeable result) => FaeReturn result valType where
  faeReturn :: valType -> AnyFae (result, Maybe (EntryID, Dynamic))

instance (Typeable valType) => FaeReturn valType valType where
  faeReturn x = return (x, Nothing)

instance {-# OVERLAPPING #-} 
  (Typeable argType, Typeable valType) =>
  FaeReturn (EscrowID argType valType) (Escrow argType valType) where

  faeReturn e = Fae $ do
    eID <- EntryID . digest <$> lift (FaeContract $ view _contractID)
    return (EscrowID eID, Just (eID, toDyn e))

instance {-# OVERLAPPING #-} 
  (Typeable argType, Typeable valType) =>
  FaeReturn (PrivateEscrowID argType valType) (PrivateEscrow argType valType) where

  faeReturn e = Fae $ do
    eID <- EntryID . digest <$> lift (FaeContract $ view _contractID)
    return (PrivateEscrowID eID, Just (eID, toDyn e))

concrete :: 
  forall result argType accumType valType.
  (Typeable argType, Typeable result, FaeReturn result valType) =>
  StateData accumType ->
  Fae argType accumType valType -> 
  ConcreteContract argType result
concrete state f InputData{..} = do -- FaeContract
  let 
    state' =
      case inputEscrow of
        Nothing -> state
        Just (eID, escrow) -> state & _escrows %~ Map.insert eID escrow
  ((retVal, newEscrow), newState, outputContracts) <-
    runRWST (getFae $ faeReturn =<< f) inputArg state'
  contractID <- FaeContract $ view $ _contractID
  when (not $ Map.null $ escrows newState) $
    throw $ OpenEscrows contractID
  return $ 
    OutputData 
    {
      updatedContract = 
        if spent newState
        then Nothing
        else Just $ abstract $ concrete @result newState f,
      ..
    }    

abstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
abstract c InputData{..} = do
  cID <- FaeContract $ view _contractID
  let 
    arg = fromDyn inputArg $
      throw $ BadArgType cID (typeRep (Proxy @argType)) (dynTypeRep inputArg)
  val <- c arg
  return $ val & _retVal %~ toDyn

returnContract ::
  (Typeable argType, Typeable valType) =>
  accumType -> [AnyEscrowID] -> 
  Fae argType accumType valType ->
  AnyFae (ConcreteContract argType valType)
returnContract accum gives contract = Fae $ do
  escrowList <- forM gives $ \(SomeEscrowID (EscrowID eID)) -> do
    entry <- use $ _escrows . at eID . defaultLens (throw $ BadEscrowID eID)
    _escrows %= sans eID
    return (eID, entry)
  let escrows = Map.fromList escrowList
  let state = StateData{spent = False, ..}
  contractID <- lift $ FaeContract $ view _contractID
  let inputs = Seq.fromList $ zip (repeat contractID) $ map (toDyn.fst) escrowList
  return $ 
    FaeContract . local (_inputs .~ inputs) . getFaeContract . 
    concrete state contract

spend :: Fae argType accumType ()
spend = Fae $ _spent .= True

class IsEscrow escrow where
  escrow :: 
    (Typeable argType, Typeable valType) => 
    ConcreteContract argType valType -> escrow argType valType

instance IsEscrow Escrow where
  escrow = Escrow

instance IsEscrow PrivateEscrow where
  escrow = PrivateEscrow

returnEscrow ::
  forall escrow argType valType.
  (Typeable argType, Typeable valType, IsEscrow escrow) =>
  [AnyEscrowID] -> 
  Fae argType () valType ->
  AnyFae (escrow argType valType)
returnEscrow gives contract = escrow <$> returnContract () gives contract

transferEscrow ::
  (Typeable argType, Typeable valType) =>
  EscrowID argType valType ->
  AnyFae (Escrow argType valType)
transferEscrow (EscrowID eID) = Fae $ do
  eDyn <- use $ _escrows . at eID . defaultLens (throw $ BadEscrowID eID)
  let 
    e = 
      fromDyn eDyn $ 
        throw $ BadEscrowType eID (dynTypeRep $ toDyn eID) (dynTypeRep eDyn)
  _escrows %= sans eID
  return e

outputContract :: 
  (Typeable argType, Typeable valType) =>
  accumType -> [AnyEscrowID] ->
  Fae argType accumType valType ->
  AnyFae ContractID
outputContract accum gives contract = do
  (c, i) <- Fae $ listens Seq.length $ getFae $ returnContract accum gives contract
  Fae $ tell $ Seq.singleton $ abstract c
  outputID <- Fae $ lift $ FaeContract $ view $ _outputID . to ($ i)
  return outputID

open ::
  forall argType valType argType' accumType'.
  (Typeable argType, Typeable valType) =>
  [AnyEscrowID] ->
  Fae argType () valType ->
  AnyFae (EscrowID argType valType)
open gives contract = do
  (e :: Escrow argType valType) <- returnEscrow gives contract
  (esID, Just (eID, eDyn)) <- faeReturn e
  Fae $ _escrows . at eID ?= eDyn
  return esID

class HasEntryID escrowID where
  entryID :: escrowID argType valType -> EntryID

instance HasEntryID EscrowID where
  entryID (EscrowID eID) = eID

instance HasEntryID PrivateEscrowID where
  entryID (PrivateEscrowID eID) = eID

close :: 
  forall input result argType escrowID.
  (HasEntryID escrowID, FaeReturn input argType, Typeable input, Typeable result) =>
  escrowID input result -> argType -> AnyFae result
close escrowID arg = do
  let eID = entryID escrowID
  eDyn <- Fae $ use $ _escrows . at eID . defaultLens (throw $ BadEscrowID eID)
  let 
    Escrow e = 
      fromDyn eDyn $ 
        throw $ BadEscrowType eID (dynTypeRep $ toDyn eID) (dynTypeRep eDyn)
  (inputArg :: input, inputEscrow) <- faeReturn arg
  OutputData{..} <- Fae $ lift $ e InputData{..}
  when (isNothing updatedContract) $ Fae $ _escrows %= sans eID 
  Fae $ tell outputContracts
  case newEscrow of
    Nothing -> return retVal
    Just (eID, escrow) -> Fae $ do
      _escrows . at eID ?= escrow
      return retVal

inputValue ::
  forall valType.
  (Typeable valType) =>
  Int -> AnyFae valType
inputValue i = Fae $ do
  inputM <- lift $ FaeContract $ asks $ Seq.lookup i . inputs
  (inputID, inputDyn) <-
    maybe
      (throw $ MissingInput i)
      return
      inputM
  let err = BadValType inputID (typeRep $ Proxy @valType) (dynTypeRep inputDyn)
  maybe (throw err) return $ fromDynamic inputDyn

