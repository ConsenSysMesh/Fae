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

class FaeReturn result valType where
  faeReturn :: valType -> Fae argType accumType (result, Maybe (EntryID, Dynamic))

instance FaeReturn valType valType where
  faeReturn x = return (x, Nothing)

instance {-# OVERLAPPING #-} 
  (Typeable argType, Typeable valType) =>
  FaeReturn (EscrowID argType valType) (Escrow argType valType) where

  faeReturn e = Fae $ do
    eID <- EntryID . digest <$> lift (FaeContract $ view _contractID)
    return (EscrowID eID, Just (eID, toDyn e))

concrete :: 
  forall result argType accumType valType.
  (Typeable argType, Typeable result, FaeReturn result valType) =>
  StateData accumType ->
  Fae argType accumType valType -> 
  ConcreteContract argType result
concrete state f arg = do -- FaeContract
  ((retVal, newEscrow), newState, outputContracts) <-
    runRWST (getFae $ faeReturn =<< f) arg state
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
abstract c argDyn = do
  cID <- FaeContract $ view _contractID
  let 
    arg = fromDyn argDyn $
      throw $ BadArgType cID (typeRep (Proxy @argType)) (dynTypeRep argDyn)
  val <- c arg
  return $ val & _retVal %~ toDyn

returnContract ::
  (Typeable argType, Typeable valType) =>
  accumType -> [EntryID] -> 
  Fae argType accumType valType ->
  Fae argType' accumType' (ConcreteContract argType valType)
returnContract accum gives contract = Fae $ do
  escrowList <- forM gives $ \eID -> do
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

returnEscrow ::
  (Typeable argType, Typeable valType) =>
  [EntryID] -> 
  Fae argType () valType ->
  Fae argType' accumType' (Escrow argType valType)
returnEscrow gives contract = Escrow <$> returnContract () gives contract

outputContract :: 
  (Typeable argType, Typeable valType) =>
  accumType -> [EntryID] ->
  Fae argType accumType valType ->
  Fae argType' accumType' ContractID
outputContract accum gives contract = do
  (c, i) <- Fae $ listens Seq.length $ getFae $ returnContract accum gives contract
  Fae $ tell $ Seq.singleton $ abstract c
  outputID <- Fae $ lift $ FaeContract $ view $ _outputID . to ($ i)
  return outputID

open ::
  (Typeable argType, Typeable valType) =>
  [EntryID] ->
  Fae argType () valType ->
  Fae argType' accumType' (EscrowID argType valType)
open gives contract = do
  e <- returnEscrow gives contract
  (esID, Just (eID, eDyn)) <- faeReturn e
  Fae $ _escrows . at eID ?= eDyn
  return esID

close :: 
  (Typeable argType, Typeable result, FaeReturn result valType) =>
  EscrowID argType valType -> argType -> Fae argType' accumType' result
close (EscrowID eID) arg = do
  eDyn <- Fae $ use $ _escrows . at eID . defaultLens (throw $ BadEscrowID eID)
  let 
    Escrow e = 
      fromDyn eDyn $ 
        throw $ BadEscrowType eID (dynTypeRep $ toDyn eID) (dynTypeRep eDyn)
  OutputData{..} <- Fae $ lift $ e arg
  when (isNothing updatedContract) $ Fae $ _escrows %= sans eID 
  Fae $ tell outputContracts
  case newEscrow of
    Nothing -> return retVal
    Just (eID, escrow) -> Fae $ do
      _escrows . at eID ?= escrow
      return retVal

inputValue ::
  forall argType accumType valType.
  (Typeable valType) =>
  Int -> Fae argType accumType valType
inputValue i = Fae $ do
  inputM <- lift $ FaeContract $ asks $ Seq.lookup i . inputs
  (inputID, inputDyn) <-
    maybe
      (throw $ MissingInput i)
      return
      inputM
  let err = BadValType inputID (typeRep $ Proxy @valType) (dynTypeRep inputDyn)
  maybe (throw err) return $ fromDynamic inputDyn

