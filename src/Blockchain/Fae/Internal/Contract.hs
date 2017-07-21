module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto 
import Blockchain.Fae.Internal.Escrow
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Monads

import Control.Monad.Fix
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Trans

import Data.Dynamic
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Proxy

type ConcreteContract argType valType = argType -> FaeContract valType

newtype Contract argType accumType valType =
  Contract
  {
    getContract :: FaeContract (Fae argType accumType valType)
  }

concrete :: 
  (Typeable argType, Typeable valType) =>
  StateData accumType ->
  Contract argType accumType valType -> 
  ConcreteContract argType valType
concrete state contract arg = do -- FaeContract
  Fae f <- getContract contract
  escrowArgM <- FaeContract $ view _escrowArg
  let
    state' = 
      case escrowArgM of
        Nothing -> state
        Just (eID, entry) -> state & _escrows . _useEscrows . at eID ?~ entry
    inputValues = Seq.empty
    input = InputData{..}
    (retVal, newState, outputs) = runRWS f input state'
  sequence_ $ zipWith newOutput [0 ..] outputs
  setContract $ concrete newState contract
  return retVal

newOutput :: Integer -> AbstractContract -> FaeContract ()
newOutput basename f = FaeContract $ do
  cID <- view $ _callPath . to (flip ContractID basename)
  let f' arg = pushContractState (_contractID .~ cID) $ f arg
  lift $ FaeTX $ at cID ?= f'

setContract :: 
  (Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> 
  FaeContract ()
setContract f = FaeContract $ do
  cID <- view _contractID
  lift $ FaeTX $ at cID ?= abstract f

abstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
abstract c argDyn = do -- FaeContract
  cID <- FaeContract $ view _contractID
  let 
    arg = fromDyn argDyn $
      throw $ BadArgType cID (typeRep (Proxy @argType)) (dynTypeRep argDyn)
  val <- c arg
  return $ toDyn val

-- Construction functions

newContract :: 
  Fae argType accumType valType -> 
  Fae argType' accumType' (Contract argType accumType valType)
newContract = return . Contract . return

class ContractInput a where
  inputContract :: 
    (Typeable a) => 
    ContractID -> a -> 
    Contract argType accumType valType ->
    Fae argType' accumType' (Contract argType accumType valType)

inputContract_ ::
  (Typeable a) =>
  Maybe (EntryID, Dynamic) ->
  ContractID -> a ->
  Contract argType accumType valType ->
  Fae argType' accumType' (Contract argType accumType valType)
inputContract_ eArg cID x (Contract mf) = return $ Contract $ do -- FaeContract
  CallPath path <- FaeContract $ view _callPath
  c <- FaeContract $ lift $ FaeTX $ use $ 
    at cID . 
    defaultLens (throw $ BadContractID cID)
  let 
    newPath = CallPath $ snoc path $ shorten cID
    update s =
      s & _callPath .~ newPath
        & _escrowArg .~ eArg
  y <- pushContractState update (c $ toDyn x)
  let update = _inputValues %~ flip snoc (cID, y)
  pushFaeState update <$> mf

instance ContractInput a where
  inputContract = inputContract_ Nothing

instance {-# OVERLAPPING #-} ContractInput (EscrowID tokT pubT privT) where
  inputContract cID x c0 = do
    (eID, entry) <- getEscrow $ entryID x
    inputContract_ (Just (eID, entry)) cID x c0

getEscrow :: EntryID -> Fae argType accumType (EntryID, Dynamic)
getEscrow eID = Fae $ do
  entry <- use $ _escrows . _useEscrows . at eID .
    defaultLens (throw $ BadEscrowID eID)
  _escrows . _useEscrows %= sans eID
  return (eID, entry)

outputContract :: 
  (Typeable argType, Typeable valType) =>
  accumType -> [EntryID] ->
  Contract argType accumType valType ->
  Fae argType' accumType' ()
outputContract accum gives contract = do
  escrows <- Escrows . Map.fromList <$> traverse getEscrow gives
  let state = StateData{..}
  Fae $ tell [abstract $ concrete state contract]

inputValue ::
  forall argType accumType valType.
  (Typeable valType) =>
  Int -> Fae argType accumType valType
inputValue i = do
  inputM <- Fae $ asks $ Seq.lookup i . inputValues
  (inputID, inputDyn) <-
    maybe
      (throw $ MissingInput i)
      return
      inputM
  let err = BadValType inputID (typeRep $ Proxy @valType) (dynTypeRep inputDyn)
  maybe (throw err) return $ fromDynamic inputDyn

