module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto hiding ((<>))
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Fae
import Blockchain.Fae.Internal.Lens

import Control.Monad.State
import Control.Monad.Trans

import Data.Dynamic
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Proxy

data Contract argType accumType valType =
  Contract
  {
    inputs :: Fae (Seq Dynamic), -- Invocation return values
    result :: ResultF argType accumType valType AbstractIDContract,
    accum :: accumType
  }

type AbstractIDContract = ContractID -> AbstractContract
type ResultF argType accumType valType output =
  Seq Dynamic -> argType -> StateT accumType Fae ([output], valType)

data CallTree =
  CallTree
  {
    directInputs :: [InputContract],
    outputTrees :: [CallTree]
  }

newtype InputContract = 
  InputContract 
  { 
    getInputContract :: Fae Dynamic 
  }

newtype OutputContract = 
  OutputContract 
  { 
    getOutputContract :: CallTree -> AbstractIDContract
  }

makeLenses ''Contract

newContract :: 
  (Typeable argType, Typeable accumType, Typeable valType) =>
  CallTree ->
  accumType ->
  ResultF argType accumType valType OutputContract ->
  Contract argType accumType valType
newContract callTree accum0 f =
  Contract
  {
    inputs = Seq.fromList <$> traverse getInputContract (directInputs callTree),
    accum = accum0,
    result = \retVals arg -> do
      (outputCs, val) <- f retVals arg
      let 
        xOutputCs = 
          outputCs ++ repeat (OutputContract $ \_ -> throw . MissingOutput)
        outputs = zipWith getOutputContract xOutputCs (outputTrees callTree) 
      return (outputs, val)
  }

newPureContract ::
  (Typeable argType, Typeable valType) =>
  CallTree ->
  (Seq Dynamic -> argType -> Fae ([OutputContract], valType)) ->
  Contract argType () valType
newPureContract callTree f = 
  newContract callTree () $ \retVals arg -> lift $ f retVals arg

inputContract :: (Typeable a) => ContractID -> a -> InputContract
inputContract !cID !x = InputContract $ do
  cM <- Fae $ use $ _transientState . _contractUpdates . _useContracts . at cID
  fE <- maybe
    (throwM $ BadContractID cID)
    return
    cM
  either
    (throwM . BadContract cID) 
    ($ toDyn x)
    fE

outputContract :: 
  (Typeable argType, Typeable accumType, Typeable valType) =>
  accumType ->
  ResultF argType accumType valType OutputContract ->
  OutputContract
outputContract accum0 f = OutputContract $ \callTree cID ->
  abstract cID $ evalContract (Just cID) $ newContract callTree accum0 f

evalContract :: 
  (Typeable argType, Typeable valType) =>
  Maybe ContractID -> Contract argType accumType valType -> argType -> Fae valType
evalContract thisIDM c@Contract{..} arg = do
  inputSeq <- inputs
  ((outputs, retVal), newAccum) <- runStateT (result inputSeq arg) accum
  sequence_ $ zipWith newOutput [0 ..] outputs
  case thisIDM of
    Nothing -> return ()
    Just thisID -> setContract thisID $ evalContract thisIDM c{accum = newAccum}
  return retVal

setContract :: 
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ContractID -> (argType -> Fae valType) -> Fae ()
setContract cID f = Fae $
  _transientState . _contractUpdates . _useContracts . at cID ?= 
    Right (abstract cID f)

abstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ContractID -> (argType -> Fae valType) -> AbstractContract
abstract cID f argDyn = 
  case toDyn f `dynApply` argDyn of
    Nothing -> 
      throwM (BadArgType cID (dynTypeRep argDyn) $ typeRep (Proxy @argType))
    Just x -> return x

newOutput :: Int -> (ContractID -> AbstractContract) -> Fae ()
newOutput i f = do
  cID <- ContractID <$> uniqueDigest i
  cOrErr <- try (evaluate $ f cID) 
  Fae $ _transientState . _contractUpdates . _useContracts . at cID ?= cOrErr

