module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto hiding ((<>))
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Data.Dynamic
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Proxy

newtype InputContract = 
  InputContract 
  { 
    getInputContract :: Fae Dynamic 
  }

newtype OutputContract = 
  OutputContract 
  { 
    getOutputContract :: ContractID -> AbstractContract 
  }

newContract :: 
  (Typeable argType, Typeable accumType, Typeable valType) =>
  [InputContract] ->
  accumType ->
  DataF argType accumType
    (
      [OutputContract],
      valType,
      accumType
    ) ->
  Contract argType accumType valType
newContract inputCs accum0 f =
  Contract
  {
    inputs = Seq.fromList <$> sequence (map getInputContract inputCs),
    accum = accum0,
    result = \retVals arg acc -> do
      (outputCs, val, acc') <- f retVals arg acc
      return (map getOutputContract outputCs, val, acc')
  }

newPureContract ::
  (Typeable argType, Typeable valType) =>
  [InputContract] ->
  (Seq Dynamic -> argType -> Fae ([OutputContract], valType)) ->
  Contract argType () valType
newPureContract inputCs f = 
  newContract inputCs () $ \retVals arg _ -> do
    (outputCs, val) <- f retVals arg
    return (outputCs, val, ())

newMonoidContract ::
  (Monoid argType, Typeable argType, Typeable valType) =>
  [InputContract] ->
  (Seq Dynamic -> argType -> argType -> Fae ([OutputContract], valType)) ->
  Contract argType argType valType
newMonoidContract inputCs f =
  newContract inputCs mempty $ \retVals arg accum -> do
    (outputCs, val) <- f retVals arg accum
    return (outputCs, val, accum <> arg)

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
  (Typeable argType, Typeable valType) =>
  Contract argType accumType valType -> OutputContract
outputContract c = OutputContract $ \cID ->
  abstract cID $ evalContract (Just cID) c

evalContract :: 
  (Typeable argType, Typeable valType) =>
  Maybe ContractID -> Contract argType accumType valType -> argType -> Fae valType
evalContract thisIDM c@Contract{..} arg = do
  inputSeq <- inputs
  (outputs, retVal, newAccum) <- result inputSeq arg accum
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
  Fae $ _transientState . _contractUpdates . _useContracts . at cID ?= 
    Right (f cID)

