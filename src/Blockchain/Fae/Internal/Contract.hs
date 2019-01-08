{- |
Module: Blockchain.Fae.Internal.Contract
Description: The core 'Contract' type that underlies Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Everything in Fae is a contract, and this module defines contracts and
their construction.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Suspend

import Common.Lens

import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.DeepSeq

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Typeable

import qualified Data.Map as Map

import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import GHC.Generics (Generic)

import Text.Read (readMaybe)

-- * Types
-- ** Transaction data

-- | The relevant transaction info
data TXData =
  TXData
  {
    thisTXSigners :: Signers,
    localHash :: Digest,
    thisTXID :: TransactionID,
    localMaterials :: MaterialsMap
  }

-- | Useful shorthand.
type MaterialsMap = Map String ReturnValue

-- | A single declaration of a material (or signer) for a nested call.
data Assignment = Assignment Assignable String

-- | The (private) constructors allow one of the following options: remap
-- a signer's role name; remap a material's role name; introduce a new
-- material.
data Assignable = 
  SignerName String | MaterialsName String | NewMaterial ReturnValue

-- | Similar to 'WithEscrows' below, but one level more abstract: packages
-- a contract argument with a set of implicit "materials" values whose
-- escrows can be extracted in 'acceptLocal' (using the 'Container' modifier).
data WithMaterials a = WithMaterials (Container MaterialsMap) a 

-- ** Contract data

-- | Another dynamic type, this time including 'Exportable'.  Its
-- constructor is strict, so as not to distinguish between an error in the
-- outer value and an error in the inner value.
data ReturnValue =
  forall a. (HasEscrowIDs a, Exportable a) => ReturnValue !a

-- | Something like a closure for the initial state of a new contract
-- (currently only escrows).  The actual contract function can be
-- reconstructed from this, but a 'NamedContract' itself is 'Serializable'
-- while a 'ContractF' is not.
--
-- The fields are strict to ensure that undefined values are detected in
-- the present contract call, and not unexpectedly, later.
data NamedContract name =
  NamedContract
  {
    contractName :: !name,
    contractNextID :: !Digest,
    endowment :: !EscrowMap
  }
  deriving (Generic)
-- | The union of contract entry types for all possible names.
data AnyNamedContract = 
  forall name. (Typeable name) => AnyNamedContract (NamedContract name)
-- | Each escrow has a continually updated version that we need to track;
-- this holds both for the escrow map of a contract and the escrows
-- accompanying a returned value.
--
-- The fields are strict to ensure that undefined values are detected in
-- the present contract call, and not unexpectedly, later.
data EscrowEntry =
  EscrowEntry
  {
    escrowVersion :: !VersionID,
    escrowNameOrFunction :: !(Either AnyNamedContract AbstractLocalContract)
  }
-- | Convenience type.
type EscrowMap = Map EntryID EscrowEntry
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via 'spend' and 'release'.
--
-- This is actually a 'Monad' isomorphic to @Writer EscrowMap@, but it is
-- more work than it is worth to write the instances for that.
data WithEscrows a = 
  WithEscrows 
  {
    withEscrows :: EscrowMap,
    getWithEscrows :: a
  }
  deriving (Generic)
-- | How escrows are kept in each contract.  Contracts track their escrow
-- ID sequence separately to prevent coupling between different ones.
data Escrows =
  Escrows
  {
    escrowMap :: EscrowMap,
    nextID :: Digest
  }

-- ** Contract outputs

-- | Stores the actual contract function, the type of its 'ContractName'
-- for exporting purposes, and the "nonce", or number of successful contract
-- calls.
data Output =
  Output
  {
    storedContract :: Maybe StoredContract,
    -- | This is not @Maybe@ because even when the contract is spent, we
    -- need its type for exporting.
    contractNameType :: TypeRep ,
    -- | Likewise, this is needed to type-check the contract when it is
    -- called (without examining the possibly-uncomputable return value).
    contractValType :: TypeRep
  }
  deriving (Generic)

-- | If the contract can be called again, this contains its function (which
-- is called) and nonce (which is verified when the contract ID used
-- contains a nonce).
data StoredContract =
  StoredContract
  {
    storedFunction :: AbstractGlobalContract,
    storedVersion :: VersionID
  }
  deriving (Generic)

-- ** Internal contract monads

-- | Monad modifier; several of ours use escrows.
type EscrowsT = StateT Escrows
-- | Also used in "Transaction", so needs a name
type TXDataM = Reader TXData
-- | The internal operational monad for Fae contracts.
type FaeExternalM = WriterT [Output] TXDataM
-- | The authoring monad for Fae contracts (when wrapped in 'Fae')
type FaeContractM argType valType = 
  EscrowsT (SuspendT (WithEscrows argType) (WithEscrows valType) FaeExternalM)
-- | The authoring monad for Fae transactions (when wrapped in 'FaeTX')
type FaeTXM = EscrowsT FaeExternalM
-- | The monad in which the transaction body is run.  Escrows are kept, but
-- not for casual use; the list is a stack tracing the descent into
-- sub-input calls.
type TXBodyM = StateT [EscrowMap] TXDataM

-- ** Internal contract functions

-- | User contract with escrows hidden
type PreContractF argType valType = 
  SuspendPreF (WithEscrows argType) (WithEscrows valType) FaeExternalM
-- | A type-correct contract function; 'WithEscrows' is omitted here
-- because neither of the abstract contracts takes it (though they do use
-- it internally).
newtype ContractF m argType valType = 
  ContractF { getContractF :: SuspendStepF argType valType m }
  deriving (Generic, NFData)

-- ** External contract functions

-- | The form of a contract function intended to be called from within
-- a contract, as well as a precursor to an 'AbstractGlobalContract'.
type AbstractLocalContract = ContractF FaeTXM BearsValue BearsValue
-- | The form of a contract function intended to be called from
-- a transaction.  Note that it returns into a 'TXBodyM', i.e. does not
-- expose the outputs state, which is returned read-only in the return value.
type AbstractGlobalContract = 
  ContractF TXBodyM String (WithEscrows ReturnValue, [Output])

-- ** User-visible

-- | The monad that users actually write in, a newtype because we need to
-- limit the capabilities users are allowed to access.
newtype Fae argType valType a = 
  Fae 
  {
    getFae :: FaeContractM argType valType a
  }
  deriving (Functor, Applicative, Monad)
-- | Monad for writing transactions (no continuation), a newtype because we
-- need to limit the capabilities users are allowed to access.
newtype FaeTX a = 
  FaeTX 
  {
    getFaeTX :: FaeTXM a
  }
  deriving (Functor, Applicative, Monad)

-- | The user-provided form of a contract function.  Concretely, this is:
--
-- >>> type Contract argType valType = argType -> Fae argType valType (WithEscrows valType)
--
-- That is, a function from the argument type to the escrow-bearing value
-- type, inside a Fae monad that expects these argument and value types.
type Contract argType valType = ContractT (Fae argType valType) argType valType
-- | Useful generalization to add effects
type ContractT m argType valType = argType -> m (WithEscrows valType)

-- * Fae typeclasses

-- | Instances of this class can be serialized, at least with the
-- assistance of some Fae contextual data (namely, the escrow storage).
class (Typeable a) => Exportable a where
  -- | A lousy imitation of the 'put' function from 'Serialize' that
  -- doesn't use a builder but goes straight to bytes.
  exportValue :: (MonadState EscrowMap m) => a -> m ByteString
  -- | A lousy imitation of the 'get' function from 'Serialize' that
  -- doesn't use a parser but takes straight from bytes.
  importValue :: (MonadState EscrowMap m) => ByteString -> m (Maybe a)

-- | Instances of 'ContractName' are always defined by contract authors,
-- making 'theContract' a global function.  Since it's global, if a name is
-- serialized and then deserialized in another Fae instance, the function is
-- still present when we deserialize, so this is effectively a contract
-- that is portable between Fae instances (provided that both instances
-- have the module with the instance; it need not be run, just
-- interpreted).
--
-- A 'ContractName' is the only way to pass data into a new contract, so
-- the type should have fields for all the parameters of the contract,
-- /including/ valuables that it is supposed to "own".  Their escrows are
-- deposited at the beginning of the contract.
class 
  (HasEscrowIDs a, Typeable (ArgType a), HasEscrowIDs (ValType a)) => 
  ContractName a where

  type ArgType a
  type ValType a
  theContract :: a -> Contract (ArgType a) (ValType a)

-- |
-- Instances of this class have access to the full Fae API, allowing them
-- to define multi-stage contracts.  As for 'MonadTX', these instances must
-- have their own evaluation function to get down to the base 'Fae' monad.
-- Notably, @Transaction@s are /not/ written in a 'MonadContract', because
-- they are one-shot.
class (MonadTX m) => MonadContract argType valType m | m -> argType valType where
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

-- * Template Haskell

makeLenses ''WithEscrows
makeLenses ''Escrows
makeLenses ''TXData
makeLenses ''Output
makeLenses ''StoredContract

{- Instances -}

-- | -
instance NFData Output

-- | -
instance NFData StoredContract

-- | -
instance NFData EscrowEntry where
  rnf = (`seq` ())

-- | -
instance HasEscrowIDs ReturnValue where
  traverseEscrowIDs f (ReturnValue x) = ReturnValue <$> traverseEscrowIDs f x

-- | -
instance (HasEscrowIDs a) => HasEscrowIDs (WithMaterials a) where
  traverseEscrowIDs f (WithMaterials mM x) =
    WithMaterials
      <$> traverseEscrowIDs f mM
      <*> traverseEscrowIDs f x

-- | -
instance 
  (Typeable (t a), Exportable a, Traversable t, Serialize (t ByteString)) => 
  Exportable (Container (t a)) where
    
  exportValue = fmap S.encode . traverse exportValue . getContainer
  importValue = 
    either (const $ return Nothing) 
           (fmap (fmap Container . sequence) . traverse importValue) . 
    S.decode

-- | -
instance {-# OVERLAPPABLE #-}
  (Typeable a, Serialize a) => Exportable (Container a) where

  exportValue = return . S.encode . getContainer
  importValue = return . either (const Nothing) (Just . Container) . S.decode

-- | -
instance (HasEscrowIDs a, HasEscrowIDs b) => ContractName (Contract a b) where
  type ArgType (Contract a b) = a
  type ValType (Contract a b) = b
  theContract = id

-- | -
instance MonadTX FaeTX where
  liftTX = id

-- | -
instance MonadTX FaeTXM where
  liftTX = getFaeTX

-- | -
instance MonadTX (Fae argType valType) where
  liftTX = Fae . mapStateT lift . getFaeTX

-- | -
instance MonadContract argType valType (Fae argType valType) where
  liftContract = id

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

-- * API Functions

-- | Looks up a material by name, maybe.  A 'Nothing' can mean that the
-- name was not found /or/ that the named value had the wrong type.
lookupMaterial :: forall a m. (MonadTX m, Typeable a) => String -> m (Maybe a)
lookupMaterial name = liftTX . FaeTX $
  view _localMaterials <&> (Map.lookup name >=> getReturnValue) 

-- | Looks up a material by name, throwing individual errors in case of
-- missing name or incorrect material type under an existing name.
material :: forall a m. (MonadTX m, Typeable a) => String -> m a
material name = liftTX . FaeTX $
  getRV . Map.findWithDefault missingErr name <$> view _localMaterials 
  where
    getRV xRV = fromMaybe (badTypeErr xRV) $ getReturnValue xRV
    missingErr = throw $ MissingMaterial name
    badTypeErr xRV = 
      throw $ BadMaterialType name (typeRep $ Proxy @a) (returnValueType xRV)

-- | Uses all the materials in the map with a given type, ignoring the ones
-- with other types.
materials :: forall a m. (MonadTX m, Typeable a) => m (Map String a)
materials = liftTX . FaeTX $ Map.mapMaybe getReturnValue <$> view _localMaterials

-- | Looks up a named signatory, maybe. 
lookupSigner :: (MonadTX m) => String -> m (Maybe PublicKey)
lookupSigner s = liftTX $ FaeTX $ view $ _thisTXSigners . _getSigners . at s

-- | Looks up a named signatory, or throws a 'MissingSigner' if not found.
signer :: (MonadTX m) => String -> m PublicKey
signer s = fromMaybe (throw $ MissingSigner s) <$> lookupSigner s

-- | Returns the map of all signatories.  This map is immutable within
-- a single transaction, but when descending into a contract or escrow
-- call, may be modified by "renamings" that reassign old roles' public
-- keys to new role names.
signers :: (MonadTX m) => m (Map String PublicKey)
signers = liftTX $ FaeTX $ view $ _thisTXSigners . _getSigners

-- | Terminates the contract entirely, returning its argument and
-- automatically transferring escrows backing the return value.
spend :: 
  forall m argType valType.
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m (WithEscrows valType)
spend = liftContract . Fae . (takeEscrows >=> lift . terminate)

-- | Terminates the current contract call, returning its argument and
-- automatically transferring escrows backing the return value to the
-- caller.  A 'release' expression evaluates to the argument of the
-- subsequent contract call, whose escrows are automatically deposited into
-- the contract at that point.
release :: 
  forall argType valType m.
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release = liftContract . Fae . (takeEscrows >=> lift . suspend >=> putEscrows)

-- | Emits a new output contract with a given 'ContractName', whose backing
-- escrows are deposited into the contract's context at the first call.
-- Unlike 'newEscrow', this returns nothing rather than the 'ContractID',
-- because there is no use for contract IDs within contract code.
newContract :: 
  forall name m.
  (
    ContractName name, Read (ArgType name), Exportable (ValType name),
    MonadTX m
  ) => 
  name -> m ()
newContract x = liftTX $ FaeTX $ do
  (storedVersion, nextID) <- forkNextID
  xE <- takeEscrows x
  let storedFunction = 
        globalContract $ hideEscrows (withEscrows xE) nextID (theContract x)
      contractNameType = typeRep $ Proxy @name
      contractValType = typeRep $ Proxy @(ValType name)
      storedContract = Just StoredContract{..}
  tell [Output{..}]

-- | Creates a new escrow with a given 'ContractName', whose backing
-- escrows are deposited into the new escrow's context at the first call.
newEscrow :: 
  (ContractName name, MonadTX m) =>
  name -> m (EscrowID name)
newEscrow contractName = liftTX $ FaeTX $ do
  (entID, contractNextID) <- forkNextID
  WithEscrows endowment _ <- takeEscrows contractName
  let escrowNameOrFunction = Left $ AnyNamedContract NamedContract{..}
      escrowVersion = entID
  _escrowMap %= Map.insert entID EscrowEntry{..}
  return $ EscrowID entID

-- ** Using escrows
-- The 'useEscrow' API function gets its own subsection due to the
-- "assignments" feature, which has its own interface.

-- | Calls an escrow by ID, which must exist in the present context.  
--
-- The first argument declares the named signers and materials that the
-- escrow gets during execution.  The names declared in this list are the
-- /only/ ones available to the escrow, in order to prevent escrow authors
-- from building in back doors that respond to signers or materials passed
-- outside the control of the calling contract.
useEscrow :: 
  (ContractName name, HasEscrowIDs (ArgType name), MonadTX m) =>
  [Assignment] -> EscrowID name -> ArgType name -> m (ValType name)
useEscrow rolePairs eID x = liftTX . FaeTX . joinEscrowState . useNamedEscrow eID $
  \entID escrowVersion nameOrFunction -> return $ do
    renamedMaterials <- view $ 
      _localMaterials . to (mapNames MissingMaterial materialsRenames)
    let makeLocalCF NamedContract{..} = localContract $ 
          hideEscrows endowment contractNextID (theContract contractName)
        localCF = either makeLocalCF id nameOrFunction
        localMaterials = newMaterials `Map.union` renamedMaterials
        endowedArg = WithMaterials (Container localMaterials) x
    ~(y, resultCFM) <- typeify (callContract localCF) endowedArg & 
      remapMaterials localMaterials . remapSigners signerRenames

    -- The "local hash" corresponds either to the transaction ID (if not in
    -- a contract call) or to the arguments of the contract call, and so
    -- the version will accurately reflect the call history of the escrow.
    hash <- view _localHash
    let newVer = digest (escrowVersion, hash)

    _escrowMap . at entID .= fmap (EscrowEntry newVer . Right) resultCFM
    return y
  where typeify f = fmap (_1 %~ returnTyped) . f . acceptTyped
        (signerRenames, materialsRenames, newMaterials) = makeAssignments rolePairs

-- | Infix operator for adding a signer remap.
(<-|) :: String -> String -> Assignment
(<-|) = flip $ Assignment . SignerName

-- | Nicer Unicode alternative to '<-|' (U+21a4)
(↤) :: String -> String -> Assignment
(↤) = (<-|)

-- | Infix operator for adding a materials remap.
(<=|) :: String -> String -> Assignment
(<=|) = flip $ Assignment . MaterialsName

-- | Nicer Unicode alternative to '<=|' (U+2906)
(⤆ ) :: String -> String -> Assignment
(⤆ ) = (<=|)

-- | Infix operator for adding a new material.
(*<-) :: (HasEscrowIDs a, Exportable a) => String -> a -> Assignment
(*<-) = flip $ Assignment . NewMaterial . ReturnValue

-- | Nicer Unicode alternative to '*<-' (U+291d)
(⤝) :: (HasEscrowIDs a, Exportable a) => String -> a -> Assignment
(⤝) = (*<-)

-- * Internal functions

-- ** Static data

-- | Cases the renamings into signer-specific and materials-specific.
makeAssignments :: 
  [Assignment] -> (Map String String, Map String String, Map String ReturnValue)
makeAssignments = 
  (_1 %~ Map.fromList) . (_2 %~ Map.fromList) . (_3 %~ Map.fromList) .
  foldr splitRename ([],[],[])
  where
    splitRename (Assignment (SignerName old) new) 
                (signerPairs, materialsPairs, newMaterialsPairs) 
      = ((new, old) : signerPairs, materialsPairs, newMaterialsPairs)
    splitRename (Assignment (MaterialsName old) new) 
                (signerPairs, materialsPairs, newMaterialsPairs) 
      = (signerPairs, (new, old) : materialsPairs, newMaterialsPairs)
    splitRename (Assignment (NewMaterial x) new) 
                (signerPairs, materialsPairs, newMaterialsPairs) 
      = (signerPairs, materialsPairs, (new, x) : newMaterialsPairs)

-- | Composes two maps as though they were functions (composition is
-- left-to-right, unlike '(.)').
mapNames :: 
  (String -> ContractException) -> 
  Map String String -> 
  Map String b -> 
  Map String b
mapNames mkErr renames oldNames = flip renameKey oldNames <$> renames where
  renameKey oldName = Map.findWithDefault (throw $ mkErr oldName) oldName

-- | Locally replaces the 'localMaterials' datum.
remapMaterials :: (MonadReader TXData m) => MaterialsMap -> m a -> m a
remapMaterials newMaterials = local $ _localMaterials .~ newMaterials

-- | Locally replaces the 'thisTXSigners' datum.
remapSigners :: (MonadReader TXData m) => Map String String -> m a -> m a
remapSigners renames = local $ 
  _thisTXSigners . _getSigners %~ mapNames MissingSigner renames

-- | Bumps the local hash by hashing it; if performed with each call,
-- ensures that its starting local hash reflects the external circumstances
-- (i.e. preceding calls that might affect it) of that call.
pushCall :: (MonadReader TXData m) => m a -> m a
pushCall = local (_localHash %~ digest)

-- | Basically the 'TXData' constructor, except for the policy choice to
-- initialize the 'localHash' to the transaction ID.
txData :: TransactionID -> Signers -> TXData
txData txID txSigners =
  TXData
  {
    thisTXSigners = txSigners,
    localHash = txID,
    thisTXID = txID,
    localMaterials = Map.empty
  }

-- ** Calling

-- | Converts a deeply wrapped function returning an awkward type into
-- a natural stepwise function call.
callContract :: 
  (MonadReader TXData m) =>
  ContractF m argType valType -> 
  argType -> m (valType, Maybe (ContractF m argType valType))
callContract (ContractF (SuspendStepF f)) = 
  pushCall . fmap (fmap (fmap ContractF)) . f

-- ** Contract function converters

-- | Generalizes an operationally-correct explicitly-typed contract into an
-- abstract one suitable for calling within other contract code.
localContract :: 
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  PreContractF argType valType -> AbstractLocalContract
localContract = ContractF . 
  alterSuspendStepF acceptLocal returnLocal lift . startSuspendF

-- | Generalizes an operationally-correct explicitly-typed contract into an
-- abstract one suitable for being called as a transaction input.
globalContract :: 
  (
    Read argType, Typeable argType,
    Exportable valType, HasEscrowIDs valType
  ) =>
  PreContractF argType valType -> AbstractGlobalContract
globalContract = ContractF . 
  alterSuspendStepF acceptGlobal returnGlobal extractOutputs . startSuspendF
  where 
    extractOutputs mp = lift $ do
      ~(~(y, sfM), w) <- runWriterT mp
      let wForced = force w
          yForced = wForced `seq` sfM `deepseq` y
      return ((yForced, wForced), yForced `seq` sfM)

-- | Prepares a value-bearing argument.
acceptLocal :: 
  forall argType.
  (HasEscrowIDs argType) =>
  BearsValue -> FaeTXM (WithEscrows argType)
acceptLocal xDyn = do
  WithEscrows escrowMap (WithMaterials _ x) <- takeEscrows xWM
  return $ WithEscrows escrowMap x
  where
    xWM = unBear xDyn $ 
      throw $ BadArgType (typeRep $ Proxy @argType) (bearerType xDyn)

-- | Prepares a value-bearing result.
returnLocal :: 
  (HasEscrowIDs valType) =>
  WithEscrows valType -> FaeTXM BearsValue
returnLocal = fmap bearer . putEscrows

-- | Parses the literal argument into the argument type and collects all
-- the ambient escrows, which are those from this contract's materials.
acceptGlobal :: forall argType.
  (Read argType, Typeable argType) => String -> TXBodyM (WithEscrows argType)
acceptGlobal argS = flip WithEscrows arg <$> pop where 
  err = throw $ BadInputParse argS $ typeRep $ Proxy @argType
  -- Laziness assurance: the 'fromMaybe' function (which is not lazy) is
  -- nonetheless safe here because 'argS' is not provided by user code, and
  -- 'readMaybe' always returns a good value.
  arg = fromMaybe err $ readMaybe $! argS 

-- | Prepares a value-bearing result together with its table of versioned
-- values.  N.B. The 'ReturnValue' component is /always/ defined, though
-- its inner value may not be.  The escrows are /not/ emplaced in the state
-- because they are returned by the 'AbstractGlobalContract'.
returnGlobal :: 
  (HasEscrowIDs valType, Exportable valType) =>
  (WithEscrows valType, [Output]) -> TXBodyM (WithEscrows ReturnValue, [Output])
returnGlobal (WithEscrows escrows y, outputs) = do
  keep escrowsForced
  return (WithEscrows escrowsForced rv, outputsForced)
  where 
    escrowsForced = force escrows
    outputsForced = escrowsForced `seq` outputs
    rv = escrowsForced `seq` ReturnValue y

-- | Prepares a typed value to be passed to an abstract function.
acceptTyped :: (HasEscrowIDs argType) => argType -> BearsValue
acceptTyped = bearer

-- | Prepares an abstract function's return value as typed.
returnTyped :: forall valType. (HasEscrowIDs valType) => BearsValue -> valType
returnTyped yDyn = unBear yDyn $
  throw $ BadValType (typeRep $ Proxy @valType) (bearerType yDyn)

-- ** Escrow manipulation

-- | Sets up the contract function to accept an escrow-backed initial
-- argument, then initializes its storage, removing it from scope.
hideEscrows :: 
  EscrowMap -> Digest -> Contract argType valType -> PreContractF argType valType
hideEscrows escrowMap nextID f = 
  \xE -> evalStateT (putEscrows xE >>= getFae . f) Escrows{..} 

-- | Computes the current next ID and the one that starts a new "branch",
-- and bumps the next ID in the current "branch".
forkNextID :: FaeTXM (Digest, Digest)
forkNextID = do
  oldNextID <- use _nextID
  hash <- view _localHash
  -- It is less crucial that we start the ID chain at a place that reflects
  -- the transaction, but this is nicely uniform with 'useEscrow'.
  _nextID %= digest
  let forkedNextID = digest (oldNextID, hash)
  return (oldNextID, forkedNextID)

-- | Places the escrows backing a value into storage.
putEscrows :: (MonadState Escrows m) => WithEscrows a -> m a
putEscrows ~(WithEscrows escrows a) = do
  _escrowMap %= Map.union escrows
  return a

-- | Extracts the escrows backing a single value.
takeEscrows :: 
  (HasEscrowIDs valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
takeEscrows x = do
  escrowMap <- liftEscrowState $ getEntIDMap (takeEntry . entID) x
  return $ WithEscrows escrowMap x

-- | Gets an escrow entry, casting the contract name alternative to its
-- correct type, and applies a well-typed function.
useNamedEscrow :: 
  (MonadState EscrowMap m, Typeable name) => 
  EscrowID name -> 
  (
    EntryID -> 
    VersionID -> 
    Either (NamedContract name) AbstractLocalContract -> 
    m a
  ) ->
  m a
useNamedEscrow eID@EscrowID{..} f = checkApply =<< getEntry entID where
  checkApply x@EscrowEntry{..} = f entID escrowVersion nameOrFunction where
    nameOrFunction = bimap checkedCast id escrowNameOrFunction
    checkedCast (AnyNamedContract c) =
      fromMaybe (throw $ BadEscrowName entID (typeRep eID) (typeRep c)) $ cast c

-- | Upgrades a computation using only the escrow map itself to one using
-- the whole 'Escrows' object.
liftEscrowState :: (MonadState Escrows m) => State EscrowMap a -> m a
liftEscrowState xm = do
  (y, newEscrowMap) <- runState xm <$> use _escrowMap
  _escrowMap .= newEscrowMap
  return y

-- | Helpful for encapsulating computations in 'useNamedEscrow' that
-- require a full @MonadState Escrows@.
joinEscrowState :: (MonadState Escrows m) => State EscrowMap (m a) -> m a
joinEscrowState = join . liftEscrowState

-- | Gets the escrow version from a specific escrow storage map.
lookupWithEscrows :: EscrowMap -> EntryID -> VersionID
lookupWithEscrows escrowMap entID =
  maybe (throw $ BadEscrowID entID) escrowVersion $ Map.lookup entID escrowMap

-- ** 'ReturnValue' manipulation

-- | Like 'unBear'.
getReturnValue :: (Typeable a) => ReturnValue -> Maybe a
getReturnValue (ReturnValue x) = cast x

-- | Like 'bearerType'.
returnValueType :: ReturnValue -> TypeRep
returnValueType (ReturnValue x) = typeRep (Just x)

-- | Taking advantage of the existential type
exportReturnValue :: (MonadState EscrowMap m) => ReturnValue -> m ByteString
exportReturnValue (ReturnValue x) = exportValue x

-- * 'TXBodyM' stack manipulation

-- | Add an escrow map to the stack
push :: EscrowMap -> TXBodyM ()
push = modify . cons 

-- | Remove an escrow map from the stack
pop :: TXBodyM EscrowMap
pop = do
  (em, rest) <- unconsTXBodyM
  put rest >> return em

-- | Modify the top of the stack with additional escrows.
keep :: EscrowMap -> TXBodyM ()
keep newEM = do
  (oldEM, rest) <- unconsTXBodyM
  put $ newEM `Map.union` oldEM : rest

-- | The common denominator for 'pop' and 'keep', carefully written so that
-- the monad is always defined and any bottom, not necessarily just the one
-- from the 'Nothing' case of 'uncons', lies in the inner values.
unconsTXBodyM :: TXBodyM (EscrowMap, [EscrowMap])
unconsTXBodyM = do
  ~(em, rest) <- fromMaybe (throw EmptyInputStack) <$> gets uncons 
  return (em, rest)

