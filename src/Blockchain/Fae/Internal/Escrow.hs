module Blockchain.Fae.Internal.Escrow where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Lens

import Data.Dynamic
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

data Escrow tokType pubType privType =
  Escrow
  {
    private :: privType,
    public :: pubType
  }

newtype PublicEscrowID tokT pubT privT = PublicEscrowID EntryID
newtype PrivateEscrowID tokT pubT privT = PrivateEscrowID EntryID
type EscrowID tokT pubT privT =
  (PublicEscrowID tokT pubT privT, PrivateEscrowID tokT pubT privT)

makeLenses ''Escrow

escrowID :: EntryID -> EscrowID tokT pubT privT
escrowID eID = (PublicEscrowID eID, PrivateEscrowID eID)

entryID :: EscrowID tokT pubT privT -> EntryID
entryID (_, PrivateEscrowID eID) = eID

escrow ::
  forall tokT pubT privT argType accumType.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  tokT -> pubT -> privT -> 
  Fae argType accumType (EscrowID tokT pubT privT)
escrow tok pub priv = Fae $ do
  eID <- use $ _escrows . _useEscrows . to Map.size . to EntryID
  _escrows . _useEscrows . at eID ?= toDyn (Escrow @tokT priv pub)
  return $ escrowID eID

peek ::
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  PublicEscrowID tokT pubT privT -> Fae argType accumType pubT
peek pID@(PublicEscrowID eID) = useEscrow _public pID eID

close ::
  (Typeable tokT, Typeable pubT, Typeable privT) =>
  PrivateEscrowID tokT pubT privT -> tokT -> Fae argType accumType privT
-- Need strictness here to force the caller to actually have a token, and not
-- just a typed 'undefined'.  The token itself is unused; we just need to
-- know that the caller was able to access the correct type.
close pID@(PrivateEscrowID eID) !_ = do
  priv <- useEscrow _private pID eID
  Fae $ _escrows . _useEscrows . at eID .= Nothing
  return priv

useEscrow :: 
  forall tokT pubT privT proxy argType accumType a.
  (Typeable tokT, Typeable pubT, Typeable privT, Typeable proxy) =>
  Lens' (Escrow tokT pubT privT) a ->
  proxy tokT pubT privT ->
  EntryID -> 
  Fae argType accumType a
useEscrow l p eID = Fae $ do
  escrowDM <- use $ _escrows . _useEscrows . at eID
  let
    escrowD = fromMaybe (throw $ BadEscrowID eID) escrowDM
    escrow = fromDyn escrowD $
      throw $ 
        BadEscrowType eID (typeRep $ Proxy @(proxy tokT pubT privT)) (dynTypeRep escrowD)
  return $ escrow ^. l

