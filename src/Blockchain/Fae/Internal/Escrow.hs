module Blockchain.Fae.Internal.Escrow where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Fae
import Blockchain.Fae.Internal.Lens

import Data.Dynamic
import qualified Data.Map as Map
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

escrow ::
  forall tokT pubT privT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  tokT -> pubT -> privT -> 
  Fae (EscrowID tokT pubT privT)
escrow tok pub priv = do
  eID <- newEscrowID
  Fae $ _transientState . _contractEscrows . _useEscrows . at eID ?= 
    toDyn (Escrow @tokT priv pub)
  return (PublicEscrowID eID, PrivateEscrowID eID)

peek ::
  forall tokT privT pubT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  PublicEscrowID tokT pubT privT -> Fae pubT
peek pID@(PublicEscrowID escrowID) = useEscrow _public pID escrowID

close ::
  forall tokT pubT privT.
  (Typeable tokT, Typeable pubT, Typeable privT) =>
  PrivateEscrowID tokT pubT privT -> tokT -> Fae privT
-- Need strictness here to force the caller to actually have a token, and not
-- just a typed 'undefined'.  The token itself is unused; we just need to
-- know that the caller was able to access the correct type.
close pID@(PrivateEscrowID escrowID) !_ = do
  priv <- useEscrow _private pID escrowID
  Fae $ _transientState . _contractEscrows . _useEscrows . at escrowID .= Nothing
  return priv

useEscrow :: 
  forall tokT pubT privT proxy a.
  (Typeable tokT, Typeable pubT, Typeable privT, Typeable proxy) =>
  Lens' (Escrow tokT pubT privT) a ->
  proxy tokT pubT privT ->
  EntryID -> 
  Fae a
useEscrow l p eID = Fae $ do
  escrowDM <- use $ _transientState . _contractEscrows . _useEscrows . at eID
  escrowD <- maybe
    (throwM $ BadEscrowID eID)
    return
    escrowDM
  escrow <- fromDyn escrowD $
    throwM $ 
      BadEscrowType eID (typeRep $ Proxy @(proxy tokT pubT privT)) (dynTypeRep escrowD)
  return $ escrow ^. l

newEscrowID :: Fae EntryID
newEscrowID = Fae $ use $ 
  _transientState . _contractEscrows . _useEscrows . 
  to (EntryID . digest . Map.size)
