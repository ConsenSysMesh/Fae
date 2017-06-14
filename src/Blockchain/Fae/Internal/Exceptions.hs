module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Ex0
  ) where

import Blockchain.Fae.Internal.Types

import Control.Exception as Ex0 hiding (throwIO)
import qualified Control.Exception as Ex (throwIO)

import Control.Monad.IO.Class

import Data.Typeable

throwIO :: (Exception e, MonadIO m) => e -> m a
throwIO = liftIO . throwIO

data EntryException =
  EvaluatedBadEntryID EntryID |
  EvaluatedInWrongFacet EntryID FacetID FacetID |
  BadEntryArgType EntryID TypeRep TypeRep |
  BadEntryValType EntryID TypeRep TypeRep
  deriving (Typeable, Show)

instance Exception EntryException 

