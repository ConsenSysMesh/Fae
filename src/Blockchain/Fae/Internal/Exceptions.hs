{- |
Module: Blockchain.Fae.Internal.Exceptions
Description: Wrapper library for "Control.Monad.Catch"
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module just re-exports "Control.Monad.Catch", as well as 'Typeable' so that we can derive 'Exception' with just this module imported, and also 'throw' and 'evaluate' from "Control.Exception", which seem not to be re-exported elsewhere.
-}
module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch,
    Ex.throw, Ex.evaluate,
    T.Typeable
  ) where

import qualified Control.Exception as Ex
import Control.Monad.Catch
import Data.Typeable as T
