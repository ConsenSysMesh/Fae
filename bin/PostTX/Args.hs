module PostTX.Args where

import Blockchain.Fae (TransactionID)

import Control.Lens hiding (view)

import Data.Bool
import Data.List
import Data.Maybe

parseArgs :: [String] -> (Either String TransactionID, String, Bool, Bool)
parseArgs args = finalize $
  foldl argGetter (Nothing, Nothing, False, False, False) args
          
argGetter :: 
  (Maybe String, Maybe String, Bool, Bool, Bool) -> String -> 
  (Maybe String, Maybe String, Bool, Bool, Bool)
argGetter st "--fake" = st & _3 .~ True
argGetter st "--view" = st & _4 .~ True
argGetter st "--lazy" = st & _5 .~ True
argGetter st x 
  | "--" `isPrefixOf` x = error $ "Unrecognized option: " ++ x
  | Nothing <- st ^. _1 = st & _1 ?~ x
  | Nothing <- st ^. _2 = st & _2 ?~ x
  | otherwise = error "TX name and host already given"

finalize :: 
  (Maybe String, Maybe String, Bool, Bool, Bool) -> 
  (Either String TransactionID, String, Bool, Bool)
finalize (txSpecOrTXIDM, hostM, fake, view, lazy) = 
  (txSpecOrTXID, host, fake, lazy)
  where
    host = fromMaybe "0.0.0.0:27182" hostM
    txSpecOrTXID
      | Just x <- txSpecOrTXIDM = bool Left (Right . read) view x
      | False <- view = Left "TX"
      | otherwise = error $ "Unspecified transaction ID to view"

