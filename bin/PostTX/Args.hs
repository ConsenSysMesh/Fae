module PostTX.Args where

import Control.Lens

import Data.Maybe

parseArgs :: [String] -> (String, String, Bool)
parseArgs args =
  triple & _1 %~ fromMaybe "TX"            
         & _2 %~ fromMaybe "0.0.0.0:27182"
  where triple = foldl argGetter (Nothing, Nothing, False) args
          
argGetter :: 
  (Maybe String, Maybe String, Bool) -> String -> 
  (Maybe String, Maybe String, Bool)
argGetter st "--fake" = st & _3 .~ True
argGetter st x 
  | Nothing <- st ^. _1 = st & _1 ?~ x
  | Nothing <- st ^. _2 = st & _2 ?~ x
  | otherwise = error "TX name and host already given"
