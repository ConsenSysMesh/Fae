{-# LANGUAGE TemplateHaskell #-}
module PostTX.TXMessage where

import PostTX.EnvVars

import Blockchain.Fae (ContractID, TransactionID)

import Control.Lens hiding ((<.>))

import Data.Char
import Data.List
import Data.Maybe

import Prelude hiding (readList)

data TXData =
  TXData
  {
    _bodyM :: Maybe String,
    _others :: [String],
    _fallback :: [String],
    _inputs :: [(ContractID, String)],
    _keys :: [(String, String)],
    _reward :: Bool,
    _parent :: Maybe TransactionID
  }

makeLenses ''TXData

buildTXData :: String -> IO TXData
buildTXData txSpec = do
  specLines <- lines <$> readFile txSpec
  readData specLines 
    TXData
    {
      _bodyM = Nothing, 
      _others = [], 
      _fallback = [],
      _inputs = [],
      _keys = [], 
      _reward = False,
      _parent = Nothing
    } 

readData :: [String] -> TXData -> IO TXData
readData [] txData = return txData
readData (line1 : rest) txData =
  case breakEquals line1 of
    ("", Nothing) -> readData rest txData
    ("body", _bodyM) -> readData rest txData{_bodyM} 
    ("reward", Just rewardS) -> 
      let _reward = read rewardS in
      readData rest txData{_reward} 
    ("parent", Just parentS) -> do
      _parent <- Just . read <$> resolveLine parentS 
      readData rest txData{_parent} 
    ("others", sM) 
      | maybe True null sM -> readList "others" others rest txData 
      | otherwise -> forbiddenArgument "others" sM
    ("fallback", sM)
      | maybe True null sM -> readList "fallback" fallback rest txData
      | otherwise -> forbiddenArgument "fallback" sM
    ("inputs", sM) 
      | maybe True null sM -> readInputs rest txData
      | otherwise -> forbiddenArgument "inputs" sM
    ("keys", sM)
      | maybe True null sM -> readKeys rest txData
      | otherwise -> forbiddenArgument "keys" sM
    _ -> error $ "Unrecognized spec line: " ++ line1
  where 
    forbiddenArgument name (Just s) = 
      error $ "Forbidden argument to '" ++ name ++ "': " ++ s

readList :: String -> Lens' TXData [String] -> [String] -> TXData -> IO TXData
readList name listLens lines txData 
  | null $ txData ^. listLens = readData rest $ txData & listLens .~ list
  | otherwise = error $ "Multiple '" ++ name ++ "' blocks"
  where
    list = 
      fmap (\l -> catchInvalid l $ dropSpaces <$> stripPrefix "  -" l) dashList
    catchInvalid l = fromMaybe (error $ "Invalid " ++ name ++ " spec: " ++ l) 
    (dashList, rest) = span (isPrefixOf "  -") lines

readInputs :: [String] -> TXData -> IO TXData
readInputs lines txData@TXData{..} = do
  (pairs, rest) <- readEqualsSpec (null _inputs) "inputs" read lines
  readData rest txData{_inputs = pairs} 

readKeys :: [String] -> TXData -> IO TXData
readKeys lines txData@TXData{..} = do
  (pairs, rest) <- readEqualsSpec (null _keys) "keys" id lines
  readData rest txData{_keys = pairs} 

readEqualsSpec :: 
  Bool -> String -> (String -> a) -> [String] -> IO ([(a, String)], [String])
readEqualsSpec good name readA lines
  | good = do
      let (inputsL, rest) = span (isPrefixOf "  ") lines
      pairs <- mapM 
        (\l -> 
          resolvePair $
          catchInvalid l $ 
          breakEquals l
        ) inputsL
      return (pairs, rest)
  | otherwise = error $ "Multiple '" ++ name ++ "' blocks"
  where
    resolvePair (a,b) = do
      cIDS <- resolveLine a
      arg <- resolveLine b
      return (readA cIDS, arg)
    catchInvalid l (a, b)
      | null a || maybe True null b = 
          error $ "Invalid " ++ name ++ " spec: " ++ l
      | otherwise = (a, fromJust b)

dropSpaces :: String -> String
dropSpaces = dropWhile isSpace . dropWhileEnd isSpace 

breakEquals :: String -> (String, Maybe String)
breakEquals l = (dropSpaces before, dropSpaces <$> stripPrefix "=" after) where
  (before, after) = break (== '=') l


