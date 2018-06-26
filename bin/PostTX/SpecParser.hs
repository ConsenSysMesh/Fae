{-# LANGUAGE TemplateHaskell #-}
module PostTX.SpecParser where

import Blockchain.Fae (ContractID, TransactionID)

import Common.Lens hiding ((<.>))

import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import PostTX.EnvVars
import PostTX.TXSpec

import Prelude hiding (readList)

import Text.Read(readMaybe)
import System.FilePath

buildTXData :: String -> IO (TXData LoadedModules)
buildTXData txName = do
  specLines <- lines <$> readFile txName
  parsedData@TXData{dataModules = ParsedModules{..}, ..} <- readData specLines 
    TXData
    {
      dataModules = 
        ParsedModules
        {
          bodyM = Nothing, 
          others = []
        },
      fallback = [],
      inputs = [],
      keys = [], 
      reward = False,
      parent = Nothing
    } 
  mainModule <- readResolved $ fromMaybe txName bodyM
  otherModules <- Map.fromList <$> mapM readResolved others
  return parsedData{dataModules = LoadedModules{..}}

  where 
    readResolved name = do
      rawFile <- readFile fName
      fixedFile <- fmap unlines $ resolveImportVars $ lines rawFile
      return (fName, C8.pack fixedFile)
      where fName = name <.> "hs"

readData :: [String] -> TXData ParsedModules -> IO (TXData ParsedModules)
readData [] txData = return txData
readData (line1 : rest) txData =
  case breakEquals line1 of
    ("", Nothing) -> readData rest txData
    ("body", bodyM) -> readData rest (txData & _dataModules . _bodyM .~ bodyM)
    ("reward", Just rewardS) -> 
      let reward = read rewardS in
      readData rest txData{reward} 
    ("parent", Just parentS) -> do
      parent <- Just . read <$> resolveLine parentS 
      readData rest txData{parent} 
    ("others", sM) 
      | maybe True null sM -> 
          readList "others" (_dataModules . _others) rest txData 
      | otherwise -> forbiddenArgument "others" sM
    ("fallback", sM)
      | maybe True null sM -> readList "fallback" _fallback rest txData
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

readList :: 
  String -> Lens' (TXData ParsedModules) [String] -> [String] -> 
  TXData ParsedModules -> IO (TXData ParsedModules)
readList name listLens lines txData 
  | null $ txData ^. listLens = readData rest $ txData & listLens .~ list
  | otherwise = error $ "Multiple '" ++ name ++ "' blocks"
  where
    list = 
      fmap (\l -> catchInvalid l $ dropSpaces <$> stripPrefix "  -" l) dashList
    catchInvalid l = fromMaybe (error $ "Invalid " ++ name ++ " spec: " ++ l) 
    (dashList, rest) = span (isPrefixOf "  -") lines

readInputs :: [String] -> TXData ParsedModules -> IO (TXData ParsedModules)
readInputs lines txData@TXData{..} = do
  (pairs, rest) <- readEqualsSpec (null inputs) "inputs" readWithFailure lines
  readData rest txData{inputs = pairs} 
  where readWithFailure input = fromMaybe (error "Failed to parse TX input") (readMaybe input)

readKeys :: [String] -> (TXData ParsedModules) -> IO (TXData ParsedModules)
readKeys lines txData@TXData{..} = do
  (pairs, rest) <- readEqualsSpec (null keys) "keys" id lines
  readData rest txData{keys = pairs} 

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


