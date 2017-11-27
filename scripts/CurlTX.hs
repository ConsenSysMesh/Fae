{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
import Debug.Trace
import Blockchain.Fae (ContractID)
import Control.Lens
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Process

data TXData =
  TXData
  {
    body :: Maybe String,
    others :: [String],
    inputs :: [(ContractID, String)],
    keys :: [(String, String)],
    reward :: Bool
  }

main :: IO ()
main = do
  args <- getArgs
  let 
    triple0 = ("TX", "localhost:27182", False)
    (txSpec, host, fake) = view _1 $ 
      (\l x g -> foldl g x l) args (triple0, False, False) $ \st arg -> 
        case arg of
          "--fake" -> st & _1 . _3 .~ True
          x | st ^. _2 && st ^. _3 -> error "TX name and host already given"
            | st ^. _2 -> st & _1 . _2 .~ x & _3 .~ True
            | otherwise -> st & _1 . _1 .~ x & _2 .~ True
  specLines <- lines <$> readFile txSpec
  txData <- readData specLines 
    TXData{body = Nothing, others = [], inputs = [], keys = [], reward = False} 
  case body txData of
    Nothing -> error "Missing body filename"
    Just _ -> return ()
  runCurl host fake txData

runCurl :: String -> Bool -> TXData -> IO ()
runCurl host fake TXData{..} = callProcess "curl" $ args ++ [host] where
  args = ("-F" :) . intersperse "-F" $ 
    (if fake then ("fake=True" :) else ("fake=False" :)) $
    (if reward then ("reward=True" :) else ("reward=False" :)) $
    bodyArg : inputArgs ++ othersArgs ++ keysArgs
  bodyArg = "body=@" ++ fromJust body ++ ".hs"
  inputArgs = map (\p -> "input=" ++ show p) inputs
  othersArgs = map (\file -> "other=@" ++ file ++ ".hs") others
  keysArgs = map (\(signer, key) -> "key=" ++ signer ++ ":" ++ key) keys

readData :: [String] -> TXData -> IO TXData
readData [] txData = return txData
readData (line1 : rest) txData =
  case breakEquals line1 of
    ("", Nothing) -> readData rest txData
    ("body", body@Just{}) -> readData rest txData{body} 
    ("reward", Just rewardS) -> 
      let reward = read rewardS in
      readData rest txData{reward} 
    ("others", sM) 
      | maybe True null sM -> readOthers rest txData 
      | otherwise -> forbiddenArgument "others" $ fromJust sM
    ("inputs", sM) 
      | maybe True null sM -> readInputs rest txData
      | otherwise -> forbiddenArgument "inputs" $ fromJust sM
    ("keys", sM)
      | maybe True null sM -> readKeys rest txData
      | otherwise -> forbiddenArgument "keys" $ fromJust sM
    _ -> error $ "Unrecognized spec line: " ++ line1
  where 
    forbiddenArgument name s = 
      error $ "Forbidden argument to '" ++ name ++ "': " ++ s

readOthers :: [String] -> TXData -> IO TXData
readOthers lines txData 
  | null $ others txData = readData rest txData{others = others'} 
  | otherwise = error "Multiple 'others' blocks"
  where
    others' = 
      fmap (\l -> catchInvalid l $ fmap dropSpaces $ stripPrefix "  -" l) othersL
    catchInvalid l = fromMaybe (error $ "Invalid other file spec: " ++ l) 
    (othersL, rest) = span (isPrefixOf "  -") lines

readInputs :: [String] -> TXData -> IO TXData
readInputs lines txData@TXData{..} = do
  (pairs, rest) <- readEqualsSpec (null inputs) "inputs" read lines
  readData rest txData{inputs = pairs} 

readKeys :: [String] -> TXData -> IO TXData
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
      cIDS <- resolveVars a
      arg <- resolveVars b
      return (readA cIDS, arg)
    catchInvalid l (a, b)
      | null a || maybe True null b = 
          error $ "Invalid " ++ name ++ " spec: " ++ l
      | otherwise = (a, fromJust b)

resolveVars :: String -> IO String
resolveVars = fmap unwords . mapM resolveVar . words

resolveVar :: String -> IO String
resolveVar ('$' : name) = getEnv name
resolveVar s = return s

dropSpaces :: String -> String
dropSpaces = dropWhile isSpace . dropWhileEnd isSpace 

breakEquals :: String -> (String, Maybe String)
breakEquals l = (dropSpaces before, dropSpaces <$> stripPrefix "=" after) where
  (before, after) = break (== '=') l
