{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (readList)
import Blockchain.Fae (ContractID, TransactionID)
import Control.Lens
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Char
import Data.List
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import System.Directory
import System.Environment
import System.Process

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

main :: IO ()
main = do
  args <- getArgs
  let 
    (txSpec, host, fake) = 
      (\triple -> triple 
        & _1 %~ fromMaybe "TX" 
        & _2 %~ fromMaybe "0.0.0.0:27182"
      ) $
      (\l x g -> foldl g x l) args (Nothing, Nothing, False) $ \st arg -> 
        case arg of
          "--fake" -> st & _3 .~ True
          x | Nothing <- st ^. _1 -> st & _1 ?~ x
            | Nothing <- st ^. _2 -> st & _2 ?~ x
            | otherwise -> error "TX name and host already given"
  specLines <- lines <$> readFile txSpec
  txData <- readData specLines 
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
  case _bodyM txData of
    Nothing -> error "Missing body filename"
    Just _ -> return ()
  postTX host fake txData

postTX :: String -> Bool -> TXData -> IO ()
postTX host fake TXData{..} = do
  manager <- newManager defaultManagerSettings
  request <- flip formDataBody requestURL $
    partFileSource "body" (maybe (error "Missing body") (++ ".hs") _bodyM) :
    fmap (partFileSource "other" . (++ ".hs")) _others ++
    fmap (uncurry partLBS) 
      (maybe id (:) parentArg $ fakeArg : rewardArg : 
      fallbackArgs ++ inputArgs ++ keysArgs)
  response <- httpLbs request manager
  LC8.putStrLn $ responseBody response

  where
    requestURL = fromMaybe (error $ "Bad host string: " ++ host) $ 
      parseRequest $ "http://" ++ host
    fakeArg = ("fake", ) $ if fake then "True" else "False"
    rewardArg = ("reward", ) $ if _reward then "True" else "False"
    parentArg = ("parent", ) . LC8.pack . show <$> _parent
    fallbackArgs = map (("fallback",) . LC8.pack) _fallback
    inputArgs = map (("input", ) . LC8.pack . show) _inputs
    keysArgs = map (\(signer, key) -> ("key", LC8.pack $ signer ++ ":" ++ key)) _keys

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
      _parent <- Just . read <$> resolveVars parentS 
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

