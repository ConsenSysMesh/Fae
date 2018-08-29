module PostTX.Parser where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans

import Data.Char
import Data.Maybe
import Data.Void

import qualified Data.ByteString.Char8 as C8

import qualified Data.Map as Map
import Data.Map (Map)

import PostTX.TXSpec 
  (
    LoadedModules(..), TXData(TXData),
    Inputs, Module, ModuleMap, Renames(..), TransactionID
  )

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Perm

import qualified Text.Megaparsec.Char.Lexer as L

import System.Environment
import System.Exit

import Text.Read

type SpecParser = ParsecT Void String IO

-- * Functions
-- ** Interface
buildTXData :: String -> IO TXData 
buildTXData txName = 
  either err return =<< runParserT specFile txName =<< readFile txName
  where err = die . parseErrorPretty

-- ** Top-level parsers
specFile :: SpecParser TXData
specFile = makePermParser $
  (\mainModule otherModules -> TXData LoadedModules{..})
  <$$> noIndent body
  <|?> (Map.empty, noIndent others)
  <|?> ([], noIndent fallback)
  <|?> ([], noIndent inputs)
  <|?> ([], noIndent keys)
  <|?> (False, noIndent reward)
  <|?> (Nothing, Just <$> noIndent parent)
  where noIndent = L.nonIndented blank

body :: SpecParser (String, Module)
body = equalsName "body" readModule

others :: SpecParser ModuleMap
others = fmap Map.fromList $ titledList "others" $ listItem readModule

fallback :: SpecParser [String]
fallback = titledList "fallback" $ listItem literal

inputs :: SpecParser Inputs
inputs = titledList "inputs" $ do
  ((cID, arg), renamesL) <- 
    headedList (equalsItem readEnd literal) (equalsItem literalEnd literal)
  return (cID, arg, Renames $ Map.fromList renamesL)

keys :: SpecParser [(String, String)]
keys = titledList "keys" $ equalsItem literalEnd literal

reward :: SpecParser Bool
reward = equalsName "reward" readWord

parent :: SpecParser TransactionID
parent = equalsName "parent" readWord

-- ** Structural parsers

equalsName :: String -> SpecParser a -> SpecParser a
equalsName name = fmap snd . equalsItem (symbol name <*)

equalsItem :: 
  (SpecParser String -> SpecParser a) -> SpecParser b -> SpecParser (a,b)
equalsItem lhsEnd = liftA2 (,) $ lhsEnd (symbol "=")

listItem ::  SpecParser a -> SpecParser a
listItem p = symbol "-" >> p

titledList :: String -> SpecParser a -> SpecParser [a]
titledList name = fmap snd . headedList (symbol name)

headedList :: SpecParser header -> SpecParser a -> SpecParser (header, [a])
headedList header p = 
  L.indentBlock blank $ do
    h <- header
    return $ L.IndentMany Nothing (return . (h,)) p

-- ** Lexical parsers

readWord :: (Read a) => SpecParser a
readWord = readEnd eol

literal :: SpecParser String
literal = literalEnd eol

literalEnd :: SpecParser end -> SpecParser String
literalEnd end = lexeme $ resolveLine =<< someTill printChar end

readEnd :: (Read a) => SpecParser end -> SpecParser a
readEnd end = either fail return . readEither =<< literalEnd end

symbol :: String -> SpecParser String
symbol = L.symbol blank

lexeme :: SpecParser a -> SpecParser a
lexeme = L.lexeme blank

blank :: SpecParser ()
blank = L.space space1 lineComment blockComment where
  lineComment = L.skipLineComment "--"
  blockComment = L.skipBlockCommentNested "{-" "-}"

-- ** Modules and environment variable substitution

readModule :: SpecParser (String, Module)
readModule = readResolved . (++ ".hs") =<< literal

resolveImportVars :: [String] -> IO [String]
resolveImportVars [] = return []
resolveImportVars x
  | (l : rest) <- dropWhile (all isSpace) x,
    "import" : _ <- words l = do
      let (ls, iTail) = span headSpace rest
      iHeadR <- mapM resolveLine $ l : ls 
      iTailR <- resolveImportVars iTail
      return $ iHeadR ++ iTailR
  | otherwise = return x

  where
    headSpace "" = True
    headSpace (c : rest) = isSpace c

resolveLine :: (MonadIO m) => String -> m String
resolveLine [] = return []
resolveLine l = liftIO $ do
  resolvedVar <- 
    case varM of
      Nothing -> return ""
      Just "" -> return "$"
      Just var -> getEnv var
  resolvedAfter <- resolveLine after
  return $ before ++ resolvedVar ++ resolvedAfter 

  where
    (before, after0) = break (== '$') l
    (varM, after) 
      | null after0 = (Nothing, "")
      | ('$' : after1) <- after0 = 
          let (var, after) = span isAlphaNum after1 in
          (Just var, after) 

readResolved :: (MonadIO m) => String -> m (String, Module)
readResolved fName = liftIO $ do
  rawFile <- readFile fName
  fixedFile <- fmap unlines $ resolveImportVars $ lines rawFile
  return (fName, C8.pack fixedFile)

