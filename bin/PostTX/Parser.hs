{- |
Module: PostTX.Parser
Description: Transaction spec file parser
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

PostTX accepts transactions written in a yaml-like format, which is parsed by this module.
-}
module PostTX.Parser where

import Blockchain.Fae.FrontEnd (ContractID(..))

import Control.Applicative hiding (some)
import Control.Monad
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
    InputSpec(..), Module, ModuleMap, Renames(..), TransactionID
  )

import System.Environment
import System.Exit

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Perm

import qualified Text.Megaparsec.Char.Lexer as L

import Text.Read

import Type.Reflection (typeRep, Typeable)

-- | We don't use custom error types, but we do use IO because all the
-- environment variable lookups have to occur in the middle of parsing.
type SpecParser = ParsecT Void String IO

-- * Functions
-- ** Interface
-- | Parses the transaction spec at the given filename.  Parse errors
-- terminate the process with a pretty-printed message.
buildTXData :: String -> IO TXData 
buildTXData txName = 
  either err return =<< runParserT (specFile <* eof) txName =<< readFile txName
  where err = die . parseErrorPretty

-- ** Top-level parsers
-- | A spec file has a number of optional sections, and one required one,
-- corresponding to the fields of a 'TXData'.  They may occur in any order
-- and are described below.
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
  where noIndent = L.nonIndented $ multilineBlank

-- | >>> body = <module name>
-- Specifies the name of the Haskell source file (without the @.hs@
-- extension) where the transaction's 'body' function is defined.  This is
-- required, unlike in previous versions where it defaulted to the name of
-- the transaction spec.  This is largely due to a technical limitation of
-- the permutation parser construction functions that makes it difficult to
-- elegantly express the default value, which needs to be in 'IO'.
body :: SpecParser (String, Module)
body = equalsName "body" readModule

-- | >>> others
--   >>>   - <module name>
--   >>>   ...
-- Specifies, optionally, other modules that must accompany the main one
-- (say, because they are imported by it, though it is also permissible to
-- send isolated modules just to be imported by other transactions).
others :: SpecParser ModuleMap
others = fmap Map.fromList $ titledList "others" $ listItem readModule

-- | >>> fallback
--   >>>   - <function name>
--   >>>   ...
-- Specifies, optionally, a list of "fallback functions" to be run in case
-- of an exception in the transaction body's return value.
fallback :: SpecParser [String]
fallback = titledList "fallback" $ listItem literal

-- | >>> inputs
--   >>>   <contract ID> = <'Read'able argument>
--   >>>     <local role> = <global role>
--   >>>     ...
--   >>>   ...
-- Specifies, optionally, a list of contract calls by ID and argument.
-- Each one may, itself, optionally have a list of "renamings" of roles
-- (signer names, as in the @keys@ section) that hold during that call.
-- This renaming facility is essential to using stock contracts such as
-- @deposit@ that expect to authenticate the signers in specifically named
-- roles, which may not be the names used in the transaction that calls
-- it.
inputs :: SpecParser [InputSpec]
inputs = titledList "inputs" $ do
  (inputResultVersionM, ((inputCID, inputArgS), renamesL)) <- 
    liftA2 (,) (optional versSpec) (headedList argSpec renameSpec)
  return InputSpec{inputRenames = Renames $ Map.fromList renamesL, ..}
  where 
    -- Matches the 'Show' instance in "Blockchain.Fae.Internal.IDs.Types"
    contractID end = 
      ContractID <$> readPath <*> readPath <*> readPath <*> readEnd end
    argSpec = equalsItem contractID literal
    renameSpec = equalsItem literalEnd literal
    versSpec = equalsName "version" readWord

-- | >>> keys
--   >>>   <global role name> = <private key name> | <public key hex string>
--   >>>   ...
-- Specifies a list of signers; this is optional because if the section is
-- omitted or empty, the list defaults to @self = self@.  If the private
-- key name is given, it is created or looked up and signs the transaction;
-- if the public key hex string is given, the transaction is /not/ signed
-- but the requisite public key is still recorded in it.
keys :: SpecParser [(String, String)]
keys = titledList "keys" $ equalsItem literalEnd literal

-- | >>> reward = <'Bool'>
-- Optionally specifies whether this transaction is to be a "reward", i.e.
-- receive a @RewardEscrowID@ as its first argument.  This feature is only
-- allowed as a means of testing transactions.  The default is 'False'.
reward :: SpecParser Bool
reward = equalsName "reward" readWord

-- | >>> parent = <transaction ID>
-- Optionally specifies a particular transaction as the "parent" of this
-- one.  This means that the Fae history is rolled back (losslessly) to its
-- state just after that transaction was run before running this one,
-- creating a branch in the transaction tree.  If this field is not
-- present, the parent transaction is the latest one in the "best" (i.e.
-- longest) chain.
parent :: SpecParser TransactionID
parent = equalsName "parent" readWord

-- ** Structural parsers

-- | Parses an expression like @name = b@ on a single line, absorbing
-- whitespace including newlines after it.  Thus, it is intended to parse
-- assignments that are not items in a list but are first-class objects in
-- the specification.
equalsName :: String -> SpecParser a -> SpecParser a
equalsName name p = fmap snd $ equalsItem (symbol name <*) p <* multilineBlank

-- | Parses an expression like @a = b@, where at least the @a =@ part is on
-- one line.  Its behavior with respect to following whitespace depends on
-- the second parameter; if it doesn't absorb newlines, an 'equalsItem' may
-- be part of a list.  This reads right up to the equals, so if you want to
-- absorb preceding spaces, the @lhsEnd@ must be like @literalEnd@, which
-- does so.
equalsItem :: 
  (SpecParser String -> SpecParser a) -> SpecParser b -> SpecParser (a,b)
equalsItem lhsEnd = liftA2 (,) $ lhsEnd $ symbol "="

-- | Parses an expression like @- a@, where the behavior with respect to
-- following whitespace depends on the argument; if it doesn't absorb
-- newlines, a 'listItem' may be part of a list (of course).
listItem ::  SpecParser a -> SpecParser a
listItem p = symbol "-" >> p

-- | Parses an expression like
-- >>> header
-- >>>   indented item
-- >>>   ...
-- The second argument /must not/ absorb newlines.
titledList :: String -> SpecParser a -> SpecParser [a]
titledList name = fmap snd . headedList (symbol name)

-- | Parses an expression like
-- >>> expression
-- >>>   indented item
-- >>>   ...
-- Neither the header nor the item may absorb newlines.
headedList :: SpecParser header -> SpecParser a -> SpecParser (header, [a])
headedList header p = 
  L.indentBlock multilineBlank $ do
    h <- header
    return $ L.IndentMany Nothing (return . (h,)) p

-- ** Lexical parsers

-- | Reads a /single/ path component, absorbing the slash (thus, not the
-- "basename").
readPath :: (Read a, Typeable a) => SpecParser a
readPath = readEnd $ symbol "/"

-- | Grabs text to the end of the line (but doesn't absorb the newline) and
-- 'read's it.
readWord :: (Read a, Typeable a) => SpecParser a
readWord = readEnd endl

-- | Grabs text to the end of the line (but doesn't absorb the newline) and
-- returns it as-is.
literal :: SpecParser String
literal = literalEnd endl

-- | Grabs text until the argument succeeds in parsing what follows; the
-- intermediate failures consume no characters.  Its behavior with respect
-- to newlines depends on that of the argument.  The @end@ is modified to
-- absorb preceding spaces.
literalEnd :: SpecParser end -> SpecParser String
literalEnd end = resolveLine =<< someTill printChar (try $ lineBlank *> end)

-- | To 'literalEnd' as 'readWord' is to 'literal'.
readEnd :: forall a end. (Read a, Typeable a) => SpecParser end -> SpecParser a
readEnd end = (\s -> either (err s) return $ readEither s) =<< literalEnd end
  where err s = error $ "Couldn't read " ++ s ++ " as type " ++ show (typeRep @a)

-- | A lookahead newline, important since list items need to continue until
-- a newline but not absorb it.
endl :: SpecParser Char
endl = lookAhead eol *> return '\n'

-- | Parses the given string on a single line, not absorbing the newline.
symbol :: String -> SpecParser String
symbol = L.symbol lineBlank

-- | Parses non-newline whitespace and comments.
lineBlank :: SpecParser ()
lineBlank = blank lineSpace1

-- | Parses any whitespace and comments.
multilineBlank :: SpecParser ()
multilineBlank = blank space1

-- | Absorbs whitespace (as per the first argument) and comments.
blank :: SpecParser () -> SpecParser ()
blank sp = L.space sp lineComment blockComment where
  lineComment = L.skipLineComment "--"
  blockComment = L.skipBlockCommentNested "{-" "-}"

-- | Parses just non-newline whitespace, without absorbing any terminating
-- newlines.
lineSpace1 :: SpecParser ()
lineSpace1 = void $ some (notFollowedBy eol *> spaceChar)

-- ** Modules and environment variable substitution
-- These functions are the source of the 'IO' aspect of a 'SpecParser'.
-- They all perform a parsing task and ought to be rewritten with
-- combinators rather than hand-coded.

-- | Takes a piece of text as a module name, and reads the module contents
-- with the appropriate environment variables substituted.
readModule :: SpecParser (String, Module)
readModule = readResolved . (++ ".hs") =<< literal

-- | Substitutes environment variables in the block of @import@s at the
-- beginning of a module.
-- FIXME: Only works for the @body@ module; needs to be aware of the
-- @module@ header for others.
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

-- | Resolves environment variables in a piece of text.
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

-- | Reads in a module and resolves the variables in its import statements.
readResolved :: (MonadIO m) => String -> m (String, Module)
readResolved fName = liftIO $ do
  rawFile <- readFile fName
  fixedFile <- fmap unlines $ resolveImportVars $ lines rawFile
  return (fName, C8.pack fixedFile)

