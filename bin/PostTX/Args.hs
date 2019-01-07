{- |
Module: PostTX.Args
Description: Parser for postTX's command-line arguments
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This is an ad-hoc, but relatively straightforward, command-line parser.
For "historical" (i.e. obsolete) reasons, postTX has a slightly odd
convention for passing the mandatory data identifying the transaction and
the server to connect to.  In addition, because of the ad-hoc nature of the
parser, the argument format, which superficially resembles the *nix
conventions, is inflexible and does not have for all arguments both long
and short variants, or with-equals or as-separate-args conventions for
flags taking arguments.
-}
{-# LANGUAGE TemplateHaskell #-}
module PostTX.Args where

import Blockchain.Fae.FrontEnd 

import Control.Exception (throw)
import Control.Monad (replicateM)

import Common.Lens
import Common.ProtocolT

import Data.List
import Data.Maybe
import Data.Void
import Data.Char
import qualified Data.Text as T

import Text.Megaparsec 
import qualified Text.Megaparsec.Char as C

import Text.Read

-- | This blob of data just records in memory the content of the command
-- line arguments as they are passed.
data PostTXArgs =
  PostTXArgs
  {
    -- | Could be several things that affect the operational mode; see the
    -- usage.
    argDataM :: Maybe String,
    argHostM :: Maybe String,
    argFake :: Bool,
    argView :: Bool,
    argLazy :: Bool,
    argResend :: Bool,
    argImportExport :: (Maybe String, Maybe String),
    argJSON :: Bool,
    argFaeth :: FaethArgs,
    argUsage :: Maybe Usage,
    argShowKeys :: Maybe [String]
  }

-- | This more nuanced structure describes the mode that we decided to use
-- based on the command line, and its relevant parameters.
data FinalizedPostTXArgs =
  -- | Send a transaction normally, or possibly via Faeth
  PostArgs  
  {
    postArgTXNameOrID :: Either TransactionID String,
    postArgHost :: String,
    postArgFake :: Bool,
    postArgLazy :: Bool,
    postArgJSON :: Bool,
    postArgFaeth :: FaethArgs
  } |
  -- | Add metadata to an existing Faeth transaction
  OngoingFaethArgs
  {
    ongoingFaethHost :: String,
    ongoingEthTXID :: EthTXID,
    ongoingFaethArgs :: FaethArgs
  } |
  -- | Recall the results of a previously run transaction (need not have
  -- been sent by this postTX)
  ViewArgs
  {
    viewArgTXID :: TransactionID,
    viewArgJSON :: Bool,
    viewArgHost :: String
  } |
  -- | Just report the public keys we are aware of
  ShowKeysArgs [String] |
  -- | Pipe the result of a contract call between two servers.
  ImportExportArgs
  {
    exportTXID :: TransactionID,
    exportIx :: Int,
    exportHost :: String,
    importHost :: String
  } |
  -- | Print the usage.
  UsageArgs Usage

-- | The usage can be requested explicitly, or printed to correct an
-- invalid command line, in the latter case with an associated error
-- message.
data Usage =
  UsageFailure String |
  UsageSuccess

-- | Various things to do with Faeth.
data FaethArgs =
  FaethArgs
  {
    -- | Regardless of the other flags, a single indicator whether or not
    -- Faeth is activated.  TODO: this should actually be a separate
    -- constructor.
    useFaeth :: Bool,
    faethFee :: Maybe Integer,
    faethValue :: Maybe Integer,
    faethRecipient :: Maybe EthAddress,
    faethTo :: Maybe EthAddress,
    faethArgument :: Maybe Hex,
    newSigners :: [(String, String)]
  }

makeLenses ''PostTXArgs
makeLenses ''FaethArgs

-- | Entry point to this module: traverses the command line arguments to
-- produce a 'PostTXArgs', then processes it to produce
-- a 'FinalizedPostTXArgs'.
parseArgs :: [String] -> FinalizedPostTXArgs
parseArgs = finalize . foldl argGetter 
  PostTXArgs
  {
    argDataM = Nothing,
    argHostM = Nothing,
    argFake = False,
    argView = False,
    argLazy = False,
    argResend = False,
    argImportExport = (Nothing, Nothing),
    argJSON = False,
    argFaeth = FaethArgs False Nothing Nothing Nothing Nothing Nothing [],
    argUsage = Nothing,
    argShowKeys = Nothing
  }

-- | Gives meaning to each command-line flag.  One shortcoming of the
-- method used here is that it is not so easy to accept multiple forms of
-- the same flag, particularly those that actually combine two different
-- command-line arguments (as in @--flag value@, which is quite standard
-- elsewhere).
argGetter :: PostTXArgs -> String -> PostTXArgs
argGetter st "--help" = st & (_argUsage ?~ UsageSuccess)
argGetter st "--show-keys" = st & _argShowKeys ?~ []
argGetter st x 
  | ("--show-keys", '=' : csvKeysInput) <- break (== '=') x
    = st & _argShowKeys ?~ case parseKeysArgs csvKeysInput of
          Left err -> throw err
          Right keyNamesList -> T.unpack . T.strip . T.pack  <$> keyNamesList
argGetter st "--fake" = st & _argFake .~ True
argGetter st "--view" = st & _argView .~ True
argGetter st "--lazy" = st & _argLazy .~ True
argGetter st "--resend" = st & _argResend .~ True
argGetter st "--json" = st & _argJSON .~ True
argGetter st "--faeth" = st & _argFaeth . _useFaeth .~ True
-- The case that handles all the flags with arguments, subject to the issue
-- described above.
argGetter st x 
  | ("--export-host", '=' : exportHostArg) <- break (== '=') x
    = st & _argImportExport . _1 ?~ exportHostArg
  | ("--import-host", '=' : importHostArg) <- break (== '=') x
    = st & _argImportExport . _2 ?~ importHostArg
  | ("--faeth-eth-argument", '=' : ethArgumentArg) <- break (== '=') x
    = st
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethArgument .~ readMaybe ethArgumentArg
  | ("--faeth-add-signature", '=' : newSignerArg) <- break (== '=') x
    = let (sigName, ':' : keyName) = break (== ':') newSignerArg in
      st
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _newSigners %~ ((sigName, keyName) :)
  | ("--faeth-eth-value", '=' : faethValueArg ) <- break (== '=') x
    = st
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethValue .~ readMaybe faethValueArg
  | ("--faeth-fee", '=' : faethFeeArg) <- break (== '=') x
    = st 
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethFee .~ readMaybe faethFeeArg
  | ("--faeth-eth-to", '=' : faethToArg) <- break (== '=') x
    = st 
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethTo .~ readMaybe faethToArg
  | ("--faeth-recipient", '=' : faethRecipArg) <- break (== '=') x
    = st 
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethRecipient .~ readMaybe faethRecipArg
  | "--" `isPrefixOf` x
    = st & _argUsage ?~ UsageFailure ("Unrecognized option: " ++ x)
  | Nothing <- st ^. _argDataM = st & _argDataM ?~ x
  | Nothing <- st ^. _argHostM = st & _argHostM ?~ x
  | otherwise = 
    st & _argUsage ?~ UsageFailure
      (unlines ["Unknown argument: " ++ x, "TX name and host already given"])

-- | An unwieldy logic tree that validates the flags that were actually
-- present against each other, and selects a mode based on what they
-- require.
finalize :: PostTXArgs -> FinalizedPostTXArgs
-- When understanding this, it should be kept in mind that each case is the
-- @else@ clause of the previous ones.  Therefore it is not just the
-- positive condition in the case that selects it, but also all previous
-- negative conditions.  Take care when entering new cases, and don't
-- reorder anything.
finalize PostTXArgs{argFaeth = argFaeth@FaethArgs{..}, ..} 
  | Just u <- argUsage = UsageArgs u
  | Just u <- argShowKeys = ShowKeysArgs u
  | (Just _, Just _) <- argImportExport, 
    argView || argLazy || argFake || argResend || useFaeth
    = error $
        "--import-host and --export-host are incompatible with " ++
        "--view, --lazy, --fake, --resend, and --faeth*"
  | argFake && argView 
    = error "--fake is incompatible with --view"
  | argView && (argLazy || argResend || useFaeth)
    = error "--view is incompatible with --lazy, --resend, and --faeth"
  | argJSON && (argLazy || useFaeth)
    = error "--json is incompatible with --lazy, --faeth*"
  | not (null newSigners) && (isJust faethFee || isJust faethRecipient)
    = error $
      "--faeth-add-signature is incompatible with " ++
      "--faeth-fee and --faeth-recipient"
  | argResend && useFaeth
    = error "--resend is incompatible with --faeth-*"
  | argView, Nothing <- argDataM
    = UsageArgs $ UsageFailure "--view requires a transaction ID"
  | not (null newSigners), Just ethTXIDS <- argDataM =
    OngoingFaethArgs
    {
      ongoingEthTXID =
        fromMaybe (error $ "Couldn't parse Ethereum TXID: " ++ ethTXIDS) $
        readMaybe ethTXIDS,
      ongoingFaethHost = justHost argHostM,
      ongoingFaethArgs = argFaeth
    }
  | argView, Just txIDS <- argDataM =
    ViewArgs
    {
      viewArgTXID = 
        fromMaybe (error $ "Couldn't parse transaction ID: " ++ txIDS) $ 
        readMaybe txIDS,
      viewArgHost = justHost argHostM,
      viewArgJSON = argJSON
    }
  | (exportHostM, importHostM) <- argImportExport,
    Just argData <- argDataM,
    (exportTXIDS, ':' : exportIxS) <- break (== ':') argData =
    ImportExportArgs
    {
      exportTXID = 
        fromMaybe (error $ "Couldn't parse transaction ID: " ++ exportTXIDS) $ 
        readMaybe exportTXIDS,
      exportIx = 
        fromMaybe (error $ "Couldn't parse input call index: " ++ exportIxS) $ 
        readMaybe exportIxS,
      exportHost = justHost exportHostM,
      importHost = justHost importHostM
    }
  | otherwise =
    PostArgs
    {
      postArgTXNameOrID = 
        if argResend
        then Left $
          maybe (error "--resend requires a Fae transaction ID")
            (\argData -> 
              fromMaybe (error $ "Couldn't parse transaction ID: " ++ argData) $ 
              readMaybe argData
            ) argDataM
        else Right $ fromMaybe "TX" argDataM,
      postArgHost = justHost argHostM,
      postArgFake = argFake,
      postArgLazy = argLazy,
      postArgJSON = argJSON,
      postArgFaeth = argFaeth
    }

  where
    justHost :: Maybe String -> String
    justHost = fromMaybe $ "127.0.0.1:" ++ show port where
      port 
        | useFaeth && not argFake = 8546
        | otherwise = 27182

-- | Auxiliary function for @--show-keys@
parseKeysArgs :: String -> Either (ParseError (Token String) Void) [String]
parseKeysArgs input = runParser csvParser "" input
  where csvParser = (many $ C.satisfy (/= ',')) `sepBy` C.char ',' :: Parsec Void String [String]
