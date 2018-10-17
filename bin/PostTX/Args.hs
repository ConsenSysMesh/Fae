{-# LANGUAGE TemplateHaskell #-}

module PostTX.Args where

import Control.Monad.Reader
  
import Blockchain.Fae.FrontEnd (TransactionID)
import Common.Lens hiding (view)
import Common.ProtocolT

import Data.List
import Data.Maybe

import Text.Read

data PostTXArgs =
  PostTXArgs
  {
    argDataM :: Maybe String,
    argHostM :: Maybe String,
    argFake :: Bool,
    argView :: Bool,
    argLazy :: Bool,
    argResend :: Bool,
    argTransferToM :: Maybe String,
    argImportExport :: (Maybe String, Maybe String),
    argJSON :: Bool,
    argFaeth :: FaethArgs,
    argUsage :: Maybe Usage
  } deriving (Show, Read)

data FinalizedPostTXArgs =
  PostArgs
  {
    postArgTXNameOrID :: Either TransactionID String,
    postArgHost :: String,
    postArgFake :: Bool,
    postArgLazy :: Bool,
    postArgJSON :: Bool,
    postArgFaeth :: FaethArgs
  } |
  TransferQueryArgs
  {
    transferTXID :: TransactionID,
    transferToArg :: String
  } |
  OngoingFaethArgs
  {
    ongoingFaethHost :: String,
    ongoingEthTXID :: EthTXID,
    ongoingFaethArgs :: FaethArgs
  } |
  ViewArgs
  {
    viewArgTXID :: TransactionID,
    viewArgJSON :: Bool,
    viewArgHost :: String
  } |
  ImportExportArgs
  {
    exportTXID :: TransactionID,
    exportIx :: Int,
    exportHost :: String,
    importHost :: String
  } |
  UsageArgs Usage

data Usage =
  UsageFailure String |
  UsageSuccess deriving (Show, Read)

data FaethArgs =
  FaethArgs
  {
    useFaeth :: Bool,
    faethFee :: Maybe Integer,
    faethValue :: Maybe Integer,
    faethRecipient :: Maybe EthAddress,
    faethTo :: Maybe EthAddress,
    faethArgument :: Maybe Hex,
    newSigners :: [(String, String)]
  } deriving (Show, Read)

makeLenses ''PostTXArgs
makeLenses ''FaethArgs

-- TODO: see debug.trace, tracem trace show module 
-- get field of input, print them then return PostTXArgs unchanged
-- TODO: finish function
-- printArgs :: PostTXArgs -> IO PostTXArgs -> PostTXArgs
-- printArgs args = do
--   putStrLn ("@@@ Args.hs.printArgs.")
--   print args
--   return args

parseArgs :: [String] -> FinalizedPostTXArgs
parseArgs = finalize . foldl argGetter
--parseArgs = finalize . printArgs . foldl argGetter
  PostTXArgs
  {
    argDataM = Nothing,
    argHostM = Nothing,
    argFake = False,
    argView = False,
    argLazy = False,
    argResend = False,
    argTransferToM = Nothing,
    argImportExport = (Nothing, Nothing),
    argJSON = False,
    argFaeth = FaethArgs False Nothing Nothing Nothing Nothing Nothing [],
    argUsage = Nothing
  }

argGetter :: PostTXArgs -> String -> PostTXArgs
argGetter st "--help" = st & _argUsage .~ Just UsageSuccess
argGetter st "--fake" = st & _argFake .~ True
argGetter st "--view" = st & _argView .~ True
argGetter st "--lazy" = st & _argLazy .~ True
argGetter st "--resend" = st & _argResend .~ True
argGetter st "--json" = st & _argJSON .~ True
argGetter st "--faeth" = st & _argFaeth . _useFaeth .~ True
argGetter st x
  | ("--transfer-to", '=' : transferTo) <- break (== '=')
    x = st & _argTransferToM .~ readMaybe transferTo
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
    = st & _argUsage .~ Just (UsageFailure $ "Unrecognized option: " ++ x)
  | Nothing <- st ^. _argDataM = st & _argDataM ?~ x
  | Nothing <- st ^. _argHostM = st & _argHostM ?~ x
  | otherwise = st & _argUsage .~ Just (UsageFailure $ unlines
      ["Unknown argument: " ++ x, "TX name and host already given"])

finalize :: PostTXArgs -> FinalizedPostTXArgs
finalize PostTXArgs{argFaeth = argFaeth@FaethArgs{..}, ..}
  | Just u <- argUsage = UsageArgs u
  | (Just _, Just _) <- argImportExport,
    argView || argLazy || argFake || argResend || useFaeth
    = error $
        "--import-host and --export-host are incompatible with " ++
        "--view, --lazy, --fake, --resend, and --faeth*"
  | argFake && argView
    = error "--fake is incompatible with --view"
  | argView && (argLazy || argResend || useFaeth)
    = error $
        "--view is incompatible with --lazy, --resend, and --faeth"
  | argJSON && (argLazy || useFaeth)
    = error $
        "--json is incompatible with --lazy, --faeth*"
  | not (null newSigners) && (isJust faethFee || isJust faethRecipient)
    = error $
      "--faeth-add-signature is incompatible with " ++
      "--faeth-fee and --faeth-recipient"
  | argResend &&
    (
      not (null newSigners) ||
      isJust faethFee || isJust faethValue || isJust faethArgument ||
      isJust faethRecipient || isJust faethTo
    )
    = error "--resend is incompatible with --faeth-*"
  | argView, Nothing <- argDataM
    = UsageArgs $ UsageFailure "--view requires a transaction ID"
  | Just transferToArgM <- argTransferToM,
    Just argData <- argDataM =
    TransferQueryArgs 
    {
      transferTXID =
        fromMaybe (error $ "Couldn't parse argData to assign to transferTXID: " ++ argData) $
        readMaybe argData,
      transferToArg = 
        fromMaybe (error $ "Couldn't parse [justHost transferToArgM] to assign to transferToArg: " ++ transferToArgM) $
        readMaybe transferToArgM
    }   
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
          maybe (error $ "--resend requires a Fae transaction ID")
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
    justHost
      | useFaeth && not argFake = fromMaybe "localhost:8546" 
      | otherwise = fromMaybe "localhost:5555" -- TODO: change after testing

    justTX :: Maybe String -> String
    justTX 
      | argFake = fromMaybe "TX123"
      | otherwise = fromMaybe "TXabs"