{-# LANGUAGE TemplateHaskell #-}
module PostTX.Args where

import Blockchain.Fae.FrontEnd 

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
    argImportExport :: (Maybe String, Maybe String),
    argJSON :: Bool,
    argFaeth :: FaethArgs,
    argUsage :: Maybe Usage
  }

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
    exportSCID :: ShortContractID,
    exportHost :: String,
    importHost :: String
  } |
  UsageArgs Usage

data Usage =
  UsageFailure String |
  UsageSuccess

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
  }

makeLenses ''PostTXArgs
makeLenses ''FaethArgs

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
    (exportTXIDS, ':' : exportSCIDS) <- break (== ':') argData =
    ImportExportArgs
    {
      exportTXID = 
        fromMaybe (error $ "Couldn't parse transaction ID: " ++ exportTXIDS) $ 
        readMaybe exportTXIDS,
      exportSCID = 
        fromMaybe (error $ "Couldn't parse short contract ID: " ++ exportSCIDS) $ 
        readMaybe exportSCIDS,
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

justHost :: Maybe String -> String
justHost = fromMaybe "0.0.0.0:27182"

