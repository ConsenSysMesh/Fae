{-# LANGUAGE TemplateHaskell #-}
module PostTX.Args where

import Blockchain.Fae (TransactionID)

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
    argJSON :: Bool,
    argFaeth :: FaethArgs,
    argUsage :: Maybe Usage,
    argShowKeys :: Maybe ShowKeys
  }

data FinalizedPostTXArgs =
  PostArgs  
  {
    postArgTXName :: String,
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
  UsageArgs Usage | ShowKeysArgs ShowKeys

data Usage =
  UsageFailure String |
  UsageSuccess

data ShowKeys = ShowKeys | ShowKey String 

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
    argJSON = False,
    argFaeth = FaethArgs False Nothing Nothing Nothing Nothing Nothing [],
    argUsage = Nothing,
    argShowKeys = Nothing
  }
          
argGetter :: PostTXArgs -> String -> PostTXArgs
argGetter st "--help" = st & _argUsage .~ Just UsageSuccess
argGetter st "--show-keys" = st & _argShowKeys .~ Just ShowKeys 
argGetter st x 
  | ("--show-key", '=' : name) <- break (== '=') x
    = st & _argShowKeys .~ Just (ShowKey name)
argGetter st "--fake" = st & _argFake .~ True
argGetter st "--view" = st & _argView .~ True
argGetter st "--lazy" = st & _argLazy .~ True
argGetter st "--json" = st & _argJSON .~ True
argGetter st "--faeth" = st & _argFaeth . _useFaeth .~ True
argGetter st x 
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
  | Just u <- argShowKeys = ShowKeysArgs u
  | argFake && (argView || argLazy || useFaeth)
    = error $
        "--fake is incompatible with --view, --lazy, --faeth*, " ++
        "and --new-sender-account"
  | argJSON && (argLazy || useFaeth)
    = error $
        "--json is incompatible with --lazy, --faeth*"
  | argView && (argLazy || useFaeth)
    = error
        "--view is incompatible with --lazy, --faeth*, and --new-sender-account"
  | not (null newSigners) && (isJust faethFee || isJust faethRecipient)
    = error $
      "--faeth-add-signature is incompatible with " ++
      "--faeth-fee and --faeth-recipient"
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
  | otherwise =
    PostArgs
    {
      postArgTXName = fromMaybe "TX" argDataM,
      postArgHost = justHost argHostM,
      postArgFake = argFake,
      postArgLazy = argLazy,
      postArgJSON = argJSON,
      postArgFaeth = argFaeth
    }

justHost :: Maybe String -> String
justHost = fromMaybe "0.0.0.0:27182"

