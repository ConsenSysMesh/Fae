{-# LANGUAGE TemplateHaskell #-}
module PostTX.Args where

import Blockchain.Fae (TransactionID)

import Common.Lens hiding (view)
import Common.ProtocolT

import Data.Bool
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
    argFaeth :: FaethArgs
  }

data FinalizedPostTXArgs =
  PostArgs  
  {
    postArgTXName :: String,
    postArgHost :: String,
    postArgFake :: Bool,
    postArgLazy :: Bool,
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
    viewArgHost :: String
  }

data FaethArgs =
  FaethArgs
  {
    useFaeth :: Bool,
    faethFee :: Maybe Integer,
    faethValue :: Maybe Integer,
    faethRecipient :: Maybe EthAddress,
    faethTo :: Maybe EthAddress,
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
    argFaeth = FaethArgs False Nothing Nothing Nothing Nothing []
  }
          
argGetter :: PostTXArgs -> String -> PostTXArgs
argGetter st "--fake" = st & _argFake .~ True
argGetter st "--view" = st & _argView .~ True
argGetter st "--lazy" = st & _argLazy .~ True
argGetter st "--faeth" = st & _argFaeth . _useFaeth .~ True
argGetter st x 
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
  | "--" `isPrefixOf` x = error $ "Unrecognized option: " ++ x
  | Nothing <- st ^. _argDataM = st & _argDataM ?~ x
  | Nothing <- st ^. _argHostM = st & _argHostM ?~ x
  | otherwise = error "TX name and host already given"

finalize :: PostTXArgs -> FinalizedPostTXArgs
finalize PostTXArgs{argFaeth = argFaeth@FaethArgs{..}, ..} 
  | argFake && (argView || argLazy || useFaeth)
    = error $
        "--fake is incompatible with --view, --lazy, --faeth*, " ++
        "and --new-sender-account"
  | argView && (argLazy || useFaeth)
    = error
        "--view is incompatible with --lazy, --faeth*, and --new-sender-account"
  | not (null newSigners) && (isJust faethFee || isJust faethRecipient)
    = error $
      "--faeth-add-signature is incompatible with " ++
      "--faeth-fee and --faeth-recipient"
  | argView, Nothing <- argDataM
    = error "--view requires a transaction ID"
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
      viewArgHost = justHost argHostM
    }
  | otherwise =
    PostArgs
    {
      postArgTXName = fromMaybe "TX" argDataM,
      postArgHost = justHost argHostM,
      postArgFake = argFake,
      postArgLazy = argLazy,
      postArgFaeth = argFaeth
    }

justHost :: Maybe String -> String
justHost = fromMaybe "0.0.0.0:27182"
