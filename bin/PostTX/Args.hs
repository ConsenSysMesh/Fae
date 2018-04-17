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
    argFaeth :: FaethArgs,
    argNewSender :: Bool,
    argUseSender :: Bool
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
  ViewArgs
  {
    viewArgTXID :: TransactionID,
    viewArgHost :: String
  } |
  SenderArgs
  {
    senderAddressM :: Maybe EthAddress,
    senderPassphrase :: String
  }

data FaethArgs =
  FaethArgs
  {
    useFaeth :: Bool,
    faethFee :: Maybe Integer,
    faethEthValue :: Maybe Integer
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
    argFaeth = FaethArgs False Nothing Nothing,
    argNewSender = False,
    argUseSender = False
  }
          
argGetter :: PostTXArgs -> String -> PostTXArgs
argGetter st "--fake" = st & _argFake .~ True
argGetter st "--view" = st & _argView .~ True
argGetter st "--lazy" = st & _argLazy .~ True
argGetter st "--new-sender-account" = st & _argNewSender .~ True
argGetter st "--use-sender-account" = st & _argUseSender .~ True
argGetter st "--faeth" = st & _argFaeth . _useFaeth .~ True
argGetter st x 
  | ("--faeth-eth-value", '=' : faethValueArg ) <- break (== '=') x
    = st
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethEthValue .~ readMaybe faethValueArg
  | ("--faeth-fee", '=' : faethFeeArg) <- break (== '=') x
    = st 
      & _argFaeth . _useFaeth .~ True
      & _argFaeth . _faethFee .~ readMaybe faethFeeArg
  | "--" `isPrefixOf` x = error $ "Unrecognized option: " ++ x
  | Nothing <- st ^. _argDataM = st & _argDataM ?~ x
  | Nothing <- st ^. _argHostM = st & _argHostM ?~ x
  | otherwise = error "TX name and host already given"

finalize :: PostTXArgs -> FinalizedPostTXArgs
finalize PostTXArgs{argFaeth = argFaeth@FaethArgs{..}, ..} 
  | argFake && (argView || argLazy || useFaeth || argNewSender || argUseSender)
    = error $
        "--fake is incompatible with --view, --lazy, --faeth, " ++
        "and --new-sender-account"
  | argView && (argLazy || useFaeth || argNewSender || argUseSender)
    = error "--view is incompatible with --lazy, --faeth, and --new-sender-account"
  | useFaeth && (argNewSender || argUseSender)
    = error "--faeth and --new-sender-account are incompatible options"
  | argNewSender && argUseSender
    = error
        "--new-sender-account and --use-sender-account are incompatible options"
  | argView, Nothing <- argDataM
    = error "--view requires a transaction ID"
  | argView, Just txIDS <- argDataM =
    ViewArgs
    {
      viewArgTXID = 
        fromMaybe (error $ "Couldn't parse transaction ID: " ++ txIDS) $ 
        readMaybe txIDS,
      viewArgHost = justHost argHostM
    }
  | argNewSender, Nothing <- argDataM
    = error "--new-sender-account requires a passphrase"
  | argUseSender, Nothing <- argDataM
    = error "--use-sender-account requires ethereumAddress:passphrase"
  | argNewSender, Just senderPassphrase <- argDataM = 
    SenderArgs{senderAddressM = Nothing, ..}
  | argUseSender, Just addressPassphrase <- argDataM = 
    let 
      (addressHex, ':' : senderPassphrase) = break (== ':') addressPassphrase 
      address = fromMaybe (error $ "Invalid Ethereum address: " ++ addressHex) $
        readMaybe addressHex
    in SenderArgs{senderAddressM = Just address, ..}
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
