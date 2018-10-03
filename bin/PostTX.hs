{- |
Module: PostTX
Description: Command-line utility for creating and sending Fae transactions
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This utility pairs with 'faeServer' by constructing an HTTP query of the
form that it expects in order to send a transaction specified in
human-readable form in a file on disk.  It has several additional modes
where it can operate on a transaction that has already been sent, or send
the transaction through an Ethereum client using 'faeServer''s Faeth
protocol.
-}

import Common.ProtocolT (Salt)

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Lens hiding (view)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as S
import Data.Maybe
import Data.Serialize (Serialize)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as S

import PostTX.Args
import PostTX.Faeth
import PostTX.Keys
import PostTX.ImportExport
import PostTX.Parser
import PostTX.Submit
import PostTX.TXSpec
import PostTX.View

import System.Directory
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do
  userHome <- getHomeDirectory
  faeHome <- fromMaybe (userHome </> "fae") <$> lookupEnv "FAE_HOME"
  createDirectoryIfMissing True faeHome
  createDirectoryIfMissing True $ faeHome </> "txspecs"
  txDir <- getCurrentDirectory
  setCurrentDirectory faeHome

  args <- getArgs
  case parseArgs args of
    PostArgs{postArgFaeth = postArgFaeth@FaethArgs{..}, ..} -> do
      let buildTXSpec :: 
            (MakesTXSpec m a) => 
            (m (TXSpec a) -> IO (TXSpec a)) -> IO (TXSpec a)
          -- Actually quite simple: either reads a new transaction file, or
          -- fetches an existing 'TXSpec' by id.
          buildTXSpec f = case postArgTXNameOrID of
            Left txID ->
              either (error $ "Bad transaction file: " ++ show txID) id .
                S.decode <$> (C8.readFile $ "txspecs" </> show txID)
            Right txName ->  do
              txData <- withCurrentDirectory txDir $ buildTXData txName
              txSpec <- f $ txDataToTXSpec txData
              let txID = getTXID $ txMessage txSpec
              C8.writeFile ("txspecs" </> show txID) $ S.encode txSpec
              return txSpec

          normalSubmit :: (Serialize a) => TXSpec a -> IO ()
          normalSubmit = submit postArgHost postArgFake postArgLazy postArgJSON 
      case (useFaeth, postArgFake) of
        (True, False) -> do
          txSpec <- buildTXSpec $ flip runReaderT postArgFaeth
          submitFaeth postArgHost faethValue faethTo txSpec
        (True, True) -> do
          txSpec <- buildTXSpec $ flip runReaderT postArgFaeth
          normalSubmit @Salt txSpec
        (False, _) -> do
          txSpec <- buildTXSpec id
          normalSubmit @String txSpec
    OngoingFaethArgs{..} -> 
      resubmitFaeth ongoingFaethHost ongoingEthTXID ongoingFaethArgs
    ViewArgs{..} -> view viewArgTXID viewArgHost viewArgJSON
    ImportExportArgs{..} -> 
      importExport exportTXID exportIx exportHost importHost
    UsageArgs UsageSuccess -> do
      usage
      exitSuccess
    UsageArgs (UsageFailure err) -> do
      putStrLn err
      usage
      exitFailure
    ShowKeysArgs keysList -> do
      showKeys faeHome keysList
      exitSuccess

usage :: IO ()
usage = do
  self <- getProgName
  putStrLn $ unlines
    [
      "Usage: (standalone) " ++ self ++ " args",
      "       (with stack) stack exec " ++ self ++ " -- args",
      "", 
      "where args = data [host[:port]] [options]",
      "where the available options are:",
      "  Help",
      "    --help      Print this usage",
      "",
      "  Viewing Stored Public Keys:",
      "    --show-keys                   Show all keys.",
      "    --show-keys=key1,key2,..      Show one or more keys",
      "",
      "  Regular Fae operation:",
      "    with data = (tx name)",
      "    --fake      Don't add the transaction to the history; only run it once",
      "    --lazy      Don't print transaction results; leave them unevaluated",
      "",
      "    with data = (Fae tx ID)",
      "    --view      Display the results of a previously submitted transaction",
      "    --resend    Post a previous, signed transaction to a new host",
      "",
      "    with a (tx name | Fae tx ID)",
      "    --json      Format the output of a transaction in JSON",
      "",
      "  Fae-in-Ethereum (Faeth) operation",
      "    --faeth     Enable Faeth (blockchain is Ethereum, via a Parity client)",
      "                With --fake, connects to faeServer rather than Parity",
      "                Also implied by any of the following options",
      "",
      "    with data = (tx name)",
      "    --faeth-fee=number            Set the required ether fee to run this", 
      "                                  Faeth transaction in Fae",
      "    --faeth-recipient=address     Set the required Ethereum 'to' address",
      "                                  to run this Faeth transaction in Fae",
      "",
      "    with data = (Eth tx ID)",
      "    --faeth-add-signature=name    Sign the Fae portion of an existing", 
      "                                  Faeth transaction as the given identity",
      "",
      "    with either",
      "    --faeth-eth-value=number      Set the ether value sent with a Faeth", 
      "                                  transaction",
      "    --faeth-eth-to=address        Set the Ethereum 'to' address for a", 
      "                                  Faeth transaction",
      "    --faeth-eth-argument          Set the input that the Ethereum contract",
      "                                  will see, i.e. the contract argument",
      "",
      "  Import/Export operation:",
      "    with data = (Fae tx ID):(input index)",
      "    --import-host=host[:port]     Set host to send import data",
      "    --export-host=host[:port]     Set host to request export data",
      "",
      "Recognized environment variables:",
      "  FAE_HOME      Directory where keys are stored",
      "",
      "A (tx name) reads the transaction spec file of that name.  Format is:",
      "",
      "  Simple fields:",
      "    body = (module name)     The main module of the transaction, which must",
      "                             define 'body :: a -> b -> ... -> FaeTX c'.",
      "                             Required.",
      "    reward = (True | False)  For testing; enables transaction rewards",
      "                             Defaults to 'False'",
      "    parent = (Fae tx ID)     Immediately follow a previous transaction",
      "                             If absent, postTX takes no decision but",
      "                             faeServer defaults to the end of the longest chain",
      "",
      "  Optional list blocks:",
      "    others                   Additional modules for this transaction",
      "      (- module name)+",
      "    fallback                 Top-level values in the main (body) module",
      "      (- name)+              to be run as fallbacks in case of exceptions.",
      "                             Each type is 'name :: a -> b -> ... -> FaeTX ()",
      "",
      "  Optional key-value blocks:",
      "    keys                              Sign the transaction with each role",
      "      (role name = (key name | public key))+",
      "                                      as the corresponding named key, or ",
      "                                      require a signature by a key.",
      "                                      Keys are created on demand",
      "    inputs                            Set the contract call list, each one",
      "      (Fae contract path = arg        called with the given argument and with",
      "         [newRole = oldRole]+ )+      the roles remapped as optionally indicated.",
      "",
      "Environment variables $var are expanded in each component of the",
      "input paths, in the others and fallback lists, and on the right-hand",
      "side of equals signs.",
      "",
      "A (Fae contract path) is a four-component 'path' of the form",
      "",
      "  (Fae txID) / (Body | Input n) / (output index) / (Current | Version hex)",
      "",
      "indicating, respectively, the transaction in which the contract was",
      "created, the component of the transaction that created it, which",
      "output of that component it was, and whether it should be required to",
      "match a particular version ID or not.",
      "",
      "The format of the transaction results is:",
      "",
      "  Transaction (Fae tx ID)",
      "    result: 'body' return value",
      "    outputs",
      "      (n: initial version hex)*",
      "    signers",
      "      (role name : public key)*",
      "   (input (Fae contract ID) ((Updated|Failed|Deleted))",
      "      version: new version hex (only if (Updated))",
      "      outputs",
      "        (n: initial version hex)*",
      "   )*",
      "",
      "  All hex strings, and public keys, are 32 bytes without a leading '0x'."
    ]

