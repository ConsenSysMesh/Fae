import Common.ProtocolT

import Control.Monad.Trans

import Data.Maybe

import PostTX.Args
import PostTX.Faeth
import PostTX.SpecParser
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
  txDir <- getCurrentDirectory
  setCurrentDirectory faeHome

  args <- getArgs
  case parseArgs args of
    PostArgs{postArgFaeth = postArgFaeth@FaethArgs{..}, ..} -> do
      txData <- withCurrentDirectory txDir $ buildTXData postArgTXName
      txSpec <- txDataToSpec txData postArgFaeth
      if useFaeth 
      then submitFaeth postArgHost faethValue faethTo txSpec
      else submit postArgTXName postArgHost postArgFake postArgLazy txSpec
    x@OngoingFaethArgs{..} -> 
      resubmitFaeth ongoingFaethHost ongoingEthTXID ongoingFaethArgs
    ViewArgs{..} -> view viewArgTXID viewArgHost
    UsageArgs UsageSuccess -> do
      usage
      exitSuccess
    UsageArgs (UsageFailure err) -> do
      putStrLn err
      usage
      exitFailure

usage :: IO ()
usage = do
  self <- getProgName
  putStrLn $ unlines
    [
      "Usage: (standalone) " ++ self ++ " args",
      "       (with stack) stack exec " ++ self ++ " -- args",
      "", 
      "where args = (tx name | Fae tx ID | Eth tx ID) [host[:port]] [options]",
      "where the available options are:",
      "  Help",
      "  --help        Print this usage",
      "",
      "  Regular Fae operation:",
      "    with a (tx name)",
      "    --fake      Don't add the transaction to the history; only run it once",
      "    --lazy      Don't print transaction results; leave them unevaluated",
      "",
      "    with a (Fae tx ID)",
      "    --view      Display the results of a previously submitted transaction",
      "",
      "  Fae-in-Ethereum (Faeth) operation",
      "    --faeth     Enable Faeth (blockchain is Ethereum, via a Parity client)",
      "                Also implied by any of the following options",
      "",
      "    with a (tx name)",
      "    --faeth-fee number            Set the required ether fee to run this", 
      "                                  Faeth transaction in Fae",
      "    --faeth-recipient address     Set the required Ethereum 'to' address",
      "                                  to run this Faeth transaction in Fae",
      "",
      "    with a (Eth tx ID)",
      "    --faeth-add-signature name    Sign the Fae portion of an existing", 
      "                                  Faeth transaction as the given identity",
      "",
      "    with either",
      "    --faeth-eth-value number      Set the ether value sent with a Faeth", 
      "                                  transaction",
      "    --faeth-eth-to address        Set the Ethereum 'to' address for a", 
      "                                  Faeth transaction",
      "",
      "Recognized environment variables:",
      "  FAE_HOME      Directory where keys are stored",
      "",
      "A (tx name) reads the transaction spec file of that name.  Format is:",
      "",
      "  Simple fields:",
      "    body = (module name)     The main module of the transaction.  Must",
      "                             define 'body :: Transaction argType valType'.",
      "                             Defaults to 'TX'",
      "    reward = (True | False)  For testing; enables transaction rewards",
      "                             Defaults to 'False'",
      "    parent = (Fae tx ID)     Immediately follow a previous transaction",
      "                             Defaults to the end of the longest chain",
      "",
      "  Optional list blocks:",
      "    others                   Additional modules for this transaction",
      "      (- module name)+",
      "    fallback                 Top-level values in the main (body) module",
      "      (- name)+              to be run as fallbacks in case of exceptions.",
      "                             Each type is 'name :: Transaction argType ()'",
      "",
      "  Optional key-value blocks:",
      "    keys                              Sign the transaction with each role",
      "      (role name = (key name | public key))+",
      "                                      as the corresponding named key, or ",
      "                                      require a signature by a key.",
      "                                      Keys are created on demand",
      "    inputs                            Set the contract call list, each one",
      "      (Fae contract path = arg)+      called with the given argument",
      "",
      "The 'parent' field, and each line of the key-value blocks, are scanned for",
      "environment variables ($envvar), which are expanded.",
      "",
      "Contract arguments may include literals for a type 'Versioned a', which",
      "are of the format:",
      "",
      "  (Fae contract ID) ::: (version ID)",
      "",
      "  referring to a version taken from a call to a contract.",
      "",
      "The format of a (Fae contract path) is any of:",
      "",
      "  TransactionOutput (Fae tx ID) n",
      "    The n'th contract created by the body of a transaction",
      "",
      "  InputOutput (Fae tx ID) (Fae contract ID) n",
      "    The n'th contract created by a transaction's call to some contract",
      "",
      "  (One of the above) :# n",
      "    A contract after exactly n successful calls",
      "",
      "The format of the transaction results is:",
      "",
      "  Transaction (Fae tx ID)",
      "    result: 'body' return value",
      "    outputs",
      "      (n: Fae contract ID)*",
      "    signers",
      "      (role name : public key)*",
      "   (input (Fae contract ID)",
      "      nonce: number",
      "      outputs",
      "        (n: Fae contract ID)*",
      "      versions",
      "        (version ID: type)",
      "   )*",
      "",
      "  All IDs, and public keys, are 32-byte unprefixed hex strings"
    ]

