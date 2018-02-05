module FaeServer.Git where

import Blockchain.Fae.FrontEnd

import Control.Monad

import System.Environment
import System.Exit
import System.Process

gitInit :: IO ()
gitInit = do
  setEnv "GIT_DIR" "./.fae-git"
  runGitWithArgs "init" ["--quiet"]
  runGitWithArgs "commit" ["-q", "--allow-empty", "-m", "Transaction " ++ show nullID]
  runGitWithArgs "tag" [txGitTag nullID]

gitCommit :: TransactionID -> IO ()
gitCommit txID = do
  runGitWithArgs "add" ["Blockchain"]
  runGitWithArgs "commit" ["-q", "-m", "Transaction " ++ show txID]
  runGitWithArgs "tag" [txGitTag txID]

gitReset :: TransactionID -> IO ()
gitReset oldTXID = runGitWithArgs "reset" ["--hard", "-q", txGitTag oldTXID]

gitClean :: IO ()
gitClean = runGitWithArgs "clean" ["-q", "-f", "./Blockchain"]

txGitTag :: TransactionID -> String
txGitTag txID = "TX" ++ show txID

runGitWithArgs :: String -> [String] -> IO ()
runGitWithArgs cmd args = do
  let fullArgs = "--work-tree" : "." : cmd : args
  (exitCode, out, err) <- readProcessWithExitCode "git" fullArgs ""
  case exitCode of
    ExitSuccess -> unless (null out) $ putStrLn $ unlines
      [
        "`git " ++ cmd ++ "` was successful with the following output:",
        out
      ]
    ExitFailure n -> do
      putStrLn $ unlines $
        ("`git " ++ cmd ++ "` returned code " ++ show n) :
        if null err then [] else
          [            
            "Error message:",
            err
          ]
      exitFailure
 
nullID :: TransactionID
nullID = ShortContractID $ digest ()

