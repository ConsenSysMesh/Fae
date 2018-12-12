{- |
Module: FaeServer.Git
Description: Git operations for keeping transaction modules
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae transactions come along with their source code, which is a bunch of
Haskell modules that may be visible to other, later transactions.
Therefore the correct set of modules needs to be visible on disk (in the
interpreter's search path) at each point in history.  We achieve this by
utilizing @git@ to record each post-transaction state as a commit tagged by
the transaction ID, and to reset to each one when it is used as a parent
transaction.
-}

module FaeServer.Git where

import Blockchain.Fae.FrontEnd

import Control.Monad

import System.Directory
import System.Environment
import System.Exit
import System.Process

-- | More than just @git init@, this also ensures a truly fresh repo, sets
-- up necessary user info, and creates the "genesis transaction" commit.
gitInit :: IO ()
gitInit = do
  removePathForcibly "git"
  runGitWithArgs "init" ["--quiet"]
  runGitWithArgs "config" ["user.name", "Fae"]
  runGitWithArgs "config" ["user.email", "fae"]
  runGitWithArgs "commit" ["-q", "--allow-empty", "-m", "Transaction " ++ show nullID]
  runGitWithArgs "tag" [mkTXIDName nullID]

-- | Commits and also tags the @Blockchain@ hierarchical directory.
gitCommit :: TransactionID -> IO ()
gitCommit txID = do
  runGitWithArgs "add" ["Blockchain"]
  runGitWithArgs "commit" ["-q", "-m", "Transaction " ++ show txID]
  runGitWithArgs "tag" [mkTXIDName txID]

-- | This is a /hard/ reset, because we really want to have exactly the
-- correct set of modules on disk afterwards.
gitReset :: TransactionID -> IO ()
gitReset oldTXID = runGitWithArgs "reset" ["--hard", "-q", mkTXIDName oldTXID]

-- | @git clean@ removes untracked files, e.g. modules produced by fake
-- transactions.  These would otherwise persist across hard resets.
gitClean :: IO ()
gitClean = runGitWithArgs "clean" ["-q", "-f", "Blockchain"]

-- | Invokes @git --work-tree .@ since we intentionally use a nonstandardly
-- named git directory; any output is helpfully reported.
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
 
