module PostTX.Keys where

import Blockchain.Fae.FrontEnd (PrivateKey, PublicKey, public)

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as S
import Data.List
import Data.Maybe

import PostTX.Args

import System.Directory
import System.Environment
import System.Exit
import System.FilePath


-- Prints a set of the stored public keys inside of the FaeHome directory.
-- The empty list in the first pattern match denote that all stored keys 
-- are to be shown.
showKeys :: FilePath -> [String] -> IO ()
showKeys faeHome [] = do  -- Empty list denotes that all keys should be shown 
  storedKeys <- getHomeKeys faeHome
  if null storedKeys then print $ "No keys found at " ++ show faeHome else
    putStr . unlines $ 
      storedKeys <&> \(keyName, privKey) -> 
        keyName ++ ": " ++
        maybe "Couldn't validate key" show (public privKey)
showKeys faeHome keyNamesList = 
  sequence_ $ showHomeKey faeHome <$> keyNamesList

-- Decodes and prints the contents of a given home key
showHomeKey :: FilePath -> String -> IO ()
showHomeKey faeHome keyName = do
  maybeFile <- findFile [faeHome] keyName
  case maybeFile of 
    Nothing ->
      putStrLn $ keyName ++ " " ++  "not found in " ++ faeHome
    Just file -> do
      keyBytes <- BS.readFile file
      case S.decode keyBytes of 
        Left err -> putStrLn $ 
          show faeHome ++ keyName ++ " could not be decoded" ++ " : " ++ show err 
        Right key -> putStrLn $ 
          maybe "Couldn't validate key" showKey $ public key
          where showKey key = takeBaseName file ++ ": " ++ show key

-- Retrieves all valid key files from the FaeHome directory.
getHomeKeys :: FilePath -> IO [(String, PrivateKey)]
getHomeKeys path = do
  dirList <- getDirectoryContents path
  fileList <- filterM doesFileExist dirList
  sortOn (view _1) . mapMaybe (_2 (preview _Right)) <$> 
    traverse sequenceA [(takeBaseName a, S.decode <$> BS.readFile a) | a <- fileList]
