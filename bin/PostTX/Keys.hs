module PostTX.Keys where

import Blockchain.Fae.FrontEnd (PrivateKey, PublicKey, public)

import Control.Monad
import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as S
import Data.Maybe

import PostTX.Args

import System.Directory
import System.Environment
import System.Exit
import System.FilePath

showKeys faeHome [] = do  -- Empty list denotes that all keys should be shown 
  storedKeys <- getHomeKeys faeHome
  if null storedKeys then print $ "No keys found at " ++ show faeHome else
    putStrLn $ concatMap (\(keyName, privKey) ->
      keyName ++ ": " ++ show (fromMaybe (couldNotValidateErr keyName faeHome) (public privKey)) ++ "\n") storedKeys
showKeys faeHome keysList = sequence_ $ getHomeKey faeHome <$> keysList

getHomeKey faeHome keyName = do
  maybeFile <- findFile [faeHome] keyName
  case maybeFile of 
    Nothing -> do 
      print $ "Key: " ++ keyName ++  " not found at " ++ faeHome
      exitFailure
    Just file -> do
      keyBytes <- BS.readFile file
      case S.decode keyBytes of 
        Left err -> do 
          print $ "Key file named " ++ keyName ++  " could not be decoded in " ++ faeHome ++ " : " ++ err 
          exitFailure
        Right key ->
          let pubKey = fromMaybe (couldNotValidateErr keyName faeHome) (public (key :: PrivateKey))
          in putStrLn $ takeBaseName file ++ ": " ++ show pubKey

getHomeKeys :: FilePath -> IO [(String, PrivateKey)]
getHomeKeys path = do
  dirList <- getDirectoryContents path
  fileList <- filterM doesFileExist dirList
  mapMaybe (_2 (preview _Right)) <$> 
    traverse sequenceA [(takeBaseName a, S.decode <$> BS.readFile a) | a <- fileList]

couldNotValidateErr name faeHome = error $ "Key file named " ++ name ++  " could not be validated in " ++ faeHome
