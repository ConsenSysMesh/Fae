module PostTX.Submit where

import PostTX.EnvVars
import PostTX.Network
import PostTX.TXMessage

import Control.Exception
import Control.Lens hiding ((<.>))

import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe
import Data.Text (Text)

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import System.Directory
import System.FilePath

submit :: String -> String -> Bool -> TXData -> IO ()
submit txName host fake txData@TXData{_bodyM, _others} = do
  (mainTmpName, mainModule) <- makeTempFile "body" bodyFile
  (tmpNames, otherModules) <- unzip <$> mapM (makeTempFile "other") _others
  request <- buildRequest txName host fake txData mainModule otherModules
  sendReceive request
    `finally` mapM removeFile (mainTmpName : tmpNames)

  where bodyFile = fromMaybe txName _bodyM

makeTempFile :: Text -> String -> IO (String, Part)
makeTempFile label mName = do
  fText <- readFile fName
  fTextResolved <- fmap unlines $ resolveImportVars $ lines fText
  tempDir <- getTemporaryDirectory
  let newName = tempDir </> mName <.> "temp" <.> "hs"
  writeFile newName fTextResolved
  let part = (partFileSource label newName){partFilename = Just fName}
  return (newName, part)

  where fName = mName <.> "hs"

buildRequest :: String -> String -> Bool -> TXData -> Part -> [Part] -> IO Request
buildRequest txName host fake TXData{..} mainModule otherModules =
  flip formDataBody (requestURL host) $
    mainModule :
    otherModules ++
    fmap (uncurry partLBS) 
      (maybe id (:) parentArg $ fakeArg : rewardArg : 
      fallbackArgs ++ inputArgs ++ keysArgs)

  where
    fakeArg = ("fake", ) $ if fake then "True" else "False"
    rewardArg = ("reward", ) $ if _reward then "True" else "False"
    parentArg = ("parent", ) . LC8.pack . show <$> _parent
    fallbackArgs = map (("fallback",) . LC8.pack) _fallback
    inputArgs = map (("input", ) . LC8.pack . show) _inputs
    keysArgs = map (\(signer, key) -> ("key", LC8.pack $ signer ++ ":" ++ key)) _keys

