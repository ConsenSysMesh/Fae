{- |
Module: PostTX.ImportExport
Description: Handler for postTX's import/export mode
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Import/export mode is a pipe between two Fae servers, with no processing in between.
-}
module PostTX.ImportExport where

import Blockchain.Fae.FrontEnd

import Data.ByteString (ByteString)

import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.Network

-- | Requests a contract return value, then forwards the response.  TODO:
-- actually do a little processing to figure out if there are errors,
-- rather than just printing the same message.
importExport :: TransactionID -> Int -> String -> String -> IO ()
importExport exportTXID exportIx exportHost importHost = do
  exportRequest <- buildExportRequest (exportTXID, exportIx) exportHost
  exportResponse <- sendReceiveSerialize exportRequest
  importRequest <- buildImportRequest exportResponse importHost
  sendReceiveSerialize @() importRequest
  putStrLn $
    "Transferred return value of contract call #" ++ show exportIx ++
    " in transaction " ++ show exportTXID ++
    " from " ++ exportHost ++ " to " ++ importHost

-- | Requests a transaction's input result by index.
buildExportRequest :: (TransactionID, Int) -> String -> IO Request 
buildExportRequest exportData exportHost =
  flip formDataBody (requestURL exportHost) $
    modulePart "export" "export" (S.encode exportData) : []

-- | Forwards the exported value, doing only enough processing to separate
-- the binary portion from the metadata.
buildImportRequest :: ExportData -> String -> IO Request
buildImportRequest ExportData{..} importHost =
  flip formDataBody (requestURL importHost) $
    modulePart "import" "import" 
      (S.encode (exportedCID, exportStatus, neededModules, exportNameType)) :
    modulePart "valuePackage" "valuePackage" exportedValue : []

