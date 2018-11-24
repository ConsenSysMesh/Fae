module PostTX.ImportExport where

import Blockchain.Fae.FrontEnd

import Data.ByteString (ByteString)

import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.Network

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

buildExportRequest :: (TransactionID, Int) -> String -> IO Request 
buildExportRequest exportData exportHost =
  flip formDataBody (requestURL exportHost) $
    modulePart "export" "export" (S.encode exportData) : []

buildImportRequest :: ExportData -> String -> IO Request
buildImportRequest ExportData{..} importHost =
  flip formDataBody (requestURL importHost) $
    modulePart "import" "import" 
      (S.encode (exportedCID, exportStatus, neededModules, exportValType)) :
    modulePart "valuePackage" "valuePackage" exportedValue : []

