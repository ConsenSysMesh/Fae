module PostTX.ImportExport where

import Blockchain.Fae.FrontEnd

import Data.ByteString (ByteString)

import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.Network

importExport :: TransactionID -> ShortContractID -> String -> String -> IO ()
importExport exportTXID exportSCID exportHost importHost = do
  exportRequest <- buildExportRequest (exportTXID, exportSCID) exportHost
  exportResponse <- sendReceiveSerialize exportRequest
  importRequest <- buildImportRequest exportResponse importHost
  sendReceiveSerialize @() importRequest
  putStrLn $
    "Transferred return value of contract " ++ show exportSCID ++
    " called in transaction " ++ show exportTXID ++
    " from " ++ exportHost ++ " to " ++ importHost

buildExportRequest :: (TransactionID, ShortContractID) -> String -> IO Request 
buildExportRequest exportData exportHost =
  flip formDataBody (requestURL exportHost) $
    modulePart "export" "export" (S.encode exportData) : []

buildImportRequest :: (ContractID, String, ByteString) -> String -> IO Request
buildImportRequest (cID, typeS, valueBS) importHost =
  flip formDataBody (requestURL importHost) $
    modulePart "import" "import" (S.encode (cID, typeS)) :
    modulePart "valuePackage" "valuePackage" valueBS : []

