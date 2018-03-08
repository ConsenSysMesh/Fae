import PostTX.Args
import PostTX.Submit
import PostTX.TXMessage
import PostTX.View

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let (txSpecOrTXID, host, fake, lazy) = parseArgs args
  case txSpecOrTXID of
    Left txSpec -> do
      txData <- buildTXData txSpec
      submit txSpec host fake lazy txData
    Right txID 
      | fake -> error "--fake and --view are incompatible options"
      | otherwise -> view txID host

