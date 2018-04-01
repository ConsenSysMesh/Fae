import PostTX.Args
import PostTX.SpecParser
import PostTX.Submit
import PostTX.TXSpec
import PostTX.View

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let (txNameOrTXID, host, fake, lazy) = parseArgs args
  case txNameOrTXID of
    Left txName -> do
      txData <- buildTXData txName
      txSpec <- txDataToSpec txName txData
      submit txName host fake lazy txSpec
    Right txID 
      | fake -> error "--fake and --view are incompatible options"
      | otherwise -> view txID host

