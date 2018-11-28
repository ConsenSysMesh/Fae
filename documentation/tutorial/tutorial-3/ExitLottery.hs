import Blockchain.Fae.Transactions.TX$lotteryID.Lottery

body :: LotteryResult -> FaeTX String
body WinResult{..} = return $ "Won with " ++ show others ++ " other" ++ plural where 
  others = winnersCount - 1
  plural | others == 1 = ""
         | otherwise = "s"
body ExitResult{..} = return $ "Did not win out of " ++ show lotteryLimit ++ " entries"
