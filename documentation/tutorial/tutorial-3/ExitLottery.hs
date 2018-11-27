import Blockchain.Fae.Transactions.TX$lotteryID.Lottery

body :: LotteryResult -> FaeTX Bool
body WinResult{} = return True
body _ = return False
