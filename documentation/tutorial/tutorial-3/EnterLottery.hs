import Blockchain.Fae.Transactions.TX$lotteryID.Lottery

body :: LotteryResult -> FaeTX (Natural, String)
body EnterResult{..} = return (enterCount, message)
