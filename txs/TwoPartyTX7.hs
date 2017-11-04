import Blockchain.Fae.Contracts

import TwoPartyCommon

inputs :: [(ContractID, String)]
inputs = 
  [
    (TransactionOutput "4eb29f4262087e9a890524a5a381942276e2447702b8b9979a4c84202479b1d6" 0, "Get"),
    (TransactionOutput "6dfe98a7e0bbdeefe162c080a94b04b05967883dd38aac1e25b3ae6a4d40a578" 0, "TwoPartyT.tokenID.Just")
  ]

body :: Transaction TwoPartyT String
body = return . escrowTXResult . result
