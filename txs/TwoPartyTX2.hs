import Blockchain.Fae.Contracts
import TwoPartyCommon

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body = offer2TX A 
