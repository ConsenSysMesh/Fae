import Blockchain.Fae.Contracts

inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = twoPartySwap "12edc52fb91c8433b50faf7fbb1a51295a8382ee3ba8011ffaefc7160011e423" "e5a18e2466c8bb2a9807090d17731c9c91fda7006ddc0470c250d482a1cc0df5"
