inputs :: [(ContractID, String)]
inputs = []

body :: Transaction Void ()
body _ = newContract [] c
  where
    c :: Contract String String
    c = spend

