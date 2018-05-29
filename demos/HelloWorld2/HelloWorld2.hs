body :: Transaction Void ()
body _ = newContract [] c where
  c :: Contract () String
  c _ = spend "Hello, world!"
