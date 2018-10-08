body :: FaeTX ()
body = newContract c where
  c :: Contract () String
  -- The underscore here means "there is an argument, which is ignored"
  c _ = spend "Hello, world!"
