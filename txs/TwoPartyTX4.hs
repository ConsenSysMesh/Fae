body :: Transaction (Maybe (Either String String)) String
body (Just (Right s)) = return s
body _ = error "Swap failed!"
