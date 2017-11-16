body :: Transaction (Maybe (Either String String)) String
body (Just (Left s)) = return s
body _ = error "Swap failed!"
