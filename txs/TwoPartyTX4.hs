body :: Transaction (Maybe (Either (Versioned String) (Versioned String))) String
body (Just (Right (Versioned s))) = return s
body _ = error "Swap failed!"
