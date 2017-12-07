body :: Transaction (Maybe (Either (Versioned String) (Versioned String))) String
body (Just (Left (Versioned s))) = return s
body (Just (Right (Versioned s))) = return s
body _ = return ""
