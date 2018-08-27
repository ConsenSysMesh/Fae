body :: Transaction Void ()
body _ = do
  newContract [] $ \() -> do
    newContract [] (spend @())
    spend $ Versioned ("Hello, world!" :: String)
  newContract [] (spend @String . getVersioned)
