body :: Transaction Void ()
body _ = do
  newContract [] $ \() -> spend $ Versioned ("Hello, world!" :: String)
  newContract [] (spend @String . getVersioned)
