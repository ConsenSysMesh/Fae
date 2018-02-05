body :: Transaction Void ()
body _ = newContract [] $ \() -> spend ("Called" :: String)
