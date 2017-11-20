body :: Transaction Void ()
body _ = newContract [] $ \() -> spend =<< signer "self"
