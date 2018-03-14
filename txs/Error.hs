body :: Transaction Void ()
body _ = newContract @() @() [] $ \_ -> error "error"
