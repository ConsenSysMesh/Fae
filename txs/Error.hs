body :: Transaction Void ()
body _ = newContract @() @() [] $ \_ -> do {error "error"; spend ();}
