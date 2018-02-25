import Control.Monad

body :: Transaction Void ()
body _ = do
  newContract [] $ \() -> forever $ do
    newContract [] $ \() -> forever $ release ("Output 0" :: String)
    newContract [] $ \() -> spend ("Output 1" :: String)
    release ("Created 2 outputs" :: String)
