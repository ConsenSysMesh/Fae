import Control.Monad.Trans.State

body :: Transaction Void String
body = usingState 0 $ \_ -> do
  n <- get
  return $ show n
