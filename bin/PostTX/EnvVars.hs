module PostTX.EnvVars where

import Data.Char

import System.Environment

resolveImportVars :: [String] -> IO [String]
resolveImportVars [] = return []
resolveImportVars x
  | (l : rest) <- dropWhile (all isSpace) x,
    "import" : _ <- words l = do
      let (ls, iTail) = span headSpace rest
      iHeadR <- mapM resolveLine $ l : ls 
      iTailR <- resolveImportVars iTail
      return $ iHeadR ++ iTailR
  | otherwise = return x

  where
    headSpace "" = True
    headSpace (c : rest) = isSpace c

resolveLine :: String -> IO String
resolveLine [] = return []
resolveLine l = do
  resolvedVar <- 
    case varM of
      Nothing -> return ""
      Just "" -> return "$"
      Just var -> getEnv var
  resolvedAfter <- resolveLine after
  return $ before ++ resolvedVar ++ resolvedAfter 

  where
    (before, after0) = break (== '$') l
    (varM, after) 
      | null after0 = (Nothing, "")
      | ('$' : after1) <- after0 = 
          let (var, after) = span isAlphaNum after1 in
          (Just var, after) 


