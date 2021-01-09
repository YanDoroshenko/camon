module ProcessChecker (fileInUse, filesInUse) where

import System.Process (readProcessWithExitCode)
import System.Exit
import Data.Maybe (catMaybes)

filesInUse :: [FilePath] -> IO [FilePath]
filesInUse ps = do
    maybes <- sequence $ fileInUse <$> ps
    return $ catMaybes maybes

fileInUse :: FilePath -> IO (Maybe FilePath)
fileInUse p = (\(s, _, _) -> if (codeSuccess s) then Just p else Nothing) <$> readProcessWithExitCode "fuser" [p] ""

codeSuccess :: ExitCode -> Bool
codeSuccess ExitSuccess = True
codeSuccess (ExitFailure _) = False

