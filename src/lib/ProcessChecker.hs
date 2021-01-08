module ProcessChecker (fileInUse, filesInUse) where

import System.Process (readProcessWithExitCode)
import System.Exit

filesInUse :: Traversable t => t FilePath -> IO Bool
filesInUse ps = (any id) <$> (sequence $ fileInUse <$> ps)

fileInUse :: FilePath -> IO Bool
fileInUse p = (\(s, _, _) -> codeSuccess s) <$> readProcessWithExitCode "fuser" [p] ""

codeSuccess :: ExitCode -> Bool
codeSuccess ExitSuccess = True
codeSuccess (ExitFailure _) = False

