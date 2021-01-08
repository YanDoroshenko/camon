module ProcessChecker where

import System.Process
import GHC.IO.Exception

checkFileInUse :: FilePath -> IO Bool
checkFileInUse p = do
    (\(s, _, _) -> statusSuccess s) <$> readProcessWithExitCode "fuser" [p] ""

statusSuccess :: ExitCode -> Bool
statusSuccess status = case status of
                         ExitSuccess -> True
                         ExitFailure 1 -> False
