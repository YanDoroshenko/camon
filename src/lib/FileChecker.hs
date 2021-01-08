module FileChecker where

import System.Directory (doesFileExist)

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

anyExists :: Traversable t => t FilePath -> IO Bool
anyExists ps = (any id) <$> (sequence $ fileExists <$> ps)
