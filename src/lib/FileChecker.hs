module FileChecker (findMatching) where

import System.Directory (listDirectory)
import Data.ByteString.Char8 (ByteString, pack, stripPrefix)

findMatching :: FilePath -> FilePath -> IO [FilePath]
findMatching dir filePrefix = (filter $ match filePrefix) <$> (listDirectory dir)

match :: String -> String -> Bool
match prefix str = not $ null $ stripPrefix (pack prefix) (pack str)
