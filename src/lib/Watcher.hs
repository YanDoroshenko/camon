module Watcher where

import System.INotify
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (ByteString, pack, stripPrefix)

eventVarieties :: [EventVariety]
eventVarieties = [Create, Delete, Open, CloseWrite, CloseNoWrite]

watch :: FilePath -> FilePath -> IO () -> IO WatchDescriptor
watch dir filePrefix f = do
    inotify <- initINotify
    addWatch inotify eventVarieties (pack dir) $ process filePrefix f

close :: WatchDescriptor -> IO ()
close = removeWatch

process :: Monad m => FilePath -> m () -> Event -> m ()
process filePrefix f e = fromMaybe (return ()) $ case e of
    Created _ p -> applyMatching f filePrefix $ Just p
    Deleted _ p -> applyMatching f filePrefix $ Just p
    Opened _ p -> applyMatching f filePrefix p
    Closed _ p _ -> applyMatching f filePrefix p
    _ -> Nothing

applyMatching :: Monad m => m () -> FilePath -> Maybe ByteString -> Maybe (m ())
applyMatching f filePrefix p = (const f) <$> (stripPrefix $ pack filePrefix) <$> p
