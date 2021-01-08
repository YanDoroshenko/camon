module Watcher where

import Resources (dir, pathPrefix)
import System.INotify
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (ByteString, pack, stripPrefix)

data EventFunctions m = EventFunctions {
    accessed :: m (),
    created :: m (),
    deleted :: m ()
}

eventVarieties :: [EventVariety]
eventVarieties = [Create, Delete, Access, Open, CloseWrite, CloseNoWrite]

watch :: EventFunctions IO -> IO WatchDescriptor
watch f = do
    inotify <- initINotify
    addWatch inotify eventVarieties (pack dir) $ process f

close :: WatchDescriptor -> IO ()
close = removeWatch

process :: Monad m => EventFunctions m -> Event -> m ()
process fs e = fromMaybe (return ()) $ case e of
    Accessed _ p -> applyMatching (accessed fs) $ p
    Created _ p -> applyMatching (created fs) $ Just p
    Deleted _ p -> applyMatching (deleted fs) $ Just p
    Opened _ p -> applyMatching (accessed fs) $ p
    Closed _ p _ -> applyMatching (accessed fs) $ p
    _ -> Nothing

applyMatching :: Monad m => m () -> Maybe ByteString -> Maybe (m ())
applyMatching f p = (const f) <$> (stripPrefix $ pack pathPrefix) <$> p
