module Main where

import TrayIcon (initIcon, run, setOn, setOff, setInUse)
import Watcher (watch, close)
import GI.Gtk (StatusIcon)
import FileChecker (findMatching)
import ProcessChecker (filesInUse)
import Config (dir, filePrefix)

main :: IO ()
main = do
      status <- getStatus
      icon <- initIcon
      watch <- watch dir filePrefix $ updateIcon icon
      run icon (close watch)
      return ()

getStatus :: IO Status
getStatus = do
    files <- findMatching dir filePrefix
    if (null files) then (return Off)
    else do
        inUse <- filesInUse $ (absolutePath dir) <$> files
        return (if (inUse) then InUse else On)

data Status = Off | On | InUse

updateIcon :: StatusIcon -> IO ()
updateIcon icon = do
    status <- getStatus
    case status of
      Off -> setOff icon
      On -> setOn icon
      InUse -> setInUse icon

absolutePath :: FilePath -> FilePath -> FilePath
absolutePath dir name = dir ++ "/" ++ name
