module Main where

import TrayIcon (initIcon, run, setOn, setOff, setInUse)
import Watcher (watch, close)
import GI.Gtk (StatusIcon)
import FileChecker (findMatching)
import ProcessChecker (filesInUse)
import Config (dir, filePrefix)

main :: IO ()
main = do
      icon <- initIcon
      updateIcon icon
      (inotify, watch) <- watch dir filePrefix $ updateIcon icon
      run icon (close inotify watch)
      return ()

getStatus :: IO Status
getStatus = do
    files <- findMatching dir filePrefix
    if (null files) then (return Off)
    else do
        let absoluteFiles = (absolutePath dir) <$> files
        inUse <- filesInUse absoluteFiles
        return (if (null inUse) then (On absoluteFiles) else (InUse inUse))

data Status = Off | On [String] | InUse [String]

updateIcon :: StatusIcon -> IO ()
updateIcon icon = do
    status <- getStatus
    case status of
      Off -> setOff icon
      On devices -> setOn icon devices
      InUse devices -> setInUse icon devices

absolutePath :: FilePath -> FilePath -> FilePath
absolutePath dir name = dir ++ "/" ++ name
