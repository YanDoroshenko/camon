module Main where

import TrayIcon (init, run, setAccessed, setCreated, setDeleted, setOpened, setClosed)
import Watcher
import Prelude (IO)
import System.INotify
import GI.Gtk (StatusIcon)

main :: IO ()
main = do
    icon <- TrayIcon.init
    watch <- watch (eventFunctions icon)
    run icon (close watch)

eventFunctions :: StatusIcon -> EventFunctions IO
eventFunctions icon = EventFunctions (setAccessed icon) (setCreated icon) (setDeleted icon) (setOpened icon) (setClosed icon)
