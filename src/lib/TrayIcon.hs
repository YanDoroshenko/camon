module TrayIcon (initIcon, run, setOn, setOff, setInUse) where

import Data.Time.Clock (getCurrentTime)
import GI.Gio.Objects.Notification(notificationNew)
import GI.Gtk
       (Widget, StatusIcon, noMenuPositionFunc, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetFromFile, statusIconSetTooltipText,
        statusIconSetVisible, statusIconNew)
import qualified GI.Gtk as Gtk (main, init)
import Data.Text (pack)
import Resources (onIcon, offIcon, inUseIcon, quitStr)

initIcon :: IO StatusIcon
initIcon = do
    Gtk.init Nothing
    statusIconNew

run :: StatusIcon -> IO () -> IO ()
run icon cleanup = do
    statusIconSetVisible icon True
    statusIconSetTooltipText icon $ pack "This is a test"
    menu <- mkmenu cleanup
    onStatusIconPopupMenu icon $ \b a -> do
        widgetShowAll menu
        menuPopup menu (Nothing :: Maybe Widget) (Nothing :: Maybe Widget) noMenuPositionFunc b a
    Gtk.main

mkmenu cleanup = do
    m <- menuNew
    mapM_ (mkitem m) [(pack quitStr, cleanup)]
    return m
        where
            mkitem menu (label,act) =
                do
                    i <- menuItemNewWithLabel label
                    menuShellAppend menu i
                    onMenuItemActivate i act

quit :: IO () -> IO ()
quit cleanup = do
    cleanup
    mainQuit

setInUse :: StatusIcon -> IO ()
setInUse icon = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " InUse"
    statusIconSetFromFile icon inUseIcon

setOn :: StatusIcon -> IO ()
setOn icon = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " On"
    statusIconSetFromFile icon onIcon

setOff :: StatusIcon -> IO ()
setOff icon = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " Off"
    statusIconSetFromFile icon offIcon
