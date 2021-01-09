module TrayIcon (initIcon, run, setOn, setOff, setInUse) where

import Data.Time.Clock (getCurrentTime)
import GI.Gio.Objects.Notification(notificationNew)
import GI.Gtk
       (Button, StatusIcon, noMenuPositionFunc, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetFromFile, statusIconSetTooltipText,
        statusIconSetVisible, statusIconNewFromFile, buttonNew)
import qualified GI.Gtk as Gtk (main, init)
import Data.Text (pack)
import Resources (onIcon, offIcon, inUseIcon)

initIcon :: IO StatusIcon
initIcon = do
    Gtk.init Nothing
    icon <- statusIconNewFromFile "img/cameramonitor.svg"
    onStatusIconActivate icon $ putStrLn "'activate' signal triggered"
    return icon

run :: StatusIcon -> IO () -> IO ()
run icon cleanup = do
    statusIconSetVisible icon True
    statusIconSetTooltipText icon $ pack "This is a test"
    menu <- mkmenu cleanup
    button <- buttonNew
    onStatusIconPopupMenu icon $ \b a -> do
        widgetShowAll menu
        menuPopup menu (Just button :: Maybe Button) (Just button :: Maybe Button) noMenuPositionFunc b a
    Gtk.main

mkmenu cleanup = do
    m <- menuNew
    mapM_ (mkitem m) [(pack "Quit", quit cleanup)]
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
