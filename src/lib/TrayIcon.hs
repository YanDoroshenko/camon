module TrayIcon where

import GI.Gio.Objects.Notification(notificationNew)
import GI.Gtk
       (Button, StatusIcon, noMenuPositionFunc, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetFromFile, statusIconSetTooltipText,
        statusIconSetVisible, statusIconNewFromFile, buttonNew)
import qualified GI.Gtk as Gtk (main, init)
import Data.Text (pack)
import Resources

init :: IO StatusIcon
init = do
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

quit cleanup = do
    cleanup
    mainQuit

updateIcon :: StatusIcon -> Maybe String -> IO ()
updateIcon icon path = case path of
                         Just p -> do
                             statusIconSetFromFile icon p
                         Nothing -> statusIconSetFromFile icon "/home/yan/Pictures/logo.svg"

setAccessed :: StatusIcon -> IO ()
setAccessed icon = do
    putStrLn "Accessed"
    updateIcon icon $ Just activeIcon

setCreated :: StatusIcon -> IO ()
setCreated icon = do
    putStrLn "Created"
    updateIcon icon $ Just onIcon

setDeleted :: StatusIcon -> IO ()
setDeleted icon = do
    putStrLn "Deleted"
    updateIcon icon $ Nothing

setOpened :: StatusIcon -> IO ()
setOpened icon = do
    putStrLn "Opened"
    updateIcon icon $ Just activeIcon

setClosed :: StatusIcon -> IO ()
setClosed icon = do
    putStrLn "Closed"
    updateIcon icon $ Just onIcon
