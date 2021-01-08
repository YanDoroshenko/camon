module TrayIcon where

import GI.Gio.Objects.Notification(notificationNew)
import GI.Gtk
       (Button, noMenuPositionFunc, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetFromFile, statusIconSetTooltipText,
        statusIconSetVisible, statusIconNewFromFile, buttonNew)
import qualified GI.Gtk as Gtk (main, init)

start = do
    Gtk.init Nothing
    icon <- statusIconNewFromFile $ if (False) then "img/cameramonitor.svg" else "img/cameramonitor_active.svg"
    statusIconSetVisible icon True
    statusIconSetTooltipText icon "This is a test"
    inotify <- initINotify
    menu <- mkmenu icon watch
    button <- buttonNew
    onStatusIconPopupMenu icon $ \b a -> do
        widgetShowAll menu
        menuPopup menu (Just button :: Maybe Button) (Just button :: Maybe Button) noMenuPositionFunc b a
    onStatusIconActivate icon $
        putStrLn "'activate' signal triggered"
    Gtk.main

mkmenu s watch = do
    m <- menuNew
    mapM_ (mkitem m) [("Quit",quit watch)]
    return m
        where
            mkitem menu (label,act) =
                do
                    i <- menuItemNewWithLabel label
                    menuShellAppend menu i
                    onMenuItemActivate i act

quit watch = do
    removeWatch watch
    mainQuit
