module TrayIcon (initIcon, run, setOn, setOff, setInUse) where

import Data.Time.Clock (getCurrentTime)
import GI.Gtk
       (Widget, StatusIcon, noMenuPositionFunc, onMenuItemActivate,
        menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew,
        onStatusIconActivate, menuPopup, widgetShowAll,
        onStatusIconPopupMenu, statusIconSetFromFile, statusIconSetHasTooltip, statusIconSetTooltipText,
        statusIconSetVisible, statusIconNew)
import qualified GI.Gtk as Gtk (main, init)
import Data.Text (Text, pack)
import Data.List (intersperse)
import Resources

initIcon :: IO StatusIcon
initIcon = do
    Gtk.init Nothing
    icon <- statusIconNew
    onStatusIconActivate icon $ putStrLn "'activate' signal triggered"
    return icon

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

setInUse :: StatusIcon -> [String] -> IO ()
setInUse icon devices = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " InUse"
    statusIconSetTooltipText icon $ getTooltip devices
    statusIconSetFromFile icon inUseIcon

setOn :: StatusIcon -> [String] -> IO ()
setOn icon devices = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " On"
    statusIconSetTooltipText icon $ getTooltip devices
    statusIconSetFromFile icon onIcon

setOff :: StatusIcon -> IO ()
setOff icon = do
    time <- getCurrentTime
    putStrLn $ (show time) ++ " Off"
    statusIconSetTooltipText icon $ pack noCamera
    statusIconSetFromFile icon offIcon

getTooltip :: [String] -> Text
getTooltip = pack . (foldr (++) "") . (intersperse "\n")
