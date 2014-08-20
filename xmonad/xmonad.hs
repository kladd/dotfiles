
import qualified Data.Map as M
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.Run(spawnPipe)

-------------------------------------------------------------------------------
-- Colors and borders
--
myNormalBorderColor  = "#404040"
myFocusedBorderColor = "#7C7C7C"

xmobarTitleColor = "#FFB6B0"
xmobarCurrentWorkspaceColor = "#CEFFAC"

-------------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1.term", "2.code", "3.web", "4.irc", "5.vm"] ++ map show [6..9]

-------------------------------------------------------------------------------
-- dmenu
--
dmenu_command = concat
    [ "dmenu_run -i "
    , "-fn \"xft:Terminus:pixelsize=11,style=italic\" "
    , "-nb \"#000000\" "
    , "-nf \"#FFFFFF\" "
    , "-sb \"#CCEFAC\" "
    , "-sf \"#000000\""
    ]

dzen_left = concat
    [ "dzen2 "
    , "-fn 'xft:Terminus:pixelsize=11,style=italic' "
    , "-ta 'l' "
    , "-w '1920' "
    ]

dzen_right = concat
    [ "i3status | dzen2 "
    , "-fn 'xft:Terminus:pixelsize=11,style=italic' "
    , "-ta 'r' "
    , "-w '920' "
    , "-x '1000' "
    ]

-------------------------------------------------------------------------------
-- Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return),
        spawn $ XMonad.terminal conf)
    , ((modMask, xK_p),
        spawn dmenu_command)
    , ((modMask, xK_l),
        spawn "slimlock")
    , ((0, 0x1008FF11),
        spawn "amixer -q set Master 5%-")
    , ((0, 0x1008FF13),
        spawn "amixer -q set Master 5%+")
    ]

-------------------------------------------------------------------------------
-- Startup
--
startup :: X ()
startup = do
    spawn "/usr/lib/gnome-settings-daemon/gnome-settings-daemon"
    spawn "/usr/bin/compton"
    spawn "/usr/bin/feh --bg-scale ~/linen.png"
    spawn dzen_right

main = do
    left <- spawnPipe dzen_left
    xmonad $ defaultConfig
        { terminal = "/usr/bin/gnome-terminal"
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys = myKeys <+> keys defaultConfig
        , layoutHook = spacing 5 $ avoidStruts $ layoutHook defaultConfig
        , startupHook = startup
        , borderWidth = 2
        , modMask = mod4Mask
        , logHook = dynamicLogWithPP $ dzenPP
            { ppOutput = hPutStrLn left
            , ppTitle = dzenColor xmobarTitleColor "" . shorten 100 . pad
            , ppCurrent = dzenColor xmobarCurrentWorkspaceColor "" . pad
            , ppVisible = dzenColor "#FFFFFF" "" . pad
            , ppHidden = dzenColor "#AAAAAA" "" . pad
            , ppHiddenNoWindows = const ""
            , ppSep = "    "
            , ppLayout = const ""
            }
        }
