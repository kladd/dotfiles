
import qualified Data.Map as M
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-------------------------------------------------------------------------------
-- Colors and borders
--
<<<<<<< HEAD
myNormalBorderColor  = "#404040"
myFocusedBorderColor = "#CEFFAC"
=======
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#7C7C7C"
>>>>>>> a857c3022ffa7bdd7d69f357f2de2abac931636f

xmobarTitleColor = "#FFB6B0"
xmobarCurrentWorkspaceColor = "#CEFFAC"

-------------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1.term", "2.code", "3.web"] ++ map show [4..9]

-------------------------------------------------------------------------------
-- dmenu
--
dmenu_command = concat
    [ "dmenu_run -i "
    , "-fn \"xft:Segoe UI-10\" "
    , "-nb \"#000000\" "
    , "-nf \"#FFFFFF\" "
    , "-sb \"#CCEFAC\" "
    , "-sf \"#000000\""
    ]

dzen_command = concat
    [ "dzen2 "
    , "-fn 'xft:Terminus-8' "
    , "-ta 'l' "
    , "-w '1920' "
    ]

-------------------------------------------------------------------------------
-- Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask, xK_Return),
        spawn $ XMonad.terminal conf)
    , ((mod1Mask, xK_p),
        spawn dmenu_command)
    , ((mod1Mask .|. controlMask, xK_l),
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

main = do
    h <- spawnPipe dzen_command
    xmonad $ defaultConfig
        { terminal = "/usr/bin/gnome-terminal"
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys = myKeys <+> keys defaultConfig
        , layoutHook = spacing 5 $ avoidStruts $ layoutHook defaultConfig
        , startupHook = startup
        , borderWidth = 2
        , modMask = mod4Mask
        , logHook = dynamicLogWithPP $ dzenPP
            { ppOutput = hPutStrLn h
            , ppTitle = dzenColor xmobarTitleColor "" . shorten 100 . pad
            , ppCurrent = dzenColor xmobarCurrentWorkspaceColor "" . pad
            , ppVisible = dzenColor "#FFFFFF" "" . pad
            , ppHidden = dzenColor "#AAAAAA" "" . pad
            , ppHiddenNoWindows = const ""
            , ppSep = "    "
            , ppLayout = const ""
            }
        }
