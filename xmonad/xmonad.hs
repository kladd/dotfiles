
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
myNormalBorderColor  = "#404040"
myFocusedBorderColor = "#CEFFAC"

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

-------------------------------------------------------------------------------
-- Keybindings
--
myModMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return),
       spawn $ XMonad.terminal conf)
    , ((modMask, xK_p),
       spawn dmenu_command)
    , ((modMask .|. controlMask, xK_l),
       spawn "gnome-screensaver-command -l")
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

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaultConfig
        { terminal = "/usr/bin/gnome-terminal"
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys = myKeys <+> keys defaultConfig
        , layoutHook = spacing 5 $ avoidStruts $ layoutHook defaultConfig
        , startupHook = startup
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
            , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
            , ppSep = "   "
            }
        }
