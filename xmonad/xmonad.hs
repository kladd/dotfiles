--
-- xmonad config file
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import System.IO

import qualified Data.Map as M

myTerminal = "/usr/bin/urxvt"

myFocusFollowsMouse = True

myFont          = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
myBorderWidth   = 1
myColorBG       = "#151515"
myColorWhite    = "#ebebeb"
myColorRed      = "#C3143B"
myColorGrey     = "#545454"
myColorDarkgrey = "#353535"

myModMask = mod4Mask

myWorkspaces = ["*", "code", "web", "irc"] ++ map show [6..8] ++ ["jail"]

dmenu = concat
    [ "dmenu_run -i "
    , "-fn \"" ++ myFont ++ "\" "
    , "-nb \"" ++ myColorBG ++ "\" "
    , "-nf \"" ++ myColorWhite ++ "\" "
    , "-sb \"" ++ myColorRed ++ "\" "
    , "-sf \"" ++ myColorWhite ++ "\" "
    ]
myBitmapsDir = "/home/kladd/.xmonad/statusbar/icons"
myXmonadBarL = "dzen2 -x '0' -y '0' -h '16' -w '1000' -ta 'l' -fg '"++myColorWhite++"' -bg '"++myColorBG++"' -fn '"++myFont++"' "
myXmonadBarR = "conky -c /home/kladd/.xmonad/statusbar/conky_dzen | dzen2 -x '1000' -y '0' -w '920' -h '16' -ta 'r' -bg '"++myColorBG++"' -fg '"++myColorWhite++"' -fn '"++myFont++"'"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_p), spawn dmenu)
    , ((modMask, xK_l), spawn "gnome-screensaver-command -l")
    , ((modMask, xK_q), spawn "xmonad --recompile && xmonad --restart")
    , ((0, 0x1008FF11), spawn "amixer -c 1 -q set PCM 5%-")
    , ((0, 0x1008FF13), spawn "amixer -c 1 -q set PCM 5%+")
    ]

myStartupHook = do
    spawnOnce "dunst"
    spawnOnce "$HOME/bin/wallpaper.sh"
    spawnOnce "/usr/lib/gnome-settings-daemon/gnome-settings-daemon"
    spawnOnce "compton -c -b -e 0.8 -t -8 -l -9 -r 6 -o 0.7 -m 1.0"

myLayoutHook = spacing 16 $ avoidStruts $ layoutHook defaultConfig

myLogHook h  = dynamicLogWithPP $ defaultPP
      { ppOutput           = hPutStrLn h
      , ppCurrent          = dzenColor myColorWhite myColorRed . pad
      , ppHidden           = dzenColor myColorGrey myColorBG  . pad
      , ppVisible          = dzenColor myColorWhite "" . pad
      , ppHiddenNoWindows  = const ""
      , ppOrder            = \(ws:l:t:_) -> [ws,l,t]
      , ppSep              = dzenColor myColorRed myColorBG " | "
      , ppTitle            = dzenColor myColorWhite myColorBG  . shorten 75
      , ppWsSep            = dzenColor myColorRed myColorBG ""
      , ppLayout           = dzenColor myColorWhite myColorBG  .
                             (\x -> case x of
                                 "oneBig"       -> "^i("++myBitmapsDir++"/mini/nbstack.xbm)"
                                 "tiled"        -> "^i("++myBitmapsDir++"/mini/tile.xbm)"
                                 "lined"        -> "^i("++myBitmapsDir++"/mini/bstack2.xbm)"
                                 "monocle"      -> "^i("++myBitmapsDir++"/mini/monocle.xbm)"
                                 "grid"         -> "^i("++myBitmapsDir++"/mini/grid.xbm)"
                                 "float"        -> "^i("++myBitmapsDir++"/mini/float.xbm)"
                                 "gimp"         -> "^i("++myBitmapsDir++"/fox.xbm)"
                                 "Full"         -> "^i("++myBitmapsDir++"/mini/monocle2.xbm)"
                                 _              -> x
                             )
      } where noScratchPad ws = if ws == "NSP" then "" else pad ws

main = do
    dzenLeftBar  <- spawnPipe myXmonadBarL
    dzenRightBar <- spawnPipe myXmonadBarR
    xmonad $ defaultConfig
        { terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , borderWidth = myBorderWidth
        , normalBorderColor = myColorDarkgrey
        , focusedBorderColor = myColorWhite
        , modMask = myModMask
        , workspaces = myWorkspaces
        , keys               = myKeys <+> keys defaultConfig
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , logHook = myLogHook dzenLeftBar
        }
