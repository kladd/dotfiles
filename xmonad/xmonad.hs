    -- Base
import XMonad
import Data.Maybe (isJust)
import XMonad.Config.Azerty
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, dzenColor, pad, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..)) 
import XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Warp (warpToWindow, banishScreen, Corner(LowerRight))
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir 
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Colors and borders
--
myFont          = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
myBorderWidth   = 1
myColorBG       = "#151515"
myColorWhite    = "#ebebeb"
myColorRed      = "#C3143B"
myColorGray     = "#545454"
myColorDarkgray = "#353535"

-------------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["term", "code", "web", "irc", "vm"] ++ map show [6..9]

-------------------------------------------------------------------------------
-- dmenu
--
dmenu_command = concat
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

myLogHook h  = dynamicLogWithPP $ defaultPP
      { ppOutput           = hPutStrLn h
      , ppCurrent          = dzenColor myColorWhite myColorRed . pad
      , ppHidden           = dzenColor myColorGray myColorBG  . pad
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

myLayoutHook = gaps [(L, 30),(R,30)] $ spacing 30 $ avoidStruts $ layoutHook defaultConfig
-- myLayoutHook =  spacing 30 $ avoidStruts $ layoutHook defaultConfig

-------------------------------------------------------------------------------
-- Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_p), spawn dmenu_command)
    , ((modMask, xK_l), spawn "gnome-screensaver -l")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 5%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 5%+")
    ]

-------------------------------------------------------------------------------
-- Startup
--
startup :: X ()
startup = do
    spawnOnce "/usr/lib/gnome-settings-daemon/gnome-settings-daemon &"
    spawnOnce "/usr/bin/compton -r 6 -l -9 -t -9 -b -e 0.8 &"

main = do
    dzenLeftBar  <- spawnPipe myXmonadBarL
    dzenRightBar <- spawnPipe myXmonadBarR
    xmonad $ defaultConfig
        { terminal = "/usr/bin/gnome-terminal"
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myColorWhite
        , normalBorderColor  = myColorDarkgray
        , keys               = myKeys <+> keys defaultConfig
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook dzenLeftBar
        , modMask            = mod4Mask
        , startupHook        = startup
        , workspaces         = myWorkspaces
        }
