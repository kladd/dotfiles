-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config
  { font = "xft:Segoe UI-9"
  , bgColor = "#000000"
  , fgColor = "#ffffff"
  , position = Static { xpos = 0, ypos = 0, width = 1920, height = 22 }
  , lowerOnStart = True
  , commands =
    [ Run Weather "KARB" ["-t","<tempF>F - <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000
    , Run Memory ["-t","Mem: <usedratio>%","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
    , Run Swap ["-t","Swap: <usedratio>%","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
    , Run Network "wlan0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10
    , Run Date "%a %b %_d %l:%M" "date" 10
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %memory%   %swap%   %wlan0%   <fc=#FFFFCC>%date%</fc>   %KARB%"
  }
