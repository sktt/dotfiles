import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import System.IO


myTerminal = "/usr/bin/urxvtc"
myFont = "xft:inconsolata:size=10:antialias=true"
myModMask = mod4Mask -- Rebind Mod to the windows key

myBorderWidth = 2

myFocusFollowsMouse = False

------------------------------
-- LAYOUTS
------------------------------

myTiled = smartBorders $ Tall nmaster delta ratio
  where nmaster = 1
        delta = 3 / 100
        ratio = 1 / 2

myTabbed = tabbed shrinkText tabTheme

myLayout = avoidStruts $ smartBorders $ 
            myTabbed 
            ||| myTiled 
            ||| Mirror myTiled 
            ||| Full

------------------------------
-- THEME
------------------------------

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

myNormalBorderColor = base01
myFocusedBorderColor = red

myTheme :: Theme
myTheme = defaultTheme { activeColor = base03
                       , activeBorderColor = base03
                       , inactiveColor = base02
                       , inactiveBorderColor = base02
                       , activeTextColor = base1
                       , inactiveTextColor = base00
                       , fontName = "xft:Inconsolata:size=13"
                       , decoHeight = 24 }

baseTheme :: Theme
baseTheme = defaultTheme
    { activeColor           = base03
    , activeBorderColor     = base03
    , activeTextColor       = base01 -- blue also good
    , inactiveBorderColor   = base02
    , inactiveColor         = base02
    , inactiveTextColor     = base01
    , urgentColor           = yellow
    , urgentBorderColor     = yellow
    , urgentTextColor       = base02
    , fontName              = myFont
    , decoHeight            = 22
    }

tabTheme :: Theme
tabTheme = baseTheme
    { -- base00, base01, blue all good activeColors
      activeColor           = base03
    , activeBorderColor     = base03
    , activeTextColor       = base00
    }

myKeys =
     -- XF86AudioMute
    [ ((0 , 0x1008FF12), spawn "amixer -q set Master 0")
    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff11), spawn "amixer -q set Master 10%- unmute && amixer -q set Headphone unmute && amixer -q set Speaker unmute")
    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff13), spawn "amixer -q set Master 10%+ unmute && amixer -q set Headphone unmute && amixer -q set Speaker unmute")
    ]
myKeysP = 
    [ ("M-p", spawn "exec dmenu-dark -b")
    , ("M-g", spawn "exec google-chrome")
    ]
main = do
    xmproc <- spawnPipe "/home/jnes/.cabal/bin/xmobar /home/jnes/.xmobarrc"
    xmonad $ defaultConfig 
        {  manageHook = manageDocks <+> manageHook defaultConfig
        ,  layoutHook = myLayout
      	,  logHook = dynamicLogWithPP xmobarPP 
            {  ppOutput = hPutStrLn xmproc
		    ,  ppTitle = xmobarColor red "" . shorten 60
		    }
        ,  terminal = myTerminal
        ,  modMask = myModMask
        ,  borderWidth = myBorderWidth
        ,  normalBorderColor = myNormalBorderColor
        ,  focusedBorderColor = myFocusedBorderColor
        ,  focusFollowsMouse = myFocusFollowsMouse
        } `additionalKeys` myKeys `additionalKeysP` myKeysP 
      
