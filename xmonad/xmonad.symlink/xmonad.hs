import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
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

myLayout = smartBorders 
    $ mkToggle (NOBORDERS ?? FULL ?? EOT) 
    $ avoidStruts 
    $ myTiled ||| Full 

------------------------------
-- SCRATCHPADS
------------------------------

myScratchpads = let 
  reallyFull = customFloating $ W.RationalRect 0.025 0.025 0.95 0.95
  full = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
  top = customFloating $ W.RationalRect 0.025 0.05 0.95 0.45
  bottom = customFloating $ W.RationalRect 0.2 0.7 0.60 0.3
  in [
    NS "Calendar" 
       "google-chrome --app=https://calendar.google.com"
       (appName =? "calendar.google.com") full 
  , NS "Mail" 
       "google-chrome --app=https://mail.google.com"
       (appName =? "mail.google.com") full 
  , NS "TopTerminal"
       "urxvtc -name TopTerminal"
       (appName =? "TopTerminal") top 
  , NS "BottomTerminal"
       "urxvtc -name BottomTerminal"
       (appName =? "BottomTerminal") bottom 
  ]
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

myKeys = myModKeys ++ myFnKeys 
  where
    myModKeys =
      let binds =
            [ (xK_p, spawn "exec dmenu-dark -b")
            , (xK_g, spawn "exec google-chrome")
            , (xK_v, namedScratchpadAction myScratchpads "TopTerminal")
            , (xK_b, namedScratchpadAction myScratchpads "BottomTerminal")
            , (xK_m, namedScratchpadAction myScratchpads "Mail")
            , (xK_c, namedScratchpadAction myScratchpads "Calendar")
            , (xK_Caps_Lock, sendMessage $ Toggle FULL)
            ]
      in [((myModMask, key), action) | (key, action) <- binds]
    myFnKeys = 
       -- XF86AudioMute
      [ ((0 , 0x1008FF12), spawn "amixer -q set Master 0")
      -- XF86AudioLowerVolume
      , ((0 , 0x1008ff11), spawn "amixer -q set Master 10%- unmute && amixer -q set Headphone unmute && amixer -q set Speaker unmute")
      -- XF86AudioRaiseVolume
      , ((0 , 0x1008ff13), spawn "amixer -q set Master 10%+ unmute && amixer -q set Headphone unmute && amixer -q set Speaker unmute")
      ]

myManageHook = manageDocks 
            <+> manageHook defaultConfig 
            <+> namedScratchpadManageHook myScratchpads
myPP = defaultPP
    { ppCurrent             = xmobarColor base02 blue . wrap " " " "
    , ppTitle               = xmobarColor base0 "" . shorten 200
    , ppVisible             = wrap "(" ")"
    , ppUrgent              = xmobarColor base02 yellow . wrap " " " "
    , ppHidden              = id
    , ppHiddenNoWindows     = const ""
    , ppSep                 = "  "
    , ppWsSep               = " "
    , ppLayout              = const ""
    , ppOrder               = id
    , ppSort                = fmap 
                              (namedScratchpadFilterOutWorkspace.)
                              (ppSort defaultPP)
    , ppExtras              = []
    }
  
main = do
    xmproc <- spawnPipe "/home/jnes/.cabal/bin/xmobar /home/jnes/.xmobarrc"
    xmonad $ defaultConfig 
        {  layoutHook         = myLayout
      	,  logHook            = dynamicLogWithPP myPP
            {  ppOutput       = hPutStrLn xmproc
            }
        ,  terminal           = myTerminal
        ,  modMask            = myModMask
        ,  borderWidth        = myBorderWidth
        ,  normalBorderColor  = myNormalBorderColor
        ,  focusedBorderColor = myFocusedBorderColor
        ,  focusFollowsMouse  = myFocusFollowsMouse
        ,  manageHook         = myManageHook
        } `additionalKeys` myKeys 
      
