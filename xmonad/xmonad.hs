import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.Volume
import System.IO
import Data.Map
import Data.Monoid

myTerminal = "gnome-terminal"

myModMask = mod4Mask -- Rebind Mod to the windows key

myBorderWidth = 2

myNormalBorderColor = "339944"

myFocusedBorderColor = "#66cc77"

myFocusFollowsMouse = False
main = do
    xmproc <- spawnPipe "xmobar"
    xmproc <- spawnPipe "/home/jnes/.cabal/bin/xmobar /home/jnes/.xmobarrc"
    xmonad $ defaultConfig {
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
      	logHook = dynamicLogWithPP xmobarPP {
		      ppOutput = hPutStrLn xmproc,
		      ppTitle = xmobarColor "red" "" . shorten 50
		    },
	      terminal = myTerminal,
	      modMask = myModMask,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        focusFollowsMouse = myFocusFollowsMouse
      }
keys =
    keys defaultConfig `mappend`
    \c -> fromList 
    [ ((0, xK_F11), lowerVolume 4 >> return ())
    , ((0, xK_F12), raiseVolume 4 >> return ())
    ]
