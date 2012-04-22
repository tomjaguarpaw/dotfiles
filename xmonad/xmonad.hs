import System.IO(hPutStrLn)
import XMonad
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
                           , layoutHook = avoidStruts  $  layoutHook defaultConfig
                           , logHook = dynamicLogWithPP xmobarPP
                                       { ppOutput = hPutStrLn xmproc
                                       , ppTitle = xmobarColor "green" "" . shorten 50
                                       }
                           }
    	     		   `additionalKeys` [((0, xK_F1), spawn "python /home/tom/Config/Desktop/wrapselect.py rxvt-screen")
                                            ,((0, xK_F2), spawn "python /home/tom/Config/Desktop/wrapselect.py todo")
                                            ,((mod1Mask .|. shiftMask, xK_q), spawn "ls") -- how do I actually remove keys?  `removeKeys` doesn't seem to work
                                            ,((mod1Mask, xK_q), spawn "ls")
                                            ]
                           `removeKeys` [(mod1Mask, xK_w), (mod1Mask, xK_q), (mod1Mask .|. shiftMask, xK_q)]
