import System.IO(hPutStrLn)
import XMonad
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

wrapSelect :: String -> X ()
wrapSelect s = spawn $ "python /home/tom/Config/Desktop/wrapselect.py " ++ s

tomppLayout "Tall" = "|||"
tomppLayout "Mirror Tall" = "|-|"
tomppLayout "Full" = "| |"
tomppLayout s = s

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
                           , layoutHook = avoidStruts  $  layoutHook defaultConfig
                           , logHook = dynamicLogWithPP xmobarPP
                                       { ppOutput = hPutStrLn xmproc
                                       , ppTitle = xmobarColor "green" "" . shorten 50
				       , ppLayout = xmobarColor "lightblue" "" . tomppLayout
                                       }
                           }
    	     		   `additionalKeys` [((0, xK_F1), wrapSelect "rxvt-screen")
                                            ,((0, xK_F2), wrapSelect "todo")
                                            ,((mod1Mask .|. shiftMask, xK_q), spawn "ls") -- how do I actually remove keys?  `removeKeys` doesn't seem to work
                                            ,((mod1Mask, xK_q), spawn "ls")
                                            ]
                           `removeKeys` [(mod1Mask, xK_w), (mod1Mask, xK_q), (mod1Mask .|. shiftMask, xK_q)]
