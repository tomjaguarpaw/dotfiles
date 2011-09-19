import XMonad
import XMonad.Util.EZConfig(additionalKeys, removeKeys)

main = do
    xmonad $ defaultConfig `additionalKeys` [((0, xK_F1), spawn "python /home/tom/Config/Desktop/wrapselect.py rxvt-screen")
                                            ,((0, xK_F2), spawn "python /home/tom/Config/Desktop/wrapselect.py todo")
                                            ,((mod1Mask .|. shiftMask, xK_q), spawn "ls") -- how do I actually remove keys?  `removeKeys` doesn't seem to work
                                            ,((mod1Mask, xK_q), spawn "ls")
                                            ]
