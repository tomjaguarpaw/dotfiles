import System.IO(hPutStrLn)
import XMonad
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders

wrapSelect :: String -> X ()
wrapSelect s = spawn $ "python /home/tom/Config/Desktop/wrapselect.py " ++ s

tomppLayout "Tall" = "|||"
tomppLayout "Mirror Tall" = "|-|"
tomppLayout "Full" = "| |"
tomppLayout s = s

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ def { manageHook = manageDocks <+> manageHook def
                 , layoutHook = smartBorders $ avoidStruts $ layoutHook def
                 , logHook = dynamicLogWithPP xmobarPP
                               { ppOutput = hPutStrLn xmproc
                               , ppTitle = xmobarColor "green" "" . shorten 100
                               , ppLayout = xmobarColor "lightblue" "" . tomppLayout
                               }
                 , borderWidth = 2
                 -- The handleEventHook entry seems to be needed so that
                 -- windows don't cover xmobar on the desktop that is active
                 -- when xmonad is (re)started.  See
                 --
                 -- * https://mail.haskell.org/pipermail/xmonad/2016-May/015103.html
                 --
                 -- * https://bbs.archlinux.org/viewtopic.php?id=206890
                 , handleEventHook = mconcat
                                       [ docksEventHook
                                       , handleEventHook def ]
                 }
              `additionalKeys` [((0, xK_F1), wrapSelect "rxvt-tmux")
                               ,((0, xK_F2), wrapSelect "todo")
                               ]
              `removeKeys` [ (mod1Mask, xK_w)
                           , (mod1Mask, xK_q)
                           , (mod1Mask .|. shiftMask, xK_q)
                           , (mod1Mask .|. shiftMask, xK_c)
                           , (mod1Mask .|. shiftMask, xK_Return)
                           ]
