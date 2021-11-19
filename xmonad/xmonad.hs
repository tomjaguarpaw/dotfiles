-- To restart
--
-- ~/.cabal/bin/xmonad --recompile && ~/.cabal/bin/xmonad --restart

import System.IO(hPutStrLn)
import XMonad
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.StatusBar.PP (dynamicLogWithPP,
                                  xmobarColor, shorten,
                                  xmobarPP, ppOutput, ppTitle, ppExtras, ppLayout)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.StackSet (greedyView, shift)
import qualified Graphics.X11
import qualified Graphics.X11.Xinerama
import Data.List (intercalate)

wrapSelect :: String -> X ()
wrapSelect s = spawn $ "exec python /home/tom/Config/dotfiles/wrapselect.py " ++ s

tomppLayout "Tall" = "|||"
tomppLayout "Mirror Tall" = "|-|"
tomppLayout "Full" = "| |"
tomppLayout s = s

xineramaDebug :: X ()
xineramaDebug = do
  rectangles <- liftIO $ Graphics.X11.openDisplay [] >>= Graphics.X11.Xinerama.getScreenInfo
  -- Really ought to quote xinerama and show rectangle, but this will
  -- do for now.
  spawn ("xmessage \""
         ++ "Compiled with Xinerema: "
         ++ show Graphics.X11.Xinerama.compiledWithXinerama ++ "\n"
         ++ "Rectangles:\n"
         ++ intercalate "\n" (map show rectangles)
         ++ "\"")

myExtraWorkspaces :: [(KeySym, String)]
myExtraWorkspaces = [ (xK_0, "0") ]

workspaceKeys :: (b, WorkspaceId) -> [((KeyMask, b), X ())]
workspaceKeys (key, ws) =
  [ ((mod1Mask, key), windows (greedyView ws))
  , ((mod1Mask .|. shiftMask, key), windows (shift ws)) ]

myWorkspaces :: [String]
myWorkspaces = workspaces def ++ map snd myExtraWorkspaces

main :: IO ()
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
                 , workspaces = myWorkspaces
                 }
              `additionalKeys` ([((0, xK_F1), wrapSelect "rxvt-screen")
                                ,((0, xK_F2), wrapSelect "todo")
                                ,((mod1Mask .|. shiftMask .|. controlMask, xK_x),
                                  xineramaDebug)
                                ]
                                ++ concatMap workspaceKeys myExtraWorkspaces)
              `removeKeys` [ (mod1Mask, xK_w)
                           , (mod1Mask, xK_q)
                           , (mod1Mask, xK_n)
                           , (mod1Mask, xK_p)
                           , (mod1Mask .|. shiftMask, xK_p)
                           , (mod1Mask .|. shiftMask, xK_q)
                           , (mod1Mask .|. shiftMask, xK_c)
                           , (mod1Mask .|. shiftMask, xK_Return)
                           ]
