-- To restart
--
-- ~/.cabal/bin/xmonad --recompile && ~/.cabal/bin/xmonad --restart
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

import Data.List (intercalate)
import Data.Ratio (denominator, numerator)
import qualified Graphics.X11 (openDisplay)
import qualified Graphics.X11.Xinerama (compiledWithXinerama, getScreenInfo)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.StatusBar.PP
  ( dynamicLogWithPP,
    ppExtras,
    ppLayout,
    ppOutput,
    ppTitle,
    shorten,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spiral (Direction (..), Rotation (..))
import XMonad.StackSet (greedyView, integrate, shift)
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Util.Loggers (battery, loadAvg, logCmd)
import XMonad.Util.Run (spawnPipe)

wrapSelect :: String -> X ()
wrapSelect s = spawn $ "exec python /home/tom/Config/dotfiles/wrapselect.py " ++ s

-- logCmd takes only the first line.  If the string ends in a newline
-- everything will end up getting pushed off the xmobar!
freeMem :: X (Maybe String)
freeMem = logCmd "/bin/sh /home/tom/free.sh"

tomppLayout "Tall" = "|||"
tomppLayout "Mirror Tall" = "|-|"
tomppLayout "Full" = "| |"
tomppLayout "Spiral" = "𖦹"
tomppLayout "MySpiral" = "𖦹"
tomppLayout s = s

xineramaDebug :: X ()
xineramaDebug = do
  rectangles <- liftIO $ Graphics.X11.openDisplay [] >>= Graphics.X11.Xinerama.getScreenInfo
  -- Really ought to quote xinerama and show rectangle, but this will
  -- do for now.
  spawn
    ( "xmessage \""
        ++ "Compiled with Xinerema: "
        ++ show Graphics.X11.Xinerama.compiledWithXinerama
        ++ "\n"
        ++ "Rectangles:\n"
        ++ intercalate "\n" (map show rectangles)
        ++ "\""
    )

myExtraWorkspaces :: [(KeySym, String)]
myExtraWorkspaces = [(xK_0, "0")]

workspaceKeys :: (b, WorkspaceId) -> [((KeyMask, b), X ())]
workspaceKeys (key, ws) =
  [ ((mod1Mask, key), windows (greedyView ws)),
    ((mod1Mask .|. shiftMask, key), windows (shift ws))
  ]

myWorkspaces :: [String]
myWorkspaces = workspaces def ++ map snd myExtraWorkspaces

data MySpiralWithDir a
  = MkMySpiralWithDir Direction XMonad.Layout.Spiral.Rotation Rational
  deriving (Read, Show)

instance LayoutClass MySpiralWithDir a where
  pureLayout (MkMySpiralWithDir dir rot scale) sc stack = zip ws rects
    where
      ws = integrate stack
      -- ratios = blend scale . reverse . take (length ws - 1) . mkRatios $ drop 1 fibs
      ratios = replicate (length ws - 1) 0.5 ++ [1]
      rects = divideRects (zip ratios dirs) sc
      dirs = dropWhile (/= dir) $ case rot of
        CW -> cycle [East .. North]
        CCW -> cycle [North, West, South, East]
  description _ = "MySpiral"

myLayout :: (MySpiralWithDir `Choose` (MySpiralWithDir `Choose` Full)) Window
myLayout = MkMySpiralWithDir South CW 1 ||| MkMySpiralWithDir East CW 1 ||| Full

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    ewmhFullscreen $
      ewmh
        def
          { manageHook = manageDocks <+> manageHook def,
            layoutHook = smartBorders $ avoidStruts $ myLayout,
            logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmproc,
                    ppTitle = xmobarColor "green" "" . shorten 50,
                    ppLayout = xmobarColor "lightblue" "" . tomppLayout,
                    ppExtras = [loadAvg, battery, freeMem]
                  },
            borderWidth = 2,
            -- The handleEventHook entry seems to be needed so that
            -- windows don't cover xmobar on the desktop that is active
            -- when xmonad is (re)started.  See
            --
            -- \* https://mail.haskell.org/pipermail/xmonad/2016-May/015103.html
            --
            -- \* https://bbs.archlinux.org/viewtopic.php?id=206890
            handleEventHook =
              mconcat
                [ docksEventHook,
                  handleEventHook def
                ],
            workspaces = myWorkspaces
          }
        `additionalKeys` ( [ ((0, xK_F1), wrapSelect "rxvt-screen"),
                             ((0, xK_F2), wrapSelect "todo"),
                             ( (mod1Mask .|. shiftMask .|. controlMask, xK_x),
                               xineramaDebug
                             )
                           ]
                             ++ concatMap workspaceKeys myExtraWorkspaces
                         )
        `removeKeys` [ (mod1Mask, xK_w),
                       (mod1Mask, xK_q),
                       (mod1Mask, xK_n),
                       (mod1Mask, xK_p),
                       (mod1Mask .|. shiftMask, xK_p),
                       (mod1Mask .|. shiftMask, xK_q),
                       (mod1Mask .|. shiftMask, xK_c),
                       (mod1Mask .|. shiftMask, xK_Return)
                     ]

-- This will produce one more rectangle than there are splits details
divideRects :: [(Rational, Direction)] -> Rectangle -> [Rectangle]
divideRects [] r = [r]
divideRects ((r, d) : xs) rect = case divideRect r d rect of
  (r1, r2) -> r1 : divideRects xs r2

-- It's much simpler if we work with all Integers and convert to
-- Rectangle at the end.
data Rect = Rect Integer Integer Integer Integer

fromRect :: Rect -> Rectangle
fromRect (Rect x y w h) = Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

toRect :: Rectangle -> Rect
toRect (Rectangle x y w h) = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

divideRect :: Rational -> Direction -> Rectangle -> (Rectangle, Rectangle)
divideRect r d rect =
  let (r1, r2) = divideRect' r d $ toRect rect
   in (fromRect r1, fromRect r2)

divideRect' :: Rational -> Direction -> Rect -> (Rect, Rect)
divideRect' ratio dir (Rect x y w h) =
  case dir of
    East -> let (w1, w2) = chop ratio w in (Rect x y w1 h, Rect (x + w1) y w2 h)
    South -> let (h1, h2) = chop ratio h in (Rect x y w h1, Rect x (y + h1) w h2)
    West -> let (w1, w2) = chop (1 - ratio) w in (Rect (x + w1) y w2 h, Rect x y w1 h)
    North -> let (h1, h2) = chop (1 - ratio) h in (Rect x (y + h1) w h2, Rect x y w h1)

chop :: Rational -> Integer -> (Integer, Integer)
chop rat n =
  let f = (fromIntegral n * numerator rat) `div` denominator rat
   in (f, n - f)
