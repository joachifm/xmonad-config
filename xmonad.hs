{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib

import qualified XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Hooks.DynamicLog hiding (statusBar)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run

------------------------------------------------------------------------------

main :: IO ()
main = XMonad.xmonad =<< statusBar config

------------------------------------------------------------------------------

statusBar :: XMonad.XConfig l -> IO (XMonad.XConfig l)
statusBar conf = do
    xmproc <- spawnPipe "xmobar" -- "xmobar ~/.xmobarrc"
    return $ conf { XMonad.logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle  = xmobarColor "green" "" . shorten 50
                    }
                  }

------------------------------------------------------------------------------

config = ewmh $ XMonad.defaultConfig
       { XMonad.terminal = "urxvt"
       , XMonad.modMask = XMonad.mod4Mask
       , XMonad.focusFollowsMouse = False
       , XMonad.keys = keys
       , XMonad.layoutHook = layoutHook
       , XMonad.manageHook = manageHook
       , XMonad.workspaces = workspaces
       }

------------------------------------------------------------------------------

workspaces = [ "Work", "Terminals", "Web", "Media" ]
             ++ map show ([5..9] :: [Int])

------------------------------------------------------------------------------

manageHook = managePlacement
         <+> manageMoves
         <+> manageFloats
         <+> manageDocks
         <+> manageFocus
         <+> (isFullscreen --> doFullFloat)
         <+> XMonad.manageHook XMonad.defaultConfig

managePlacement = composeOne
    [ className =? x -?> placeHook (fixed (1,1)) -- place in the bottom-right corner
                 | x <- [ "MPlayer" ]
    ]

manageFloats = composeOne
    [ className =? x -?> doFloat
                 | x <- [ "Wine"
		     	, "Xmessage"
                        , "feh"
                        ]
    ]

manageMoves = composeOne
    [ className =? x -?> doShift w
                | (x, w) <- [ ("Emacs", "Work")
                            , ("Firefox", "Web")
                            , ("Sonata", "Media")
			    , ("Deluge", "9")
                            ]
    ]

-- Adapted from http://ruderich.org/simon/config/xmonad
manageFocus = composeOne
    [ -- prevent new windows from spawning in the master pane
      return True -?> doF avoidMaster
      -- prevent windwos moved to other workspaces to steal focus
    , return True -?> doF W.focusDown
    ]

-- | Prevent windows from spawning in the master pane.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
    otherwise           -> c

{- Window property helpers
windowRole = stringProperty "WM_WINDOW_ROLE"
windowName = stringProperty "WM_NAME"
iconName   = stringProperty "WM_ICON_NAME"
-}

------------------------------------------------------------------------------

layoutHook = modifiers layout
    where
        modifiers = showWName . layoutHints . avoidStruts . smartBorders
        layout = tall ||| Full

        tall = Tall nmaster delta ratio

        nmaster = 1
        ratio = 1/2
        delta = 3/100

------------------------------------------------------------------------------

keys conf@(XMonad.XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), XMonad.spawn $ XMonad.terminal conf)
    , ((modm, xK_d), XMonad.spawn "(date '+%Y-%m-%d %T'; sleep 1) | dzen2")
    , ((modm, xK_p), XMonad.spawn "dmenu_run")
    , ((modm, xK_x), shellPrompt defaultXPConfig)
    , ((modm, xK_l), XMonad.spawn "xlock")
    , ((modm .|. shiftMask, xK_b),
        runOrRaise "firefox" (className =? "Firefox"))
    , ((modm .|. shiftMask, xK_e),
       runOrRaise "edit-server" (className =? "Emacs"))

    -- Window management
    , ((modm, xK_w), goToSelected defaultGSConfig)
    , ((modm, xK_s), windows copyToAll)
    , ((modm .|. shiftMask, xK_c), kill1)
    , ((modm .|. shiftMask, xK_s), killAllOtherCopies)
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Floating window movement
    --
    -- Note that these bindings shadow bindings for moving windows from
    -- one workspace to the next
    -- XXX: why don't these work?
    , ((modm, xK_Left), withFocused $ snapMove L Nothing)
    , ((modm, xK_Right), withFocused $ snapMove R Nothing)
    , ((modm, xK_Up), withFocused $ snapMove U Nothing)
    , ((modm, xK_Down), withFocused $ snapMove D Nothing)
    , ((modm .|. shiftMask, xK_Left), withFocused $ snapShrink R Nothing)
    , ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
    , ((modm .|. shiftMask, xK_Up), withFocused $ snapShrink D Nothing)
    , ((modm .|. shiftMask, xK_Down), withFocused $ snapGrow D Nothing)

    -- Workspaces
    , ((modm, xK_Left), moveTo Prev NonEmptyWS)
    , ((modm, xK_Right), moveTo Next NonEmptyWS)
    , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)

    -- Layouts
    , ((modm, xK_space), sendMessage XMonad.NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_n), refresh)

    -- Window focus
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_m), windows W.focusMaster)

    -- Master window
    , ((modm, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Slave windows
    , ((modm .|. shiftMask, xK_Tab), rotSlavesUp)

    -- XMonad control
    , ((modm .|. shiftMask, xK_q), XMonad.io (exitWith ExitSuccess))
    , ((modm, xK_q), XMonad.spawn "xmonad --recompile && xmonad --restart")
    -- Mouse control
    , ((modm, xK_b), banish LowerRight)
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
