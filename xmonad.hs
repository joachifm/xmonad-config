{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.Default (def)

import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Exit

import XMonad (X)
import qualified XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations

import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (safeSpawn)
import XMonad.Actions.GroupNavigation

------------------------------------------------------------------------------

commands :: X [(String, X ())]
commands = defaultCommands

------------------------------------------------------------------------------

main :: IO ()
main = XMonad.xmonad (ewmh config)

------------------------------------------------------------------------------

config = def
       { XMonad.terminal = "xterm"
       , XMonad.modMask = XMonad.mod4Mask
       , XMonad.focusFollowsMouse = False
       , XMonad.keys = keys
       , XMonad.layoutHook = layoutHook
       , XMonad.manageHook = manageHook
       , XMonad.workspaces = workspaces
       , XMonad.startupHook = banish LowerRight
       , XMonad.logHook = historyHook
       }

------------------------------------------------------------------------------

workspaces = [ "Default", "Edit", "Web", "Mail", "IM" ] ++ map show [7..9]

------------------------------------------------------------------------------

manageHook = composeAll [
    checkDock --> doIgnore

  , composeOne [ isDialog     -?> doFloat
               , isFullscreen -?> doFullFloat
               ]

    -- Assign clients to specific work spaces
  , composeOne [ p =? x -?> doShift w
                 | (p, x, w) <- [ (className, "Emacs", "Edit")
                                , (className, "Firefox", "Web")
                                ]
               ]

    -- Floating clients
  , composeOne [ className =? x -?> doFloat | x <- [ "mpv" ] ]
  ]

------------------------------------------------------------------------------

layoutHook = smartBorders Full

------------------------------------------------------------------------------

keys conf@(XMonad.XConfig {XMonad.modMask = modm}) = M.fromList $ [
    ((modm .|. shiftMask, xK_Return), safeSpawn (XMonad.terminal conf) [])
   ,((modm, xK_p), safeSpawn "dmenu_run" [])
  -- Group navigation
  , ((modm, xK_comma), nextMatchWithThis Forward  className)
  , ((modm, xK_period), nextMatchWithThis Backward className)
  , ((modm, xK_BackSpace), nextMatch History (return True)) -- most recent
  , ((modm .|. controlMask, xK_e),
      nextMatchOrDo Forward (className =? "Emacs") (safeSpawn "emacsclient" ["-c"]))
  , ((modm .|. controlMask, xK_t),
      nextMatchOrDo Forward (className =? "XTerm") (safeSpawn "xterm" []))
  , ((modm .|. controlMask, xK_b),
      nextMatchOrDo Forward (className =? "Firefox") (safeSpawn "web" []))

  -- Window management
  , ((modm, xK_w), goToSelected def)
  , ((modm .|. shiftMask, xK_c), kill1)

  -- Workspaces
  , ((modm, xK_h), moveTo Prev NonEmptyWS)
  , ((modm, xK_l), moveTo Next NonEmptyWS)

  -- Window focus
  , ((modm, xK_k), windows W.focusUp)
  , ((modm, xK_j), windows W.focusDown)
  , ((modm, xK_m), windows W.focusMaster)

  -- Master window
  , ((modm, xK_Return), windows W.swapMaster)
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)

  -- XMonad control
  , ((modm .|. controlMask, xK_y), commands >>= runCommand)
  , ((modm .|. shiftMask, xK_q), XMonad.io exitSuccess)
  , ((modm, xK_q), XMonad.spawn "xmonad --recompile && xmonad --restart")

  -- Mouse control
  , ((modm, xK_b), banish LowerRight)
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
