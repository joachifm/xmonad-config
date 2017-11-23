{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.Default (def)

import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Exit

import qualified XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (safeSpawn)

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
       }

------------------------------------------------------------------------------

workspaces = [ "Edit", "Web", "Mail", "IM" ] ++ map show [6..9]

------------------------------------------------------------------------------

manageHook = composeAll [
    checkDock --> doIgnore

  , composeOne [ isDialog     -?> doFloat
               , isFullscreen -?> doFullFloat
               ]

    -- Assign clients to specific work spaces
  , composeOne [ className =? x -?> doShift w
                 | (x, w) <- [ ("Emacs", "Edit")
                             , ("Firefox", "Web")
                             , ("Pidgin", "IM")
                             ]
               ]

    -- Floating clients
  , composeOne [ className =? x -?> doFloat | x <- floatingClients ]
  ]

floatingClients = [ "mpv" ]

------------------------------------------------------------------------------

layoutHook = smartBorders Full

------------------------------------------------------------------------------

keys conf@(XMonad.XConfig {XMonad.modMask = modm}) = M.fromList $ [
    ((modm .|. shiftMask, xK_Return), safeSpawn (XMonad.terminal conf) [])
   ,((modm, xK_p), safeSpawn "dmenu_run" [])

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

  -- Slave windows
  , ((modm .|. shiftMask, xK_Tab), rotSlavesUp)

  -- XMonad control
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
