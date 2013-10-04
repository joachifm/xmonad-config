{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import XMonad.Action.AvoidMaster

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
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Prompt
import XMonad.Prompt.Shell

------------------------------------------------------------------------------

main :: IO ()
main = XMonad.xmonad config

------------------------------------------------------------------------------

config = XMonad.defaultConfig
       { XMonad.terminal = "urxvt"
       , XMonad.modMask = XMonad.mod4Mask
       , XMonad.focusFollowsMouse = False
       , XMonad.keys = keys
       , XMonad.layoutHook = layoutHook
       , XMonad.manageHook = manageHook
       , XMonad.workspaces = workspaces
       }

------------------------------------------------------------------------------

workspaces = [ "Work", "Terminals", "Web", "Media", "Games" ]
             ++ map show ([6..9] :: [Int])

------------------------------------------------------------------------------

manageHook = composeAll [
    checkDock               --> doIgnore
  , isDialog                --> doFloat
  , isFullscreen            --> doFullFloat

  , className =? "MPlayer"  --> doFloat
  , className =? "MPlayer"  --> placeHook (fixed (1,1))
  , className =? "feh"      --> doFloat
  , className =? "Wine"     --> doFloat
  , className =? "Emacs"    --> doShift "Work"
  , className =? "Firefox"  --> doShift "Web"
  , className =? "Keepassx" --> doShift "8"

  , return True             --> doF avoidMaster
  ]

------------------------------------------------------------------------------

layoutHook = modifiers layout
    where
        modifiers = showWName . layoutHints . avoidStruts . smartBorders
        layout = onWorkspace "Games" Full (onWorkspace "Web" simpleTabbed tall) ||| Full

        tall = Tall nmaster delta ratio

        nmaster = 1
        ratio = 1/2
        delta = 3/100

------------------------------------------------------------------------------

keys conf@(XMonad.XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), XMonad.spawn $ XMonad.terminal conf)
    , ((modm, xK_d), XMonad.spawn "(date '+%Y-%m-%d %T'; sleep 1) | dzen2")
    , ((modm, xK_p), XMonad.spawn "SHELL=/usr/bin/dash /usr/bin/dmenu_run")
    , ((modm, xK_x), shellPrompt defaultXPConfig)
    , ((modm, xK_l), XMonad.spawn "xlock")
    , ((modm, xK_g), XMonad.spawn "urxvt -e ghci")
    , ((modm .|. shiftMask, xK_b),
        runOrRaise "firefox" (className =? "Firefox"))
    , ((modm .|. shiftMask, xK_e),
       runOrRaise "edit-server" (className =? "Emacs"))
    , ((modm .|. controlMask, xK_h),
       runOrRaise "keepassx" (className =? "Keepassx"))
    , ((modm .|. controlMask, xK_k),
       XMonad.spawn "xvkbd")

    -- Window management
    , ((modm, xK_w), goToSelected defaultGSConfig)
    , ((modm, xK_s), windows copyToAll)
    , ((modm .|. shiftMask, xK_c), kill1)
    , ((modm .|. shiftMask, xK_s), killAllOtherCopies)
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Floating window movement
    , ((modm .|. controlMask, xK_Left),
       withFocused $ snapMove L Nothing)
    , ((modm .|. controlMask, xK_Right),
       withFocused $ snapMove R Nothing)
    , ((modm .|. controlMask, xK_Up),
       withFocused $ snapMove U Nothing)
    , ((modm .|. controlMask, xK_Down),
       withFocused $ snapMove D Nothing)
    , ((modm .|. controlMask .|. shiftMask, xK_Left),
       withFocused $ snapShrink R Nothing)
    , ((modm .|. controlMask .|. shiftMask, xK_Right),
       withFocused $ snapGrow R Nothing)
    , ((modm .|. controlMask .|. shiftMask, xK_Up),
       withFocused $ snapShrink D Nothing)
    , ((modm .|. controlMask .|. shiftMask, xK_Down),
       withFocused $ snapGrow D Nothing)

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
