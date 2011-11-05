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
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

------------------------------------------------------------------------------

main :: IO ()
main = XMonad.xmonad config

------------------------------------------------------------------------------

config = ewmh $ XMonad.defaultConfig
       { XMonad.terminal = "urxvt"
       , XMonad.modMask = XMonad.mod4Mask
       , XMonad.focusFollowsMouse = False
       , XMonad.keys = keys
       , XMonad.layoutHook = layoutHook
       , XMonad.manageHook = manageHook
       }

------------------------------------------------------------------------------

manageHook = manageMoves
         <+> manageFloats
         <+> manageDocks
         <+> (isFullscreen --> doFullFloat)
         <+> XMonad.manageHook XMonad.defaultConfig
    where
        manageFloats = composeOne [ className =? x -?> doFloat
                                    | x <- ["Xmessage"
                                           ,"feh"
                                           ,"Wine"
                                           ,"Dialog"
                                           ,"Minecraft Launcher"
                                           ]
                                  ]
        manageMoves = composeOne [ className =? x -?> doShift w
                                   | (x, w) <- [("Sonata", "5")
                                               ,("Gpodder", "5")
                                               ,("Qbittorrent", "6")
                                               ]
                                 ]
        -- Window property helpers
        {-
        windowRole = stringProperty "WM_WINDOW_ROLE"
        windowName = stringProperty "WM_NAME"
        iconName   = stringProperty "WM_ICON_NAME"
        -}

------------------------------------------------------------------------------

layoutHook = modifiers layout
    where
        modifiers = layoutHints . avoidStruts . smartBorders
        layout = Full ||| wide ||| tall

        wide = Mirror tall
        tall = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 3/100

------------------------------------------------------------------------------

keys conf@(XMonad.XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), XMonad.spawn $ XMonad.terminal conf)
    , ((modMask, xK_p), XMonad.spawn "yeganesh_run")
    , ((modMask .|. shiftMask, xK_b),
        runOrRaise "firefox" (className =? "Firefox"))
    , ((modMask .|. shiftMask, xK_e),
       runOrRaise "emacs" (className =? "Emacs"))

    -- Window management
    , ((modMask, xK_g), goToSelected defaultGSConfig)
    , ((modMask, xK_s), windows copyToAll)
    , ((modMask .|. shiftMask, xK_c), kill1)
    , ((modMask .|. shiftMask, xK_s), killAllOtherCopies)

    -- Workspaces
    , ((modMask, xK_Left), moveTo Prev NonEmptyWS)
    , ((modMask, xK_Right), moveTo Next NonEmptyWS)
    , ((modMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((modMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)

    -- Layouts
    , ((modMask, xK_space), sendMessage XMonad.NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_n), refresh)

    -- Window focus
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_m), windows W.focusMaster)

    -- Master window
    , ((modMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)

    -- XMonad
    , ((modMask .|. shiftMask, xK_q), XMonad.io (exitWith ExitSuccess))
    , ((modMask, xK_q), XMonad.spawn "xmonad --recompile && xmonad --restart")
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
