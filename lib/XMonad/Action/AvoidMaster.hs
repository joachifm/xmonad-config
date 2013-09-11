{-
Prevent new windows from spawning in the master pane.
Source: <http://ruderich.org/simon/config/xmonad>
-}

module XMonad.Action.AvoidMaster
   ( manageFocus
   , avoidMaster
   ) where

import XMonad.Core
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import qualified XMonad.StackSet as W

-- | Keep focus on master.
manageFocus :: ManageHook
manageFocus = composeOne
    [
      -- prevent new windows from spawning in the master pane
      return True -?> doF avoidMaster
      -- prevent windows moved to other workspaces to steal focus
    , return True -?> doF W.focusDown
    ]

-- | Prevent windows from spawning in the master pane.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack r [] (t:rs)
    _                   -> c
