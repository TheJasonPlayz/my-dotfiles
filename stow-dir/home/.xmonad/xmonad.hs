{- IMPORTS -}

-- Custom
import Colors
import Polybar
import Vars

-- Core
import XMonad 
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- Data
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

-- Utilities
import XMonad.Util.EZConfig

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isDialog)

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.MessageFeedback
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces (removeWorkspace)

-- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.BinaryColumn
import XMonad.Layout.Dishes
import XMonad.Layout.Dwindle as D
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ZoomRow
import XMonad.Layout.Maximize
import XMonad.Layout.Hidden

-- DBus
import qualified DBus as D
import qualified DBus.Client as D

{- MAIN -}
main :: IO ()
main = mkDbusClient >>= main'

main' :: D.Client -> IO ()
main' dbus = xmonad $ ewmhFullscreen . ewmh $ docks $ dynamicProjects projects $ def 
    { terminal = myTerminal
    , modMask = mod1Mask
    , borderWidth = 2
    , normalBorderColor = color2
    , focusedBorderColor = color3
    , startupHook = myStartupHook True
    , layoutHook = myLayoutHook
    , manageHook = manageDocks
    , workspaces = names
    , logHook = myPolybarLogHook dbus
    } `removeKeysP` removedKeys `additionalKeysP` newKeys `removeMouseBindings` removedMouseBindings `additionalMouseBindings` newMouseBindings 

{- HOOKS -}
myStartupHook :: Bool -> X ()
myStartupHook isPolybar = do 
    setWMName "LG3D"

    spawn "killall xmobar"
    when (isPolybar == True) $ spawn "polybar -r xmonad"

    spawn "picom"
    spawn "pipewire && pipewire-pulse"
    spawn "discord-screenaudio"
    spawn "steam"
    spawn "copyq --start-server"
    spawn "redshift -l 38.973320:-104.622970"

    spawn "flameshot"     

    spawn "nitrogen --restore &"


myLayoutHook = avoidStruts $
    smartBorders $
    gaps (zip [U,D,R,L] (repeat (0))) $
    hiddenWindows $
    maximize $
    smartSpacing 0 $ 
    (Full ||| 
    ResizableThreeColMid 1 (3/100) (1/2) [] |||
    ResizableTall 1 (3/100) (1/2) [] |||
    GridRatio (4/4) |||
    BinaryColumn 1.0 32 ||| 
    Dishes 2 (1/6) |||
    Dwindle R D.CW 1.5 1.1 |||
    Spiral R D.CW 1.5 1.1 |||
    OneBig (1/2) (1/2))

{- BINDINGS -}
removedKeys :: [String]
removedKeys = []

newKeys :: [(String, X ())]
newKeys = [
        -- Programs
    ("M1-S-c", kill)
    , ("M1-S-<Return>", spawn myTerminal)
    , ("M1-S-f", spawn "krusader")
    , ("M1-S-p", spawn "rofi -show combi")
    --, ("M1-S-c", spawn "resource monitor")
    , ("M1-s", spawn "flameshot gui")

    , ("M1-w", switchProjectPrompt promptConfig)
    , ("M1-S-w", shiftToProjectPrompt promptConfig)
--    , ("M1-C-w", shiftAndSwitch
    , ("M1-r", renameProjectPrompt promptConfig)
    , ("M1-<Backspace>", removeWorkspace)
    , ("M1-<Left>", prevWS)
    , ("M1-<Right>", nextWS)
    , ("M1-S-<Left>", shiftToPrev)
    , ("M1-S-<Right>", shiftToNext)
    , ("M1-C-<Left>", shiftToPrev >> prevWS)
    , ("M1-C-<Right>", shiftToNext >> nextWS)

    , ("M1-\\", withFocused hideWindow)
    , ("M1-S-\\", popOldestHiddenWindow)
    , ("M1-m", windows W.focusMaster)
    , ("M1-S-m", windows W.swapMaster)
    , ("M1-C-m", withFocused (sendMessage . maximizeRestore))
    , ("M1-S-<Down>", sendMessage $ Shrink)
    , ("M1-S-<Up>", sendMessage $ Expand)
    , ("M1-C-<Down>", sendMessage $ (IncMasterN (-1)))
    , ("M1-C-<Up>", sendMessage $ (IncMasterN (1)))
    , ("M1-g", sendMessage $ ToggleGaps)
    --, ("M1-S-g", sendMessage $ ToggleStruts) XMOBAR TOGGLE STRUTS
    , ("M1-S-g", togglePolybar >> sendMessage ToggleStruts)
    , ("M1-S-s", incWindowSpacing 2)
    , ("M1-C-s", decWindowSpacing 2)
    , ("M1-t", withFocused $ windows . W.sink)
    
    -- Window Navigation
    , ("M1-h", windows W.swapUp)
    , ("M1-j", windows W.focusUp)
    , ("M1-k", windows W.focusDown)
    , ("M1-l", windows W.swapDown)
    ] where
        togglePolybar = spawn "polybar-msg cmd toggle &"

newMouseBindings :: [((ButtonMask, Button), Window -> X ())]
newMouseBindings = []

removedMouseBindings :: [(ButtonMask, Button)]
removedMouseBindings = []
