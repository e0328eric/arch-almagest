{-

Almagest Xmonad setting Haskell File
The package xmonad, xmonad-contrib, dbus and utf8-string are installed
Also, install xmonad-log to run polybar
Use pacman to install it

-}
-- XMonad Modules
-- Basic Modules
import Data.Default
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Exit
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.FloatSnap
import XMonad.Actions.GroupNavigation
import XMonad.Actions.MouseResize
import XMonad.Actions.PhysicalScreens

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import qualified XMonad.Hooks.ManageDocks as HM
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

-- Layout Modifiers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.SimpleDecoration (shrinkText)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))

-- Layouts
import XMonad.Layout.Dwindle
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.IM (withIM, Property(Role))
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))

-- Util
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare

------------------------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------------------------

myLayout = HM.avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
  where
    myDefaultLayout = tall
        ||| grid
        ||| threeCol
        ||| threeRow
        ||| oneBig
        ||| noBorders monocle
        ||| space
        ||| floats

tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ spacing 6 $ Mirror $ mkToggle (single MIRROR) $
    mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 $ Full
space      = renamed [Replace "space"]    $ limitWindows 4  $ spacing 6 $ Mirror $ mkToggle (single MIRROR) $
    mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat

------------------------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------------------------

myWorkspaces = ["1","2","3","4","5","6","7","8","9"] ++ (map snd myExtraWorkspaces)
myExtraWorkspaces = [(xK_0, "10")]

------------------------------------------------------------------------------------------
-- ManageHook
------------------------------------------------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "albert"           --> doFloat
    , className =? "Plugin-container" --> doFloat
    , className =? "keepassx"         --> doFloat
    , className =? "Gpick"            --> doFloat
    , className =? "Thunar"           --> doFloat
    , className =? "Pcmanfm"          --> doFloat
    , className =? "Civ6Sub"          --> unFloat
    , className =? "qutebrowser"      --> unFloat
    , className =? "Terminator"       --> unFloat
    , className =? "scrcpy"           --> doFloat
    , className =? "Kakaotalk.exe"           --> doFloat
    -- Used by Chromium developer tools, maybe other apps as well
    , role =? "pop-up"                --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    unFloat = ask >>= doF . W.sink

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

------------------------------------------------------------------------------------------
-- Keybinding
------------------------------------------------------------------------------------------

myKeysKeyBoard conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modMask .|. shiftMask, xK_q), kill)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_h), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask, xK_n), refresh)

    -- Window Navigation
    , ((modMask, xK_Tab), windows W.focusDown)
    , ((mod1Mask, xK_Tab), windows W.focusUp)
    , ((modMask, xK_e), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_Tab), windows W.swapDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.swapUp)
    , ((modMask .|. shiftMask, xK_e), windows W.swapMaster)

    , ((modMask .|. controlMask .|. mod1Mask, xK_w), sendMessage Arrange)
    , ((modMask .|. controlMask .|. mod1Mask, xK_s), sendMessage DeArrange)
    , ((modMask, xK_w), withFocused $ snapMove U Nothing)
    , ((modMask, xK_a), withFocused $ snapMove L Nothing)
    , ((modMask, xK_s), withFocused $ snapMove D Nothing)
    , ((modMask, xK_d), withFocused $ snapMove R Nothing)
    , ((modMask .|. shiftMask, xK_w), withFocused $ snapShrink D Nothing)
    , ((modMask .|. shiftMask, xK_a), withFocused $ snapShrink R Nothing)
    , ((modMask .|. shiftMask, xK_s), withFocused $ snapGrow D Nothing)
    , ((modMask .|. shiftMask, xK_d), withFocused $ snapGrow R Nothing)

    -- Layouts
    , ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), sendMessage HM.ToggleStruts)
    , ((modMask .|. shiftMask, xK_n), sendMessage $ Toggle NOBORDERS)
    , ((modMask .|. shiftMask, xK_equal), sendMessage (Toggle NBFULL) >> sendMessage HM.ToggleStruts)
    , ((modMask .|. shiftMask, xK_f), sendMessage (T.Toggle "floats"))
    , ((modMask .|. shiftMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask, xK_o), sendMessage $ Toggle REFLECTX)
    , ((modMask .|. shiftMask, xK_p), sendMessage $ Toggle REFLECTY)
    , ((modMask .|. shiftMask, xK_m), sendMessage $ Toggle MIRROR)
    , ((modMask, xK_k), sendMessage (IncMasterN 1))
    , ((modMask, xK_j), sendMessage (IncMasterN (-1)))
    , ((modMask, xK_k), increaseLimit)
    , ((modMask, xK_j), decreaseLimit)

    , ((controlMask .|. mod1Mask, xK_a), sendMessage Shrink)
    , ((controlMask .|. mod1Mask, xK_d), sendMessage Expand)
    , ((controlMask .|. mod1Mask .|. shiftMask, xK_a), sendMessage MirrorShrink)
    , ((controlMask .|. mod1Mask .|. shiftMask, xK_d), sendMessage MirrorExpand)
    , ((modMask .|. shiftMask, xK_semicolon), sendMessage MirrorExpand)
    , ((modMask, xK_semicolon), sendMessage MirrorExpand)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((modMask, xK_b), sendMessage ToggleStruts)
    --, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    --, ((0, xK_Print), spawn "scrot") -- 0 means no extra modifier key needs to be pressed in this case.
    , ((controlMask, xK_Print), spawn "spectacle -r")
    , ((controlMask .|. shiftMask, xK_Print), spawn "spectacle -u")
    , ((0, xK_Print), spawn "spectacle") -- 0 means no extra modifier key needs to be pressed in this case.
    , ((modMask, xK_F3), spawn "krusader")
    , ((modMask .|. controlMask, xK_a), spawn "scrcpy")
    , ((controlMask .|. mod1Mask, xK_s), spawn "dmenu_extended_run") -- albert
    , ((modMask .|. shiftMask, xK_Return), spawn "google-chrome-stable") -- run browser
    , ((modMask, xK_f), spawn "st -e ranger") -- ranger
    , ((modMask .|. controlMask, xK_Return), spawn "st -e cmus") -- terminal based music player
    , ((modMask .|. controlMask .|. shiftMask, xK_Return), spawn "auryo") -- soundcloud music player

    -- End of Computer
    , ((modMask .|. shiftMask, xK_c), spawn "check-twice \"Are you sure you want to shutdown?\" \"poweroff\"")
    , ((modMask .|. shiftMask, xK_v), spawn "check-twice \"Are you sure you want to reboot?\" \"reboot\"")
    , ((modMask .|. shiftMask, xK_z), spawn "check-twice \"Are you sure you want to suspend?\" \"systemctl suspend -i\"")
    , ((modMask .|. shiftMask, xK_x), io (exitWith ExitSuccess))

    -- Volume Settings
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- Brightness Setting
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10 # increase screen brightness")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10 # decrease screen brightness")

    -- Reset xmonad
    , ((modMask .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    --, ((modMask .|. shiftMask, xK_F3), spawn "gksu pcmanfm")
    ]

    ++
      [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [x | x <-[xK_1 .. xK_9] ++ [xK_0]]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myKeysMouse conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicMove (Just 50) (Just 50) w)))
    , ((modMask .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)))
    ]

------------------------------------------------------------------------------------------
-- LogHook
------------------------------------------------------------------------------------------

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " : "
    , ppTitle = shorten 40
    }
  where
    bg1 = "#3c3836"
    bg2 = "#504945"
    red = "#fb4934"

------------------------------------------------------------------------------------------
-- Startup Applications
------------------------------------------------------------------------------------------

myStartupHook = do
    -- Transparent Terminal
    spawn "compton --backend xrender --xrender-sync --xrender-sync-fence"
    -- screen locking
    spawnOnce "light-locker"
    -- Wallpaper
    spawnOnce "feh --bg-scale ~/wallpapers/lockimage.jpg"
    -- Polybar Start
    spawn "~/.config/polybar/launch.sh"
    -- Xmodmap keychange setting
    --spawnOnce "~/.script/keysetting_xmodmap.sh"
    -- KDE Connect
    spawn "kdeconnect-indicator"

------------------------------------------------------------------------------------------
-- DBus
------------------------------------------------------------------------------------------

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"


------------------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------------------

main :: IO ()
main = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad $ ewmh def
        { modMask = mod4Mask
        , layoutHook = myLayout
        , workspaces = myWorkspaces
        , handleEventHook = handleEventHook def <+> HM.docksEventHook
        , logHook = dynamicLogWithPP (myLogHook dbus)
        , manageHook = placeHook (fixed (0.5, 0.3))
                    <+> HM.manageDocks
                    <+> myManageHook
                    <+> myManageHook'
                    <+> manageHook def
        , terminal = "st"
        , borderWidth = 2
        , keys = myKeysKeyBoard
        , mouseBindings = myKeysMouse
        , startupHook = myStartupHook
        -- This is the color of the borders of the windows themselves.
        , normalBorderColor  = "#2f3d44"
        , focusedBorderColor = "#268bd2"
        }
