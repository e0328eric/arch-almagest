{-

Almagest Xmonad setting Haskell File
The package xmonad, xmonad-contrib, dbus and utf8-string are installed
Also, install xmonad-log to run polybar
Use pacman to install it

-}
-- XMonad Modules
-- Basic Modules
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (forM_, join)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Default
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
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
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT(EOT), Toggle(..), (??), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances
    ( StdTransformers(MIRROR, NBFULL, NOBORDERS)
    )
import XMonad.Layout.Reflect
    ( REFLECTX(..)
    , REFLECTY(..)
    , reflectHoriz
    , reflectVert
    )
import XMonad.Layout.SimpleDecoration (shrinkText)
import XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts as T
    ( ToggleLayout(Toggle)
    , toggleLayouts
    )
import XMonad.Layout.WindowArranger (WindowArrangerMsg(..), windowArrange)

-- Layouts
import XMonad.Layout.Dwindle
import XMonad.Layout.GridVariants as GV
import XMonad.Layout.IM (Property(Role), withIM)
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Renamed (Rename(CutWordsLeft, Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed as LT
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.ZoomRow
    ( ZoomMessage(ZoomFullToggle)
    , zoomIn
    , zoomOut
    , zoomReset
    , zoomRow
    )

-- Util
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare

---------------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------------
myterm = "alacritty"

main :: IO ()
main = do
    dbus <- D.connectSession
    D.requestName
        dbus
        (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad $
        ewmh
            def
                { modMask = mod4Mask
                , layoutHook = myLayout
                , workspaces = myWorkspaces
                , handleEventHook = handleEventHook def <+> HM.docksEventHook
                , logHook = dynamicLogWithPP (myLogHook dbus)
                , manageHook =
                      placeHook (fixed (0.5, 0.3)) <+>
                      HM.manageDocks <+>
                      myManageHook <+> myManageHook' <+> manageHook def
                , terminal = myterm
                , keys = myKeysKeyboard
                , borderWidth = 3
                , mouseBindings = myKeysMouse
                , startupHook = myStartupHook
        -- This is the color of the borders of the windows themselves.
                , normalBorderColor = "#2f3d44"
                , focusedBorderColor = "#25cea7"
                }

---------------------------------------------------------------------------------------
-- Layouts
---------------------------------------------------------------------------------------
myLayout =
    HM.avoidStruts $
    mouseResize $
    windowArrange $
    T.toggleLayouts floats $
    mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
        tall |||
        grid |||
        threeCol |||
        threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats

spacingBorder = spacingRaw True (Border 7 7 7 7) True (Border 7 7 7 7) True

tall =
    renamed [Replace "tall"] $
    limitWindows 12 $ spacingBorder $ ResizableTall 1 (3 / 100) (1 / 2) []

grid =
    renamed [Replace "grid"] $
    limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ GV.Grid (16 / 10)

threeCol =
    renamed [Replace "threeCol"] $ limitWindows 3 $ ThreeCol 1 (3 / 100) (1 / 3)

threeRow =
    renamed [Replace "threeRow"] $
    limitWindows 3 $ Mirror $ mkToggle (single MIRROR) zoomRow

oneBig =
    renamed [Replace "oneBig"] $
    limitWindows 6 $
    spacing 6 $
    Mirror $
    mkToggle (single MIRROR) $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $ OneBig (5 / 9) (8 / 12)

monocle = renamed [Replace "monocle"] $ limitWindows 20 Full

space =
    renamed [Replace "space"] $
    limitWindows 4 $
    spacing 6 $
    Mirror $
    mkToggle (single MIRROR) $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $ OneBig (2 / 3) (2 / 3)

floats = renamed [Replace "floats"] $ limitWindows 20 simplestFloat

---------------------------------------------------------------------------------------
-- Workspaces
---------------------------------------------------------------------------------------
myWorkspaces =
    ["1", "2", "3", "4", "5", "6", "7", "8", "9"] ++ map snd myExtraWorkspaces

myExtraWorkspaces = [(xK_0, "10")]

---------------------------------------------------------------------------------------
-- ManageHook
---------------------------------------------------------------------------------------
myManageHook =
    composeAll
        [ className =? "MPlayer" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "Plugin-container" --> doFloat
        , className =? "keepassx" --> doFloat
        , className =? "Gpick" --> doFloat
        , className =? "Thunar" --> doFloat
        , className =? "Pcmanfm" --> doFloat
        , className =? "Civ6Sub" --> unFloat
        , className =? "firefox" --> unFloat
        , className =? "naver-whale" --> unFloat
        , className =? "scrcpy" --> doFloat
        , className =? "Kakaotalk.exe" --> doFloat
        , resource =? "scratchpad" --> doFloat
    -- Used by Chromium developer tools, maybe other apps as well
        , role =? "pop-up" --> doFloat
        ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    unFloat = ask >>= doF . W.sink

myManageHook' = composeOne [isFullscreen -?> doFullFloat]

---------------------------------------------------------------------------------------
-- Keybinding
---------------------------------------------------------------------------------------
myKeysKeyboard :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeysKeyboard conf =
    mkKeymap conf $
    -- launch a terminal
    [ ("M-<Return>", spawn $ XMonad.terminal conf)
    , ("M-M1-<Return>", namedScratchpadAction myScratchPads "terminal")
    -- close focused window
    , ("M-S-q", kill)
    --  Reset the layouts on the current workspace to default
    , ("M-S-h", setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ("M-n", refresh)
    -- Window Navigation
    , ("M-<Tab>", windows W.focusDown)
    , ("M1-<Tab>", windows W.focusUp)
    , ("M-<R>", windows W.focusDown)
    , ("M-<L>", windows W.focusUp)
    , ("M-S-C-e", windows W.focusMaster)
    , ("M-S-<Tab>", windows W.swapDown)
    , ("M1-S-<Tab>", windows W.swapUp)
    , ("M-S-<R>", windows W.swapDown)
    , ("M-S-<L>", windows W.swapUp)
    , ("M-S-e", windows W.swapMaster)
    , ("M-C-M1-w", sendMessage Arrange)
    , ("M-C-M1-s", sendMessage DeArrange)
    , ("M-w", withFocused $ snapMove LT.U Nothing)
    , ("M-a", withFocused $ snapMove LT.L Nothing)
    , ("M-s", withFocused $ snapMove LT.D Nothing)
    , ("M-d", withFocused $ snapMove LT.R Nothing)
    , ("M-S-w", withFocused $ snapShrink LT.U Nothing)
    , ("M-S-a", withFocused $ snapShrink LT.L Nothing)
    , ("M-S-s", withFocused $ snapGrow LT.D Nothing)
    , ("M-S-d", withFocused $ snapGrow LT.R Nothing)
    -- Layouts
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", sendMessage HM.ToggleStruts)
    , ("M-S-n", sendMessage $ Toggle NOBORDERS)
    , ( "M-S-<KP_Equal>"
      , sendMessage (Toggle NBFULL) >> sendMessage HM.ToggleStruts)
    , ("M-S-f", sendMessage (T.Toggle "floats"))
    , ("M-S-t", withFocused $ windows . W.sink)
    , ("M-S-o", sendMessage $ Toggle REFLECTX)
    , ("M-S-p", sendMessage $ Toggle REFLECTY)
    , ("M-S-m", sendMessage $ Toggle MIRROR)
    , ("M-k", sendMessage (IncMasterN 1))
    , ("M-j", sendMessage (IncMasterN (-1)))
    , ("M-S-k", increaseLimit)
    , ("M-S-j", decreaseLimit)
    , ("M1-S-a", sendMessage Shrink)
    , ("M1-S-d", sendMessage Expand)
    , ("M1-S-C-a", sendMessage MirrorShrink)
    , ("M1-S-C-d", sendMessage MirrorExpand)
    -- Run Browser
    , ("M-S-<Return>", spawn "firefox")
    -- Run Text editor
    , ("M-e", spawn "code-insiders")
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ("<Print>", spawn "spectacle")
    , ("C-<Print>", spawn "spectacle -r")
    , ("C-S-<Print>", spawn "spectacle -u")
    , ("M-<KP_F3>", spawn "krusader")
    , ("C-M1-s", spawn "dmenu_extended_run") -- dmenu_extended
    , ("M-f", spawn $ myterm ++ " -e vifm") -- vifm
    , ("M-m", spawn $ "mathematica") -- mathematica
    , ("M-n", spawn $ "notion-app") -- notion
    , ("M-C-<Return>", spawn $ myterm ++ " -e mocp") -- terminal based music player
    , ( "M-S-C-k"
      , spawn
            "wine ~/.wine/drive_c/Program\\ Files/Kakao/KakaoTalk/KakaoTalk.exe")
    -- End of Computer
    , ( "M-S-c"
      , spawn
            "check-twice.sh \"Are you sure you want to shutdown?\" \"poweroff\"")
    , ( "M-S-v"
      , spawn "check-twice.sh \"Are you sure you want to reboot?\" \"reboot\"")
    , ( "M-S-z"
      , spawn
            "check-twice.sh \"Are you sure you want to suspend?\" \"systemctl suspend -i\"")
    , ("M-S-C-x", io exitSuccess)
    -- Volume Settings
    , ( "<XF86AudioLowerVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ( "<XF86AudioRaiseVolume>"
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    -- Brightness Setting
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
    -- Reset xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
    ] ++
    [ ("M-" ++ m ++ show k, windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([1 .. 9] ++ [0])
    , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
    ]

myKeysMouse conf@XConfig {XMonad.modMask = modMask} =
    M.fromList
        [ ( (modMask, button1)
          , \w ->
                focus w >> mouseMoveWindow w >>
                ifClick (snapMagicMove (Just 50) (Just 50) w))
        , ( (modMask .|. shiftMask, button1)
          , \w ->
                focus w >> mouseResizeWindow w >>
                ifClick (snapMagicResize [LT.R, LT.D] (Just 50) (Just 50) w))
        ]

---------------------------------------------------------------------------------------
-- LogHook
---------------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus =
    def
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

---------------------------------------------------------------------------------------
-- Startup Applications
---------------------------------------------------------------------------------------
myStartupHook = do
    spawn "picom -b --config $HOME/.config/compton/compton.conf"
    -- screen locking
    spawnOnce "light-locker"
    -- Google Drive
    spawnOnce "insync"
    -- Alert Low Battery
    spawnOnce "battery-low"
    -- Wallpaper
    spawnOnce "nitrogen --restore"
    -- Palm touchpad
    spawnOnce "syndaemon -i 1.0 -t -K -R"
    -- Polybar Start
    spawn "~/.config/polybar/launch.sh"
    -- Xmodmap keychange setting
    --spawnOnce "~/.script/keysetting_xmodmap.sh"
    -- KDE Connect
    spawn "kdeconnect-indicator"

---------------------------------------------------------------------------------------
-- DBus
---------------------------------------------------------------------------------------
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal =
            (D.signal objectPath interfaceName memberName)
                {D.signalBody = [D.toVariant $ UTF8.decodeString str]}
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

---------------------------------------------------------------------------------------
-- Scratchpads
---------------------------------------------------------------------------------------
myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm = myterm ++ " --class scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 1
        t = 1 - h
        l = 1 - w
