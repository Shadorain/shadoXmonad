-- Shadomonad Config --

-------------------------------------------------------------------------------
-- Imports:
-------------------------------------------------------------------------------
    -- Base
import XMonad
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

    -- Actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect

    -- Data
import Data.Maybe (isJust)

    -- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

    -- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layout Mods
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-------------------------------------------------------------------------------
-- Variables:
-------------------------------------------------------------------------------
    -- Base
myBrowser       = "firefox" -- Set default browser
myFilemngr      = "vifmrun" -- Set default file manager
myFont          = "xft:Agave:pixelsize=14" -- Set font
myModMask       = mod1Mask -- Default Modkey (Alt)
myTerminal      = "kitty " -- Set default terminal
myTextEditor    = "nvim" -- Set default text editor
windowCount :: X (Maybe String)
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset -- Get count of windows in selected workspace

    -- Borders
myBorderWidth   = 3
myNormalBorderColor  = "##8897F4"
myFocusedBorderColor = "#9188ff"

    -- Focus
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True -- Whether focus follows the mouse pointer.
myClickJustFocuses :: Bool
myClickJustFocuses = False -- Whether clicking on a window to focus also passes the click to the window

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-------------------------------------------------------------------------------
-- Key Bindings:
-------------------------------------------------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- Xmonad
    [ ((modm .|. controlMask,   xK_q     ), io (exitWith ExitSuccess)                   ) -- Quit
    , ((modm,                   xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ((mod4Mask.|.controlMask, xK_F12   ), spawn "~/.config/scripts/switch_gpu"        ) -- Switch GPU
    , ((modm .|. controlMask,   xK_b     ), spawn "killall xmobar; xmobar") -- Xmobar Restart
        -- Base
    , ((modm .|. shiftMask,     xK_Return), spawn $ XMonad.terminal conf                ) -- Terminal
    , ((modm,                   xK_p     ), spawn "dmenu_run"                           ) -- Dmenu
    , ((modm,                   xK_b     ), sendMessage ToggleStruts                    ) -- Toggle Bar
    , ((modm .|. shiftMask,     xK_b     ), sendMessage $ (MT.Toggle NOBORDERS)         ) -- Toggle Borders
        -- Layouts
    , ((modm,                   xK_space ), sendMessage NextLayout                      ) -- Rotate available layouts
    , ((modm .|. shiftMask,     xK_space ), setLayout $ XMonad.layoutHook conf          ) -- Reset layouts on current workspace
    , ((modm,                   xK_f     ), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles Fullscreen
        -- Windows
    , ((modm .|. shiftMask, xK_c     ), kill                                            ) -- close window
    , ((modm,                   xK_n     ), refresh                                     ) -- Resize viewed windows to the correct size
    , ((modm,                   xK_Tab   ), windows W.focusDown                         ) -- Focus next
    , ((modm,                   xK_j     ), windows W.focusDown                         ) -- Focus next
    , ((modm,                   xK_k     ), windows W.focusUp                           ) -- Focus prev
    , ((modm,                   xK_m     ), windows W.focusMaster                       ) -- Focus master
    , ((modm,                   xK_Return), windows W.swapMaster                        ) -- Swap focused win with master
    , ((modm .|. shiftMask,     xK_j     ), windows W.swapDown                          ) -- Swap focused win with next win
    , ((modm .|. shiftMask,     xK_k     ), windows W.swapUp                            ) -- Swap focused win with previous win
    , ((modm,                   xK_h     ), sendMessage Shrink                          ) -- Shrink master area
    , ((modm,                   xK_l     ), sendMessage Expand                          ) -- Expand master area
    , ((modm,                   xK_t     ), withFocused $ windows . W.sink              ) -- Push win into tiling
    , ((modm,                   xK_bracketright), sendMessage (IncMasterN 1)            ) -- Increment num of windows in master area
    , ((modm,                   xK_bracketleft), sendMessage (IncMasterN (-1))          ) -- Deincrement num of windows in master area
    , ((modm,                   xK_comma ), nextScreen                                  ) -- Focus next mon
    , ((modm,                   xK_period), prevScreen                                  ) -- Focus prev mon
        -- Scratchpads
    , ((modm,                   xK_s), namedScratchpadAction myScratchPads "terminal"   ) -- Terminal Scrtchpd
    , ((modm .|. shiftMask,     xK_s), namedScratchpadAction myScratchPads "ncmpcpp"    ) -- Ncmpcpp Scrtchpd
        -- Multimedia (Volume, MPD)
    , ((0,                      0x1008FF11), spawn "pulsemixer --change-volume -2"      ) -- Volume Down 
    , ((0,                      0x1008FF13), spawn "pulsemixer --change-volume +2"      ) -- Volume Up
    , ((0,                      0x1008FF12), spawn "pulsemixer --toggle-mute"           ) -- Mute
    , ((0,                      0x1008FF14), spawn "mpc toggle"                         ) -- Play/Pause
    , ((modm,                   0x1008FF15), spawn "mpc shuffle"                        ) -- Shuffle
    , ((0,                      0x1008FF16), spawn "mpc prev"                           ) -- Prev Track
    , ((0,                      0x1008FF17), spawn "mpc next"                           ) -- Next Track
        -- Brightness
    , ((0,                      0x1008FF02), spawn "xbacklight -inc 5"                  ) -- Inc Brightness
    , ((0,                      0x1008FF03), spawn "xbacklight -dec 5"                  ) -- Dec Brightness

        -- Open Applications
    , ((mod4Mask,                   xK_b      ), spawn myBrowser                                         ) -- Browser
    , ((mod4Mask .|. controlMask,   xK_m      ), spawn (myTerminal ++ "calcurse")                        ) -- Calcurse
    , ((mod4Mask .|. shiftMask,     xK_d      ), spawn "discord"                                         ) -- Discord
    , ((mod4Mask,                   xK_v      ), spawn (myTerminal ++ myFilemngr)                        ) -- File Manager
    , ((mod4Mask,                   xK_a      ), spawn (myTerminal ++ "pulsemixer")                      ) -- Mixer
    , ((mod4Mask .|. shiftMask,     xK_m      ), spawn (myTerminal ++ "ncmpcpp")                         ) -- Ncmpcpp
    , ((mod4Mask,                   xK_w      ), spawn (myTerminal ++ "nmtui")                           ) -- Netork
    , ((mod4Mask,                   xK_p      ), spawn (myTerminal ++ "htop")                            ) -- Processes
    , ((mod4Mask,                   xK_s      ), spawn "~/.config/rofi/scripts/menu_powermenu.sh"        ) -- Processes
        -- Screenshots
    , ((shiftMask .|. controlMask,  xK_Print  ), spawn "flameshot gui -p ~/Pictures/Screenshots"         ) -- Area
    , ((0,                          xK_Print  ), spawn "scrot '~/Pictures/Screenshots/%F_%T.png'"        ) -- Fullscreen
    , ((mod4Mask .|. modm,          xK_Print  ), spawn "flameshot screen -r -c -p ~/Pictures/Screenshots") -- Monitor
    , ((controlMask,                xK_Print  ), spawn "scrot -u '~/Pictures/Screenshots'"               ) -- Window
        -- Grid Select
    , ((modm,                       xK_g      ), goToSelected $ mygridConfig myColorizer ) -- Go to grid item
    , ((modm .|. shiftMask,         xK_g      ), bringSelected $ mygridConfig myColorizer) -- Grab and brind over grid item
    , ((modm .|. controlMask,       xK_g      ), spawnSelected' myAppGrid) -- Custom program list
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings:
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-------------------------------------------------------------------------------
-- Layouts:
-------------------------------------------------------------------------------

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True
-- mySpacing 8
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     tiled   = Tall nmaster delta ratio -- Master/Stack
     nmaster = 1 -- Default master count
     ratio   = 1/2 -- Default size ratio of master:stack size
     delta   = 3/100 -- Percent of screen inc/dec when resizing

-------------------------------------------------------------------------------
-- Window rules:
-------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "lutris"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-------------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

-------------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

-------------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = do
    spawnOnce "feh --bg-scale --no-fehbg ~/Pictures/Backgrounds/forest.png &"
    spawnOnce "flameshot &"
    spawnOnce "picom --experimental-backends &"

-------------------------------------------------------------------------------

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    xmonad $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
        -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
        -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
        -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook,
    startupHook        = myStartupHook
}

--------------------------------------------------------------------------------
-- Grid Select:
--------------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg

mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

myAppGrid :: [(String, String)]
myAppGrid = [ (a,b) | (a,b,c) <- xs]
  where xs = myApplications

myApplications :: [(String, String, String)]
myApplications = [ 
        ("Shadoplan", "sp", "My TODO program")
        , ("Audacity", "audacity", "Music/Audio editor and recorder")
        , ("Krita", "krita", "Advanced art/drawing program")
        , ("Dolphin", "dolphin", "GUI File manager")
        ]
--------------------------------------------------------------------------------
-- Named Scratchpads:
--------------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp
                ]
    where
        spawnTerm    = myTerminal ++ " --name scratchpad"
        findTerm     = resource =? "scratchpad"
        manageTerm   = customFloating $ W.RationalRect l t w h
                       where
                       h = 0.9
                       w = 0.9
                       t = 0.95 -h
                       l = 0.95 -w
        spawnNcmpcpp  = myTerminal ++ " --name ncmpcpp 'ncmpcpp'"
        findNcmpcpp   = resource =? "ncmpcpp"
        manageNcmpcpp = customFloating $ W.RationalRect l t w h
                       where
                       h = 0.9
                       w = 0.9
                       t = 0.95 -h
                       l = 0.95 -w

