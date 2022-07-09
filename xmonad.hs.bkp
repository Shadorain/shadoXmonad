{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, AllowAmbiguousTypes, DeriveDataTypeable, MultiParamTypeClasses, TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- Shadomonad Config --
-------------------------------------------------------------------------------
-- Imports: {{{
-------------------------------------------------------------------------------
    -- Base
import XMonad
import System.Exit
import System.IO (appendFile)
import Control.Arrow (first)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CycleWS (nextWS, prevWS, nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Actions.NoBorders (toggleBorder)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

    -- Data
import Data.Maybe (fromJust)
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsLogHookCustom)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.InsertPosition

    -- Layouts
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.Hidden (hiddenWindows, popOldestHiddenWindow, hideWindow)
import XMonad.Layout.IndependentScreens (countScreens, withScreens, unmarshallS, unmarshallWindowSpace, onCurrentScreen, workspaces')
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Master (mastered)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL,NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.LayoutScreens (layoutScreens, fixedLayout)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect (REFLECTX(..))
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.WindowArranger (windowArrange)

    -- Prompts
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Man (manPrompt)

    -- Utilities
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)
----------------------------------------------------------------------------}}}
-- Variables: {{{
-------------------------------------------------------------------------------
    -- Base
myFont      = "xft:Jura:pixelsize=14" -- Set font
minimalFont = "xft:Jura:pixelsize=1"  -- Set minfont
myBrowser   = "firefox "              -- Set default browser
myFilemngr  = "vifm"                  -- Set default file manager
myTerminal  = "st "                   -- Set default terminal
myEditor    = "nvim"                  -- Set default text editor
mySpacing   = 5                       -- Set gaps between windows
noSpacing   = 0                       -- Set nogaps between windows

    -- Get count of windows in selected workspace
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

    -- Dmenu
myLauncher       = "dmenu_run"               -- Launcher
myLauncherCalc   = "$HOME/.config/scripts/=" -- Dmenu calculator
myDmenuClipMenu  = "clipmenu"                -- Dmenu clipboard prompt

    -- Borders
myBorderWidth = 2
myNormalBorderColor  = "#8897F4"
myFocusedBorderColor = "#ff79c6"

    -- Focus
myFocusFollowsMouse = True
myClickJustFocuses  = False

    -- Mod Masks
myModMask = mod1Mask -- Default Modkey (Alt)
mods      = mod4Mask -- Super Key

    -- Workspaces
m0ws1 = "一" -- " " -- "src" -- Browser
m0ws2 = "二" -- " " -- "wrk" -- Work   
m0ws3 = "三" -- "ﭮ " -- "com" -- Discord
m0ws4 = "四" -- " " -- "prj" -- Projects
m0ws5 = "五" -- " " -- "sys" -- System 
m0ws6 = "六" -- " " -- "dev" -- Dev    
m0ws7 = "七" -- " " -- "hsk" -- Haskell
m0ws8 = "八" -- " " -- "clg" -- C lang 
m0ws9 = "九" -- " " -- "rev" -- Reversing

    -- My Scripts
dateScript = "~/.config/scripts/datenotif"
----------------------------------------------------------------------------}}}
-- Defaults: {{{
-------------------------------------------------------------------------------
myConfig = def {
        -- Base
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
        -- Keys
    keys          = myKeys,
    mouseBindings = myMouseBindings,
        -- Hooks/Layouts
    layoutHook       = myLayoutHook,
    manageHook       = manageDocks
                      <+> myManageHook
                      <+> manageSpawn
                      <+> insertPosition End Newer -- SETS NEW WINDOW POSITION AND FOCUS
                      <+> namedScratchpadManageHook myScratchPads,
    handleEventHook = docksEventHook,
    startupHook     = myStartupHook
                      <+> docksStartupHook
}
-----------------------------------------------------------------------------}}}
-- Themes: {{{
-------------------------------------------------------------------------------
    -- Colors
fg        = "#BFAAE3"
bg        = "#09090d"
bgGray    = "#1B1B29"
bgGray2   = "#2F2F4A"
cGray     = "#073642"
cRed      = "#f0416d"
cPurp     = "#e1acff"
cBlue     = "#2384de"
cPurpBlue = "#9188ff"
cPink     = "#ff79c6"
cMint     = "#23dea9"
cTeal     = "#87b0d6"
cMagenta  = "#d33682"
cViolet   = "#6c71c4"
cSkyBlue  = "#268bd2"
cCyan     = "#2aa198"
cEmpty    = "#1ea69d"
cNormal   = "#37d4a7"
cWarning  = "#c9083f"

active       = cPurpBlue
activeWarn   = cRed
inactive     = bgGray2
focusColor   = cPurpBlue
unfocusColor = cGray

gap = 5
topbar = 5

overLineTheme = def
    { fontName            = myFont
    , inactiveBorderColor = bgGray2
    , inactiveColor       = bgGray2
    , inactiveTextColor   = bgGray2
    , activeBorderColor   = active
    , activeTextColor     = active
    , activeColor         = active
    , urgentBorderColor   = cWarning
    , urgentTextColor     = cPink
    , decoHeight          = topbar
    }

myTabTheme = def 
    { fontName            = minimalFont 
    , activeColor         = active
    , inactiveColor       = inactive
    , activeBorderColor   = active
    , inactiveBorderColor = cViolet
    , activeTextColor     = active
    , inactiveTextColor   = inactive
    , decoHeight          = 12
    }
----------------------------------------------------------------------------}}}
-- Layouts: {{{
-------------------------------------------------------------------------------
myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation        = centerNavigation
    , screenNavigation       = lineNavigation
    , layoutNavigation       = [("Full", centerNavigation)]
    , unmappedWindowRect     = [("Full", singleWindowRect)]
    }

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)
barFull = avoidStruts $ Simplest

myLayoutHook = fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ hiddenWindows
             $ windowArrange
             -- $ fixFocus
             $ shadoLayout ||| fixFocus spanMid -- ||| fixFocus spanFull ||| fixFocus spanMid ||| monocle ||| tiled

  where
    fullScreenToggle = mkToggle (single FULL)
    fullBarToggle    = mkToggle (single FULLBAR)
    mirrorToggle     = mkToggle (single MIRROR)
    reflectToggle    = mkToggle (single REFLECTX)

    nmaster          = 1     -- Default master count
    ratio            = 1/2   -- Default size ratio of master:stack size
    delta            = 3/100 -- Percent of screen inc/dec when resizing

    named n          = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n    = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n       = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                (XMonad.Layout.Renamed.AppendWords n)]

    addOverline      = noFrillsDeco shrinkText overLineTheme
    mySpacing        = spacing gap
    myGaps           = gaps [(U, gap),(D, gap),(L,gap),(R,gap)]

    -- Layouts
    tiled            = named "Tall" $ avoidStruts(Tall nmaster delta ratio) -- Default Master/Stack (No Gaps)
    mirrorTiled      = named "Mirror Tall" $ avoidStruts(Mirror tiled)      -- Default master stack but horizontal (No Gaps)
    monocle          = named "Monocle" $ avoidStruts(fullScreenToggle Full)

    spanFull         = named "Span Full"
        -- $ avoidStruts
        -- $ addOverline
        $ windowNavigation
        $ mySpacing
        $ myGaps
        $ addTabs shrinkText myTabTheme
        $ ThreeCol 1 (1/1000) (2/3)

    spanMid         = named "Span Mid"
        -- $ avoidStruts
        -- $ addOverline
        $ windowNavigation
        $ mySpacing
        $ myGaps
        $ addTabs shrinkText myTabTheme
        $ ThreeColMid 1 (1/1000) (1/3)

    masterTabbed     = named "Master Tabbed"
        $ avoidStruts
        $ addOverline
        $ mySpacing
        $ myGaps
        $ mastered (1/100) (1/2)
        $ tabbed shrinkText myTabTheme

    shadoLayout      = named "Shadolayout"
        -- $ addOverline
        $ avoidStruts
        $ windowNavigation
        $ addTabs shrinkText myTabTheme
        $ subLayout [] (Simplest ||| Accordion)
        $ ifWider 5760 wideLayouts stdLayouts
        where
            stdLayouts = myGaps $ mySpacing
                $ (suffixed "T2" $ ResizableTall 1 (1/100) (1/2) []) |||
                  (suffixed "BSP" $ emptyBSP)
            wideLayouts = myGaps $ mySpacing
                $ (suffixed "W 3C" $ ThreeColMid 1 (1/20) (1/2)) |||
                  (trimSuffixed 1 "W BSP" $ hiddenWindows emptyBSP)
----------------------------------------------------------------------------}}}
-- Window rules: {{{
-------------------------------------------------------------------------------
myManageHook = (composeAll . concat $
    [[ className =? "lutris"                  --> doFloat ]
    ,[ className =? "wall-d"                  --> doFloat ]
    -- ,[ className =? "Sxiv"                    --> doFloat ]
    ,[ title =? "QEMU"                        --> doFloat ]
    ,[ resource  =? "desktop_window"          --> doIgnore ] ])
        -- where
        --     role = stringProperty "WM_WINDOW_ROLE"
        --     doMaster = doF W.shiftMaster
        --     doFloatAt' x y = doFloatAt x y <+> doMaster
        --     doRectFloat' r = doRectFloat r <+> doMaster
        --     doCenterFloat' = doCenterFloat <+> doMaster
        --     keepMaster c = assertSlave <+> assertMaster where
        --         assertSlave = fmap (/= c) className --> doF W.swapDown
        --         assertMaster = className =? c --> doF W.swapMaster
    -- ,[ title     =? "ncmpcpp-ueberzug"        -?> doFloatAt' (46/1680) (1-176/1050) ]
    -- ,[ title     =? "ncmpcpp-ueberzug"        -?> doRectFloat' (W.RationalRect 0.65 0.65 0.3 0.3) ]
----------------------------------------------------------------------------}}}
-- Fix Focus: {{{
-------------------------------------------------------------------------------
data FixFocus a = FixFocus (Maybe a) deriving (Read, Show)
instance LayoutModifier FixFocus Window where
    modifyLayout (FixFocus mlf) ws@(W.Workspace id lay Nothing) r = runLayout ws r
    modifyLayout (FixFocus Nothing) ws r = runLayout ws r
    modifyLayout (FixFocus (Just lf)) (W.Workspace id lay (Just st)) r = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        is_rf_floating <- maybe (return False) (\rf -> withWindowSet $ return . M.member rf . W.floating) mreal_f -- real focused window is floating?
        let new_stack_f = if is_rf_floating then lf else stack_f --if yes: replace stack's focus with our last saved focus
        let new_st' = until (\s -> new_stack_f == W.focus s) W.focusUp' st -- new stack with focused new_stack_f
        let new_st = if (new_stack_f `elem` (W.integrate st)) then new_st' else st -- use it only when it's possible to
        runLayout (W.Workspace id lay (Just new_st)) r

    redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Just $ FixFocus mlf)
    redoLayout (FixFocus mlf) r (Just st) wrs = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        let crf_in_stack = maybe False ((flip elem) (W.integrate st)) mreal_f -- current real focus belongs to stack?
        let new_saved_f = if crf_in_stack then fromJust mreal_f else stack_f -- if yes: replace saved focus
        return (wrs, Just $ FixFocus $ Just new_saved_f)

fixFocus :: LayoutClass l a => l a -> ModifiedLayout FixFocus l a
fixFocus = ModifiedLayout $ FixFocus Nothing
----------------------------------------------------------------------------}}}
-- Autostart: {{{
-------------------------------------------------------------------------------
-- myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    spawn "~/.local/bin/bgrot"
    spawn "libinput-gestures-setup start"
    spawn "killall picom ; picom --experimental-backends &"
    spawn "killall polybar ; polybar -c ~/.config/shadobar/config-xmonad shadobar &"
    spawn "xset r rate 200 30"
    -- spawn "~/.screenlayout/triple.sh"
    spawn "dbus-run-session --exit-with-session xmonad"
    setDefaultCursor xC_left_ptr
----------------------------------------------------------------------------}}}
-- Main: {{{
-------------------------------------------------------------------------------
main :: IO ()
main = do
    nScreens <- countScreens -- Gets current screen count
    xmonad 
        $ withNavigation2DConfig myNav2DConf
        $ ewmh 
        $ myConfig { workspaces = withScreens nScreens [m0ws1,m0ws2,m0ws3,m0ws4,m0ws5,m0ws6,m0ws7,m0ws8,m0ws9], logHook = myLogHook }
----------------------------------------------------------------------------}}}
-- Loghook: {{{
-------------------------------------------------------------------------------
workspacesOn' :: ScreenId -> [WindowSpace] -> [WindowSpace]
workspacesOn' s = filter onScreen where onScreen ws = unmarshallS (W.tag ws) == s

mkLayoutStr :: String -> String -> String
mkLayoutStr colour rep =
  concat ["|%{T2}%{F", colour, "} ", rep, " %{T-}%{F-}|"]

layoutParse :: String -> String
layoutParse s | s == "Float"              = mkLayoutStr "#b789cd" "FLT"
              | s == "Hidden Tall"        = mkLayoutStr "#bd93f9" "T"
              | s == "Hidden Mirror Tall" = mkLayoutStr "#bd93f9" "M(T)"
              | s == "Hidden Monocle"     = mkLayoutStr "#87b0d6" "M"
              | s == "Hidden M Tab"       = mkLayoutStr "#8be9fd" "MT"
              | s == "Fullscreen"         = mkLayoutStr "#b789cd" "F"
              | s == "Hidden Shadolayout" = mkLayoutStr "#6a5acd" "Shado"
              | s == "Hidden Span Full"   = mkLayoutStr "#b789cd" "Span"
              | s == "Hidden Span Mid"    = mkLayoutStr "#b789cd" "Mid"
              | otherwise                 = s -- fallback for changes in C.Layout

logger :: X ()
logger = withWindowSet $ \ws -> do
  let layoutName = layoutParse . description . W.layout . W.workspace $ W.current ws
  io $ System.IO.appendFile ("/home/shadow/.xmonad/xmonad-layout") (layoutName ++ "\n")

    -- ewmhDesktopsLogHookCustom (map unmarshallWindowSpace . workspacesOn' 0 . namedScratchpadFilterOutWorkspace)
myLogHook = do
    ewmhDesktopsLogHookCustom (map unmarshallWindowSpace . workspacesOn' 0 . XMonad.Util.WorkspaceCompare.filterOutWs [scratchpadWorkspaceTag])
    logger
----------------------------------------------------------------------------}}}
-- Grid Select: {{{
--------------------------------------------------------------------------------
myGridTheme :: Window -> Bool -> X (String, String)
myGridTheme = colorRangeFromClassName
                  (0x1B,0x1B,0x29) -- lowest inactive bg
                  (0x1B,0x1B,0x29) -- highest inactive bg
                  (0x2F,0x2F,0x4A) -- active bg
                  (0xBD,0x93,0xF9) -- inactive fg
                  (0xFF,0x79,0xC6) -- active fg

mygridConfig colorizer = (buildDefaultGSConfig myGridTheme)
    { gs_cellheight   = 40
    , gs_cellwidth    = 300
    , gs_cellpadding  = 15
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
-----------------------------------------------------------------------------}}}
-- Search Engines: {{{
--------------------------------------------------------------------------------
archwiki, reddit, cppref :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit   = S.searchEngine "reddit"   "https://www.reddit.com/search?q="
cppref   = S.searchEngine "cppref"   "https://en.cppreference.com/mwiki/index.php?search="
----------------------------------------------------------------------------}}}
-- Xprompt: {{{
--------------------------------------------------------------------------------
shXPConfig :: XPConfig
shXPConfig = def
    { font                = myFont
    , bgColor             = "#0f0f17"
    , fgColor             = "#b4a1f0"
    , bgHLight            = "#0f0f17"
    , fgHLight            = "#ff79c6"
    , borderColor         = "#9188ff"
    , promptBorderWidth   = 0
    , promptKeymap        = shXPKeymap
    , position            = Top
    , height              = 20
    , historySize         = 256
    , historyFilter       = id
    , defaultText         = []
    , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
    , showCompletionOnTab = False
    , searchPredicate     = fuzzyMatch
    , alwaysHighlight     = True
    , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
    }

shXPConfig' :: XPConfig
shXPConfig' = shXPConfig { autoComplete = Nothing }

shXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
shXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) mods)    -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]
-----------------------------------------------------------------------------}}}
-- Named Scratchpads: {{{
-------------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "miniterm" spawnMiniTerm findMiniTerm manageMiniTerm
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp ]
    where
        spawnTerm    = myTerminal ++ " -n scratchpad"
        findTerm     = resource =? "scratchpad"
        manageTerm   = customFloating $ W.RationalRect x y w h
                       where
                           x = 0.2
                           y = 0.2
                           w = 0.6
                           h = 0.6
        spawnMiniTerm  = myTerminal ++ " -n mini_scratch sox -t pulseaudio default ~/Videos/Recordings/`date +%d-%m-%Y_%H-%M-%S`_recording.mp3"
        findMiniTerm   = resource =? "mini_scratch"
        manageMiniTerm = customFloating $ W.RationalRect x y w h
                       where
                           x = 0.4
                           y = 0.4
                           w = 0.2
                           h = 0.2
        spawnNcmpcpp  = myTerminal ++ " -n 'ncmpcpp_scratch' '/home/shadow/.ncmpcpp/ncmpcpp-ueberzug/ncmpcpp-ueberzug'"
        findNcmpcpp   = resource =? "ncmpcpp_scratch"
        manageNcmpcpp = customFloating $ W.RationalRect x y w h
                       where
                           x = 0.2
                           y = 0.3
                           w = 0.6
                           h = 0.4
----------------------------------------------------------------------------}}}
-- Key Bindings: {{{
-------------------------------------------------------------------------------
dirKeys        = ["j","k","h","l"]
arrowKeys        = ["<D>","<U>","<L>","<R>"]
dirs           = [ D,  U,  L,  R ]

zipM  m ks as f   = zipWith (\k d -> (m ++ k, f d  )) ks as
zipM' m ks as f b = zipWith (\k d -> (m ++ k, f d b)) ks as

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- Xmonad -----------------------------------------------------------------------------------------------------------
    [ ((modm .|. controlMask,   xK_q     ), io (exitWith ExitSuccess)                                                       ) -- Quit
    , ((modm,                   xK_q     ), spawn "xmonad --recompile; xmonad --restart"                                    ) -- Restart
    , ((mods.|.controlMask,     xK_F12   ), spawn "~/.config/scripts/switch_gpu"                                            ) -- Switch GPU
    , ((modm,                   xK_F9    ), spawn "killall picom"                                                           ) -- Kill picom
    , ((modm .|. shiftMask,     xK_F9    ), spawn ("picom --experimental-backends &")                                       ) -- Start Picom
        -- Base -------------------------------------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_Return), spawn $ XMonad.terminal conf                                                    ) -- Terminal
    , ((modm,                   xK_p     ), spawn myLauncher                                                                ) -- Dmenu
    , ((modm .|. controlMask,   xK_p     ), spawn myLauncherCalc                                                            ) -- Calculator
    , ((modm,                   xK_b     ), sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle"                      ) -- Toggle Bar
    , ((modm .|. shiftMask,     xK_b     ), spawn "killall polybar"                                                         ) -- Kill polybar
    , ((modm .|. controlMask,   xK_b     ), spawn "killall polybar; polybar -c ~/.config/shadobar/config-xmonad shadobar"   ) -- Restart polybar
    , ((modm,                   xK_o     ), withFocused toggleBorder                                                        ) -- Toggle Borders
    , ((modm .|. controlMask,   xK_n     ), spawn (myTerminal ++ "nmcli dev wifi connect GENEVASTUDENT")                    ) -- Restart network
    , ((modm,              xK_KP_Add     ), spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/pretty.jpg &"       ) -- Set Lofi Background
    , ((modm,              xK_KP_Subtract), spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/forest.png &"       ) -- Set Forest Background
    , ((mods,                   xK_d     ), spawn (dateScript)                                                              ) -- Display date
    , ((modm,                   xK_F7    ), spawn "~/.screenlayout/triple.sh; xmonad --restart"                             ) -- Fix Screens
    , ((mods,                   xK_p     ), spawn "betterlockscreen -l -r 1920x1080 -t 'Welcome back, Shado...'"            ) -- Lock Screen
    , ((mods,                   xK_grave ), spawn "~/.local/bin/bgrot"                                                      ) -- Random Background
        -- Layouts ----------------------------------------------------------------------------------------------------------
    , ((modm,                   xK_t     ), withFocused $ windows . W.sink                                                  ) -- Push win into tiling
    , ((modm .|. shiftMask,     xK_t     ), sendMessage $ Toggle MIRROR                                                     ) -- Toggles Mirror Layout mode
    , ((modm,                   xK_space ), sendMessage NextLayout                                                          ) -- Rotate available layouts
    , ((modm .|. shiftMask,     xK_space ), toSubl NextLayout                                                               )
    , ((modm .|. controlMask,   xK_space ), setLayout $ XMonad.layoutHook conf                                              ) -- Reset layouts on current workspace
    , ((mods,                   xK_space ), layoutScreens 1 (fixedLayout [Rectangle 0 0 5760 1080])                         ) -- Make 3 monitors into 1
    , ((mods .|. shiftMask,     xK_space ), rescreen                                                                        ) -- Fix layoutScreens
        -- Workspaces -------------------------------------------------------------------------------------------------------
    , ((modm .|. controlMask,   xK_Right ), nextWS                                                                          ) -- Cycle Right
    , ((modm .|. controlMask,   xK_Left  ), prevWS                                                                          ) -- Cycle Left
        -- Tabs -------------------------------------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_semicolon ), windows W.swapUp                                                            ) -- Swap tab up
    , ((modm .|. shiftMask,     xK_apostrophe), windows W.swapDown                                                          ) -- Swap tab down
    , ((modm .|. controlMask,   xK_h         ), sendMessage $ pullGroup L                                                   ) -- Pull group from the left
    , ((modm .|. controlMask,   xK_j         ), sendMessage $ pullGroup R                                                   ) -- Pull group from the bottom
    , ((modm .|. controlMask,   xK_k         ), sendMessage $ pullGroup U                                                   ) -- Pull group from the top
    , ((modm .|. controlMask,   xK_l         ), sendMessage $ pullGroup D                                                   ) -- Pull group from the right
    , ((modm,                   xK_z         ), withFocused (sendMessage . UnMerge)                                         ) -- Unmerges focused window from sublayout
    , ((modm .|. shiftMask,     xK_z         ), withFocused (sendMessage . MergeAll)                                        ) -- Merges all windows into sublayout
        -- Windows ----------------------------------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_c     ), kill                                                                            ) -- close window
    , ((modm,                   xK_n     ), refresh                                                                         ) -- Resize viewed windows to the correct size
    , ((modm,                   xK_d     ), withFocused hideWindow                                                          ) -- Hide focused
    , ((modm .|. shiftMask,     xK_d     ), popOldestHiddenWindow                                                           ) -- Show oldest hidden window
    , ((modm,                   xK_Tab   ), windows W.focusDown                                                             ) -- Focus next
    , ((modm,                   xK_j     ), windows W.focusDown                                                             ) -- Focus next
    , ((modm,                   xK_k     ), windows W.focusUp                                                               ) -- Focus prev
    , ((modm,                   xK_m     ), windows W.focusMaster                                                           ) -- Focus master
    , ((modm,                   xK_Return), windows W.swapMaster                                                            ) -- Swap focused win with master
    , ((modm .|. shiftMask,     xK_j     ), windows W.swapDown                                                              ) -- Swap focused win with next win
    , ((modm .|. shiftMask,     xK_k     ), windows W.swapUp                                                                ) -- Swap focused win with previous win
    , ((modm,                   xK_h     ), sendMessage Shrink                                                              ) -- Shrink master area
    , ((modm,                   xK_l     ), sendMessage Expand                                                              ) -- Expand master area
    , ((modm .|. shiftMask,     xK_h     ), sendMessage MirrorShrink                                                        ) -- Shrink vertically
    , ((modm .|. shiftMask,     xK_l     ), sendMessage MirrorExpand                                                        ) -- Expand vertically
    , ((modm,                   xK_bracketright), sendMessage (IncMasterN 1)                                                ) -- Increment num of windows in master area
    , ((modm,                   xK_bracketleft), sendMessage (IncMasterN (-1))                                              ) -- Deincrement num of windows in master area
    , ((modm,                   xK_comma ), nextScreen                                                                      ) -- Focus next mon
    , ((modm,                   xK_period), prevScreen                                                                      ) -- Focus prev mon
      -- Scratchpads --------------------------------------------------------------------------------------------------------
    , ((modm,                   xK_s), namedScratchpadAction myScratchPads "terminal"                                       ) -- Terminal Scrtchpd
    , ((modm .|. shiftMask,     xK_s), namedScratchpadAction myScratchPads "ncmpcpp"                                        ) -- Ncmpcpp Scrtchpd
        -- Multimedia (Volume, MPD) -----------------------------------------------------------------------------------------
    , ((0,                          0x1008FF11), spawn "pulsemixer --change-volume -2"                                      ) -- Volume Down 
    , ((0,                          0x1008FF13), spawn "pulsemixer --change-volume +2"                                      ) -- Volume Up
    , ((0,                          0x1008FF12), spawn "pulsemixer --toggle-mute"                                           ) -- Mute
    , ((0,                          0x1008ffb2), spawn "amixer set Capture toggle"                                          ) -- Mute MIC
    , ((0,                          0x1008FF14), spawn "mpc --host=localhost --port=6601 toggle"                            ) -- Play/Pause
    , ((modm,                       0x1008FF15), spawn "mpc --host=localhost --port=6601 shuffle"                           ) -- Shuffle
    , ((0,                          0x1008FF16), spawn "mpc --host=localhost --port=6601 prev"                              ) -- Prev Track
    , ((0,                          0x1008FF17), spawn "mpc --host=localhost --port=6601 next"                              ) -- Next Track
        -- Brightness
    , ((0,                          0x1008FF02), spawn "xbacklight -inc 2"                                                  ) -- Inc Brightness
    , ((0,                          0x1008FF03), spawn "xbacklight -dec 2"                                                  ) -- Dec Brightness
        -- Tomb -------------------------------------------------------------------------------------------------------------
    , ((mods,                       xK_t), spawn (myTerminal ++ "tomb open ~/dev/data -k ~/dev/.non -f")                    ) -- Open tomb
    , ((mods .|. shiftMask,         xK_t), spawn (myTerminal ++ "sudo tomb close")                                          ) -- Close tomb
    , ((mods .|. controlMask,       xK_t), spawn (myTerminal ++ "sudo tomb slam")                                           ) -- Tomb Slam!
        -- Open Applications ------------------------------------------------------------------------------------------------
    , ((modm,                       xK_KP_Multiply), spawn "wall-d ~/Pictures/Backgrounds"                                  ) -- Wall-d
    , ((mods .|. shiftMask,         xK_s      ), spawn "powermenu"                                                          ) -- Powermenu
    , ((mods,                       xK_b      ), spawn (myBrowser)                                                          ) -- Browser
    , ((mods .|. shiftMask,         xK_d      ), spawn ("~/.local/bin/Discord")                                             ) -- Discord
    , ((mods .|. controlMask,       xK_d      ), spawn "killall Discord"                                                    ) -- Kill Discord
    , ((mods,                       xK_k      ), spawn ("kdeconnect-sms --style 'kvantum'")                                 ) -- KDEConnect SMS
    , ((mods,                       xK_e      ), spawn ("emacsclient -c")                                                   ) -- Emacsclient
    , ((mods .|. shiftMask,         xK_e      ), spawn ("emacs")                                                            ) -- Emacs
    , ((mods .|. controlMask,       xK_e      ), spawn (myTerminal ++ "emacs -nw")                                          ) -- Emacs NW
    , ((mods,                       xK_g      ), spawn ("ghidra")                                                           ) -- Ghidra
        -- Screenshots ------------------------------------------------------------------------------------------------------
    , ((shiftMask .|. controlMask,  xK_Print  ), spawn ("flameshot gui -p ~/Pictures/Screenshots")                          ) -- Area
    , ((0,                          xK_Print  ), spawn "scrot '~/Pictures/Screenshots/%F_%T.png'"                           ) -- Fullscreen
    , ((mods .|. modm,              xK_Print  ), spawn "flameshot screen -r -c -p ~/Pictures/Screenshots"                   ) -- Monitor
    , ((controlMask,                xK_Print  ), spawn "scrot -u '~/Pictures/Screenshots'"                                  ) -- Window
    , ((modm .|. controlMask,       xK_Print  ), spawn "~/.config/scripts/imgurup"                                          ) -- Imgur
        -- Grid Select ------------------------------------------------------------------------------------------------------
    , ((modm,                       xK_g      ), goToSelected $ mygridConfig myGridTheme                                    ) -- Go to grid item
    , ((modm .|. shiftMask,         xK_g      ), bringSelected $ mygridConfig myGridTheme                                   ) -- Grab and brind over grid item
        -- Search Engine ----------------------------------------------------------------------------------------------------
    , ((modm,                      xK_slash   ), SM.submap $ searchEngineMap $ S.promptSearch shXPConfig'                   ) -- Searches via prompt
    , ((modm .|. shiftMask,        xK_slash   ), SM.submap $ searchEngineMap $ S.selectSearch                               ) -- Searches via clipboard
        -- Record Button ----------------------------------------------------------------------------------------------------
    , ((mods,                      xK_r), namedScratchpadAction myScratchPads "miniterm"                                    ) -- Ncmpcpp Scrtchpd
        -- Prompts ----------------------------------------------------------------------------------------------------------
    , ((mods,                      xK_v       ), spawn "VBoxManage startvm Wraith_Arch"                                     ) -- Open Black_Arch VM
    , ((mods .|. shiftMask,        xK_v       ), spawn "VBoxManage startvm Win10Burner"                                     ) -- Open Win10 VM
    , ((mods,                      xK_x       ), SM.submap $ promptMap                                                      ) -- Prompts submap
    -------------------------------------------------------------------------------------------------------------------------
    ]
    ++
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    where
        searchEngineMap method = M.fromList $
             [ ((0, xK_a), method archwiki)
             , ((0, xK_c), method cppref)
             , ((0, xK_d), method S.duckduckgo)
             , ((0, xK_g), method S.google)
             , ((0, xK_h), method S.hoogle)
             , ((0, xK_i), method S.images)
             , ((0, xK_r), method reddit)
             , ((0, xK_s), method S.stackage)
             , ((0, xK_t), method S.thesaurus)
             , ((0, xK_w), method S.wikipedia)
             , ((0, xK_y), method S.youtube)
             , ((0, xK_z), method S.amazon)
             ]
        promptMap = M.fromList $
            [ ((0, xK_m), manPrompt shXPConfig), ((0, xK_c), spawn myDmenuClipMenu) ]
---------------------------------------------------------------------}}}
-- Mouse bindings: {{{
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- mod-button1, Set the window to floating mode and move by dragging
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- mod-button2, Raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) -- mod-button3, Set the window to floating mode and resize by dragging
    ] -- you may also bind events to the mouse scroll wheel (button4 and button5)
---------------------------------------------------------------------}}}
-------------------------------------------------------------------------------
