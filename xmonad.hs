{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, AllowAmbiguousTypes, DeriveDataTypeable, MultiParamTypeClasses, TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- Shadomonad Config --
-------------------------------------------------------------------------------
-- TODO: 
  -- [o] have alot more layouts and sublayout abilities
  -- [x] spanning layout
-------------------------------------------------------------------------------
-- Imports: {{{
-------------------------------------------------------------------------------
    -- Base
import XMonad
import Data.Monoid
import System.Exit
import System.IO
import Control.Monad (forM_, join)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CycleWS  --(moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
-- import qualified XMonad.Actions.ConditionalKeys  as CK
-- import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicProjects
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import qualified XMonad.Actions.Submap           as SM
-- import qualified XMonad.Actions.TreeSelect       as TS
import qualified XMonad.Actions.Search as S

    -- Data
import Data.List
import Data.Maybe
-- import Data.Ratio ((%))
-- import Data.Tree
-- import Data.Function (on)
import qualified Data.Map        as M
-- import qualified Data.Tuple.Extra as TE
-- import qualified Codec.Binary.UTF8.String as UTF8

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition

    -- Layouts
import XMonad hiding ( (|||) )
-- import qualified XMonad.Layout.LayoutCombinators as LC
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
-- import XMonad.Layout.BoringWindows
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizeScreen
-- import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layout Mods
-- import XMonad.Layout.Decoration
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.IndependentScreens
-- import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Master
import XMonad.Layout.MultiToggle --(mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL,NBFULL, MIRROR, NOBORDERS))
-- import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutScreens
import XMonad.Layout.PerScreen
-- import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
-- import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
-- import XMonad.Prompt.Layout
import XMonad.Prompt.Man
-- import XMonad.Prompt.Shell (shellPrompt)
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.Cursor
-- import XMonad.Util.CustomKeys
import XMonad.Util.Loggers
-- import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
-- import XMonad.Util.NamedWindows (getName)
-- import XMonad.Util.Scratchpad
-- import XMonad.Util.Run
-- import XMonad.Util.SpawnOnce
-- import XMonad.Util.WorkspaceCompare
----------------------------------------------------------------------------}}}
-- Variables: {{{
-------------------------------------------------------------------------------
    -- Base
myBrowser       = "firefox " -- Set default browser
myFilemngr      = "vifmrun" -- Set default file manager
myFont          = "xft:Agave:pixelsize=14" -- Set font
mySpacing       :: Int
mySpacing       = 5 -- Set gaps between windows
noSpacing       :: Int
noSpacing       = 0 -- Set nogaps between windows
-- myTerminal      = "kitty " -- Set default terminal
myTerminal      = "st " -- Set default terminal
myEditor        = "nvim" -- Set default text editor
windowCount :: X (Maybe String)
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset -- Get count of windows in selected workspace

    -- Dmenu
-- myDmenuFlags     = " -fn 'Agave:size=12' -nb '#1B1B29' -nf '#8897F4' -sb '#2F2F4A' -sf '#ff79c6'" -- Color flags for dmenu
myJail           = "firejail --seccomp --nonewprivs --private-tmp "
myLauncher       = myJail ++ "dmenu_run"-- Set main launcher
myLauncherCalc   = myJail ++ "$HOME/.config/scripts/="-- Set main launcher
myDmenuWebSearch = myJail ++ "$HOME/.config/scripts/dmenu_websearch" -- Dmenu web search prompt
myDmenuTodo      = myJail ++ "$HOME/.config/scripts/shadotask" -- Dmenu todo prompt
-- myDmenuTodo      = "$HOME/.config/scripts/todo" -- Dmenu todo prompt
myDmenuClipMenu  = myJail ++ "clipmenu" -- Dmenu todo prompt
myDmenuMPDMenu   = myJail ++ "$HOME/.config/scripts/mpdmenu" -- Dmenu todo prompt

    -- Borders
myBorderWidth   = 0
myNormalBorderColor  = "#8897F4"
myFocusedBorderColor = "#9188ff"

    -- Focus
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True -- Whether focus follows the mouse pointer.
myClickJustFocuses :: Bool
myClickJustFocuses = False -- Whether clicking on a window to focus also passes the click to the window

    -- Mod Masks
myModMask       = mod1Mask -- Default Modkey (Alt)
mods            = mod4Mask -- Super Key
modb            = mod4Mask -- Backslash Key: TODO 
modt            = mod4Mask -- Tab Key: TODO

    -- Workspaces
-- Mon 1
m0ws1 = "一" -- m1ws1 = "src" -- " " -- Browser
m0ws2 = "二" -- m1ws2 = "wrk" -- " " -- Work
m0ws3 = "三" -- m1ws3 = "com" -- "ﭮ " -- Discord
m0ws4 = "四" -- m1ws4 = "prj" -- " " -- Projects
m0ws5 = "五" -- m1ws5 = "sys" -- " " -- System
m0ws6 = "六" -- m1ws6 = "dev" -- " " -- Dev
m0ws7 = "七" -- m1ws7 = "hsk" -- " " -- Haskell
m0ws8 = "八" -- m1ws8 = "clg" -- " " -- C lang
m0ws9 = "九" -- m1ws9 = "rev" -- " " -- Reversing

    -- My Scripts
dateScript = "~/.config/scripts/datenotif"
bigO = "sxiv -o -r -b -g 1000x700+450+200 ~/Pictures/bigocheatsheet.png &"
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
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
        -- Hooks/Layouts
    layoutHook         = myLayoutHook,
    manageHook         = manageDocks
                        <+> myManageHook
                        <+> manageSpawn
                        <+> insertPosition End Newer -- SETS NEW WINDOW POSITION AND FOCUS
                        <+> namedScratchpadManageHook myScratchPads,
                        -- <+> manageHook def,
    handleEventHook    = docksEventHook
                        <+> minimizeEventHook,
                        -- <+> fullscreenEventHook,
    startupHook        = myStartupHook
}
                        -- <+> (scratchpadManageHook $ W.RationalRect 0 0 1 (3/4))
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
    { fontName            = myFont 
    , activeColor         = active
    , inactiveColor       = inactive
    , activeBorderColor   = active
    , inactiveBorderColor = cViolet
    , activeTextColor     = active
    , inactiveTextColor   = inactive
    }

----------------------------------------------------------------------------}}}
-- Layouts: {{{
-------------------------------------------------------------------------------
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)
barFull = avoidStruts $ Simplest

-- ||| masterTabbed
myLayoutHook = fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ hiddenWindows
             $ windowArrange
             -- $ fixFocus
             $ shadoLayout ||| fixFocus spanFull ||| fixFocus spanMid ||| monocle ||| tiled

  where
    fullScreenToggle = mkToggle (single FULL)
    fullBarToggle    = mkToggle (single FULLBAR)
    mirrorToggle     = mkToggle (single MIRROR)
    reflectToggle    = mkToggle (single REFLECTX)

    nmaster          = 1 -- Default master count
    ratio            = 1/2 -- Default size ratio of master:stack size
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
    tiled            = named "Tall" $ avoidStruts(Tall nmaster delta ratio)  -- Default Master/Stack (No Gaps)
    mirrorTiled      = named "Mirror Tall" $ avoidStruts(Mirror tiled) -- Default master stack but horizontal (No Gaps)
    monocle          = named "Monocle" $ avoidStruts(fullScreenToggle Full)

    spanFull         = named "Span Full"
        -- $ avoidStruts
        $ addOverline
        $ windowNavigation
        $ mySpacing
        $ myGaps
        $ addTabs shrinkText myTabTheme
        $ ThreeCol 1 (1/1000) (2/3)

    spanMid         = named "Span Mid"
        -- $ avoidStruts
        $ addOverline
        $ windowNavigation
        $ mySpacing
        $ myGaps
        $ addTabs shrinkText myTabTheme
        $ ThreeColMid 1 (1/1000) (2/3)

    masterTabbed     = named "Master Tabbed"
        -- $ avoidStruts
        $ addOverline
        $ mySpacing
        $ myGaps
        $ mastered (1/100) (1/2)
        $ tabbed shrinkText myTabTheme

    shadoLayout      = named "Shadolayout"
        -- $ avoidStruts
        $ windowNavigation
        $ addOverline
        $ addTabs shrinkText myTabTheme
        $ subLayout [] (Simplest ||| Accordion)
        $ ifWider 5760 wideLayouts stdLayouts
        where
            stdLayouts = myGaps $ mySpacing
                $ (suffixed "T2 |" $ ResizableTall 1 (1/100) (1/2) []) |||
                  (suffixed "BSP |" $ emptyBSP)
            wideLayouts = myGaps $ mySpacing
                $ (suffixed "W 3C |" $ ThreeColMid 1 (1/20) (1/2)) |||
                  (trimSuffixed 1 "W BSP |" $ hiddenWindows emptyBSP)
----------------------------------------------------------------------------}}}
-- Window rules: {{{
-------------------------------------------------------------------------------
myManageHook = (composeAll . concat $
    [[ className =? "lutris"                  --> doFloat ]
    ,[ className =? "wall-d"                  --> doFloat ]
    -- ,[ className =? "Sxiv"                    --> doFloat ]
    ,[ title =? "QEMU"                    --> doFloat ]
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
    spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/forest.png &"
    spawn "flashfocus &"
    spawn "killall picom; picom --experimental-backends &"
    spawn "/usr/bin/emacs --daemon &"
    spawn "killall polybar; polybar -c ~/.config/shadobar/config-xmonad shadobar" -- 2>~/.config/shadobar/log"
    spawn "xset r rate 200 30"
    spawn "dbus-run-session --exit-with-session xmonad"
    -- spawn "sleep 1; killall stalonetray; stalonetray &"
    -- spawn "sleep 1; killall nm-applet; nm-applet &"
    spawn "ps -ef | grep hideIt | grep -v grep | awk '{print $2}' | xargs kill; setsid /home/shadow/.config/shadobar/scripts/hideIt.sh --region 1920x0+1920+30 --peek -2 --name '^polybar-shadobar_DP-2$' &" -- Polybar hoverhider
    setDefaultCursor xC_left_ptr
----------------------------------------------------------------------------}}}
-- Main: {{{
-------------------------------------------------------------------------------
main :: IO ()
main = do
    nScreens <- countScreens -- Gets current screen count

    xmonad 
        $ fullscreenSupport
        $ withNavigation2DConfig myNav2DConf
        $ withUrgencyHook NoUrgencyHook 
        -- $ dynamicProjects projects
        $ ewmh 
        $ myConfig { workspaces = withScreens nScreens [m0ws1,m0ws2,m0ws3,m0ws4,m0ws5,m0ws6,m0ws7,m0ws8,m0ws9], logHook = myLogHook }
----------------------------------------------------------------------------}}}
-- Loghook: {{{
-------------------------------------------------------------------------------
workspacesOn :: ScreenId -> [WindowSpace] -> [WindowSpace]
workspacesOn s = filter onScreen where onScreen ws = unmarshallS (W.tag ws) == s

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

myLogHook = do
    ewmhDesktopsLogHookCustom (map unmarshallWindowSpace . workspacesOn 0 . namedScratchpadFilterOutWorkspace)
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
    where conf = defaultGSConfig
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
    -- , bgColor             = "#1B1B29"
    -- , fgColor             = "#BFAAE3"
    -- , bgHLight            = cPurpBlue
    -- , fgHLight            = "#000000"
    -- , borderColor         = "#9188ff"
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
---------------------------------------------------------------------}}}
-- Xprompt Keymap: {{{
------------------------------------------------------------------------
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
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp
                , NS "nvim" spawnNvim findNvim manageNvim ]
    where
        spawnTerm    = myTerminal ++ " -n scratchpad"
        findTerm     = resource =? "scratchpad"
        manageTerm   = customFloating $ W.RationalRect x y w h
                       where
                       x = 0.2
                       y = 0.2
                       w = 0.6
                       h = 0.6
        spawnNcmpcpp  = myTerminal ++ " -n 'ncmpcpp_scratch' '/home/shadow/.ncmpcpp/ncmpcpp-ueberzug/ncmpcpp-ueberzug'"
        findNcmpcpp   = resource =? "ncmpcpp_scratch"
        manageNcmpcpp = customFloating $ W.RationalRect x y w h
                       where
                       x = 0.2
                       y = 0.3
                       w = 0.6
                       h = 0.4
        spawnNvim    = "xdotool key 'alt+w';" ++ myTerminal ++ " -n scratchvim"
        findNvim     = resource =? "scratchvim"
        manageNvim   = customFloating $ W.RationalRect x y w h
                       where
                       x = 0.005
                       y = 0.01
                       w = 2.99
                       h = 0.982
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
        -- Xmonad ---------------------------------------------------------------------------------
    [ ((modm .|. controlMask,   xK_q     ), io (exitWith ExitSuccess)                   ) -- Quit
    , ((modm,                   xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ((mods.|.controlMask,     xK_F12   ), spawn "~/.config/scripts/switch_gpu"        ) -- Switch GPU
    , ((modm,                   xK_F9    ), spawn "killall picom"                       ) -- Kill picom
    , ((modm .|. shiftMask,     xK_F9    ), spawn myJail ++ "picom --experimental-backends &"     ) -- Start Picom
    , ((modm .|. shiftMask,     xK_x     ), shiftToProjectPrompt shXPConfig             ) -- Project Prompt
   -- , ((modm .|. shiftMask,     xK_x     ), switchProjectPrompt shXPConfig              ) -- Project Prompt
        -- Session --------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_m     ), spawn "lwsm save"   ) -- Save Session
    , ((modm .|. controlMask,   xK_m     ), spawn "lwsm restore") -- Restore session
        -- Base -----------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_Return), spawn $ myJail ++ XMonad.terminal conf                                                    ) -- Terminal
    , ((modm,                   xK_p     ), spawn myLauncher                                                                ) -- Dmenu
    , ((modm .|. controlMask,   xK_p     ), spawn myLauncherCalc                                                            ) -- Calculator
    , ((modm,                   xK_b     ), sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle"                      ) -- Toggle Bar
    , ((modm .|. shiftMask,     xK_b     ), sendMessage $ (MT.Toggle NOBORDERS)                                             ) -- Toggle Borders
    -- , ((modm .|. controlMask,   xK_n     ), spawn (myTerminal ++ "nmcli dev wifi connect GENEVASTUDENT")                    ) -- Restart net GENEVASTUDENT
    , ((modm .|. controlMask,   xK_n     ), spawn (myJail ++ myTerminal ++ "nmcli dev wifi connect 'ionit 2.4'")                    ) -- Restart net
    , ((modm,     xK_KP_Add     ), spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/pretty.jpg &"                ) -- Set Lofi Background
    , ((modm,     xK_KP_Subtract), spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/forest.png &"                ) -- Set Forest Background
    , ((mods,                   xK_d     ), spawn dateScript                                                                ) -- Display date
    , ((modm,                   xK_o     ), spawn bigO                                                                      ) -- BigOcheatsheet
    , ((modm,                   xK_F7    ), spawn "~/.screenlayout/scs.sh; xmonad --restart"                                                  ) -- Fix Screens
    -- , ((modm,                   xK_F7    ), spawn "~/.screenlayout/home.sh; xmonad --restart"                                                  ) -- Fix Screens
    -- , ((modm,                   xK_F7    ), spawn "~/.screenlayout/triple.sh; xmonad --restart"                                                  ) -- Fix Screens
    , ((modm .|. controlMask,   xK_b     ), spawn "killall polybar; polybar -c ~/.config/shadobar/config-xmonad shadobar"   ) -- Restart polybar
    -- , ((mods,                   xK_p     ), spawn "betterlockscreen -l blur -r 1920x1080 -b 0.2 -t 'Welcome back, Shado...'") -- Lock Screen
    , ((mods,                   xK_p     ), spawn myJail ++ "betterlockscreen -l -r 1920x1080 -t 'Welcome back, Shado...'") -- Lock Screen
        -- Layouts --------------------------------------------------------------------------------
    , ((modm,                   xK_t     ), withFocused $ windows . W.sink              ) -- Push win into tiling
    , ((modm .|. shiftMask,     xK_t     ), sendMessage $ Toggle MIRROR                 ) -- Toggles Mirror Layout mode
    -- , ((modm,                   xK_space ), sequence_ [sendMessage NextLayout, rescreen]) -- Rotate available layouts
    , ((modm,                   xK_space ), sendMessage NextLayout                      ) -- Rotate available layouts
    , ((modm .|. shiftMask,     xK_space ), toSubl NextLayout                           )
    , ((modm .|. controlMask,   xK_space ), setLayout $ XMonad.layoutHook conf          ) -- Reset layouts on current workspace
    , ((mods,                   xK_space ), layoutScreens 1 (fixedLayout [Rectangle 0 0 5760 1080])) -- Make 3 monitors into 1
    , ((mods .|. shiftMask,     xK_space ), rescreen                                    ) -- Fix layoutScreens
        -- Workspaces -----------------------------------------------------------------------------
    , ((modm .|. controlMask,   xK_Right ), nextWS                                      ) -- Cycle Right
    , ((modm .|. controlMask,   xK_Left  ), prevWS                                      ) -- Cycle Left
        -- Tabs -----------------------------------------------------------------------------------
    -- , ((modm,                   xK_semicolon ), CK.bindOn CK.LD [("M Tab", windows W.focusUp),  ("", onGroup W.focusUp')]  ) -- Focus next tab up
    -- , ((modm,                   xK_apostrophe), CK.bindOn CK.LD [("M Tab", windows W.focusDown),("", onGroup W.focusDown')]) -- Focus next tab down
    , ((modm .|. shiftMask,     xK_semicolon ), windows W.swapUp                        ) -- Swap tab up
    , ((modm .|. shiftMask,     xK_apostrophe), windows W.swapDown                      ) -- Swap tab down
    , ((modm .|. controlMask,   xK_h         ), sendMessage $ pullGroup L               ) -- Pull group from the left
    , ((modm .|. controlMask,   xK_j         ), sendMessage $ pullGroup R               ) -- Pull group from the bottom
    , ((modm .|. controlMask,   xK_k         ), sendMessage $ pullGroup U               ) -- Pull group from the top
    , ((modm .|. controlMask,   xK_l         ), sendMessage $ pullGroup D               ) -- Pull group from the right
    , ((modm,                   xK_z         ), withFocused (sendMessage . UnMerge)     ) -- Unmerges focused window from sublayout
    , ((modm .|. shiftMask,     xK_z         ), withFocused (sendMessage . MergeAll)    ) -- Merges all windows into sublayout
        -- BSP --------------------------------------------------------------------------------
    -- , ((modm,    xK_semicolon ), CK.bindOn CK.LD [("Shadolayout", windows W.focusUp),  ("", onGroup W.focusUp')]  ) -- Change size BSP
    -- , ((modm,    xK_apostrophe), CK.bindOn CK.LD [("Shadolayout", windows W.focusDown),("", onGroup W.focusDown')]) -- Change size BSP
    -- , ((modm,                 xK_h     ), CK.bindOn CK.LD [("Shadolayout", sendMessage $ ExpandTowards L)])
    -- , ((modm,                 xK_j     ), sendMessage $ ExpandTowards D)
    -- , ((modm,                 xK_k     ), sendMessage $ ExpandTowards U)
    -- , ((modm,                 xK_k     ), sendMessage $ ExpandTowards R)
    -- , ((modm .|. shiftMask,   xK_h     ), sendMessage $ ShrinkFrom L   )
    -- , ((modm .|. shiftMask,   xK_j     ), sendMessage $ ShrinkFrom D   )
    -- , ((modm .|. shiftMask,   xK_k     ), sendMessage $ ShrinkFrom U   )
    -- , ((modm .|. shiftMask,   xK_l     ), sendMessage $ ShrinkFrom R   )
        -- Windows --------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_c     ), kill                                        ) -- close window
    , ((modm,                   xK_n     ), refresh                                     ) -- Resize viewed windows to the correct size
    , ((modm,                   xK_d     ), withFocused hideWindow                      ) -- Hide focused
    , ((modm .|. shiftMask,     xK_d     ), popOldestHiddenWindow                       ) -- Show oldest hidden window
    -- , ((modm,                   xK_a     ), toggleCopyToAll                             ) -- Sticky Window
    , ((modm,                   xK_Tab   ), windows W.focusDown                         ) -- Focus next
    , ((modm,                   xK_j     ), windows W.focusDown                         ) -- Focus next
    , ((modm,                   xK_k     ), windows W.focusUp                           ) -- Focus prev
    -- , ((modm,                   xK_m     ), windows W.focusMaster                       ) -- Focus master
    , ((modm,                   xK_Return), windows W.swapMaster                        ) -- Swap focused win with master
    , ((modm .|. shiftMask,     xK_j     ), windows W.swapDown                          ) -- Swap focused win with next win
    , ((modm .|. shiftMask,     xK_k     ), windows W.swapUp                            ) -- Swap focused win with previous win
    , ((modm,                   xK_h     ), sendMessage Shrink                          ) -- Shrink master area
    , ((modm,                   xK_l     ), sendMessage Expand                          ) -- Expand master area
    , ((modm .|. shiftMask,     xK_h     ), sendMessage MirrorShrink                    ) -- Shrink vertically
    , ((modm .|. shiftMask,     xK_l     ), sendMessage MirrorExpand                    ) -- Expand vertically
    , ((modm,                   xK_bracketright), sendMessage (IncMasterN 1)            ) -- Increment num of windows in master area
    , ((modm,                   xK_bracketleft), sendMessage (IncMasterN (-1))          ) -- Deincrement num of windows in master area
    , ((modm,                   xK_comma ), nextScreen                                  ) -- Focus next mon
    , ((modm,                   xK_period), prevScreen                                  ) -- Focus prev mon
      -- Scratchpads ----------------------------------------------------------------------------
    , ((modm,                   xK_s), namedScratchpadAction myScratchPads "terminal"   ) -- Terminal Scrtchpd
    , ((modm .|. shiftMask,     xK_s), namedScratchpadAction myScratchPads "ncmpcpp"    ) -- Ncmpcpp Scrtchpd
    , ((modm,                   xK_v), namedScratchpadAction myScratchPads "nvim"       ) -- Ncmpcpp Scrtchpd
        -- Multimedia (Volume, MPD) ---------------------------------------------------------------------
    , ((0,                          0x1008FF11), spawn "pulsemixer --change-volume -2"                   ) -- Volume Down 
    , ((0,                          0x1008FF13), spawn "pulsemixer --change-volume +2"                   ) -- Volume Up
    , ((0,                          0x1008FF12), spawn "pulsemixer --toggle-mute"                        ) -- Mute
    , ((0,                          0x1008FF14), spawn "mpc --host=localhost --port=6601 toggle"                                      ) -- Play/Pause
    , ((modm,                       0x1008FF15), spawn "mpc --host=localhost --port=6601 shuffle"                                     ) -- Shuffle
    , ((0,                          0x1008FF16), spawn "mpc --host=localhost --port=6601 prev"                                        ) -- Prev Track
    , ((0,                          0x1008FF17), spawn "mpc --host=localhost --port=6601 next"                                        ) -- Next Track
        -- Brightness
    , ((0,                          0x1008FF02), spawn "xbacklight -inc 2"                               ) -- Inc Brightness
    , ((0,                          0x1008FF03), spawn "xbacklight -dec 2"                               ) -- Dec Brightness
        -- Youtube Download ------------------------------------------------------------------------------
    , ((modm .|. shiftMask,         xK_y      ), spawn "ytdl"                                            ) -- Yt->Mpv script
        -- Open Applications -----------------------------------------------------------------------------
    , ((modm,                       xK_KP_Multiply), spawn "wall-d ~/Pictures/Backgrounds"               ) -- Wall-d
    , ((mods,                       xK_b      ), spawn myJail ++ myBrowser                                         ) -- Browser
    , ((mods .|. controlMask,       xK_m      ), spawn myJail ++ (myTerminal ++ "calcurse")                        ) -- Calcurse
    , ((mods .|. shiftMask,         xK_d      ), spawn myJail ++ "Discord"                                         ) -- Discord
    , ((mods .|. controlMask,       xK_d      ), spawn "killall Discord"                                 ) -- Kill Discord
    , ((mods,                       xK_k      ), spawn myJail ++ "kdeconnect-sms --style 'kvantum'"                ) -- KDEConnect SMS
    -- , ((mods .|. shiftMask,         xK_v      ), spawn (myTerminal ++ myFilemngr)                        ) -- File Manager
    -- , ((mods .|. shiftMask,         xK_a      ), spawn (myTerminal ++ "pulsemixer")                      ) -- Mixer
    -- , ((mods .|. shiftMask,         xK_m      ), spawn (myTerminal ++ "ncmpcpp")                         ) -- Ncmpcpp
    -- , ((mods .|. shiftMask,         xK_w      ), spawn (myTerminal ++ "nmtui")                           ) -- Netork
    -- , ((mods .|. shiftMask,         xK_h      ), spawn (myTerminal ++ "htop")                            ) -- Processes
    -- , ((mods .|. shiftMask,         xK_s      ), spawn "~/.config/rofi/scripts/menu_powermenu.sh"        ) -- Powermenu
    , ((mods,                       xK_e      ), spawn myJail ++ "emacsclient -c"                                  ) -- Emacsclient
    , ((mods .|. shiftMask,         xK_e      ), spawn myJail ++ "emacs"                                           ) -- Emacs
    , ((mods .|. controlMask,       xK_e      ), spawn myJail ++ (myTerminal ++ "emacs -nw")                       ) -- Emacs NW
    , ((mods,                       xK_g      ), spawn myJail ++ "ghidra"                                          ) -- Ghidra
        -- Screenshots -----------------------------------------------------------------------------------
    , ((shiftMask .|. controlMask,  xK_Print  ), spawn "flameshot gui -p ~/Pictures/Screenshots"         ) -- Area
    , ((0,                          xK_Print  ), spawn "scrot '~/Pictures/Screenshots/%F_%T.png'"        ) -- Fullscreen
    , ((mods .|. modm,              xK_Print  ), spawn "flameshot screen -r -c -p ~/Pictures/Screenshots") -- Monitor
    , ((controlMask,                xK_Print  ), spawn "scrot -u '~/Pictures/Screenshots'"               ) -- Window
    , ((modm .|. controlMask,       xK_Print  ), spawn "~/.config/scripts/imgurup"                       ) -- Imgur
        -- Grid Select -----------------------------------------------------------------------------------
    , ((modm,                       xK_g      ), goToSelected $ mygridConfig myGridTheme                 ) -- Go to grid item
    , ((modm .|. shiftMask,         xK_g      ), bringSelected $ mygridConfig myGridTheme                ) -- Grab and brind over grid item
        -- Search Engine ---------------------------------------------------------------------------------
    , ((modm,                      xK_slash   ), SM.submap $ searchEngineMap $ S.promptSearch shXPConfig') -- Searches via prompt
    , ((modm .|. shiftMask,        xK_slash   ), SM.submap $ searchEngineMap $ S.selectSearch            ) -- Searches via clipboard
        -- Prompts ---------------------------------------------------------------------------------------
    , ((modm,                      xK_m       ), spawn myDmenuMPDMenu                                          ) -- MPD Menu
    , ((mods,                      xK_x       ), SM.submap $ promptMap                                   ) -- Prompts submap
    ]
    ++
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f)) --mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3 mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
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
            [ ((0, xK_m), manPrompt shXPConfig)
            , ((0, xK_s), spawn myDmenuWebSearch)
            , ((0, xK_t), spawn myDmenuTodo)
            , ((0, xK_c), spawn myDmenuClipMenu)
            ]
        toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                                                        [] -> windows copyToAll
                                                        _  -> killAllOtherCopies
---------------------------------------------------------------------}}}
-- Mouse bindings: {{{
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- mod-button1, Set the window to floating mode and move by dragging
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- mod-button2, Raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) -- mod-button3, Set the window to floating mode and resize by dragging
    ] -- you may also bind events to the mouse scroll wheel (button4 and button5)
---------------------------------------------------------------------}}}
-- Comments: {{{
------------------------------------------------------------------------
-- mkLayoutStr :: String -> String -> String -> String
-- mkLayoutStr colour logo rep =
--   concat ["%{T2}%{F", colour, "} ", logo, "%{T-}%{F", "#f55966", "} ", rep]


-- layoutParse :: String -> String
-- layoutParse s | s == "Float"              = mkLayoutStr "#b789cd" "+++ " "FLT "
--               | s == "Hidden Tall"        = mkLayoutStr "#6a5acd" "||+ " "| T |"
--               | s == "Hidden Mirror Tall" = mkLayoutStr "#6a5acd" "||| " "| M(T) |"
--               | s == "Hidden Monocle"     = mkLayoutStr "#87b0d6" "||| " "| M |"
--               | s == "Hidden M Tab"       = mkLayoutStr "#8be9fd" "___ " "| MT |"
--               | s == "Fullscreen"         = mkLayoutStr "#bd93f9" "| | " "| F |"
--               | s == "Hidden Shadolayout" = mkLayoutStr "#b789cd" "| | " "| Shado |"
--               | otherwise               = s -- fallback for changes in C.Layout

    -- , ((modm,                   xK_space ), sendMessage NextLayout >> (dynamicLogString def >>= \d->spawn $"echo "++d++" > ~/.xmonad/.xmonad-layout-log"))
    -- , ((modm .|. shiftMask,     xK_space ), toSubl NextLayout >> (dynamicLogString def >>= \d->spawn $"echo "++d++" > ~/.xmonad/.xmonad-layout-log")) -- Rotate available layouts
-- Projects: {{{
-------------------------------------------------------------------------------
-- projects :: [Project]
-- projects =
--   [ Project { projectName      = "Shado Shell"
--             , projectDirectory = "~/ShadoSH"
--             , projectStartHook = Just $ do spawn (myTerminal ++ "-e nvim")
--             }
--   , Project { projectName      = "Research"
--             , projectDirectory = "~/"
--             , projectStartHook = Just $ do spawn myBrowser
--                                            spawn myTerminal
--             }
--   ]
  -- , Project { projectName      = "program"
  --           , projectDirectory = "~/Documents/Programming/"
  --           }
  -- , Project { projectName      = "system"
  --           , projectDirectory = "~/Documents/"
  --           , projectStartHook = Just $ do spawn (myTerminal ++ "ncmpcpp")
  --                                          spawn (myTerminal ++ "htop")
  --           , projectStartHook = Just $ do spawn "discord"
  --                                          spawn (myTerminal ++ "irssi")
  --           }

----------------------------------------------------------------------------}}}
-- Tree Select: {{{
--------------------------------------------------------------------------------
-- tsAction :: TS.TSConfig (X ()) -> X ()
-- tsAction a = TS.treeselectAction a
--     [ Node (TS.TSNode "My Apps" "List of many gui/tui applications" (return ()))
--       [Node (TS.TSNode (TE.fst3 $ myApplications !! n)
--                        (TE.thd3 $ myApplications !! n)
--                        (spawn $ TE.snd3 $ myApplications !! n)
--             ) [] | n <- [0..(length myApplications - 1)]
--       ]
--     , Node (TS.TSNode "My Bookmarks" "List of my more important bookmarks" (return ()))
--       [Node (TS.TSNode (TE.fst3 $ myBookmarks !! n)
--                        (TE.thd3 $ myBookmarks !! n)
--                        (spawn $ TE.snd3 $ myBookmarks !! n)
--             ) [] | n <- [0..(length myBookmarks - 1)]
--       ]
--     , Node (TS.TSNode "My Files" "List of my most used files" (return ()))
--       [Node (TS.TSNode (TE.fst3 $ myFiles !! n)
--                        (TE.thd3 $ myFiles !! n)
--                        (spawn $ TE.snd3 $ myFiles !! n)
--             ) [] | n <- [0..(length myFiles - 1)]
--       ]
--     ]

-- myTreeSelConfig :: TS.TSConfig a
-- myTreeSelConfig  = TS.TSConfig { TS.ts_hidechildren = True
--                                , TS.ts_background   = 0x7A1B1B29
--                                , TS.ts_font         = myFont
--                                , TS.ts_node         = (0xffbfaae3, 0xff202331)
--                                , TS.ts_nodealt      = (0xffbfaae3, 0xff292d3e)
--                                , TS.ts_highlight    = (0xffffffff, 0xff755999)
--                                , TS.ts_extra        = 0xffbfaae3
--                                , TS.ts_node_width   = 200
--                                , TS.ts_node_height  = 20
--                                , TS.ts_originX      = 0
--                                , TS.ts_originY      = 0
--                                , TS.ts_indent       = 80
--                                , TS.ts_navigate     = myTreeNav
--                                }

-- myTreeNav = M.fromList
--     [ ((0, xK_Escape),  TS.cancel          )
--     , ((0, xK_Return),  TS.select          )
--     , ((0, xK_space ),  TS.select          )
--     , ((0, xK_Up    ),  TS.movePrev        )
--     , ((0, xK_Down  ),  TS.moveNext        )
--     , ((0, xK_Left  ),  TS.moveParent      )
--     , ((0, xK_Right ),  TS.moveChild       )
--     , ((0, xK_k     ),  TS.movePrev        )
--     , ((0, xK_j     ),  TS.moveNext        )
--     , ((0, xK_h     ),  TS.moveParent      )
--     , ((0, xK_l     ),  TS.moveChild       )
--     , ((0, xK_o     ),  TS.moveHistBack    )
--     , ((0, xK_i     ),  TS.moveHistForward )
--     ]

-----------------------------------------------------------------------------}}}
-- My Everything: {{{
--------------------------------------------------------------------------------
-- myApplications :: [(String, String, String)]
-- myApplications = [ ("Shadoplan", (myTerminal ++ "sp"), "My TODO program")
--                  , ("Neovim", (myTerminal ++ "nvim"), "My TODO program")
--                  , ("My Wiki", (myTerminal ++ "nvim ~/vimwiki/index.md"), "My personal wiki")
--                  , ("Audacity", "audacity", "Music/Audio editor and recorder")
--                  , ("Krita", "krita", "Advanced art/drawing program")
--                  , ("Dolphin", "dolphin", "GUI File manager")
--                  , ("Discord", "Discord", "Communications Software")
--                  , ("Irssi", (myTerminal ++ "irssi"), "IRC Client")
--                  , ("Calcurse", (myTerminal ++ "calcurse"), "TUI Calendar useful for many things")
--                  , ("Qutebrowser", "qutebrowser", "Lightweight qt5 browser")
--                  , ("Lynx Browser", (myTerminal ++ "lynx"), "TUI Browser")
--                  , ("Firefox", "firefox", "Main browser")
--                  , ("Minecraft", "minecraft-launcher", "I mean just Minecraft")
--                  , ("Lutris", "lutris", "Game launcher")
--                  , ("Evince", "evince", "Simple PDF Viewer")
--                  ]

-- myBookmarks :: [(String, String, String)]
-- myBookmarks  = [ ("XMonad Doc Extending", myBrowser ++ "http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Doc-Extending.html", "Massive doc page for XMonad")
--                  -- Programming
--                , ("Learn you a haskell", myBrowser ++ "http://learnyouahaskell.com/chapters", "Amazing book for haskell lang")
--                , ("Assembly Programming Tutorial", myBrowser ++ "https://www.tutorialspoint.com/assembly_programming/", "Very good guide to assembly")
--                , ("Guide to x86 Assembly", myBrowser ++ "http://www.cs.virginia.edu/~evans/cs216/guides/x86.html", "Nice asm guide")
--                , ("CPP Reference", myBrowser ++ "https://en.cppreference.com/w/", "For all things C/C++")
--                , ("Programming E-Books", myBrowser ++ "https://github.com/trumpowen/All-Programming-E-Books-PDF#c-books", "Big 'awesome' github, with a dump of prog books for many langs")
--                  -- Font
--                , ("Text to ASCII Art Generator", myBrowser ++ "http://patorjk.com/software/taag/#p=testall&f=Graffiti&t=Shado.vim", "Great site for ascii text/art")
--                , ("FontDrop!", myBrowser ++ "https://fontdrop.info/", "Site to view fonts")
--                , ("Nerd Fonts Cheatsheet", myBrowser ++ "https://www.nerdfonts.com/cheat-sheet", "Site for nerd font icons")
--                  -- Security/Hacking
--                , ("Hack The Box", myBrowser ++ "https://hackthebox.eu", "Amazing resource for getting out there with sec/hacking")
--                , ("TryHackMe", myBrowser ++ "https://tryhackme.com", "Another good CTF/Learning type resource")
--                , ("Reverse Engineering 101", myBrowser ++ "https://malwareunicorn.org/workshops/re101.html#0", "Great reversing walkthrough (covers alot of malware analysis)")
--                , ("Mem Labs", myBrowser ++ "https://github.com/stuxnet999/MemLabs", "Very good memory forensics resource")
--                , ("0x00 Sec", myBrowser ++ "https://0x00sec.org/", "The 'Reddit' for all things Security")
--                , ("Thug Crowd", myBrowser ++ "https://thugcrowd.com/archive.html", "Some fun hacking exercises")
--                , ("Hacking Books", myBrowser ++ "https://haxf4rall.com/hacking-books/", "Site for free security books")
--                ]

-- myFiles :: [(String, String, String)]
-- myFiles  = [ ("XMonad Config", myEditor ++ "~/.xmonad/xmonad.hs", "My xmonad configuration file")
--            , ("Doom Config", myEditor ++ "~/.doom.d/config.el", "My doom emacs config.el file")
--            , ("Doom Init", myEditor ++ "~/.doom.d/init.el", "My doom emacs init.el file")
--            , ("Dwm Config", myEditor ++ "~/dwm/config.h", "My dwm configuration file")
--            , ("Zshrc", myEditor ++ "~/.zshrc", "My zsh config file")
--             ]

-- Allows me to use the same functions for treeselect in gridselect
-- myAppGrid :: [(String, String)]
-- myAppGrid = [ (a,b) | (a,b,c) <- xs]
--   where xs = myApplications
        
-- myBookmarksGrid :: [(String, String)]
-- myBookmarksGrid  = [ (a,b) | (a,b,c) <- xs]
--   where xs = myBookmarks

-- myFilesGrid :: [(String, String)]
-- myFilesGrid  = [ (a,b) | (a,b,c) <- xs]
--   where xs = myFiles

-----------------------------------------------------------------------------}}}
--}}}
-------------------------------------------------------------------------------
