-------------------------------------------------------------------------------
-- Shadomonad Config --
-------------------------------------------------------------------------------
-- TODO: add some bindings
    --   change layouts
    --   change polybar
    --   Fix up the example projects in there and make my own! 
    --   also have some default apps open for certain workspaces!!!
    --   fix tabbing
-------------------------------------------------------------------------------
-- Imports: {{{
-------------------------------------------------------------------------------
    -- Base
import XMonad
import Data.Monoid
import System.Exit
import System.IO (hClose)
-- import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CycleWS  --(moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.ConditionalKeys
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.PerWorkspaceKeys
import qualified XMonad.Actions.Submap           as SM
import qualified XMonad.Actions.TreeSelect       as TS
import XMonad.Actions.WorkspaceNames
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.Tree
import qualified Data.Map        as M
import qualified Data.Tuple.Extra as TE

    -- Dbus
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory    -- (For tree select)

    -- Layouts
import XMonad hiding ( (|||) )
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layout Mods
-- import XMonad.Layout.Decoration
import XMonad.Layout.Gaps
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutBuilder
-- import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Master
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell (shellPrompt)
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare
----------------------------------------------------------------------------}}}
-- Variables: {{{
-------------------------------------------------------------------------------
    -- Base
myBrowser       = "firefox " -- Set default browser
myFilemngr      = "vifmrun" -- Set default file manager
myFont          = "xft:Agave:pixelsize=14" -- Set font
myLauncher      = "dmenu_run -fn 'Agave:size=15' -nb '#1B1B29' -nf '#8897F4' -sb '#2F2F4A' -sf '#ff79c6'" -- Set font
mySpacing       :: Int
mySpacing       = 5 -- Set gaps between windows
noSpacing       :: Int
noSpacing       = 0 -- Set nogaps between windows
myTerminal      = "kitty " -- Set default terminal
myEditor        = "nvim" -- Set default text editor
windowCount :: X (Maybe String)
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset -- Get count of windows in selected workspace

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
    manageHook         = placeHook (smart (0.5, 0.5))
                        <+> manageDocks
                        <+> myManageHook
                        <+> manageHook def,
    -- logHook            = myLogHook,
    handleEventHook    = docksEventHook
                        <+> minimizeEventHook,
                        -- <+> fullscreenEventHook,
    startupHook        = myStartupHook
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
    , layoutNavigation          = [("Full",          centerNavigation)]
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayoutHook = avoidStruts(tiled ||| Mirror tiled) ||| fullscreenFocus Full ||| masterTabbed
  where
    nmaster = 1 -- Default master count
    ratio   = 1/2 -- Default size ratio of master:stack size
    delta   = 3/100 -- Percent of screen inc/dec when resizing
    named n     = renamed [(XMonad.Layout.Renamed.Replace n)]
    addOverline = noFrillsDeco shrinkText overLineTheme
    mySpacing   = spacing gap
    myGaps      = gaps [(U, gap),(D, gap),(L,gap),(R,gap)]

    -- Layouts
    tiled           = Tall nmaster delta ratio  -- Default Master/Stack (No Gaps) 
    
    masterTabbed    = named "M Tab"
        $ addOverline
        $ avoidStruts
        $ mySpacing' 0
        $ myGaps
        $ mastered (1/100) (1/2)
        $ tabbed shrinkText myTabTheme

----------------------------------------------------------------------------}}}
-- Window rules: {{{
-------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "lutris"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
        where
            role = stringProperty "WM_WINDOW_ROLE"

----------------------------------------------------------------------------}}}
-- Autostart: {{{
-------------------------------------------------------------------------------
-- myStartupHook :: X ()
myStartupHook = do
    setWMName "ShadoWM"
    spawn "feh --bg-scale --no-fehbg $HOME/Pictures/Backgrounds/forest.png &"
    spawn "killall flameshot; flameshot &"
    spawn "killall flashfocus; flashfocus &"
    spawn "killall picom; picom --experimental-backends &"
    spawn "killall polybar; polybar -c ~/.config/polybar/config-xmonad shadobar"
    spawn "killall xcape; xcape -e 'Hyper_L=Tab;Hyper_R=backslash'"
    
    setDefaultCursor xC_left_ptr

    -- spawn "killall polybar; polybar -c ~/.config/polybar/config-xmonad shadobar2"
----------------------------------------------------------------------------}}}
-- Main: {{{
-------------------------------------------------------------------------------
main :: IO ()
main = do
    nScreens <- countScreens -- Gets current screen count
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad 
        $ dynamicProjects projects
        $ fullscreenSupport
        $ withNavigation2DConfig myNav2DConf
        $ withUrgencyHook NoUrgencyHook 
        $ ewmh 
        $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus), workspaces = withScreens nScreens [m0ws1,m0ws2,m0ws3,m0ws4,m0ws5,m0ws6,m0ws7,m0ws8,m0ws9] }

    -- xmproc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
----------------------------------------------------------------------------}}}
-- Loghook: {{{
-------------------------------------------------------------------------------
    -- Bar Customization
screen1LogHook :: D.Client -> PP
screen1LogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent          = wrap ("%{B#2f2f4a80}%{F" ++ cPink ++ "}%{o"++ cPurpBlue ++"}%{A4:xdotool key alt+shift+Right:}%{A5:xdotool key alt+shift+Left:}  ") "  %{A}%{A}%{-o}%{B- F-}" -- Focused wkspc
    , ppVisible          = wrap ("%{F" ++ cBlue ++ "} ") " %{F-}" -- not working
    , ppVisibleNoWindows = Just (wrap ("%{F" ++ cMagenta ++ "} ") " %{F-}") -- not working
    , ppUrgent           = wrap ("%{F" ++ cRed ++ "}%{A4:xdotool key alt+shift+Right:}%{A5:xdotool key alt+shift+Left:} ") " %{A}%{A}%{F-}" -- Urgent wkspc
    , ppHidden           = wrap ("%{F" ++ cPurpBlue ++ "}%{A4:xdotool key alt+shift+Right:}%{A5:xdotool key alt+shift+Left:} ") " %{A}%{A}%{F-}" -- Hidden with windows
    , ppHiddenNoWindows  = wrap ("%{F" ++ cEmpty ++ "}%{A4:xdotool key alt+shift+Right:}%{A5:xdotool key alt+shift+Left:} ") " %{A}%{A}%{F-}" -- Hidden and empty
    , ppWsSep            = ""
    , ppSep              = " | "
    -- , ppTitle = myAddSpaces 25
    , ppLayout = \x -> case x of     -- Changes layout name to be displayed
                        "Tall" -> "%{A4:xdotool key alt+space:}%{A5:xdotool key alt+space:}T %{A}%{A}|"
                        "Mirror Tall" -> "%{A4:xdotool key alt+space:}%{A5:xdotool key alt+space:}M %{A}%{A}|"
                        "Full" -> "%{A4:xdotool key alt+space:}%{A5:xdotool key alt+space:}F %{A}%{A}|"
                        "M Tab" -> "%{A4:xdotool key alt+space:}%{A5:xdotool key alt+space:}MT %{A}%{A}|"
                        _ -> "? |"
    , ppOrder = \(ws:l:_) -> [ws,l] -- [workspace, layout] (Removed window title)
    }

-- Emit a DBus signal on log updates
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

myLogHook :: D.Client -> PP
myLogHook dbus = (marshallPP 0 (screen1LogHook dbus)) 
    { ppSort = selectScreen 0 (ppSort def) }

selectScreen :: ScreenId -> X WorkspaceSort -> X WorkspaceSort
selectScreen s = fmap (fmap (filter onScreen)) where
    onScreen ws = unmarshallS (W.tag ws) == s

-- myAddSpaces :: Int -> String -> String
-- myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
--   where
--     sstr = shorten len str

----------------------------------------------------------------------------}}}
-- My Everything: {{{
--------------------------------------------------------------------------------
myApplications :: [(String, String, String)]
myApplications = [ ("Shadoplan", (myTerminal ++ "sp"), "My TODO program")
                 , ("Neovim", (myTerminal ++ "nvim"), "My TODO program")
                 , ("My Wiki", (myTerminal ++ "nvim ~/vimwiki/index.md"), "My personal wiki")
                 , ("Audacity", "audacity", "Music/Audio editor and recorder")
                 , ("Krita", "krita", "Advanced art/drawing program")
                 , ("Dolphin", "dolphin", "GUI File manager")
                 , ("Discord", "Discord", "Communications Software")
                 , ("Irssi", (myTerminal ++ "irssi"), "IRC Client")
                 , ("Calcurse", (myTerminal ++ "calcurse"), "TUI Calendar useful for many things")
                 , ("Qutebrowser", "qutebrowser", "Lightweight qt5 browser")
                 , ("Lynx Browser", (myTerminal ++ "lynx"), "TUI Browser")
                 , ("Firefox", "firefox", "Main browser")
                 , ("Minecraft", "minecraft-launcher", "I mean just Minecraft")
                 , ("Lutris", "lutris", "Game launcher")
                 , ("Evince", "evince", "Simple PDF Viewer")
                 ]

myBookmarks :: [(String, String, String)]
myBookmarks  = [ ("XMonad Doc Extending", myBrowser ++ "http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Doc-Extending.html", "Massive doc page for XMonad")
                 -- Programming
               , ("Learn you a haskell", myBrowser ++ "http://learnyouahaskell.com/chapters", "Amazing book for haskell lang")
               , ("Assembly Programming Tutorial", myBrowser ++ "https://www.tutorialspoint.com/assembly_programming/", "Very good guide to assembly")
               , ("Guide to x86 Assembly", myBrowser ++ "http://www.cs.virginia.edu/~evans/cs216/guides/x86.html", "Nice asm guide")
               , ("CPP Reference", myBrowser ++ "https://en.cppreference.com/w/", "For all things C/C++")
               , ("Programming E-Books", myBrowser ++ "https://github.com/trumpowen/All-Programming-E-Books-PDF#c-books", "Big 'awesome' github, with a dump of prog books for many langs")
                 -- Font
               , ("Text to ASCII Art Generator", myBrowser ++ "http://patorjk.com/software/taag/#p=testall&f=Graffiti&t=Shado.vim", "Great site for ascii text/art")
               , ("FontDrop!", myBrowser ++ "https://fontdrop.info/", "Site to view fonts")
               , ("Nerd Fonts Cheatsheet", myBrowser ++ "https://www.nerdfonts.com/cheat-sheet", "Site for nerd font icons")
                 -- Security/Hacking
               , ("Hack The Box", myBrowser ++ "https://hackthebox.eu", "Amazing resource for getting out there with sec/hacking")
               , ("TryHackMe", myBrowser ++ "https://tryhackme.com", "Another good CTF/Learning type resource")
               , ("Reverse Engineering 101", myBrowser ++ "https://malwareunicorn.org/workshops/re101.html#0", "Great reversing walkthrough (covers alot of malware analysis)")
               , ("Mem Labs", myBrowser ++ "https://github.com/stuxnet999/MemLabs", "Very good memory forensics resource")
               , ("0x00 Sec", myBrowser ++ "https://0x00sec.org/", "The 'Reddit' for all things Security")
               , ("Thug Crowd", myBrowser ++ "https://thugcrowd.com/archive.html", "Some fun hacking exercises")
               , ("Hacking Books", myBrowser ++ "https://haxf4rall.com/hacking-books/", "Site for free security books")
               ]

myFiles :: [(String, String, String)]
myFiles  = [ ("XMonad Config", myEditor ++ "~/.xmonad/xmonad.hs", "My xmonad configuration file")
           , ("Doom Config", myEditor ++ "~/.doom.d/config.el", "My doom emacs config.el file")
           , ("Doom Init", myEditor ++ "~/.doom.d/init.el", "My doom emacs init.el file")
           , ("Dwm Config", myEditor ++ "~/dwm/config.h", "My dwm configuration file")
           , ("Zshrc", myEditor ++ "~/.zshrc", "My zsh config file")
            ]

-- Allows me to use the same functions for treeselect in gridselect
myAppGrid :: [(String, String)]
myAppGrid = [ (a,b) | (a,b,c) <- xs]
  where xs = myApplications
        
myBookmarksGrid :: [(String, String)]
myBookmarksGrid  = [ (a,b) | (a,b,c) <- xs]
  where xs = myBookmarks

myFilesGrid :: [(String, String)]
myFilesGrid  = [ (a,b) | (a,b,c) <- xs]
  where xs = myFiles

-----------------------------------------------------------------------------}}}
-- Tree Select: {{{
--------------------------------------------------------------------------------
tsAction :: TS.TSConfig (X ()) -> X ()
tsAction a = TS.treeselectAction a
    [ Node (TS.TSNode "My Apps" "List of many gui/tui applications" (return ()))
      [Node (TS.TSNode (TE.fst3 $ myApplications !! n)
                       (TE.thd3 $ myApplications !! n)
                       (spawn $ TE.snd3 $ myApplications !! n)
            ) [] | n <- [0..(length myApplications - 1)]
      ]
    , Node (TS.TSNode "My Bookmarks" "List of my more important bookmarks" (return ()))
      [Node (TS.TSNode (TE.fst3 $ myBookmarks !! n)
                       (TE.thd3 $ myBookmarks !! n)
                       (spawn $ TE.snd3 $ myBookmarks !! n)
            ) [] | n <- [0..(length myBookmarks - 1)]
      ]
    , Node (TS.TSNode "My Files" "List of my most used files" (return ()))
      [Node (TS.TSNode (TE.fst3 $ myFiles !! n)
                       (TE.thd3 $ myFiles !! n)
                       (spawn $ TE.snd3 $ myFiles !! n)
            ) [] | n <- [0..(length myFiles - 1)]
      ]
    ]

myTreeSelConfig :: TS.TSConfig a
myTreeSelConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0x7A1B1B29
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffbfaae3, 0xff202331)
                              , TS.ts_nodealt      = (0xffbfaae3, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffbfaae3
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNav
                              }

myTreeNav = M.fromList
    [ ((0, xK_Escape),  TS.cancel          )
    , ((0, xK_Return),  TS.select          )
    , ((0, xK_space ),  TS.select          )
    , ((0, xK_Up    ),  TS.movePrev        )
    , ((0, xK_Down  ),  TS.moveNext        )
    , ((0, xK_Left  ),  TS.moveParent      )
    , ((0, xK_Right ),  TS.moveChild       )
    , ((0, xK_k     ),  TS.movePrev        )
    , ((0, xK_j     ),  TS.moveNext        )
    , ((0, xK_h     ),  TS.moveParent      )
    , ((0, xK_l     ),  TS.moveChild       )
    , ((0, xK_o     ),  TS.moveHistBack    )
    , ((0, xK_i     ),  TS.moveHistForward )
    ]

-----------------------------------------------------------------------------}}}
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
    , bgColor             = "#1B1B29"
    , fgColor             = "#BFAAE3"
    , bgHLight            = cPurpBlue
    , fgHLight            = "#000000"
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

----------------------------------------------------------------------------}}}
-- Projects: {{{
-------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = m0ws3
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "discord"
                                           spawn (myTerminal ++ "irssi")
            }
  , Project { projectName      = "term"
            , projectDirectory = "~/Documents/"
            , projectStartHook = Just $ do spawn myBrowser
                                           spawn myTerminal
            }
  , Project { projectName      = "program"
            , projectDirectory = "~/Documents/Programming/"
            , projectStartHook = Just $ do spawn (myTerminal ++ "nvim")
            }
  , Project { projectName      = "system"
            , projectDirectory = "~/Documents/"
            , projectStartHook = Just $ do spawn (myTerminal ++ "ncmpcpp")
                                           spawn (myTerminal ++ "htop")
            }

  ]
----------------------------------------------------------------------------}}}
-- Key Bindings: {{{
-------------------------------------------------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- Xmonad ---------------------------------------------------------------------------------
    [ ((modm .|. controlMask,   xK_q     ), io (exitWith ExitSuccess)                   ) -- Quit
    , ((modm,                   xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ((mods.|.controlMask, xK_F12   ), spawn "~/.config/scripts/switch_gpu"        ) -- Switch GPU
    , ((modm .|. controlMask,   xK_b     ), spawn "killall polybar; polybar -c ~/.config/polybar/config-xmonad shadobar") -- Restart polybar
    , ((mods,               xK_p     ), spawn "betterlockscreen -l blur -r 1920x1080  -b 0.2 -t 'Welcome back, Shado...'") -- Lock Screen
        -- Base -----------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_Return), spawn $ XMonad.terminal conf                ) -- Terminal
    , ((modm,                   xK_p     ), spawn myLauncher                            ) -- Dmenu
    , ((modm,                   xK_b     ), sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle") -- Toggle Bar
    , ((modm .|. shiftMask,     xK_b     ), sendMessage $ (MT.Toggle NOBORDERS)         ) -- Toggle Borders
        -- Layout ---------------------------------------------------------------------------------
    , ((modm,                   xK_space ), sendMessage NextLayout                      ) -- Rotate available layouts
    , ((modm .|. shiftMask,     xK_space ), setLayout $ XMonad.layoutHook conf          ) -- Reset layouts on current workspace
    , ((modm,                   xK_f     ), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle") -- Toggles Fullscreen
        -- Workspaces -----------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_Right ), nextWS                                      ) -- Cycle Right
    , ((modm .|. shiftMask,     xK_Left  ), prevWS                                      ) -- Cycle Left
        -- Tabs -----------------------------------------------------------------------------------
    -- , ((modm,                   xK_semicolon ), bindOn LD [("M Tab", windows W.focusUp),("", onGroup W.focusUp')]    ) -- Focus next tab up
    -- , ((modm,                   xK_apostrophe), bindOn LD [("M Tab", windows W.focusDown),("", onGroup W.focusDown')]) -- Focus next tab down
    , ((modm .|. shiftMask,     xK_semicolon ), windows W.swapUp                                                  ) -- Swap tab up
    , ((modm .|. shiftMask,     xK_apostrophe), windows W.swapDown                                                ) -- Swap tab down
        -- Windows --------------------------------------------------------------------------------
    , ((modm .|. shiftMask,     xK_c     ), kill                                        ) -- close window
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
        -- Scratchpads ----------------------------------------------------------------------------
    , ((modm,                   xK_s), namedScratchpadAction myScratchPads "terminal"   ) -- Terminal Scrtchpd
    , ((modm .|. shiftMask,     xK_s), namedScratchpadAction myScratchPads "ncmpcpp"    ) -- Ncmpcpp Scrtchpd
        -- Multimedia (Volume, MPD) ---------------------------------------------------------------
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

        -- Open Applications -------------------------------------------------------------------------------------
    , ((mods,                       xK_b      ), spawn myBrowser                                         ) -- Browser
    , ((mods .|. controlMask,       xK_m      ), spawn (myTerminal ++ "calcurse")                        ) -- Calcurse
    , ((mods .|. shiftMask,         xK_d      ), spawn "discord"                                         ) -- Discord
    , ((mods,                       xK_v      ), spawn (myTerminal ++ myFilemngr)                        ) -- File Manager
    , ((mods,                       xK_a      ), spawn (myTerminal ++ "pulsemixer")                      ) -- Mixer
    , ((mods .|. shiftMask,         xK_m      ), spawn (myTerminal ++ "ncmpcpp")                         ) -- Ncmpcpp
    , ((mods,                       xK_w      ), spawn (myTerminal ++ "nmtui")                           ) -- Netork
    , ((mods,                       xK_h      ), spawn (myTerminal ++ "htop")                            ) -- Processes
    , ((mods,                       xK_s      ), spawn "~/.config/rofi/scripts/menu_powermenu.sh"        ) -- Processes
        -- Screenshots -------------------------------------------------------------------------------------------
    , ((shiftMask .|. controlMask,  xK_Print  ), spawn "flameshot gui -p ~/Pictures/Screenshots"         ) -- Area
    , ((0,                          xK_Print  ), spawn "scrot '~/Pictures/Screenshots/%F_%T.png'"        ) -- Fullscreen
    , ((mods .|. modm,              xK_Print  ), spawn "flameshot screen -r -c -p ~/Pictures/Screenshots") -- Monitor
    , ((controlMask,                xK_Print  ), spawn "scrot -u '~/Pictures/Screenshots'"               ) -- Window
    , ((modm .|. controlMask,       xK_Print  ), spawn "~/.config/scripts/imgurup"                       ) -- Imgur
        -- Grid Select -------------------------------------------------------------------------------------------
    , ((modm,                       xK_g      ), goToSelected $ mygridConfig myGridTheme                 ) -- Go to grid item
    , ((modm .|. shiftMask,         xK_g      ), bringSelected $ mygridConfig myGridTheme                ) -- Grab and brind over grid item
    , ((modm .|. controlMask,       xK_g      ), spawnSelected' myAppGrid                                ) -- Custom program list
        -- Tree Select -------------------------------------------------------------------------------------------
    , ((modm .|. shiftMask,         xK_t      ), tsAction myTreeSelConfig                                ) -- Custom program list
        -- Search Engine -----------------------------------------------------------------------------------------
    , ((modm,               xK_slash), SM.submap $ searchEngineMap $ S.promptSearch shXPConfig'            ) -- Searches via prompt
    , ((modm .|. shiftMask, xK_slash), SM.submap $ searchEngineMap $ S.selectSearch                        ) -- Searches via clipboard
    ]
    ++
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3 mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
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

---------------------------------------------------------------------}}}
-- Mouse bindings: {{{
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
--}}}
-------------------------------------------------------------------------------
