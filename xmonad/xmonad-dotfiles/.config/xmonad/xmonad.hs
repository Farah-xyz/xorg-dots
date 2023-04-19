-- ===========================================================================
-- ██╗  ██╗███╗   ███╗ ██████╗ ███╗   ██╗ █████╗ ██████╗
-- ╚██╗██╔╝████╗ ████║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗
--  ╚███╔╝ ██╔████╔██║██║   ██║██╔██╗ ██║███████║██║  ██║
--  ██╔██╗ ██║╚██╔╝██║██║   ██║██║╚██╗██║██╔══██║██║  ██║
-- ██╔╝ ██╗██║ ╚═╝ ██║╚██████╔╝██║ ╚████║██║  ██║██████╔╝
-- ╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═════╝
-- ===========================================================================
{-
      written by Farah Abderrazzak Aka FrhXM(https://github.com/frhxm)
      first Edit 25/4/2022 + (I think ?)
-}
-------------------------------------------------------------------------------
-- Import modules
-------------------------------------------------------------------------------
--- Main
import XMonad
import System.Exit (exitSuccess)
import Control.Monad (liftM2)
--- Actions
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace, tagToEmptyWorkspace)
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll, sinkAll, killOthers)
import XMonad.Actions.Search (google, duckduckgo, youtube, images, github, searchEngine, promptSearchBrowser)
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import XMonad.Actions.CycleWindows (rotUnfocusedDown, rotUnfocusedUp)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn)
import XMonad.Actions.DynamicProjects
--- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import XMonad.ManageHook (doFloat)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat, doFullFloat, isFullscreen)
import XMonad.Hooks.FadeWindows (fadeWindowsLogHook, isFloating, isUnfocused, transparency, solid)
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp, defToggleStrutsKey)
import XMonad.Hooks.StatusBar.PP (PP (ppCurrent, ppExtras, ppHidden, ppOrder, ppSep, ppWsSep, ppUrgent, ppVisible, ppTitle, ppLayout, ppHiddenNoWindows), shorten, wrap, xmobarColor, filterOutWsPP)
import XMonad.Hooks.UrgencyHook
--- Utilities
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedActions
import XMonad.Util.EZConfig
--- Layouts/Modifiers
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.LimitWindows
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
--- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Dishes
import XMonad.Layout.Dwindle
import XMonad.Layout.HintedGrid
import XMonad.Layout.Circle
--- prompt
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Man
import XMonad.Prompt.FuzzyMatch
--- Others
import qualified XMonad.StackSet  as W
import qualified Data.Map         as M
------------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------------
myBrowser            = "qutebrowser":: String     -- Browser
myTerminal           = "kitty"      :: String     -- Terminal
myAltMask            = mod1Mask     :: KeyMask    -- leader key (Alt)
mySuperMask          = mod4Mask     :: KeyMask    -- Windows Key (Super)
myBorderWidth        = 3            :: Dimension  -- Border size
myNormalBorderColor  = "#32344a"    :: String     -- Border color of unfocus window
myFocusedBorderColor = "#7aa2f7"    :: String     -- Border color of focus window
myFocusFollowsMouse  = True         :: Bool       -- focus follow config
myClickJustFocuses   = False        :: Bool       -- focus click config
--------  Font ----------
myFont       = "xft:JetBrains Mono:style=Bold:pixelsize=13"        :: String
myNerdFont   = "xft:FiraCode Nerd Font Mono:Bold:pixelsize=14"     :: String
myNerdFontBig= "xft:FiraCode Nerd Font Mono:Bold:pixelsize=200"    :: String
myJPFont     = "xft:Noto Sans Mono CJK JP:style=Bold:pixelsize=15" :: String
myJPFontBig  = "xft:Noto Sans Mono CJK JP:style=Bold:pixelsize=200":: String
------ Workspaces -------
wsDEV           = "¹\xf120"
wsGIT           = "²\xf7a1"
wsWEB           = "³\xf26b"
wsYTB           = "⁴\xf16a"
wsCHT           = "⁵\xf099"
wsPEN           = "⁶\xf79f"
wsMED           = "⁷\xf07b"
wsSIT           = "⁸\xf013"
wsAll           = "⁹\xf49c"
myWorkspaces    = [wsDEV, wsGIT, wsWEB, wsYTB, wsCHT, wsPEN, wsMED, wsSIT, wsAll]
---- Other WS Icons -----
-- myWorkspaces    = ["一", "二", "三", "四", "五", "六", "七", "八", "九"]
-- myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]
-- myWorkspaces    = ["\xf120", "\xf268", "\xe7c5", "\xfad9", "\xf292", "\xf200", "\xf040", "\xfa6f", "\xf49c"]
-- myWorkspaces    = ["\xf269", "\xf7a1", "\xf687", "\xe795", "\xf1b2", "\xf16a", "\xf001", "\xf7b3", "\xf013"]
------------------------------------------------------------------------------
-- Projects
------------------------------------------------------------------------------
projects =
    [ Project { projectName = wsDEV
              , projectDirectory = "~/prjcts"
              , projectStartHook = Just $ do spawnOn wsDEV "kitty -e nvim"
              }

    , Project { projectName = wsGIT
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsGIT "qutebrowser --target=window github.com/frhxm"
              }

    , Project { projectName = wsWEB
              , projectDirectory = "~/dl"
              , projectStartHook = Just $ do spawnOn wsWEB "qutebrowser --target=window"
              }

    , Project { projectName = wsYTB
              , projectDirectory = "~/vids"
              , projectStartHook = Just $ do spawnOn wsYTB "qutebrowser --target=window youtube.com"
              }

    , Project { projectName = wsPEN
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsPEN "qutebrowser --target=window https://codepen.io/pen/"
              }

    , Project { projectName = wsMED
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawnOn wsMED "nemo"
              }

    , Project { projectName = wsSIT
              , projectDirectory = "~/.config"
              , projectStartHook = Nothing
              }
    ]
------------------------------------------------------------------------------
-- Autostart
------------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "xwallpaper --zoom ~/pix/wall/dream.jpg"   -- Wallpapers
    spawnOnce "~/.config/xmobar/scripts/battnotify.sh"   -- battery notifction
    spawnOnce "xset r rate 200 80"                       -- speeds cursor
    spawnOnce "picom -b"                                 -- Compositor
    setDefaultCursor xC_left_ptr                         -- Default Cursor
------------------------------------------------------------------------------
-- ManageHooks
------------------------------------------------------------------------------
myManageHook = composeAll
     [ className =? "Gimp"              --> doViewShift wsMED
     , className =? "mpv"               --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
     , className =? "Sxiv"              --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
     , className =? "Lxappearance"      --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
     , className =? "Xmessage"          --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
     , className =? "notification"      --> doCenterFloat
     , isFullscreen                     --> doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads
       <+> manageSpawn
    where
     doViewShift = doF . liftM2 (.) W.view W.shift
------------------------------------------------------------------------------
-- FadeWindowHooks
------------------------------------------------------------------------------
myFadeHook = composeAll
     [ className =? "kitty"              --> transparency 0.05
     , className =? "Nemo"               --> transparency 0.1
     , isUnfocused                       --> transparency 0.1
     , isFloating                        --> solid
     ]
------------------------------------------------------------------------------
-- ScratchPads
------------------------------------------------------------------------------
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "browser" spawnBrowser findBrowser manageBrowser
                ]
               where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnBrowser = "firefox"
    findBrowser = className =? "firefox"
    manageBrowser = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
-------------------------------------------------------------------------------
-- XMonad Prompt (XP)
-------------------------------------------------------------------------------
myXPConfig = def
          { font                =  myNerdFont
          , bgColor             = "#32344a"
          , fgColor             = "#a9b1d6"
          , bgHLight            = "#9ece6a"
          , fgHLight            = "#32344a"
          , borderColor         = "#11121D"
          , promptBorderWidth   = 6
          , position            = CenteredAt (1 / 15) (1 / 3)
          , alwaysHighlight     = True
          , height              = 40
          , maxComplRows        = Just 5       -- set to Just 5 for 5 rows Or Nothing
          , maxComplColumns     = Just 5       -- set to Just 5 for 5 coulmn Or Nothing
          , historySize         = 256
          , historyFilter       = deleteAllDuplicates
          , promptKeymap        = vimLikeXPKeymap
          , completionKey       = (0, xK_Tab)
          , changeModeKey       = xK_grave
          , defaultText         = []
          , autoComplete        = Nothing       -- set Just 100000 for .1 sec
          , showCompletionOnTab = False
          , complCaseSensitivity= CaseInSensitive
          , defaultPrompter     = id
                -- Prompt.FuzzyMatch
          , searchPredicate     = fuzzyMatch
          , sorter              = fuzzySort
          }
------------------------------------------------------------------------------
-- XMonad Prompt Search (XP Search)
------------------------------------------------------------------------------
aur         = searchEngine "aur" "https://aur.archlinux.org/packages?O=0&K="
archwiki    = searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search="
reddit      = searchEngine "reddit" "https://www.reddit.com/r/unixporn/search/?q="
wallhaven   = searchEngine "wallhaven" "https://wallhaven.cc/search?q="
------------------------------------------------------------------------------
-- Layout Algorithms
------------------------------------------------------------------------------
myTabTheme      = def
                { fontName              = myFont
                , activeColor           = "#7aa2f7"
                , inactiveColor         = "#11121D"
                , activeBorderColor     = "#7aa2f7"
                , inactiveBorderColor   = "#11121D"
                , activeTextColor       = "#11121D"
                , inactiveTextColor     = "#a9b1d6"
                }
tabs            = renamed [Replace "TABBED"]
                $ noBorders
                $ maximizeWithPadding 10
                $ minimize
                $ myGaps
                $ tabbed shrinkText myTabTheme
twoPane         = renamed [Replace "TWOPANE"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ TwoPanePersistent Nothing (3/100) (1/2)
dishes          = renamed [Replace "DISHES"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ Dishes 2 (1/5)
oneBig          = renamed [Replace "ONEBIG"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ OneBig (3/4) (3/4)
threeColMid     = renamed [Replace "MID"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ ThreeColMid 1 (3/100) (1/2)
threeCol        = renamed [Replace "THREECOL"]
                $ maximizeWithPadding 10
                $ minimize
                $ limitWindows 7
                $ mySpacings
                $ ThreeCol 1 (3/100) (1/2)
spirals         = renamed [Replace "SPIRAL"]
                $ maximizeWithPadding 10
                $ minimize
                $ limitWindows 7
                $ mySpacings
                $ Spiral L CW 1.5 1.1
circle          = renamed [Replace "circle"]
                $ minimize
                $ maximize
                $ maximizeWithPadding 10
                $ limitWindows 12
                $ mySpacings
                $ Circle
grid            = renamed [Replace "grid"]
                $ minimize
                $ maximizeWithPadding 10
                $ limitWindows 12
                $ mySpacings
                $ GridRatio (4/3) False
tall            = renamed [Replace "TILD"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ ResizableTall 1 (3/100) (1/2) []
floats          = renamed [Replace "floats"]
                $ maximizeWithPadding 10
                $ minimize
                $ limitWindows 20
                $ mySpacings
                $ simplestFloat
full            = renamed [Replace "FULL"]
                $ maximizeWithPadding 10
                $ minimize
                $ mySpacings
                $ limitWindows 20 Full
------------------------------------------------------------------------------
-- Layout/Modifiers
------------------------------------------------------------------------------
mySpacings       = spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True
myGaps           = gaps [(U, 2),(D, 5),(L, 10),(R, 10)]
myShowWNameTheme = def
                { swn_font              = myNerdFontBig
                , swn_fade              = 1.0
                , swn_bgcolor           = "#11121D"
                , swn_color             = "#7aa2f7"
                }
myLayoutHook    = showWName' myShowWNameTheme
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                $ limitWindows 6
                $ avoidStruts
                $ onWorkspaces [wsDEV, wsGIT] codeLayouts
                $ onWorkspace wsWEB webLayouts
                $ onWorkspace wsCHT chatLayouts
                $ allLayouts
               where
    allLayouts  = tall ||| threeColMid ||| dishes ||| oneBig ||| grid ||| twoPane ||| spirals ||| circle ||| floats ||| tabs
    webLayouts  = oneBig ||| threeColMid ||| dishes ||| tall ||| grid ||| twoPane ||| spirals ||| circle ||| floats ||| tabs
    codeLayouts = tabs ||| twoPane ||| dishes
    chatLayouts = grid ||| threeColMid ||| dishes ||| oneBig ||| tall ||| twoPane ||| spirals ||| circle ||| floats ||| tabs
-------------------------------------------------------------------------------
-- Custom Keys
-------------------------------------------------------------------------------
myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
        --- XMonad
        [ ("M-q",    addName "Recompile XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
        , ("M-S-q",  addName "Quit XMonad"             $ io exitSuccess)
        , ("M-S-c",  addName "Kill focused window"     $ kill1)
        , ("M-S-a",  addName "Kill all windows on WS"  $ killAll)
        , ("M-S-o",  addName "Kill all Other window except focused"  $ killOthers)
        , ("M-t",    addName "Sink a floating window"  $ withFocused $ windows . W.sink)
        , ("M-S-t",  addName "Sink all floated windows"$ sinkAll)
        , ("M-S-f",  addName "Full Screen Window Focused with Padding"$ withFocused (sendMessage . maximizeRestore))
        , ("M-f",    addName "Full Screen Window Focused"             $sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)]
        --- Switch to workspace
        ^++^
        [ ("M-1",    addName "Switch to workspace 1"   $ (windows $ W.greedyView $ myWorkspaces !! 0))
        , ("M-2",    addName "Switch to workspace 2"   $ (windows $ W.greedyView $ myWorkspaces !! 1))
        , ("M-3",    addName "Switch to workspace 3"   $ (windows $ W.greedyView $ myWorkspaces !! 2))
        , ("M-4",    addName "Switch to workspace 4"   $ (windows $ W.greedyView $ myWorkspaces !! 3))
        , ("M-5",    addName "Switch to workspace 5"   $ (windows $ W.greedyView $ myWorkspaces !! 4))
        , ("M-6",    addName "Switch to workspace 6"   $ (windows $ W.greedyView $ myWorkspaces !! 5))
        , ("M-7",    addName "Switch to workspace 7"   $ (windows $ W.greedyView $ myWorkspaces !! 6))
        , ("M-8",    addName "Switch to workspace 8"   $ (windows $ W.greedyView $ myWorkspaces !! 7))
        , ("M-9",    addName "Switch to workspace 9"   $ (windows $ W.greedyView $ myWorkspaces !! 8))]
        --- Send window to workspace
        ^++^
        [ ("M-S-1",  addName "Send to workspace 1"     $ (windows $ W.shift $ myWorkspaces !! 0))
        , ("M-S-2",  addName "Send to workspace 2"     $ (windows $ W.shift $ myWorkspaces !! 1))
        , ("M-S-3",  addName "Send to workspace 3"     $ (windows $ W.shift $ myWorkspaces !! 2))
        , ("M-S-4",  addName "Send to workspace 4"     $ (windows $ W.shift $ myWorkspaces !! 3))
        , ("M-S-5",  addName "Send to workspace 5"     $ (windows $ W.shift $ myWorkspaces !! 4))
        , ("M-S-6",  addName "Send to workspace 6"     $ (windows $ W.shift $ myWorkspaces !! 5))
        , ("M-S-7",  addName "Send to workspace 7"     $ (windows $ W.shift $ myWorkspaces !! 6))
        , ("M-S-8",  addName "Send to workspace 8"     $ (windows $ W.shift $ myWorkspaces !! 7))
        , ("M-S-9",  addName "Send to workspace 9"     $ (windows $ W.shift $ myWorkspaces !! 8))]
        --- Increase/decrease windows in the master pane or the stack
        ^++^
        [ ("M-;",    addName "Increase clients in master pane" $ sendMessage (IncMasterN 1))
        , ("M-.",    addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1)))
        , ("M-=",    addName "Increase max # of windows for layout" $ increaseLimit)
        , ("M--",    addName "Decrease max # of windows for layout" $ decreaseLimit)]
        --- Window navigation
        ^++^
        [ ("M-j",    addName "Move focus to next window"              $ windows W.focusDown)
        , ("M-k",    addName "Move focus to prev window"              $ windows W.focusUp)
        , ("M-m",    addName "Move focus to master window"            $ windows W.focusMaster)
        , ("M-S-j",  addName "Swap focused window with next window"   $ windows W.swapDown)
        , ("M-S-k",  addName "Swap focused window with prev window"   $ windows W.swapUp)
        , ("M-<Return>",    addName "Swap focused window with master window" $ windows W.swapMaster)
        , ("M-<Backspace>", addName "Move focused window to master"   $ promote)
        , ("M-<Space>",     addName "Next Algorithm Layouts"          $ sendMessage NextLayout)
        , ("M-S-r",  addName "Rotate all windows except master"       $ rotSlavesDown)
        , ("M-C-r",  addName "Rotate all windows current stack"       $ rotAllDown)
        , ("M-S-.",  addName "Rotate all windows Up except Focused"   $ rotUnfocusedUp  )
        , ("M-S-,",  addName "Rotate all windows Down except Focused" $ rotUnfocusedDown)
        , ("M-g",    addName "Go window Focused To Empty Workspaces"  $ tagToEmptyWorkspace)
        , ("M-e",    addName "Find Empty Workspaces"                  $ viewEmptyWorkspace)
        , ("M-n",    addName "Hidden Window Focused"                  $ withFocused minimizeWindow              )
        , ("M-S-n",  addName "Show window Hidden"                     $ withLastMinimized maximizeWindowAndFocus)]
        --- Window resizing
        ^++^
        [ ("M-h",    addName "Shrink window"            $ sendMessage Shrink)
        , ("M-l",    addName "Expand window"            $ sendMessage Expand)
        , ("M-a",    addName "Expand window vertically" $ sendMessage MirrorExpand)
        , ("M-z",    addName "Shrink window vertically" $ sendMessage MirrorShrink)]
        --- Controll Window Float
        ^++^
        [ ("M-<L>",  addName "move float left"       $  withFocused (keysMoveWindow (-20,0)))
        , ("M-<R>",  addName "move float right"      $  withFocused (keysMoveWindow (20,0)))
        , ("M-<U>",  addName "move float up"         $  withFocused (keysMoveWindow (0,-20)))
        , ("M-<D>",  addName "move float down"       $  withFocused (keysMoveWindow (0,20)))
        , ("M-S-<L>",addName "shrink float at right" $  withFocused (keysResizeWindow (-20,0) (0,0)))
        , ("M-S-<R>",addName "expand float at right" $  withFocused (keysResizeWindow (20,0) (0,0)))
        , ("M-S-<D>",addName "expand float at bottom"$  withFocused (keysResizeWindow (0,20) (0,0)))
        , ("M-S-<U>",addName "shrink float at bottom"$  withFocused (keysResizeWindow (0,-20) (0,0)))
        , ("M-C-<L>",addName "expand float at left"  $  withFocused (keysResizeWindow (20,0) (1,0)))
        , ("M-C-<R>",addName "shrink float at left"  $  withFocused (keysResizeWindow (-20,0) (1,0)))
        , ("M-C-<U>",addName "expand float at top"   $  withFocused (keysResizeWindow (0,20) (0,1)))
        , ("M-C-<D>",addName "shrink float at top"   $  withFocused (keysResizeWindow (0,-20) (0,1)))]
        --- + and - Window Spacing
        ^++^
        [ ("M-C-j",  addName "Decrease window spacing"  $ decWindowSpacing 4)
        , ("M-C-k",  addName "Increase window spacing"  $ incWindowSpacing 4)
        , ("M-C-h",  addName "Decrease screen spacing"  $ decScreenSpacing 4)
        , ("M-C-l",  addName "Increase screen spacing"  $ incScreenSpacing 4)]
       --- XMonad Prompt (XPConfig)
        ^++^
        [ ("C-p p",  addName "Run Shell Command"        $ shellPrompt myXPConfig)
        , ("C-p m",  addName "Run Man Page Command"     $ manPrompt myXPConfig)
        , ("C-p g",  addName "Focused Window in Prompt" $ windowPrompt myXPConfig Goto wsWindows)
        , ("C-p b",  addName "Move Window To Current WS"$ windowPrompt myXPConfig Bring allWindows)]
       --- Prompt Search
        ^++^
        [ ("C-s d",  addName "search in XPrompt in DuckduckGo"$ promptSearchBrowser myXPConfig myBrowser duckduckgo)
        , ("C-s g",  addName "search in XPrompt in google"    $ promptSearchBrowser myXPConfig myBrowser google)
        , ("C-s y",  addName "search in XPrompt in youtube"   $ promptSearchBrowser myXPConfig myBrowser youtube)
        , ("C-s i",  addName "search in XPrompt in image"     $ promptSearchBrowser myXPConfig myBrowser images)
        , ("C-s p",  addName "search in XPrompt in github"    $ promptSearchBrowser myXPConfig myBrowser github)
        , ("C-s a",  addName "search in XPrompt in archwiki"  $ promptSearchBrowser myXPConfig myBrowser archwiki)
        , ("C-s u",  addName "search in XPrompt in aur"       $ promptSearchBrowser myXPConfig myBrowser aur)
        , ("C-s r",  addName "search in XPrompt in reddit"    $ promptSearchBrowser myXPConfig myBrowser reddit)
        , ("C-s w",  addName "search in XPrompt in wallhaven" $ promptSearchBrowser myXPConfig myBrowser wallhaven)]
       --- Prompt Launchers
        ^++^
        [ ("M-d",    addName "Run Programme with Rofi"  $ spawn "rofi -show drun -show-icons")
        , ("M-S-d",  addName "Run Software with dmenu"  $ spawn "dmenu_run -fn 'JetBrains Mono:style=Bold:pixelsize=14' -nb '#11121D' -nf '#7aa2f7' -sb '#7aa2f7' -sf '#11121D' -l 5 -p 'Execute:'")]
       --- Favorite programs
        ^++^
        [ ("M-S-<Return>", addName "Launch terminal"    $ spawn myTerminal)
        , ("M-w",    addName "Launch web browser"       $ spawn myBrowser)
        , ("<Home>", addName "Launch File Manager"      $ spawn "nemo")
        , ("M-r",    addName "Run Redshift"             $ spawn "redshift -O 3800K")
        , ("M-x",    addName "Stop Redshift"            $ spawn "redshift -x")]
        --- Scratchpads
        ^++^
        [ ("M-s t",  addName "Toggle scratchpad terminal"$ namedScratchpadAction myScratchPads "terminal")
        , ("M-s w",  addName "Toggle scratchpad Browser" $ namedScratchpadAction myScratchPads "browser" )]
        --- Multimedia keys
        ^++^
        [ ("<XF86AudioMute>", addName "Toggle audio mute"$spawn "pamixer -t && notify-send -t 200 'Toggle mute button!'")
        , ("<F9>",   addName "Raise vol"        $ spawn "pamixer -i 5 && ~/.config/xmobar/scripts/volume.sh")
        , ("<F8>",   addName "Lower vol"        $ spawn "pamixer -d 5 && ~/.config/xmobar/scripts/volume.sh")]
       --- Scripts
        ^++^
        [ ("M-S-w",  addName "Run Wifi Menu"    $ spawn "bash ~/.config/rofi/scripts/wifiMenu.sh" )
        , ("M-S-e",  addName "Run Power Menu"   $ spawn "bash ~/.config/rofi/scripts/powerMenu.sh")]
       --- ScreenShoot
        ^++^
        [ ("<Print>",   addName "Take Normal Screenshot"        $ spawn "scrot -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 2800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"     )
        , ("M-<Print>", addName "Take ScreenShot window Focused"$ spawn "scrot -u -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 2800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"  )
        , ("S-<Print>", addName "Take ScreenShot Selections "   $ spawn "scrot -s -F ~/pix/screen/%Y-%m-%d-%T-screenshot.png && notify-send -t 2800 'ScreenShot Takeen' 'Saved in ~/pix/screen/'"  )]

-------------------------------------------------------------------------------
-- EveryThings
-------------------------------------------------------------------------------
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (clickablePP (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))) defToggleStrutsKey
     . withUrgencyHook FocusHook
     . docks
     . addDescrKeys ((mod1Mask, xK_F1), xMessage) myKeys
     $ dynamicProjects projects myConfig
     	where
        myXmobarPP = def
	      -- Properties of current workspace
	    { ppCurrent = xmobarColor colorPrimary "" . wrap "<box type=Bottom width=2> " " </box>"
	      -- Properties of workspace on other monitor
	    , ppVisible = xmobarColor colorSecondary "" . wrap "<box type=Bottom width=2> " " </box>"
	      -- Properties of hidden workspaces without windows
	    , ppHiddenNoWindows = xmobarColor colorInactive ""
	      -- Properties of hidden WS (Active)
      , ppHidden = xmobarColor colorFG ""
        -- Urgent workspace
      , ppUrgent = xmobarColor colorSecondary "" . wrap "!" "!"
	      -- Type Of layout in xmobar
	    , ppLayout = xmobarColor colorInactive ""
	      -- Title of active window
	    , ppTitle = xmobarColor colorFG "" . shorten 40
	      -- Separator character
	    , ppSep =  "<fc=#3d85c6> <fn=1>\61762</fn> </fc>"
	      -- WS Separator
	    , ppWsSep = "  "
	      -- Number of windows on workspace
	    , ppExtras = [windowCount]
	      -- Order of things
	    , ppOrder  = \(ws:l:t:ex) -> ["<fn=1>" ++ ws ++ " </fn>"] ++ ex ++ ["<fc=" ++ colorInactive ++ "><fn=5>        " ++ l ++ "</fn></fc>  "]
      -- , ppOrder  = \(ws:l:t:ex) -> ["<fn=1>" ++ ws ++ " </fn>"] ++ ex ++ ["<fc=" ++ colorInactive ++ "> { " ++ l ++ " } </fc> " ++ t ]  -- With TitleWindow Focused
	    }
	    where
		colorBG :: String
		colorBG = "#1f1f1f"
		colorFG :: String
		colorFG = "#caa9fa"
		colorInactive :: String
		colorInactive = "#878787"
		colorPrimary :: String
		colorPrimary = "#3d85c6"
		colorSecondary :: String
		colorSecondary = "#c13e63"
		-- this is to show the number of windows in each workspace.
		windowCount :: X (Maybe String)
		windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
-------------------------------------------------------------------------------
--                     in  My Heart ==>                                    ---
-------------------------------------------------------------------------------
myConfig  = def { modMask                   = myAltMask
                , terminal                  = myTerminal
                , borderWidth               = myBorderWidth
                , focusedBorderColor        = myFocusedBorderColor
                , normalBorderColor         = myNormalBorderColor
                , focusFollowsMouse         = myFocusFollowsMouse
                , clickJustFocuses          = myClickJustFocuses
                , workspaces                = myWorkspaces
                , startupHook               = myStartupHook
                , layoutHook                = myLayoutHook
                , manageHook                = myManageHook
                , handleEventHook           = swallowEventHook (className =? "kitty") (return True)
                , logHook                   = updatePointer (0.5, 0.5) (0, 0)
                                            >> fadeWindowsLogHook myFadeHook
                }
