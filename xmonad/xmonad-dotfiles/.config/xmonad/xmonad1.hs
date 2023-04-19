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
import XMonad.Util.EZConfig (additionalKeysP)
--- Layouts/Modifiers
import XMonad.Layout.Magnifier
import XMonad.Layout.CenteredIfSingle
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
myFileManager        = "nemo"       :: String     -- File Manager
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
-- Set Layouts Config
------------------------------------------------------------------------------
mySpacings   = spacingRaw False (Border 0 5 5 5) True (Border 5 5 5 5) True
myGaps       = gaps [(U, 2),(D, 5),(L, 10),(R, 10)]
mySWNTheme   = def
             { swn_font              = myNerdFontBig
             , swn_fade              = 1.0
             , swn_bgcolor           = "#11121D"
             , swn_color             = "#7aa2f7"
             }
myLayoutHook = showWName' mySWNTheme
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ allLayouts
               where
                 allLayouts = tall ||| threeColMid ||| oneBig
------------------------------------------------------------------------------
-- Layout Algorithms
------------------------------------------------------------------------------
tall            = renamed [Replace "TILD"]
                $ maximizeWithPadding 5
                $ mySpacings
                $ ResizableTall 1 (3/100) (1/2) []

threeColMid     = renamed [Replace "MID"]
                $ maximizeWithPadding 5
                $ mySpacings
                $ centeredIfSingle 0.7 0.8
                $ magnifiercz' 1.3 $ ThreeColMid 1 (3/100) (1/2)

oneBig          = renamed [Replace "ONEBIG"]
                $ maximizeWithPadding 5
                $ minimize
                $ mySpacings
                $ OneBig (3/4) (3/4)

-------------------------------------------------------------------------------
-- Custom Keys
-------------------------------------------------------------------------------
        -- ================== System Control =======================
myKeys = [ ("M-q",   spawn "xmonad --recompile; xmonad --restart")
         , ("M-S-q", confirmPrompt myXPConfig "Quit XMonad" $ io exitSuccess)
         , ("M-S-a", confirmPrompt myXPConfig "kill All"    $ killAll)
         , ("M-S-o", confirmPrompt myXPConfig "kill Others" $ killOthers)
         , ("M-S-c", kill)
         , ("M-S-t", sinkAll)
         , ("M-t",   withFocused $ windows . W.sink)
         , ("M-S-f", withFocused (sendMessage . maximizeRestore))
         , ("M-<Space>", sendMessage NextLayout)
        -- ================ Switch between windows ===================
         , ("M-<Tab>",windows W.focusDown)
         , ("M-j",   windows W.focusDown)
         , ("M-k",   windows W.focusUp)
         , ("M-m",   windows W.focusMaster)
        -- ==================== Move windows ========================
         , ("M-<Return>", windows W.swapMaster)
         , ("M-S-j", windows W.swapDown)
         , ("M-S-k", windows W.swapUp)
         , ("M-S-.", rotUnfocusedUp)
         , ("M-S-,", rotUnfocusedDown)
         , ("M-S-r", rotSlavesDown)
         , ("M-C-r", rotAllDown)
        -- ==================== Shrink Windows ========================
         , ("M-C-h", sendMessage Shrink)
         , ("M-C-l", sendMessage Expand)
         , ("M-C-j", sendMessage MirrorShrink)
         , ("M-C-k", sendMessage MirrorExpand)
         , ("M-,",   sendMessage (IncMasterN 1))
         , ("M-.",   sendMessage (IncMasterN (-1)))
        -- ===================== Control Float Window  ================
         , ("M-<L>",  withFocused (keysMoveWindow (-20,0))) -- move float left
         , ("M-<R>",  withFocused (keysMoveWindow (20,0)))  -- move float right
         , ("M-<U>",  withFocused (keysMoveWindow (0,-20))) -- move float up
         , ("M-<D>",  withFocused (keysMoveWindow (0,20)))  -- move float down
         , ("M-S-<L>",withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
         , ("M-S-<R>",withFocused (keysResizeWindow (20,0) (0,0)))  --expand float at right
         , ("M-S-<D>",withFocused (keysResizeWindow (0,20) (0,0)))  --expand float at bottom
         , ("M-S-<U>",withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
         , ("M-C-<L>",withFocused (keysResizeWindow (20,0) (1,0)))  --expand float at left
         , ("M-C-<R>",withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
         , ("M-C-<U>",withFocused (keysResizeWindow (0,20) (0,1)))  --expand float at top
         , ("M-C-<D>",withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top
        -- =============== XMonad Prompt (XP) =====================
         , ("C-p p", shellPrompt myXPConfig)
         , ("C-p m", manPrompt myXPConfig)
         , ("C-p g", windowPrompt myXPConfig Goto wsWindows)
         , ("C-p b", windowPrompt myXPConfig Bring allWindows)
        -- ================= XMonad Prompt Search XPS =============
         , ("C-s d", promptSearchBrowser myXPConfig myBrowser duckduckgo)
         , ("C-s g", promptSearchBrowser myXPConfig myBrowser google)
         , ("C-s y", promptSearchBrowser myXPConfig myBrowser youtube)
         , ("C-s i", promptSearchBrowser myXPConfig myBrowser images)
         , ("C-s p", promptSearchBrowser myXPConfig myBrowser github)
         , ("C-s a", promptSearchBrowser myXPConfig myBrowser archwiki)
         , ("C-s u", promptSearchBrowser myXPConfig myBrowser aur)
         , ("C-s r", promptSearchBrowser myXPConfig myBrowser reddit)
         , ("C-s w", promptSearchBrowser myXPConfig myBrowser wallhaven)
        -- ====================  XMonad Extras ========================
         , ("M-g", tagToEmptyWorkspace)
         , ("M-e", viewEmptyWorkspace)
         , ("M-C-<Return>", promote)
        -- ======================= Meltimedia  ========================
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
         , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
         , ("<XF86MonBrightnessUp>",  spawn "xbacklight -inc 30")
         , ("<XF86MonBrightnessDown>",spawn "xbacklight -dec 5")
        -- ====================== Scratchpads  ========================
         , ("M-s t", namedScratchpadAction myScratchPads "terminal")
         , ("M-s s", namedScratchpadAction myScratchPads "cmus"    )
         , ("M-s w", namedScratchpadAction myScratchPads "browser" )
        -- ======================= Programs  =========================
         , ("M-S-<Return>", spawn myTerminal)
         , ("M-<Home>",spawn myFileManager)
         , ("M-w",     spawn myBrowser)
         , ("M-p",     spawn myLauncher)
         , ("M-S-e",   spawn myPowerMenu)
         ]
         where
        myLauncher   = "rofi -show drun -show-icons"
        myPowerMenu  = "bash /home/frhxm/.config/rofi/scripts/powerMenu.sh"
-------------------------------------------------------------------------------
--                     in  My Heart ==>                                    ---
-------------------------------------------------------------------------------
main = xmonad
  -- . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (clickablePP (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))) defToggleStrutsKey
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
		colorBG = "#1f1f1f"
		colorFG = "#caa9fa"
		colorInactive = "#878787"
		colorPrimary = "#3d85c6"
		colorSecondary = "#c13e63"
		-- this is to show the number of windows in each workspace.
		windowCount :: X (Maybe String)
		windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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
                } `additionalKeysP` myKeys
