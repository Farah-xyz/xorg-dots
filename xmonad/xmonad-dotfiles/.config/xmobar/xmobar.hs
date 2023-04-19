-- Xmobar (http://projects.haskell.org/xmobar/)
-- This is one of the xmobar configurations for DTOS.
-- This config is packaged in the DTOS repo as 'dtos-xmobar'
-- Color scheme: Dracula
-- Dependencies:
   -- ttf-font-awesome
   -- ttf-jetbrains-mono
   -- ttf-FiraCode Nerd Font
   -- htop
   -- pacman (Arch Linux)
   -- pacman-contrib (checkupdate)
   -- trayer

Config {
         font                 = "xft:JetBrains Mono:weight=bold:pixelsize=10:antialias=true:hinting=true"
       , additionalFonts      =  [ "xft:FiraCode Nerd Font Mono:weight=Bold:pixelsize=18"
                               , "xft:Font Awesome 6 Free Solid:pixelsize=12"
                               , "xft:Font Awesome 6 Brands:pixelsize=12"
                               , "xft:Noto Sans Mono CJK JP:style=Bold:pixelsize=13"
                               , "xft:JetBrains Mono:weight=Bold:pixelsize=14:antialias=true:hinting=true"
                               ]
       , bgColor        = "#11121D"
       , fgColor        = "#a9b1d6"
       , alpha          = 233
       , position       = Static {xpos = 18, ypos = 3, width = 1810, height = 35}
       , border         = BottomB
       , borderWidth    = 0
       , borderColor    = "#1d2021"
       , lowerOnStart   = True
       , hideOnStart    = False
       , allDesktops    = False
       , overrideRedirect = True,
       , pickBroadest   = False,
       , persistent     = True
       , iconRoot       = ".config/xmobar/xpm/" --path icons XPM
       , commands       = [
                        -- Echos a "volume " icon
                      Run Com "echo" ["<fn=2>\xf028</fn>"] "volicon" 3600
                        -- Get volume with pamixer --get-volume-human
                    , Run Com "pamixer" ["--get-volume-human"] "volume" 1
                        -- Echos a "penguin" icon in front of the kernel output.
                    , Run Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
                        -- Get kernel version
                    , Run Com "uname" ["-r"] "kernel" 36000
                        -- Cpu usage in percent
                    , Run Cpu ["-t", "<fn=2>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
                        -- Ram used number and percent
                    , Run Memory ["-t", "<fn=2>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
                        -- Disk space free
                    , Run DiskU [("/", "<fn=2>\xf0c7</fn>  hdd: <free> free")] [] 60
                        -- Echos an "up arrow" icon in front of the uptime output.
                    , Run Com "echo" ["<fn=2>\xf0aa</fn>"] "uparrow" 3600
                        -- Uptime
                    , Run Uptime ["-t", "uptime: <days>d <hours>h"] 360
                        -- Echos a "bell" icon in front of the pacman updates.
                    , Run Com "echo" ["<fn=2>\xf0f3</fn>"] "bell" 3600
                        -- Check for pacman updates (/home/frhxm/xmobar/scripts/pacupdate)
                    , Run Com ".config/xmobar/scripts/pacupdate.sh" [] "pacupdate" 360
                        -- Echos a "battery" icon in front of the pacman updates.
                    , Run Com "echo" ["<fn=2>\xf242</fn>"] "baticon" 3600
                        -- Battery
                    , Run BatteryP ["BAT0"] ["-t", "<acstatus><watts> (<left>%)"] 360
                        -- Time and date
                    , Run Date "<fn=2>\xf017</fn>  %b %d %Y - (%H:%M) " "date" 50
                        -- Script that dynamically adjusts xmobar padding depending on number of trayer icons.
                    -- , Run Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
                        -- Prints out the left side items such as workspaces, layout, etc.
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "  <icon=arch.xpm/>  | %UnsafeXMonadLog% }{ <box type=Bottom width=2 mb=2 color=#bd93f9><fc=#bd93f9>%penguin%  %kernel%</fc></box>    <box type=Bottom width=2 mb=2 color=#f1fa8c><fc=#f1fa8c><action=`kitty -e htop`>%cpu%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#ff5555><fc=#ff5555><action=`kitty -e htop`>%memory%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#5af78e><fc=#5af78e>%disku%</fc></box>    <box type=Bottom width=2 mb=2 color=#ff79c6><fc=#ff79c6>%uparrow%  %uptime%</fc></box>    <box type=Bottom width=2 mb=2 color=#8be9fd><fc=#8be9fd>%bell%  <action=`kitty -e sudo pacman -Syu`>%pacupdate%</action></fc></box>   <box type=Bottom width=2 mb=2 color=#ff6e67><fc=#ff6e67>%baticon%  %battery%</fc></box>    <box type=Bottom width=2 mb=2 color=#bd93f9><fc=#bd93f9><action=`pamixer -t`>%volicon% %volume%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#caa9fa><fc=#caa9fa>%date%</fc></box>"
       }

