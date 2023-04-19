#!/bin/sh
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

### merge in defaults and keymaps
# merge SysXresources
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

# merge XModamp
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# merge Xresources
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

## start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

#------------- App Demon --------------
## Auto Mount devaice
/usr/bin/unclutter &
## Auto Hideen Mouse
/usr/bin/udiskie &
## Notify Demon
/usr/bin/dunst &

#-------------- SystemTray ------------
################################
#### Start bluetooth applte
################################
#/usr/bin/blueman-applet &

################################
## Start Network Manager applte
################################
/usr/bin/nm-applet & # --no-agent to fix worning destroy in tty1

################################
## SystemTray (stalonetray)
################################
/usr/bin/stalonetray &
#----------- end System Tray ----------

###########################################################
##################### Start WindowManager #################
###########################################################
## I am In Love with XMonad
exec /usr/bin/xmonad 2> /dev/null
