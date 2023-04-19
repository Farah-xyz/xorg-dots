#!/bin/sh
userresources=$HOME/.Xresources
sysresources=/etc/X11/xinit/.Xresources

### merge in defaults and keymaps
# merge SysXresources
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
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

###########################################################
##################### Start WindowManager #################
###########################################################
## I am In Love with Qtile
exec /usr/bin/qtile start 2> /dev/null
