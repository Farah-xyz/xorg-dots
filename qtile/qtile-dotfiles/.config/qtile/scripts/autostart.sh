#!/usr/bin/sh
# APP DEMON #
#-----------#
/usr/bin/dunst &
/usr/bin/unclutter &
/usr/bin/picom -b -f &
/usr/bin/udiskie -s -N &
# SYSTEMTRAY #
#------------#
/usr/bin/nm-applet &
#/usr/bin/blueman-applet &
# OTHERS #
#--------#
xwallpaper --zoom ~/pix/wall/myGirl2.jpg &
xset r rate 200 80 &
