#! /bin/sh

chosen=$(printf "  Power Off\n  Restart\n  Lock" | rofi -dmenu -i -theme-str '@import "power.rasi"' -p " Good Bye My Friend  ")

case "$chosen" in
	"  Power Off") systemctl poweroff ;;
	"  Restart") systemctl reboot ;;
	"  Lock") slock ;;
	*) exit 1 ;;
esac
