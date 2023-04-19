#!/usr/bin/sh
sudo pacman -S --needed xorg-server xorg-xinit xorg-apps xwallpaper qtile kitty rofi \
	firefox qutebrowser \
	neovim sxiv mpv \
	nemo udisks2 udiskie dosfstools ntfs-3g \
	gvfs gvfs-mtp gvfs-smb gvfs-afc gvfs-nfs gvfs-gphoto2 \
	slock dunst libnotify \
	fzf fd exa bat xclip trash-cli man-db imagemagick \
	arc-gtk-theme papirus-icon-theme lxappearance-gtk3 unclutter picom \
	pipewire pipewire-pulse wireplumber pipewire-alsa pipewire-jack pipewire-zeroconf alsa-utils pulsemixer \
	bluez bluez-utils blueman network-manager-applet \
	ttf-dejavu ttf-font-awesome ttf-jetbrains-mono \
	xf86-video-intel xdg-user-dirs pacman-contrib neofetch
