#!/usr/bin/env bash
                            # == MY BASE PKG INSTALLER [ Arch ] == #
sudo pacman -Syy --needed xorg-server xorg-apps xorg-xinit xorg-xmessage xwallpaper xmonad xmonad-contrib xmobar kitty \
    firefox qutebrowser \
    dmenu rofi \
    fzf fd lsd bat xclip trash-cli man-db imagemagick xdotool acpi \
    scrot slock dunst libnotify \
    neovim sxiv mpv \
    nemo udisks2 udiskie \
    gvfs gvfs-mtp gvfs-smb gvfs-afc gvfs-nfs gvfs-gphoto2 \
    zip unzip unrar dosfstools ntfs-3g \
    unclutter picom lxappearance-gtk3 redshift \
    arc-gtk-theme papirus-icon-theme \
    nodejs npm \
    ttf-dejavu ttf-font-awesome ttf-jetbrains-mono \
    pipewire pipewire-pulse wireplumber pipewire-alsa pipewire-jack pipewire-zeroconf pamixer pulsemixer \
    bluez bluez-utils blueman network-manager-applet stalonetray \
    xf86-video-intel xdg-user-dirs pacman-contrib neofetch htop
