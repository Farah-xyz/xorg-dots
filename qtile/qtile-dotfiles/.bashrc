#####---------- EXPORTS ----------###
export BROWSER="qutebrowser"
export EDITOR="nvim"
export VISUAL="nvim"
export MYVIMRC="$HOME/.config/nvim/init.lua"
export VIMINIT="source $MYVIMRC"
# export LESSHISTFILE=-
# export HISTFILE="$HOME/.config/bash/.bash_history"
###---------- SHELL OPTIONS -------###
set -o vi
shopt -s autocd
###------------- PROMPT -----------###
GREEN="\e[1;92m"
CYAN="\e[1;36m"
NC='\033[0m'
export PS1="${CYAN}[\W]${GREEN} ❱❱❱${NC} "
###------------- FZF -----------###
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

###------------ ALIASES -----------###
## navigation
alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# Changing "ls" to "lsd"
alias ls='exa -l --color=always --group-directories-first'  # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -T --color=always --group-directories-first'	# tree listing
alias l.='exa -a | egrep "^\."'

## force confirm before overwriting something
alias cp="cp -f"
alias mv='mv -f'
alias rm='trash-put'

## bat && find
alias cat='bat'
alias find='fd'

## Editor Vim NeoVim
alias v='nvim'
alias vi="nvim"
alias nv="nvim"
alias vim="nvim"

## V && X && C & H & N
alias x='startx'
alias h='history'
alias n='clear && neofetch'
alias c='clear'
alias cc='clear'
alias ss='sxiv -b -t *'

## Drive
alias nm='nemo'
alias drive='nemo /run/media/frhxm/'
alias usb='cd /run/media/frhxm/ && ls'

## Pacman && yay
alias update='sudo pacman -Syu'
alias upgrade='sudo pacman -Syu && echo "UPGRADED"'
alias pr='sudo pacman -Rsn'
alias pc='sudo pacman -Scc && echo "Cache Is Clean Now"'

## Grub && (i can use hooks To auto update grub after update Linux or grub but idon't want)
alias install_grub='sudo grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=GRUB' # After Update Grub
alias update_grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'	# After Update Linux Karnel

## alias for configs
alias qtilec="nvim ~/.config/qtile/config.py"
alias fishc="nvim ~/.config/fish/config.fish"
alias xmonadc="nvim ~/.config/xmonad/xmonad.hs"
alias xmobarc="nvim ~/.config/xmobar/xmobar.hs"
alias bashc="nvim ~/.bashrc"
alias kittyc="nvim ~/.config/kitty/kitty.conf"
alias nvimc="nvim ~/.config/nvim/init.lua"
