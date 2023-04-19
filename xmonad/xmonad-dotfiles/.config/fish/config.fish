##############################################################################
############################### Fish_Config ##################################
##############################################################################
if status is-interactive
	####################
	## greeting Message
	####################
	set fish_greeting # find Coustom in ~/.config/fish/functions/fish_greeting.fish

	######################################################
	## Set the cursor shapes for the different vi modes.
	######################################################
	set fish_cursor_default     block      blink
	set fish_cursor_insert      line       blink
	set fish_cursor_replace_one underscore blink
	set fish_cursor_visual      block

	#######################################################
	## Fuzzy Finder Fzf
	#######################################################
	set -gx FZF_DEFAULT_OPTS '--multi --reverse --border --preview "bat --color=always --style=numbers --line-range=:500 {}" --height 40 --color=bg+:#302D41,bg:#1E1E2E,spinner:#F8BD96,hl:#F28FAD --color=fg:#D9E0EE,header:#F28FAD,info:#DDB6F2,pointer:#F8BD96 --color=marker:#F8BD96,fg+:#F2CDCD,prompt:#DDB6F2,hl+:#F28FAD'
	set -gx FZF_DEFAULT_COMMAND 'fd --type f --exclude="**/.git/"'
	set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND "--type=f"
	set -gx FZF_CTRL_T_OPTS "--preview='bat --style=numbers --color=always {}'"
	set -gx FZF_ALT_C_COMMAND $FZF_DEFAULT_COMMAND "--type=d"
	set -gx FZF_ALT_C_OPTS "--preview='exa --tree {}'"
	# https://github.com/junegunn/fzf/wiki/Configuring-shell-key-bindings
	set -gx FZF_CTRL_R_OPTS "--preview='echo {}' --preview-window=down:3:hidden:wrap --bind='?:toggle-preview'"

	###########################
	## TokyoNight Color Palette
	###########################
	# Color Pallate
	set -l foreground c0caf5
	set -l selection 33467C
	set -l comment 565f89
	set -l red f7768e
	set -l orange ff9e64
	set -l yellow e0af68
	set -l green 9ece6a
	set -l purple 9d7cd8
	set -l cyan 7dcfff
	set -l pink bb9af7

	# Syntax Highlighting Colors
	set -g fish_color_normal $foreground
	set -g fish_color_command $cyan
	set -g fish_color_keyword $pink
	set -g fish_color_quote $yellow
	set -g fish_color_redirection $foreground
	set -g fish_color_end $orange
	set -g fish_color_error $red
	set -g fish_color_param $purple
	set -g fish_color_comment $comment
	set -g fish_color_selection --background=$selection
	set -g fish_color_search_match --background=$selection
	set -g fish_color_operator $green
	set -g fish_color_escape $pink
	set -g fish_color_autosuggestion $comment

	# Completion Pager Colors
	set -g fish_pager_color_progress $comment
	set -g fish_pager_color_prefix $cyan
	set -g fish_pager_color_completion $foreground
	set -g fish_pager_color_description $comment
end
#############################################################################

##############################################################################
################################### Alias ####################################
##############################################################################
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
alias install_grub='sudo grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=ARCH' # After Update Grub
alias update_grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'	# After Update Linux Karnel

## alias for configs
alias qtilec="nvim ~/.config/qtile/config.py"
alias fishc="nvim ~/.config/fish/config.fish"
alias kittyc="nvim ~/.config/kitty/kitty.conf"
alias nvimc="nvim ~/.config/nvim/init.lua"
