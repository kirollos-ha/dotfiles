# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

set -o vi
export EDITOR='/usr/bin/vim'
export VISUAL='/usr/bin/vim'
export XDG_CONFIG_HOME="$HOME/.config"

alias ls='ls --color'
alias findmacs='ps -e | grep emacs'

alias nano='emacsclient -nw'

alias diocane='cowsay \"diocane\"'

PLAN9="/home/diccu/.local/plan9"
export PLAN9
PATH="$PATH:$PLAN9:$PLAN9/bin"

. "$HOME/.cargo/env"
PATH="$PATH:$Home/.cargo/bin"
PATH="$PATH:$HOME/.roswell/bin"

export PATH
unset rc

export XDG_CONFIG_DIRS="$XDG_CONFIG_DIRS:$HOME/.config"

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/usr/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
    # eval "$__conda_setup"
# else
    # if [ -f "/usr/etc/profile.d/conda.sh" ]; then
        # . "/usr/etc/profile.d/conda.sh"
    # else
        # export PATH="/usr/bin:$PATH"
    # fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<
# # 
# 
# Automatically added by the Gui  install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
     if [[ $PS1 =~ (.*)"\\$" ]]; then
         PS1="${BASH_REMATCH[1]} [env]\\\$ "
     fi
fi
# 
