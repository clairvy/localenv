# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

export PATH=$HOME/local/bin:/usr/kerberos/bin:/usr/local/bin:/bin:/usr/bin:/sbin:/usr/sbin
export PAGER='lv -Ou'
export HISTSIZE=100000
export HISTFILESIZE=1000000

set -o ignoreeof
set -o noclobber
set -o notify
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s histappend

alias lv='command lv -Ou'
alias mv='mv -i'
alias cp='cp -ip'
alias mgdir='mkdir -m 775'
alias rm='rm -i'
alias ln='ln -n'
alias x='exit'
alias his='history|tail'

if [[ x"$TERM" == x"dumb" || x"$TERM" == x"sun" || x"$TERM" == x"emacs" ]]; then
  use_color=
else
  use_color='true'
fi

if [[ x"$user_color" != x"true" ]]; then
  alias ls='ls -AF --color=auto'
  alias grep='grep --color=auto'
  alias egrep='egrep --color=auto'
else
  alias ls='ls -AF'
fi

# keybindings
stty -ixon
stty werase undef
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'
bind '"\C-w": kill-region'
