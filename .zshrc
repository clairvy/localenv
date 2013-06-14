#!/usr/bin/env zsh
# -*- coding: utf-8-unix; sh-basic-offset: 2; -*-

stty -ixon
stty -istrip
bindkey -e
bindkey '^W' kill-region

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=10000000

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
autoload is-at-least
if is-at-least 4.3.10; then
  bindkey "^R" history-incremental-pattern-search-backward
  bindkey "^S" history-incremental-pattern-search-forward
else
  bindkey "^R" history-incremental-search-backward
  bindkey "^S" history-incremental-search-forward
fi

# 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt append_history
# シェルのプロセスごとに履歴を共有
setopt share_history
# 履歴ファイルに時刻を記録
setopt extended_history
# history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
# 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_dups
# 重複したヒストリは追加しない
setopt hist_ignore_all_dups
# incremental append
setopt inc_append_history

# ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_cd
# cdのタイミングで自動的にpushd
setopt auto_pushd
setopt pushd_ignore_dups

# fpath の追加
fpath=(~/.zfunctions/Completion ${fpath})
# unfunction して，autoload する
function reload_function() {
  local f
  f=($HOME/.zfunctions/Completion/*(.))
  unfunction $f:t 2> /dev/null
  autoload -U $f:t
}
# 補完設定
autoload -Uz compinit; compinit -u

# ファイルリスト補完でもlsと同様に色をつける｡
export LSCOLORS=GxFxCxdxBxegedabagacad
export LS_COLORS='di=01;36:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' group-name ''
zstyle ':completion:*:default' use-cache true
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:default' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':compinstall' filename '/home/nagaya/.zshrc'
zstyle ':completion:*:processes' command 'ps x'
# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                                           /usr/sbin /usr/bin /sbin /bin

# 補完候補が複数ある時に、一覧表示
setopt auto_list
# 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_menu
# ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
# C-s, C-qを無効にする。
setopt NO_flow_control
# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit
# カッコの対応などを自動的に補完
setopt auto_param_keys
# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
# 最後がディレクトリ名で終わっている場合末尾の / を自動的に取り除く
setopt auto_remove_slash
# {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
# コマンドのスペルチェックをする
setopt correct
# =command を command のパス名に展開する
setopt equals
# シェルが終了しても裏ジョブに HUP シグナルを送らないようにする
setopt NO_hup
# Ctrl+D では終了しないようになる（exit, logout などを使う）
setopt ignore_eof
# コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
# auto_list の補完候補一覧で、ls -F のようにファイルの種別をマーク表示しない
setopt list_types
# 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs
# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst
# ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
# 複数のリダイレクトやパイプなど、必要に応じて tee や cat の機能が使われる
setopt multios
# ファイル名の展開で、辞書順ではなく数値的にソートされるようになる
setopt numeric_glob_sort
# for, repeat, select, if, function などで簡略文法が使えるようになる
setopt short_loops
#コピペの時rpromptを非表示する
setopt transient_rprompt
# 文字列末尾に改行コードが無い場合でも表示する
unsetopt promptcr
# リダイレクトでファイルを消さない
setopt no_clobber

setopt notify
setopt print_exit_value

# 状態変数
local os='unknown'
local uname_s=`uname -s`
if [[ $uname_s == "Darwin" ]]; then
  os='mac'
elif [[ $uname_s == "SunOS" ]]; then
  os='sun'
elif [[ $uname_s == "FreeBSD" ]]; then
  os='bsd'
elif [[ $uname_s == "Linux" ]]; then
  os='lin'
elif [[ $uname_s == "CYGWIN_NT-5.1" ]]; then
  os='win'
fi
[ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`

# すごいプロンプト
setopt prompt_subst
autoload -U colors; colors

autoload -Uz add-zsh-hook
if [[ $ZSH_VERSION != [1-3].* && $ZSH_VERSION != 4.[12].* ]]; then
  autoload -Uz vcs_info
  zstyle ':vcs_info:*' enable git svn hg bzr
  zstyle ':vcs_info:*' formats '(%s)-[%b]'
  zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
  zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
  zstyle ':vcs_info:bzr:*' use-simple true
  if is-at-least 4.3.10; then
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' stagedstr '+'
    zstyle ':vcs_info:git:*' unstagedstr '-'
    zstyle ':vcs_info:git:*' formats '(%s)-[%c%u%b]'
    zstyle ':vcs_info:git:*' actionformats '(%s)-[%c%u%b|%a]'
  fi
  function _update_vcs_info_msg() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    psvar[2]=$(_git_not_pushed)
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
  }
  add-zsh-hook precmd _update_vcs_info_msg
  function _git_not_pushed() {
    if [[ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = "true" ]]; then
      head="$(git rev-parse HEAD)"
      for x in $(git rev-parse --remotes); do
        if [[ "$head" = "$x" ]]; then
          return 0
        fi
      done
      echo "{?}"
    fi
    return 0
  }
fi

if [[ x"$os" == x"win" ]]; then
  export TERM=cygwin
fi

if [[ x"$TERM" == x"dumb" || x"$TERM" == x"sun" || x"$TERM" == x"emacs" ]]; then
  use_color=
else
  use_color='true'
fi

if [[ x"$use_color" != x"true" ]]; then
  PROMPT='%U%B%n@%m%b %h %#%u '
  RPROMPT=
else
  local prompt_color='%{[32m%}'
  local clear_color='%{[0m%}'
  local rprompt_color='%{[33m%}' # yellow [0m
  local vcs_prompot_color='%{[32m%}' # green [0m
  local prompt_char='$'
  if [[ x"$USER" == x"s-nag" || x"$USER" == x"nagaya" || x"$USER" == x"s_nag" || x"$USER" == x"nag" ]]; then
    prompt_color='%{[32m%}'      # green [0m
  elif [[ x"$USER" == x"root" ]]; then
    prompt_color='%{[37m%}'      # white [0m
    prompt_char='#'
  else
    prompt_color='%{[35m%}'      # pink [0m
  fi
  PROMPT=$prompt_color'%U%B%n'$rprompt_color'%U@'$prompt_color'%B%m%b %h '$prompt_char$clear_color'%u '
  RPROMPT=$vcs_prompot_color'%1(v|%1v%2v|)${vcs_info_git_pushed} '$rprompt_color'[%~]'$clear_color
fi

if whence -p lv 2>&1 > /dev/null; then
  if [[ $TERM_PROGRAM == "iTerm.app" ]]; then
    alias lv='command lv -Ou'
  fi
  export PAGER='lv -Ou'
  alias lc='lv | cat'
fi
if whence -p tmux 2>&1 > /dev/null; then
  function tmux() { if command tmux list-clients > /dev/null; then command tmux attach; else command tmux; fi }
  alias tml='command tmux list-sessions'
fi

if whence -p xsbt 2>&1 > /dev/null; then
  function sbt() {
    if [ *.sbt(N) ]; then
      command xsbt "$@";
    else
      command sbt "$@";
    fi
  }
fi

# default path
path=(/usr/bin /bin)

# for sbin
if [[ -d /sbin ]];then
  path=($path /sbin)
fi
if [[ -d /usr/sbin ]];then
  path=($path /usr/sbin)
fi
# /usr/local
if [[ -d /usr/local/sbin ]]; then
  path=(/usr/local/sbin $path)
fi
if [[ -d /usr/local/bin ]]; then
  path=(/usr/local/bin $path)
fi
if [[ -d /usr/local/share/man ]]; then
  manpath=(/usr/local/share/man $manpath)
fi
# rbenv
if [[ -d /usr/local/opt/rbenv ]]; then
  export RBENV_ROOT=/usr/local/opt/rbenv
  if [[ -r /usr/local/opt/rbenv/completions/rbenv.zsh ]]; then
    source "/usr/local/opt/rbenv/completions/rbenv.zsh"
  fi
  if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
  fi
fi
if [[ -d $HOME/.rbenv/bin ]]; then
  path=($HOME/.rbenv/bin $path)
  eval $(rbenv init -)
  if [[ -r $HOME/.rbenv/completions/rbenv.zsh ]]; then
    source "$HOME/.rbenv/completions/rbenv.zsh"
  fi
fi
if [[ -f /etc/profile.d/rbenv.sh ]]; then
  . /etc/profile.d/rbenv.sh
fi

# for Mac ports
if [[ $os == 'mac' ]]; then
  export LC_ALL=ja_JP.UTF-8
  if [[ -d /opt/local/bin ]]; then
    path=(/opt/local/bin /opt/local/sbin $path)
    manpath=(/opt/local/share/man $manpath)
  fi
fi
# for BSDPAN and local path
if [[ $os == 'bsd' ]]; then
  path=($path /usr/local/bin:/usr/local/sbin)
  manpath=($manpath /usr/local/share/man /usr/local/man)
  export PKG_DBDIR=$HOME/local/var/db/pkg
  export PORT_DBDIR=$HOME/local/var/db/pkg
  export INSTALL_AS_USER
  export LD_LIBRARY_PATH=$HOME/local/lib
fi
# for csw
if [[ $os == 'sun' && -d /opt/csw/bin ]]; then
  path=(/opt/csw/bin $path)
fi

# for local::lib
local_lib_path="$HOME/perl5"
function _set_perl_env () {
  export PERL_LOCAL_LIB_ROOT="${local_lib_path}";
  export PERL_MM_OPT="INSTALL_BASE=${local_lib_path}"
  export PERL_MB_OPT="--install_base ${local_lib_path}"
  export PERL5LIB="${local_lib_path}/lib/perl5:${local_lib_path}/lib/perl5/$site"
  export PERL_CPANM_OPT="--local-lib=${local_lib_path}"
  path=(${local_lib_path}/bin $path)
}
if [[ "x$HOSTNAME" == "xdv1" ]]; then
  function set_perl_env () {
    local site='i486-linux-gnu-thread-multi'
    _set_perl_env
  }
  set_perl_env
elif [[ $os == 'mac' ]]; then
  function set_perl_env () {
    local site='darwin-multi-2level'
    _set_perl_env
  }
  function set_perl_env_wx () {
    local site='darwin-thread-multi-2level'
    _set_perl_env
  }
  set_perl_env
elif [[ $os == 'bsd' ]]; then
  function set_perl_env () {
    local site='i386-freebsd-64int'
    _set_perl_env
  }
  set_perl_env
elif [[ x"$HOSTNAME" == x'kaname' ]]; then
  function set_perl_env() {
    local site='i686-linux-gnu-thread-multi'
    _set_perl_env
  }
  set_perl_env
elif [[ -d $local_lib_path ]]; then
  function set_perl_env() {
    local site=`perl -V:archname | awk -F\' '{print $2}'`
    _set_perl_env
  }
  set_perl_env
fi

if [[ -d $HOME/local ]]; then
  path=($HOME/local/bin $HOME/local/sbin $path)
  manpath=($HOME/local/man $manpath)
fi

# for cabal
if [[ -d $HOME/.cabal/bin ]]; then
  path=($HOME/.cabal/bin $path)
fi

# for gems
if [[ -d /var/lib/gems/1.8/bin ]]; then
  path=($path /var/lib/gems/1.8/bin)
fi
# for gisty
export GISTY_DIR="$HOME/work/gists"

# for perl Devel::Cover
alias cover='cover -test -ignore "^inc/"'
# for perl Test::Pod::Coverage
export TEST_POD=1
# for perldoc
if [[ $os == 'mac' ]]; then
  alias perldoc='perldoc -t'
fi
# for scaladoc
export SCALA_DOC_HOME=/Users/s_nag/s/app/InteractiveHelp/scala-2.7.5-apidocs-fixed/

# ignore mailcheck
export MAILCHECK=0

# set tmpdir
export TMPDIR=/var/tmp

# alias
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -ip'
alias ln='nocorrect ln'
alias mkdir='nocorrect mkdir'
alias mgdir='nocorrect mkdir -m 775'
alias rm='rm -i'
alias history='builtin history -Di'
alias his='history | tail'
if [[ $use_color == 'true' ]]; then
  if [[ $os == 'mac' || $os == 'bsd' ]]; then
    alias ls='command ls -AFG'
  elif [[ $os == 'sun' ]]; then
    alias ls='command ls -AF'
  else
    alias ls='command ls -AF --color=auto --show-control-chars'
  fi
else
  alias ls='command ls -AF'
fi
alias ln='ln -n'
alias x='exit'
alias first_release="perl -mModule::CoreList -le 'print Module::CoreList->first_release(@ARGV)'"
alias first_number_sort="perl -e '@l=<>;print(map{\$_->[1]}sort{\$a->[0]<=>\$b->[0]}map{[do{m/(\d+)/;\$1},\$_]}@l)'"
alias screen='command screen -U'
alias scc='screen'
alias scx='screen -x'
alias hex='perl -le "print unpack q(H*), shift"'
alias grep='grep --color'
alias egrep='egrep --color'
if whence -p vim 2>&1 > /dev/null; then
  alias vi=vim
  export EDITOR=vim
else
  export EDITOR=vi
fi
if [[ $os == 'mac' ]]; then
  if [[ -f /opt/local/var/macports/software/emacs-app/23.1_1/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ]]; then
    alias emacs-app='/opt/local/var/macports/software/emacs-app/23.1_1/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
    alias emacsclient='/opt/local/var/macports/software/emacs-app/23.1_1/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient'
  elif [[ -f /usr/local/Cellar/emacs/HEAD/Emacs.app/Contents/MacOS/Emacs ]]; then
    alias emacs-app='/usr/local/Cellar/emacs/HEAD/Emacs.app/Contents/MacOS/Emacs'
    alias emacsclient='/usr/local/Cellar/emacs/HEAD/Emacs.app/Contents/MacOS/bin/emacsclient'
  fi
  local jfe='-Dfile.encoding=UTF-8'
  alias javac="javac -J$jfe -Xlint:unchecked -Xlint:deprecation"
  alias java="java $jfe"
  alias jarsigner="jarsigner -J$jfe"
  export ANT_OPTS="$jfe"
  export MAVEN_OPTS="$jfe"
  export SETUP_SH_VMARGS="$jfe"
  if [[ -f /Applications/MacVim.app/Contents/MacOS/Vim ]]; then
    alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
    export EDITOR='/Applications/MacVim.app/Contents/MacOS/Vim'
  fi
  if [[ -d /usr/share/terminfo ]]; then
    export TERMINFO='/usr/share/terminfo'
  fi
fi
if whence -p vim 2>&1 > /dev/null; then
  alias vi=vim
  if [[ x"$EDITOR" = x"" ]]; then
    export EDITOR=vim
  fi
fi
export MYSQL_PS1='([32m\u[00m@[33m\h[00m) [34m[\d][00m > '
if whence -p mysql 2>&1 > /dev/null; then
  alias mysql='mysql --auto-rehash'
fi
alias phpcs='phpcs --standard=Symfony'
if whence -p git 2>&1 > /dev/null; then
  alias ga='git add'
  alias gad='git add .'
  alias gap='git add -p'
  alias gb='git branch'
  alias gbd='git branch -d'
  alias gbD='git branch -D'
  alias gco='git checkout'
  alias gc='git commit'
  alias gcm='git commit -m'
  alias gca='git commit --amend'
  alias gsc='git config -l'
  alias gsa='git config -l | egrep alias'
  alias gd='git diff'
  alias gd1='git diff HEAD~'
  alias gds='git diff --staged'
  alias gdsw='git diff --staged -w'
  alias gdw='git diff -w'
  alias gw='git diff -w'
  alias gf='git fetch'
  alias gl='git log'
  alias gla='git log --graph --all --color --pretty='\''%x09%h %cn%x09%s %Cred%d%Creset %C(green)- %cr%Creset'\'''
  alias g1='git log --graph -n 20 --pretty=format:'\''%C(yellow)%h%C(cyan)%d%Creset %s %C(green)- %an, %cr%Creset'\'''
  alias gp='git push'
  alias gpn='git push -n'
  alias gr='git rebase'
  alias gri='git rebase -i'
  alias grom='git rebase origin/master master'
  alias grm='git rebase master'
  alias ghh='git reset --hard'
  alias gx='git rm'
  alias gs='git status'
  alias gls='git status' # for gnu ls not to use
  alias gst='git status -sb'
  alias gsu='git submodule update'
  alias gsui='git submodule update --init'
fi

for c in ocaml gosh clisp; do
  if whence -p $c 2>&1 > /dev/null; then
    if whence -p rlwrap 2>&1 > /dev/null; then
      alias $c="command rlwrap $c"
    fi
  fi
done
if whence -p scala 2>&1 > /dev/null; then
  if whence -p rlwrap 2>&1 > /dev/null; then
    alias scala='rlwrap scala -deprecation'
  else
    alias scala='scala -deprecation'
  fi
fi
if whence -p dart 2>&1 > /dev/null; then
  alias dart='dart --checked'
fi

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

# Ctrl+wで､直前の/までを削除する｡
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# href の補完
compctl -K _href href
functions _href () {
  local href_datadir=`href_datadir`
  reply=(`cat $href_datadir/comptable|awk -F, '{print $2}'|sort|uniq`)
  # /usr/share/href/comptable の Path は自分の環境に書き換える
}

typeset -U path
typeset -U manpath
typeset -U fpath

# keychain
if whence -p keychain 2>&1 > /dev/null; then
  keychain id_rsa
  if [ -f $HOME/.keychain/$HOSTNAME-sh ]; then
    . $HOME/.keychain/$HOSTNAME-sh
  fi
  if [ -f $HOME/.keychain/$HOSTNAME-sh-gpg ]; then
    . $HOME/.keychain/$HOSTNAME-sh-gpg
  fi
fi

# z.sh
if [[ -f ~/.zfunctions/z/z.sh ]]; then
  _Z_CMD=j
  source ~/.zfunctions/z/z.sh
  precmd() {
    _z --add "$(pwd -P)"
  }
fi

# local
if [[ -f $HOME/.zshrc.local ]]; then
  . $HOME/.zshrc.local
fi

# git
if [[ -d /usr/share/git-core/Git-Hooks ]]; then
  export GIT_HOOKS_HOME=/usr/share/git-core/Git-Hooks
  source /usr/share/git-core/Git-Hooks/git-hooks-completion.bash
fi
if [[ -d /usr/local/share/git-core/Git-Hooks ]]; then
  export GIT_HOOKS_HOME=/usr/local/share/git-core/Git-Hooks
  if [[ -f /usr/local/share/git-core/Git-Hooks/git-hooks-completion.zsh ]]; then
    source /usr/local/share/git-core/Git-Hooks/git-hooks-completion.zsh
  fi
fi
if [[ -f $HOME/.zfunctions/git-flow-completion/git-flow-completion.zsh ]]; then
  source $HOME/.zfunctions/git-flow-completion/git-flow-completion.zsh
fi

# perlbrew
if [[ -f $HOME/perl5/perlbrew/etc/bashrc ]]; then
  source $HOME/perl5/perlbrew/etc/bashrc
fi

# pythonbrew
if [[ -f $HOME/.pythonbrew/etc/bashrc ]]; then
  source $HOME/.pythonbrew/etc/bashrc
fi

# rvm
if [[ -f $HOME/.rvm/scripts/rvm ]]; then
  source $HOME/.rvm/scripts/rvm
fi

# node.js
if [[ -d /usr/local/lib/node_modules ]]; then
  export NODE_PATH=/usr/local/lib/node_modules
fi
if [[ -d $HOME/.nodebrew/current/bin ]]; then
  export PATH=$HOME/.nodebrew/current/bin:$PATH
fi

# haskell
if [[ -d $HOME/Library/Haskell/bin ]]; then
  path=($path $HOME/Library/Haskell/bin)
fi

# smlnj
if [[ -d /usr/local/Cellar/smlnj/110.73/libexec/bin ]]; then
  path=($path /usr/local/Cellar/smlnj/110.73/libexec/bin)
fi

# byobu
export BYOBU_PREFIX=$(brew --prefix)
