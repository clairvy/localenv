#!/usr/bin/env zsh
# -*- coding: utf-8-unix; sh-basic-offset: 2; -*-

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=10000000

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward

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
# 補完設定
autoload -Uz compinit; compinit

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

stty -istrip
bindkey -e
bindkey '^W' kill-region

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
fi
[ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`

# すごいプロンプト
setopt prompt_subst
autoload -U colors; colors

if [[ $ZSH_VERSION == 4.3.* ]]; then
  autoload -Uz vcs_info
  zstyle ':vcs_info:*' formats '(%s)-[%b]'
  zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
  precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
  }
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
  if [[ x"$USER" == x"s-nag" || x"$USER" == x"nagaya" || x"$USER" == x"s_nag" ]]; then
    prompt_color='%{[32m%}'      # green [0m
  elif [[ x"$USER" == x"root" ]]; then
    prompt_color='%{[37m%}'      # white [0m
    prompt_char='#'
  else
    prompt_color='%{[35m%}'      # pink [0m
  fi
  PROMPT=$prompt_color'%U%B%n'$rprompt_color'%U@'$prompt_color'%B%m%b %h '$prompt_char$clear_color'%u '
  RPROMPT=$vcs_prompot_color'%1(v|%1v|) '$rprompt_color'[%~]'$clear_color
fi

if whence -p lv 2>&1 > /dev/null; then
  if [[ $TERM_PROGRAM == "iTerm.app" ]]; then
    alias lv='command lv -Ou'
  fi
  export PAGER='lv -Ou'
  alias lc='lv | cat'
fi

# default path
export PATH=/usr/bin:/bin

# for Mac ports
if [[ $os == 'mac' ]]; then
  export PATH="/opt/local/bin:/opt/local/sbin:${PATH}"
  export MANPATH="/opt/local/share/man:${MANPATH}"
fi
# for BSDPAN and local path
if [[ $os == 'bsd' ]]; then
  export PATH="${PATH}:/usr/local/bin:/usr/local/sbin"
  export MANPATH="${MANPATH}:/usr/local/share/man:/usr/local/man"
  export PKG_DBDIR=$HOME/local/var/db/pkg
  export PORT_DBDIR=$HOME/local/var/db/pkg
  export INSTALL_AS_USER
  export LD_LIBRARY_PATH=$HOME/local/lib
fi
# for csw
if [[ $os == 'sun' && -d /opt/csw/bin ]]; then
  export PATH="/opt/csw/bin:$PATH"
fi

# for local::lib
local_lib_path="$HOME/perl5"
function _set_perl_env () {
  export MODULEBUILDRC="${local_lib_path}/.modulebuildrc"
  export PERL_MM_OPT="INSTALL_BASE=${local_lib_path}"
  export PERL5LIB="${local_lib_path}/lib/perl5:${local_lib_path}/lib/perl5/$site"
  export PATH="${local_lib_path}/bin:$PATH"
}
if [[ "x$HOSTNAME" == "xdv1" ]]; then
  function set_perl_env () {
    local site='i486-linux-gnu-thread-multi'
    _set_perl_env
  }
  set_perl_env
elif [[ $os == 'mac' ]]; then
  function set_perl_env () {
    local site='darwin-2level'
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
fi

# for cabal
if [[ -d $HOME/.cabal/bin ]]; then
  export PATH="${PATH}:$HOME/.cabal/bin"
fi

export PATH="$HOME/local/bin:${PATH}"
export MANPATH="$HOME/local/man:${MANPATH}"
# for gems
if [[ -d /var/lib/gems/1.8/bin ]]; then
  export PATH="${PATH}:/var/lib/gems/1.8/bin"
fi
# for sbin
if [[ $PATH != "*:/sbin:*" && -d "/sbin" ]];then
  export PATH="${PATH}:/sbin"
fi
if [[ $PATH != "*:/usr/sbin:*" && -d "/usr/sbin" ]];then
  export PATH="${PATH}:/usr/sbin"
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
alias screen='command screen -U'
alias scc='screen'
alias scx='screen -x'
alias hex='perl -le "print unpack q(H*), shift"'
alias grep='grep --color'
alias egrep='egrep --color'
if [[ $os == 'mac' ]]; then
  alias emacs-app='/opt/local/var/macports/software/emacs-app/23.1_1/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
  alias emacsclient='/opt/local/var/macports/software/emacs-app/23.1_1/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient'
  function javac() {
    command javac -encoding utf8 -Xlint:unchecked -Xlint:deprecation $* 2>&1 | lv | cat
  }
  alias java='java -Dfile.encoding=UTF8'
  alias mvim='/Applications/MacVim.app/Contents/MacOS/Vim'
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
