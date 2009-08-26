#!/usr/bin/env zsh

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

# ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_cd
# cdのタイミングで自動的にpushd
setopt auto_pushd
setopt pushd_ignore_dups


autoload -Uz compinit; compinit

# ファイルリスト補完でもlsと同様に色をつける｡
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
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
#コピペの時rpromptを非表示する
setopt transient_rprompt
# 文字列末尾に改行コードが無い場合でも表示する
unsetopt promptcr

setopt notify
setopt print_exit_value

stty -istrip
bindkey -e
bindkey '^W' kill-region

# すごいプロンプト
setopt prompt_subst
autoload -U colors; colors
local prompt_color='%{[32m%}'
local clear_color='%{[0m%}'
local rprompt_color='%{[33m%}' # yellow [0m
local prompt_char='$'
if [[ "x$USER" == "xs-nag" || "x$USER" == "xnagaya" || "x$USER" == "xs_nag" ]]; then
  prompt_color='%{[32m%}'      # green [0m
elif [[ "x$USER" == "xroot" ]]; then
  prompt_color='%{[37m%}'      # white [0m
  prompt_char='#'
else
  prompt_color='%{[35m%}'      # pink [0m
fi
PROMPT=$prompt_color'%U%B%n'$rprompt_color'%U@'$prompt_color'%B%m%b %h '$prompt_char$clear_color'%u '
RPROMPT=$rprompt_color'[%~]'$clear_color

PAGER=lv

# for local::lib
local_lib_path="$HOME/perl5"
if [[ "x$HOSTNAME" == "xdv1" ]]; then
  export MODULEBUILDRC="${local_lib_path}/.modulebuildrc"
  export PERL_MM_OPT="INSTALL_BASE=${local_lib_path}"
  export PERL5LIB="${local_lib_path}/lib/perl5:${local_lib_path}/lib/perl5/i486-linux-gnu-thread-multi"
  export PATH="${local_lib_path}/bin:$PATH"
else
  export MODULEBUILDRC="${local_lib_path}/.modulebuildrc"
  export PERL_MM_OPT="INSTALL_BASE=${local_lib_path}"
  export PERL5LIB="${local_lib_path}/lib/perl5:${local_lib_path}/lib/perl5/darwin-thread-multi-2level"
  export PATH="${local_lib_path}/bin:$PATH"
fi

export PATH="${PATH}:$HOME/local/bin"
# for Mac ports
export PATH="${PATH}:/opt/local/bin"
# for gems
export PATH="${PATH}:/var/lib/gems/1.8/bin"

# alias
alias mv='nocorrect mv -i'
alias cp='nocorrect cp -ip'
alias ln='nocorrect ln'
alias mkdir='nocorrect mkdir'
alias mgdir='nocorrect mkdir -m 775'
alias rm='rm -i'
alias history='builtin history -Di'
alias his='history | tail'
if [[ `uname -s` == "Darwin" ]]; then
  alias ls='command ls -AFG'
else
  alias ls='command ls -AF --color=auto --show-control-chars'
fi
alias ln='ln -n'
alias x=exit
alias first_release="perl -mModule::CoreList -le 'print Module::CoreList->first_release(@ARGV)'"
alias scc='screen'
alias scx='screen -x'
alias hex='perl -le "print unpack q(H*), shift"'
alias formfu="perl -MData::Dumper -MHTML::FormFu -lwe 'my \$f = HTML::FormFu->new(); \$f->load_config_file(shift); print Data::Dumper->Dump([\$f]); print \$f'"

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

# Ctrl+wで､直前の/までを削除する｡
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
