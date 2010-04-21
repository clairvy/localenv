filetype plugin on
filetype indent on
syntax enable
" from Ubuntu default
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,utf-16,japan

set backspace=2
set tabstop=2
set shiftwidth=4
set expandtab

highlight tabs ctermbg=green guibg=green

set list
set number
set ruler
set smartindent
set laststatus=2

"{{{ PHPManual

" PHP マニュアルを設置している場合
"let phpmanual_dir = $HOME . '/.vim/manual/php_manual_ja/'
" マニュアルの拡張子
"let phpmanual_file_ext = 'html'
" マニュアルのカラー表示
let phpmanual_color = 1
" iconv 変換をしない
let phpmanual_convfilter = ''
" w3m の表示形式を utf-8 にし、auto detect を on にする
let phpmanual_htmlviewer = 'w3m -o display_charset=utf-8 -o auto_detect=2 -T text/html'
" phpmanual.vim を置いているパスを指定
"source $HOME/.vim/ftplugin/phpmanual.vim
"}}}
