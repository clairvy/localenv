" v:set ts=8 sts=2 sw=2 tw=0:
"
" phpmanual.vim - PHP Manual Viewer

scriptencoding euc-jp

" Version:     1.3
" Last Change: 2005-02-13.

let s:version = '1.3'

" Description:
"   Vim �Υ�����ɥ���ʬ�䤷�ơ�PHP �ޥ˥奢��򳫤��ޤ���
"   �ޤ��ϡ����ꤷ�������Υ֥饦���� PHP �Υޥ˥奢��򳫤��ޤ���
"
"   �ʲ��Υ��ޥ�ɤ���Ѥ��ޤ���ɬ�פ˱����ƥ��ץ����ˤ���ѹ���
"   ��ǽ�Ǥ���
"
"   wget  : Web �����Ȥ��� PHP �ޥ˥奢����������Τ�ɬ�פǤ���
"           ������� PHP �ޥ˥奢�����Ѥ���������פǤ���
"
"   iconv : PHP �ޥ˥奢��ϡ�UTF-8 �ǽ񤫤�Ƥ��뤿�ᡢEUC-JP ��
"           �Ѵ�����ݤ˻��Ѥ��ޤ������ץ����ˤ�äơ��̤Υ��ޥ��
"           ( �㤨�� UTF-8 ���Ѵ����б����� nkf )���ѹ����뤳��
"           ���ǽ�Ǥ���
"
"   w3m   : �������� PHP �ޥ˥奢��Υƥ����������� w3m ����Ѥ���
"           ���ޤ������ץ����ˤ�äơ��̤Υ��ޥ�ɤ��ѹ����뤳��
"           ���ǽ�Ǥ�����������ɸ�����Ϥ���ƥ����Ȥ��Ѵ����뤳�Ȥ�
"           �Ǥ���ɬ�פ�����ޤ���
"
" Usage:
"  �����ޥå�����(�ѹ���)
"
"      <Leader>P - ��������ˤ��� PHP �δؿ����Ф��ƥޥ˥奢��򳫤��ޤ���
"
"      <Leader>E - ���ꤷ�������֥饦���ǥ�������ˤ��� PHP �ؿ���
"                  �ޥ˥奢��򳫤��ޤ���
"
"      <Leader> �ϡ��̾\ (�Хå�����å���) �˳�����Ƥ��Ƥ��ޤ���
"      <Leader>P �ϡ�\ �򲡤��Ƥ��顢P �򲡤��ޤ���
"
"      Note: <Leader>E �ϡ������֥饦��������򤷤��Ȥ��Τ߻��Ѳ�ǽ�Ǥ���
"           �����֥饦������Ѥ���ˤϡ�let phpmanual_use_ext_browser �� 1 ��
"           ���Ƥ����������ǥե���ȤǤϳ����֥饦���ϻ��Ѥ��ʤ������
"           �ʤäƤ��ޤ���
"
" �ޤ��ϡ��ʲ��Υ��ޥ�ɤ���θƤӽФ��Ǥ��ǽ�Ǥ������ޥ��̾���ѹ���ǽ�Ǥ���
"
"     Vim ������ɥ���˥ޥ˥奢��򳫤��ޤ����ʲ��Υ��ץ���󤬤���ޤ���
"
"       :PHPManual <�ؿ�̾>      : <�ؿ�̾>�ǻ��ꤷ���ؿ��Υޥ˥奢���ɽ�����ޤ���
"       :PHPManual func <�ؿ�̾> : <�ؿ�̾>�ǻ��ꤷ���ؿ��Υޥ˥奢���ɽ�����ޤ���
"                                  func �Ͼ�ά��ǽ�ǡ���ǽ��Ʊ���Ǥ���
"       :PHPManual funclist      : �ؿ������ڡ�����ɽ�����ޤ���
"       :PHPManual ref <����̾>  : �ؿ���ե���󥹤������ڡ�����ɽ�����ޤ���
"                                  <����̾>�� :PHPManual reflist �ǳ�ǧ���Ƥ���������
"       :PHPManual reflist       : �ؿ���ե���󥹤������ڡ�����ƤӽФ������
"                                  ����̾������ɽ�����ޤ���
"       :PHPManual help          : �Ȥ�����������ɽ�����ޤ���
"
"     �����֥饦���Ǵؿ�̾���Ф���ޥ˥奢��򳫤��ޤ���
"
"       :PHPManualExtBrowser <�ؿ�̾>
"
" Option:
"  .vimrc �������ǽ�ʥ��ץ����:
"
"      let phpmanual_command         = 'PHPManual'
"          �ץ饰����ƤӽФ��� Vim ���ޥ��̾ (�ǥե����: PHPManual)
"
"      let phpmanual_mapname         = '<Leader>P'
"          ��������β��ˤ���ؿ�������ˤ��ƥ��ޥ�ɤ�ƤӽФ������
"          �����ޥå� ( �ǥե����: \P )
"
"      let phpmanual_use_ext_browser = 0
"          0: �����֥饦���ϻ��Ѥ��ޤ���(�ǥե����)
"          1: �����֥饦������Ѥ��ޤ���
"
"      let phpmanual_ext_command     = 'PHPManualExtBrowser'
"          �����֥饦����ƤӽФ��Ȥ��� Vim ���ޥ��̾
"          ( �ǥե���� : PHPManualExtBrowser )
"
"      let phpmanual_mapname         = '<Leader>E'
"          ��������β��ˤ���ؿ�������ˤ��Ƴ����֥饦����ƤӽФ������
"          �����ޥå� ( �ǥե����: \E )
"
"      let phpmanual_ext_browser_cmd = 'mozilla'
"          �����֥饦���μ¹ԥ��ޥ�� ( �ǥե����: mozilla )
"
"      let phpmanual_man_site        = 'http://jp.php.net/'
"          �����֥饦������ƤӽФ������ȡ�
"          ( �ǥե����: http://jp.php.net/ )
"
"        Note: �����֥饦������θƤӽФ��ϡ�PHP �� WEB �����ФΥȥåפ�
"              �ؿ�̾���դ��ƸƤӽФ���ǽ����Ѥ��Ƥ��뤿�ᡢ
"              PHP �Υߥ顼�����аʳ��ǤϤ��ޤ�ɽ������ޤ���
"
"      let phpmanual_dir             = 'http://jp.php.net/manual/ja/print/'
"          Vim �Υ�����ɥ�ʬ�䤫��ƤӽФ���륵���ȤǤ���
"          �ޤ��ϥ�����ǥ��쥯�ȥ�����Ǥ��ޤ���
"          ( �ǥե����: http://jp.php.net/manual/ja/print/ )
"
"        Note: ��������ޥ˥奢���������뤿�ᡢ�����Ȥ���³�˻��֤�
"              ������ȡ����Ф餯���Ǥ��ʤ��ʤ�ޤ��Τǡ��������
"              PHP �ޥ˥奢����������ɤ��ơ����Υǥ��쥯�ȥ��
"              ���ꤷ�������ɤ��Ǥ���
"
"      let phpmanual_file_ext        = 'php'
"          ��������ե�����γ�ĥ�ҡ�PHP �����Фˤ��� PHP �ޥ˥奢���
"          ��ĥ�Ҥ� php �Ǥ�������������ɤ����ޥ˥奢��ϳ�ĥ�Ҥ�
"          html �Ǥ��Τǡ�ɬ�פ˱������ѹ����Ƥ���������
"
"          php  : PHP �ޥ˥奢��γ�ĥ�Ҥ� php (�ǥե����)
"          html : PHP �ޥ˥奢��γ�ĥ�Ҥ� html
"
"      let phpmanual_convfilter      = 'iconv -c -f utf-8 -t euc-jp'
"          PHP �ޥ˥奢���ʸ�������ɤ� UTF-8 �Ǥ��Τǡ������ EUC ���Ѵ�
"          ����ɬ�פ�����ޤ���iconv ���������󥹥ȡ��뤵��Ƥ����ǽ����
"          �⤤�Τǡ��ǥե���ȤǤ� iconv �ˤ��ޤ�����
"
"      let phpmanual_htmlviewer      = 'w3m -T text/html'
"          HTML ��ƥ����Ȥ��Ѵ�����ġ���Ȥ��� w3m ����Ѥ��ޤ���
"          ɸ�����Ϥ����������Ǥ���褦�� -T ���ץ�������Ѥ��Ƥ��ޤ���
"
"      let phpmanual_color           = 1
"          PHP �ޥ˥奢��ǥ��顼ɽ����Ԥ��ޤ���
"          ���顼ɽ����ɬ�פʤ����� 0 �����ꤷ�Ƥ���������
"
"  ������:
"      let phpmanual_dir             = '/home/manual/php_manual/'
"          ������� /home/manual/php_manual/ �ˤ��� PHP �ޥ˥奢������
"          ����褦�����ꤷ�ޤ���
"
"      let phpmanual_file_ext        = 'html'
"          ������ˤ��� PHP �ޥ˥奢��γ�ĥ�Ҥ� html �Ǥ��뤳�Ȥ����
"          ���ޤ���
"
"      let phpmanual_use_ext_browser = 1
"          �����Υ֥饦������Ѥ���褦�����ꤷ�ޤ���
"
"      let phpmanual_ext_browser_cmd = '~/bin/mozilla.sh'
"          �����֥饦���θƤӽФ��Ǥ� mozilla �Υ�åѡ�������ץȤ�ƤӽФ�
"          �褦�˻��ꤷ�ޤ���
"
"      let phpmanual_convfilter      = '/usr/local/bin/nkf -e'
"          ʸ���������Ѵ��� UTF-8 �б��� nkf ����Ѥ���褦�����ꤷ�ޤ���
"
" ChangeLog:
"     1.0   : ���� (2003-07-13)
"
"     1.0.1 : �᡼��ǻ�Ŧ������ޤ����Τǡ������Ĥ�������Ԥ��ޤ�����
"             �����󡢻�Ŧ��ɤ��⤢�꤬�Ȥ��������ޤ�����(2003-12-10)
"
"             - GNU libiconv ��ʸ���������Ѵ��˼��Ԥ��Ƥ����Τ������ޤ�����
"             - �ؿ�̾�κǽ餬��ʸ�����ä���硢�����ʴؿ�̾�ˤʤäƤ��ޤäƤ���
"               ����������ޤ�����
"
"     1.1   : ��ǽ���ɲä��ޤ���(2003-12-14)
"
"             - �����æ���ν�����Ԥ��ޤ�����
"             - �إ�פ�������ޤ�����
"               :PHPManual help
"             - �ؿ���ե���󥹤������ڡ�����ɽ���Ǥ���褦�ˤ��ޤ�����
"               :PHPManual ref <����̾>
"             - �ؿ��������ؿ���ե���󥹤ΰ�����ɽ���Ǥ���褦�ˤ��ޤ�����
"               :PHPManual reflist
"               :PHPManual funclist
"
"     1.2   : ������Ⱥ٤�������
"
"             - �����æ���ν�����Ԥ��ޤ�����
"             - :PHPManual reflist ��¹Ԥ������ˡ���ե���󥹰���������
"               PHP �ޥ˥奢��� funcref.(html|php) �ե����뤫���������褦��
"               �ѹ�����������ɥ����ɽ������褦�ˤ��ޤ�����
"
"     1.3   : �ޥ˥奢��Υ��顼ɽ����ǽ���ɲä��ޤ��� (2005-02-15)
"
"             - �ǥե���ȤǤϥ��顼ɽ����Ԥ��ޤ���
"             - ���ε�ǽ��ɬ�פʤ����� phpmanual_color = 0 �����ꤷ�Ƥ���������

" 2���ɤ߹����ɻ߽���
if exists("loaded_phpManualViewer")
  finish
endif
let loaded_phpManualViewer = 1

" ���ޥ��̾(�ǥե������: PHPManual)
if !exists("g:phpmanual_command")
  let g:phpmanual_command = "PHPManual"
endif

" �����ޥå�(�ǥե������: \P)
if !exists("g:phpmanual_mapname")
  let g:phpmanual_mapname = "<Leader>P"
endif

" Syntax ����
if !exists("g:phpmanual_color")
  let g:phpmanual_color = 1
endif

" Vim �Υ�����ɥ���ɽ����������Υ��ޥ�ɤȥ����ޥåפ���Ͽ
exec 'command! -nargs=+ ' . g:phpmanual_command . ' :call <SID>loadManual(<f-args>)'
exec 'nnoremap '          . g:phpmanual_mapname . ' :call <SID>getFunctionWord("vim")<CR>'

"
" �����֥饦������Ѥ�������ˤʤäƤ�����ϥ֥饦���ȥ����Ȥ�����
"
if exists("g:phpmanual_use_ext_browser") && g:phpmanual_use_ext_browser == 1

  " �����֥饦�����Ѥξ��Υ��ޥ��̾(�ǥե������: PHPManualExtBrowser)
  if !exists("g:phpmanual_ext_command")
    let g:phpmanual_ext_command = "PHPManualExtBrowser"
  endif
  
  " �����֥饦�����Ѥξ��Υ����ޥå�(�ǥե������: \E)
  if !exists("g:phpmanual_ext_mapname")
    let g:phpmanual_ext_mapname = "<Leader>E"
  endif

  " �����֥饦�����ޥ��
  if !exists('g:phpmanual_ext_browser_cmd')
    let g:phpmanual_ext_browser_cmd = "mozilla"
  endif

  " �����֥饦����
  if !exists('g:phpmanual_man_site')
    let g:phpmanual_man_site = "http://jp.php.net/"
  endif

  " ���ޥ��̾�ȥ����ޥåפ���Ͽ
  exec 'command! -nargs=1 ' . g:phpmanual_ext_command . ' :call <SID>loadManualExtBrowser(<f-args>)'
  exec 'nnoremap '          . g:phpmanual_ext_mapname . ' :call <SID>getFunctionWord("ext")<CR>'

endif

"
" �ޥ˥奢��Υǥ��쥯�ȥ�����ȳ�ĥ������
"
if !exists('g:phpmanual_dir')
  " �̾�����ѥޥ˥奢��(�����������)
  let g:phpmanual_dir      = "http://jp.php.net/manual/ja/print/"
  let g:phpmanual_file_ext = "php"

  " ������� PHP �ޥ˥奢�뤬������λ���(��)
  "let g:phpmanual_dir      = "/home/manual/php_manual/"
  "let g:phpmanual_file_ext = "html"

  " �桼���Ρ����դ������ѥޥ˥奢��(�����������)
  "let g:phpmanual_dir      = "http://jp.php.net/manual/ja/printwn/"
  "let g:phpmanual_file_ext = "php"
endif

""
"" UTF-8 -> EUC �Ѵ��ե��륿
""
"if !exists('g:phpmanual_convfilter')
"  " iconv �����
"  let g:phpmanual_convfilter = "iconv -c -f utf-8 -t euc-jp"
"
"  " UTF-8 �б� nkf ����Ѥ�����
"  "let g:phpmanual_convfilter = "nkf -e"
"endif

"
" HTML ɽ���ӥ塼��(ɸ�����Ϥ�����ɤ߹���)
"
if !exists('g:phpmanual_htmlviewer')
  " w3m �����
  let g:phpmanual_htmlviewer = "w3m -T text/html"
endif

"
" ��ĥ�Ҥ����꤬�ʤ����϶��� -> ���顼��ȯ�����뤿������Բ�
"
if !exists("g:phpmanual_file_ext") 
  let g:phpmanual_file_ext = ""
endif

"
" �ޥ˥奢���ɤ߹��ߴؿ�
"
function! s:loadManual(command, ...)

  if a:command ==# 'help'
    let result = s:dispHelp()
    return
  elseif a:command ==# 'reflist'
    let command = 'reflist'
    let word    = 'funcref'
  elseif a:command ==# 'funclist'
    let command = 'func'
    let word    = 'funclist'
  elseif a:0 == 0
    let command = ''
    let word    = a:command
  elseif a:command =~ '^\(ref\|func\)$'
    let command = a:command
    let word    = a:1
  else
    let command = 'func'
    let word    = a:command
  endif

  " ��ĥ�ҤΥ����å�
  if g:phpmanual_file_ext !~ '^\(php\|html\)$'
    echo "��ĥ�Ҥ����꤬�����Ǥ�����ĥ�� (" . g:phpmanual_file_ext . ") �ϻ��ѤǤ��ޤ���"
    return
  endif

  " �ؿ�̾��ʸ�����Ѵ�
  let word     = tolower(word)

  " �ؿ�̾�����å�
  if s:checkFunctionName(word) == 0
    echo "�����ʴؿ�̾�Ǥ�(" . word . ")��"
    return
  endif

  " ��������(PHP �ޥ˥奢��Υե�����̾�ϡ� _ �� - ���֤���������)��Ԥ���
  " �ؿ�̾����ե�����̾����Ƭ������
  let filename = s:getFilePrefix(command, substitute(word, "_", "-", "g"))
  let resource = g:phpmanual_dir . filename . "." . g:phpmanual_file_ext

  " resource ��������ǥ��쥯�ȥ�ξ��
  if resource !~ "^http://"
    " ������ˤ��� PHP �ޥ˥奢��ե������¸�߳�ǧ
    if !filereadable(resource)
      echo word . " �Υޥ˥奢�뤬���Ĥ���ޤ���"
      return
    endif
  else
    " �������� HTML �����
    let tmpfile = tempname()
    let result  = system("wget -nv -O " . tmpfile . " " . resource)
    if filereadable(tmpfile) && getfsize(tmpfile) > 0
      let resource = tmpfile
    else
      echo "�ե�����μ����˼��Ԥ��ޤ�����"
      return
    endif
  endif

  let result = s:display(command, resource, g:phpmanual_convfilter, g:phpmanual_htmlviewer)

  " �ƥ�ݥ��ե����뤬������Ϻ��
  if exists("tmpfile") && filereadable(tmpfile)
    call delete(tmpfile)
  endif

endfunction  

function! s:display(command, resource, filter, viewer)
  " ���� PHP �ޥ˥奢���ɽ�����Ƥ��륦����ɥ�������к�����
  let thiswin = winnr()
  exec "norm! \<C-W>b"

  if &filetype != "php_manual"
    let thiswin = winnr()
    exec "norm! \<C-W>b"
    if winnr() == 1
      new
    else
      exec "norm! " . thiswin . "\<C-W>w"
      while 1
        if &filetype == "php_manual"
          break
        endif
        exec "norm! \<C-W>w"
        if thiswin == winnr()
          new
          break
        endif
      endwhile
    endif
  endif

  " ����åץե������������ʤ�
  set buftype=nofile noswapfile

  " �ޡ�������
  set ma

  " ����ɽ����ΥХåե�����
  silent exec "norm 1GdG"

  " ɽ��
  if a:command ==# "reflist"
    silent exec 'r ' . a:resource
    silent exec 'g/^HREF/norm gJ'
    silent exec 'g!/^HREF="ref\.[a-z]*\.' . g:phpmanual_file_ext . '">[^<]/d'
    silent exec '%s/^HREF="ref\.\([a-z]*\)\.' . g:phpmanual_file_ext . '">\(.*\)<\/A$/\1\t\t: \2/'
    setlocal tabstop=20
    silent exec "norm ggO"
    silent exec 'r! echo "�Ȥ��� :PHPManual ref <����̾>" && echo ""'
    silent exec 'r! echo "��: �ʲ���¹Ԥ���ȡ��ؿ���ե���󥹤� Apache �Ѵؿ��Υڡ�����ɽ������ޤ���"'
    silent exec 'r! echo "" && echo "  :PHPManual ref apache" && echo ""'
    silent exec 'r! echo "����̾�ϰʲ����̤�Ǥ���" && echo "--"'
    silent exec "norm ggdd"
  else
    if exists(a:filter)
      silent exec "r! " . a:filter . " " . a:resource . " | " . a:viewer
    else
      silent exec "r! " . a:viewer . " " . a:resource
    endif
  endif

  " ���־�˰�ư
  silent exec "norm gg"

  " ɽ���塢�ѹ����Բġ��Хåե��ˤ�ɽ�����ʤ��褦�����ꤹ��
  setlocal filetype=php_manual nomod
  setlocal bufhidden=hide
  setlocal nobuflisted

  " Syntax ������
  if has('syntax') && g:phpmanual_color == 1
     setlocal syntax=php
	 let dtd_ignore_case=0
     syntax match Ignore /\(^\| \)\(<\|>\)\(,\| \|$\)\|=>\|<=\|>=\|->\|<>/
     syntax match Function /^[a-z][0-9a-z_]*\( -- .*\|$\)/
     syntax match Tag /[a-z][0-9a-z_]*()/
     syntax match Underlined /^\<\(Description\|Example\|Table\|��\|����\)\>.*$/
     syntax match Type /\<\(int\|bool\|void\|string\|double\|float\|array\|resource\|static\|mixed\)\>/
     syntax match Keyword /\<Note:\>\|\<���:\>/
     syntax match Error /\<Warning\>\|\<�ٹ�\>/
  endif

  return 1
endfunction

"
" ��������ˤ���ؿ�̾�����
"
function! s:getFunctionWord(browser)
  let str  = expand("<cword>")
  let word = substitute(str, '(*\(\k\+\).*', '\1', '')

  " �ޥ˥奢��ƤӽФ�
  if a:browser ==# 'vim'
    call s:loadManual(word)
  elseif a:browser ==# 'ext'
    call s:loadManualExtBrowser(word)
  else
    echo word
  endif
  return
endfunction

"
" �����֥饦�����ѻ��ν���
"
function! s:loadManualExtBrowser(word)
  if s:checkFunctionName(a:word) == 0
    echo "�����ʴؿ�̾�Ǥ���"
    return
  endif

  " �����֥饦�������꤬����С������ URI �򳫤�
  if exists("g:phpmanual_ext_browser_cmd")
    " man_site �������� URI �ξ�硢PHP �����Фؤ��䤤��碌ʸ��������
    let resource = g:phpmanual_man_site . a:word
    let result   = system(g:phpmanual_ext_browser_cmd . " " . resource)
  else
    echo "�����֥饦�������꤬����������ޤ���"
  endif
  return
endfunction

"
" PHP �ؿ�̾�����å�
"
function! s:checkFunctionName(word)
  if a:word !~ '^[a-z][_a-z0-9]*$'
    return 0
  endif
  return 1
endfunction

"
" �ؿ�̾����ե��������Ƭ������
"
function! s:getFilePrefix(command, word)
  " �ؿ�������ɽ��
  if a:command ==# "ref"
    let prefix = "ref." . a:word

  " ��ե���󥹰���ɽ��
  elseif a:command ==# "reflist"
    let prefix = "funcref"

  " �ؿ�����ɽ��
  elseif a:word ==# "funclist"
    let prefix = "index.functions"

  " ���湽¤�Υޥ˥奢��
  elseif a:word =~ '^\(alternative-syntax\|break\|continue\|declare\|do\|else\|elseif\|for\|foreach\|switch\|while\)$'
    " do.while �Υޥ˥奢��� do ����ꤷ���Ȥ���ɽ��
    if a:word ==# "do"
      let prefix = "control-structures.do.while"
    else
      let prefix = "control-structures." . a:word
    endif

  " ��ե���󥹤Υȥåץڡ���
  elseif a:word =~ '^\(apache\|array\|aspell\|bc\|bzip2\|calendar\|ccvs\|classobj\|com\|cpdf\|crack\|ctype\|curl\|cybercash\|cyrus\|datetime\|dba\|dbase\|dbm\|dbplus\|dbx\|dio\|dir\|dom\|domxml\|dotnet\|errorfunc\|exec\|fam\|fbsql\|fdf\|filepro\|filesystem\|fribidi\|ftp\|funchand\|gettext\|gmp\|http\|hw\|hwapi\|ibase\|iconv\|id3\|ifx\|image\|imap\|info\|ingres\|ircg\|java\|ldap\|lzf\|mail\|mailparse\|math\|mbstring\|mcal\|mcrypt\|mcve\|memcache\|mhash\|mime-magic\|ming\|misc\|mnogosearch\|msession\|msql\|mssql\|muscat\|mysql\|mysqli\|ncurses\|network\|nis\|notes\|nsapi\|objaggregation\|oci8\|openssl\|oracle\|outcontrol\|overload\|ovrimos\|pcntl\|pcre\|pdf\|pfpro\|pgsql\|posix\|printer\|pspell\|qtdom\|readline\|recode\|regex\|sem\|sesam\|session\|shmop\|simplexml\|snmp\|soap\|sockets\|spl\|sqlite\|stream\|strings\|swf\|sybase\|tcpwrap\|tidy\|tokenizer\|uodbc\|url\|var\|vpopmail\|w32api\|wddx\|xdiff\|xml\|xmlrpc\|xsl\|xslt\|yaz\|zip\|zlib\)$'
    " array, exec, filepro, gettext, iconv, mail, mhash, msql, overolad, readline, recode �ˤĤ��Ƥϡ�
    " �ؿ�̾�Ƚ�ʣ���뤿�ᡢ�ؿ������Υޥ˥奢���ͥ��
    if a:word =~ '^\(array\|exec\|filepro\|gettext\|iconv\|mail\|mhash\|msql\|overolad\|readline\|recode\)$'
      let prefix = "function." . a:word
    else
      let prefix = "ref." . a:word
    endif

  " �ؿ������Υޥ˥奢��
  else
    let prefix = "function." . a:word
  endif

  return prefix
endfunction

"
" �إ��ɽ��
"
function! s:dispHelp()
  echo '�Ȥ���'
  echo ' '
  echo ':PHPManual (ref|reflist|func|funclist|help) <����̾(�ؿ�̾)>'
  echo ' '
  echo '���ץ����Ͼ�ά��ǽ�Ǥ������ξ��ϡ����Ĥ��ä��ؿ�̾�Υޥ˥奢���ɽ�����ޤ���'
  echo ' '
  echo '�������뤬�ؿ��ξ�ˤ�����ˡ�\P (�ǥե����) �򲡤��ȡ��б�����ޥ˥奢��� Vim �α�ư��ʬ�䤷��ɽ�����ޤ���'
  echo '�����֥饦���������ԤäƤ������ \E (�ǥե����) �򲡤��ȡ����ꤷ���֥饦���ǥޥ˥奢���ɽ�����ޤ���'
  echo ' '
  echo '��:'
  echo ' '
  echo ':PHPManual <�ؿ�̾>      : �ؿ�̾�ǻ��ꤷ���ؿ��Υޥ˥奢���ɽ�����ޤ���'
  echo ':PHPManual func <�ؿ�̾> : �ؿ�̾�ǻ��ꤷ���ؿ��Υޥ˥奢���ɽ�����ޤ���func �Ͼ�ά��ǽ�ǵ�ǽ��Ʊ���Ǥ���'
  echo ':PHPManual funclist      : �ؿ������ڡ�����ɽ�����ޤ���'
  echo ':PHPManual ref <����̾>  : �ؿ���ե���󥹤������ڡ�����ɽ�����ޤ�������̾�� :PHPManual reflist �ǳ�ǧ���Ƥ���������'
  echo ':PHPManual reflist       : �ؿ���ե���󥹤������ڡ�����ƤӽФ�����μ���̾������ɽ�����ޤ���'
  echo ':PHPManual help          : �Ȥ�����������ɽ�����ޤ���'
  echo ' '
  echo ':PHPManualExtBrowser <�ؿ�̾> : ���ꤷ�������֥饦���Ǵؿ�̾�Υޥ˥奢��򳫤��ޤ���'
  return
endfunction
