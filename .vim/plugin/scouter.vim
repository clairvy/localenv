" Measures Battle Power of a vimmer.
" Version: 0.1.0
" Author : thinca <thinca+vim@gmail.com>
" License: Creative Commons Attribution 2.1 Japan License
"          <http://creativecommons.org/licenses/by/2.1/jp/deed.en>

if exists('g:loaded_scouter')
  finish
endif
let g:loaded_scouter = 1

let s:save_cpo = &cpo
set cpo&vim


" functions  {{{1
function! s:sum(list)  " {{{2
  return eval(join(a:list, '+'))
endfunction



function! s:measure(file)  " {{{2
  let pat = '^\s*$\|^\s*"'
  let lines = readfile(a:file)
  let lines = split(substitute(join(lines, "\n"), '\n\s*\\', '', 'g'), "\n")
  return len(filter(lines,'v:val !~ pat'))
endfunction



function! s:files(files)  " {{{2
  if type(a:files) == type([])
    let files = []
    for f in a:files
      let files += s:files(f)
    endfor
    return files
  endif
  return split(glob(a:files), "\n")
endfunction



function! s:show(verbose, ...)  " {{{2
  let res = call('ScouterVerbose', a:000)
  let sum = s:sum(values(res))
  if a:verbose
    for file in sort(keys(res))
      echo file . ': ' . res[file]
    endfor
    let sum = 'Total: ' . sum
  endif
  echo sum
endfunction



function! ScouterVerbose(...)  " {{{2
  let res = {}
  for f in s:files(a:0 ? a:000 : $MYVIMRC)
    let res[f] = s:measure(f)
  endfor
  return res
endfunction



function! Scouter(...)  " {{{2
  return s:sum(values(call('ScouterVerbose', a:000)))
endfunction



" command  {{{1
command! -bar -bang -nargs=* -complete=file
\        Scouter call s:show(<bang>0, <f-args>)



let &cpo = s:save_cpo
unlet s:save_cpo
