" ocp-index support for vim
" Maintainer: INAJIMA Daisuke <inajima@sopht.jp>
" Version: 0.1

let s:jump_history = []
let s:max_jump_history = 100

function! ocpindex#init()
    setlocal omnifunc=ocpindex#complete
endfunction

function! s:env_enter()
    let isk = &l:isk
    setlocal isk+=.,'
    return isk
endfunction

function! s:env_leave(cookie)
    let &l:isk = a:cookie
endfunction

function! s:ocp_index(cmd, arg)
    let context = join(getline(1, line('.')), "\n")
    let cmdline = ['ocp-index', shellescape(a:cmd), '--context', ':', '-F',
    \              shellescape(substitute(expand('%:t:r'), '^\w', '\u\0', '')),
    \              shellescape(a:arg)]
    return system(join(cmdline), context)
endfunction

function! s:complete(entry)
    let entry = substitute(a:entry, '\n\s*', ' ', 'g')
    let ml = matchlist(entry, '^\(\S\+\)\s*\(.*\)')
    return {'word': ml[1], 'menu': ml[2]}
endfunction

function! ocpindex#complete(findstart, base)
    if a:findstart
        " locate the start of the word
        let cookie = s:env_enter()
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ '\k'
            let start -= 1
        endwhile
        call s:env_leave(cookie)
        return start
    else
        " find months matching with "a:base"
        let res = []
        let s = s:ocp_index('complete', a:base)
        return map(split(s, '\n\ze\S'), 's:complete(v:val)')
    endif
endfunction

function! ocpindex#echo_type()
    let cookie = s:env_enter()
    let word = expand('<cword>')
    call s:env_leave(cookie)
    echo substitute(s:ocp_index('type', word), '\n*$', "", "")
endfunction

function! s:jump_error()
    echo "Not found"
endfunction

function! ocpindex#jump()
    let cookie = s:env_enter()
    let loc = s:ocp_index('locate', expand('<cword>'))
    call s:env_leave(cookie)
    if v:shell_error
        return s:jump_error()
    endif
    let [fname, line, col] = split(loc, ':')
    let pos = getpos('.')
    let pos[0] = bufnr('%')
    call add(s:jump_history, pos)
    if len(s:jump_history) > s:max_jump_history
        call remove(s:jump_history, 0,
        \           len(s:jump_history) - s:max_jump_history - 1)
    endif
    " save current position to jumplist.
    " XXX: this code doesn't work as intended.  head of "fname" is registered
    " in jumplist in spite of using keepjumps.
    normal! m`
    execute 'keepjumps' 'edit' fname
    call cursor(line, col)
endfunction

function! ocpindex#jump_back()
    if empty(s:jump_history)
        return s:jump_error()
    endif
    let pos = remove(s:jump_history, -1)
    execute 'buffer' pos[0]
    call setpos('.', pos)
endfunction

nnoremap <silent> <Plug>(ocpindex-echo-type)
\       :<C-u>call ocpindex#echo_type()<CR>

nnoremap <silent> <Plug>(ocpindex-jump)
\       :<C-u>call ocpindex#jump()<CR>

nnoremap <silent> <Plug>(ocpindex-jump-back)
\       :<C-u>call ocpindex#jump_back()<CR>

