" ocp-index support for vim
" Maintainer: INAJIMA Daisuke <inajima@sopht.jp>
" Version: 0.1

if exists('b:did_ftplugin_ocaml_ocpindex')
    finish
endif
let b:did_ftplugin_ocaml_ocpindex = 1

setlocal omnifunc=ocpindex#complete

nnoremap <buffer> <silent> <Plug>(ocpindex-echo-type)
\       :<C-u>call ocpindex#echo_type()<CR>

nnoremap <buffer> <silent> <Plug>(ocpindex-jump)
\       :<C-u>call ocpindex#jump()<CR>

nnoremap <buffer> <silent> <Plug>(ocpindex-jump-back)
\       :<C-u>call ocpindex#jump_back()<CR>
