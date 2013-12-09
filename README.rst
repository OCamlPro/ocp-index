=============
vim-ocp-index
=============

* omni completion support
* type information printing
* jump to definitions


after/ftplugin/ocaml.vim example
================================

::

  if exists('b:did_ftplugin_after')
      finish
  endif
  let b:did_ftplugin_after = 1

  call ocpindex#init()

  nmap <buffer> K         <Plug>(ocpindex-echo-type)
  nmap <buffer> <C-]>     <Plug>(ocpindex-jump)
  nmap <buffer> <C-t>     <Plug>(ocpindex-jump-back)

* When types 'K', echo type information of the identifier under the
  cursor.
* When types CTRL-], jump to the definition of the identifier under the
  cursor and push it to the jump stack.
* When types CTRL-t, pop the previous position and jump back there.

