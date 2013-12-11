=============
vim-ocp-index
=============

* omni completion support
* type information printing
* jump to definitions

Add vim-ocp-index directory to runtimepath::

  :set runtimepath^=/path/to/vim-ocp-index

Then create your own after/ftplugin/ocaml.vim to override vim's
builtin ocaml settings::

  if exists('b:did_ftplugin_after')
      finish
  endif
  let b:did_ftplugin_after = 1

  call ocpindex#init()

  nmap <buffer> K         <Plug>(ocpindex-echo-type)
  nmap <buffer> <C-]>     <Plug>(ocpindex-jump)
  nmap <buffer> <C-t>     <Plug>(ocpindex-jump-back)

K
    Echo type information of the identifier under the cursor.

<C-]>
    Push the current position to the jump stack and jump to the
    definition of the identifier under the cursor.

<C-t>
    Pop the previons position from the jump stack and jump back there.

<C-x><C-o>
    Omni completion.

You can specify ocp-index path explicitly::

    let g:ocpindex_program = "/path/to/ocp-index"

