# ocp-index

`ocp-index` is designed as a simple and light-weight documentation extractor for
OCaml, for command-line use or integrated in other tools (e.g. for
completion). It gathers information from `.cmi` (à la ocamlbrowser) and
`.cmt`/`cmti` files, including structure, location, type, and ocamldoc comments
when available.

<hr/>
<div class="span12">
<h3>Ressources</h3>
<table class="table table-striped">
  <tr><td><a href="http://www.github.com/OCamlPro/ocp-index">ocp-index on Github</a></td>
  <td>Latest sources in the official GIT repository</td></tr>
</table>
</div>
<br/><br/>

## Usage

`ocp-index COMMAND params OPTIONS`

Examples:
* `ocp-index type Module.ident`
* `ocp-index complete iden`
* `ocp-index locate Module.ident`

Options:
* `-I` include dirs / loaded libraries
* `-O` consider given module as open
* `-F` open the given module, not limiting to its exported interface
* `--context` automatically open/bind modules according to the given source file position.
* `--root` specify the root of the current project, for finding cmt files and source lookups. It's generally safe to let ocp-indent guess.

* output format: `--color`, `--show`/`--hide` to control the kinds of idents to
  display

## Build

```
./configure
make
make install
```
See below to compile and install the optional `ocp-browser`.

## Other tools

### Emacs mode

A script `ocp-index.el` is included under `tools/`, and can be used together
with tuareg-mode or ocaml-mode and
[auto-complete](https://github.com/auto-complete/auto-complete) (packaged as
`auto-complete-el` in Debian) to get completions and types in a popup menu.

You can run the script `tools/emacs-setup.sh` to get hints on the configuration
of emacs for ocp-index (it won't modify any files). Adding the following
line to your `.emacs`:
```lisp
(add-to-list 'load-path "/path/to/ocp-index.el")
(require 'ocp-index)
```
Will give you:
- `C-c TAB` to auto-complete ((global-set-key (kbd "KEY") 'auto-complete) to add
  your own binding)
- `C-c t` to print the type of the identifier under cursor
- `C-c ;` to jump to the definition of the identifier under cursor (use `C-c C-;` to do that in the current window)
- `C-c :` to jump to the interface of the identifier under cursor (use `C-c C-:` to do that in the current window)

See `M-x customize ocp-index` for more options.

### Vim

A script `ocp-index.vim`, contributed by
[anyakichi](https://github.com/anyakichi/vim-ocp-index), is available under
`tools`. It supports:
* omni completion
* type information printing
* jump to definitions

To use, add vim-ocp-index directory to runtimepath:
```
:set runtimepath^=/path/to/ocp-index.vim
```

Then create your own `after/ftplugin/ocaml.vim` to override vim's
builtin ocaml settings::
```
if exists('b:did_ftplugin_after')
    finish
endif
let b:did_ftplugin_after = 1

call ocpindex#init()

nmap <buffer> K         <Plug>(ocpindex-echo-type)
nmap <buffer> <C-]>     <Plug>(ocpindex-jump)
nmap <buffer> <C-t>     <Plug>(ocpindex-jump-back)
```
You get:
- `K` Echo type information of the identifier under the cursor
- `C-]` Push the current position to the jump stack and jump to the definition of the identifier under the cursor
- `C-t` Pop the previons position from the jump stack and jump back there
- `C-x C-o` Omni completion

If needed, you can specify ocp-index path explicitly: `let g:ocpindex_program = "/path/to/ocp-index"`


### Sublime Text

There is a binding written by Peter Zotov at https://github.com/whitequark/sublime-ocp-index

### ocp-browser

A small ncurses-based browser based on ocp-index is also included. You will need
lambda-term installed to build it:
```
$ opam install lambda-term
$ ./configure
$ make ocp-browser
```
