# ocp-index

`ocp-index` is designed as a simple and light-weight documentation extractor for
OCaml, for command-line use or integrated in other tools (e.g. for
completion). It gathers information from `.cmi` (à la ocamlbrowser) and
`.cmt`/`cmti` files, including structure, location, type, and ocamldoc comments
when available.

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
(load-file "<OPAM_ROOT>/share/typerex/ocp-index/ocp-index.el")
```
Will give you:
- `C-c TAB` to auto-complete ((global-set-key (kbd "KEY") 'auto-complete) to add
  your own binding)
- `C-c t` to print the type of the identifier under cursor
- `C-c ;` to jump to the definition of the identifier under cursor
- `C-c :` to jump to the interface of the identifier under cursor

See `M-x customize ocp-index` for more options.

### Sublime Text

There is a binding written by Peter Zotov at https://github.com/whitequark/sublime-ocp-index

### ocp-browser

A small ncurses-based browser based on ocp-index is also included. You will need
ocaml-curses installed to build it:
```
$ opam install curses
$ ocp-build ocp-browser
$ ocp-build install ocp-browser
```
