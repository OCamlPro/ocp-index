# ocp-index

`ocp-index` is designed as a simple and light-weight documentation extractor for
OCaml, for command-line use or integrated in other tools (e.g. for
completion). It gathers informations from `.cmi` (à la ocamlbrowser) and `.cmt`
files.

## Usage

`ocp-index COMMAND params OPTIONS`

COMMAND is for now either `complete` or `type`

Examples:
* `ocp-index type Module.ident`
* `ocp-index complete iden`
* `ocp-index doc Module.ident` (TODO)

Options:
* `-I` include dirs / loaded libraries
* output format: for now only color enable / disable

## Build

```
ocp-build ocp-index
```

There is also a Makefile in src/ if you don't have `ocp-build` at hand.

## Configuration file

`ocp-index` will look for a `.ocp-index` file in the current project or in the
home directory. The file format is *not* final at the moment and likely to
change. That said, if you want to try it out, you can put a file `.ocp-index` at
the root of your project containing `rec dir _build` (or whatever the name of
your build dir) to have ocp-index lookup the interfaces from your current
project.


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

See `M-x customize ocp-index` for mode options.

### ocp-browser

A small ncurses-based browser based on ocp-index is also included. You will need
ocaml-curses installed to build it:
```
$ opam install curses
$ ocp-build ocp-browser
$ ocp-build install ocp-browser
```
