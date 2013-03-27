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
