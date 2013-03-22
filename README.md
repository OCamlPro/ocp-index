# ocp-index

`ocp-index` is designed as a simple and light-weight documentation extractor for
OCaml, for command-line use or integrated in other tools. It gathers
informations from `.cmi` (à la ocamlbrowser) and `.cmt` files.

Usage:

`ocp-index <query> <output>`

query: `Module` `Module.ident` `-complete <str>`
output: ident, `-type`, `-doc`

* `ocp-index -type "Module.value"`
* `ocp-index -doc "ident"`
* `ocp-index -complete "ide"`

Options:

* include dirs / loaded libraries
* output format
