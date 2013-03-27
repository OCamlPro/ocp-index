OCAMLFIND_OPTS = -package compiler-libs,compiler-libs.common,unix,cmdliner
SOURCES = trie.mli trie.ml info.mli info.ml indexMain.ml
OCAMLC_OPTS = -w +a@1..39-4-9-37 -g

ocp-index: $(SOURCES)
	ocamlfind ocamlopt $(OCAMLFIND_OPTS) -linkpkg $(OCAMLC_OPTS) \
	$^ -o $@
