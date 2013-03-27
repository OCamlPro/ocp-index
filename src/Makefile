OCAMLFIND_OPTS = -package compiler-libs,compiler-libs.common,unix,cmdliner
SOURCES = trie.mli trie.ml info.mli info.ml indexMain.ml
OCAMLC_OPTS = -w +a-4-37@x -g

ocp-index: $(SOURCES)
	ocamlfind ocamlopt $(OCAMLFIND_OPTS) -linkpkg $(OCAMLC_OPTS) \
	$^ -o $@
