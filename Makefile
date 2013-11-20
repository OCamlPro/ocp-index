include Makefile.config

PROJECTS = ocp-index-lib ocp-index

byte = _obuild/ocp-index/ocp-index.byte
native = _obuild/ocp-index/ocp-index.asm
manpage = man/man1/ocp-index.1

OCPBUILD_ARGS = -install-lib $(prefix)/lib/ocp-index

all: $(PROJECTS)

ocp-index: $(native)
	cp $^ ocp-index

ocp-index-lib: $(native)

ALWAYS:

$(byte) byte: ocp-build.root ALWAYS
	ocp-build -byte $(OCPBUILD_ARGS) $(PROJECTS)

$(native) native asm: ocp-build.root ALWAYS
	ocp-build -asm $(OCPBUILD_ARGS) $(PROJECTS)

$(manpage): ocp-index
	mkdir -p $(@D)
	./ocp-index --help=groff >$@

.PHONY: clean
clean: ocp-build.root
	ocp-build -clean $(OCPBUILD_ARGS)

.PHONY: distclean
distclean:
	rm -rf _obuild man
	rm -f ocp-index
	rm -f Makefile.config
	rm -f ocp-build.root*
	rm -rf config.* aclocal.m4 *.cache configure

.PHONY: install
install: $(PROJECTS) $(manpage)
	@if ocp-build -installed $(OCPBUILD_ARGS) \
	    | grep -q ocp-index; then \
	  ocp-build uninstall $(OCPBUILD_ARGS) $(PROJECTS); \
	fi
	ocp-build install $(OCPBUILD_ARGS) -install-bin $(prefix)/bin $(PROJECTS)
	mkdir -p $(mandir)/man1
	install -m 644 $(manpage) $(mandir)/man1/
	mkdir -p $(datarootdir)/emacs/site-lisp
	install -m 644 tools/ocp-index.el $(datarootdir)/emacs/site-lisp/
	@echo
	@echo
	@echo "=== ocp-index installed ==="
	@echo
	@if $$(which emacs >/dev/null); then \
	  tools/emacs-setup.sh $(datarootdir)/emacs/site-lisp; \
	  echo; \
	fi

.PHONY: uninstall
uninstall:
	rm $(mandir)/man1/$(notdir $(manpage))
	ocp-build uninstall $(OCPBUILD_ARGS) $(PROJECTS)

configure: configure.ac
	aclocal -I m4
	autoconf

version.ocp: configure.ac
	@echo "version.ocp not up-to-date, please rerun ./configure"
	@exit 1

ocp-build.root:
	@if (ocp-build -version 2>/dev/null |\
	     awk -F'.' '{ exit $$1 > 1 || ($$1 = 1 && $$2 >= 99) }'); then \
	  echo "Error: you need ocp-build >= 1.99." >&2;\
	  exit 1;\
	fi
	ocp-build -init $(OCPBUILD_ARGS)
