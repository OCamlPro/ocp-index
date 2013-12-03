-include Makefile.config

LIBS = ocp-index-lib
PROJECTS =
ifneq ($(has_cmdliner),"no")
  PROJECTS := ocp-index ocp-grep
  ifneq ($(has_curses),"no")
    PROJECTS := $(PROJECTS) ocp-browser
  endif
endif

OCPBUILD_ARGS = -install-lib $(prefix)/lib/ocp-index
OCPBUILD_INSTALL_ARGS = $(OCPBUILD_ARGS) -install-bin $(prefix)/bin

all: $(LIBS) $(PROJECTS)

ALWAYS:

$(LIBS): ALWAYS ocp-build.root
	ocp-build $@ $(OCPBUILD_ARGS)

$(LIBS:=.install): ALWAYS
	ocp-build install $(basename $@) $(OCPBUILD_INSTALL_ARGS)

$(PROJECTS): ALWAYS ocp-build.root
	ocp-build $@ $(OCPBUILD_ARGS)
	@if [ -x _obuild/$@/$@.asm ]; then cp _obuild/$@/$@.asm ./$@; \
	else cp _obuild/$@/$@.byte ./$@; fi

man/man1/%.1: %
	mkdir -p man/man1
	./$< --help=groff >$@

%.install: ALWAYS man/man1/%.1
	ocp-build install $* $(OCPBUILD_INSTALL_ARGS)
	mkdir -p $(mandir)/man1
	install -m 644 man/man1/$*.1 $(mandir)/man1/

.PHONY: install-lisp
install-lisp:
	mkdir -p $(datarootdir)/emacs/site-lisp
	install -m 644 tools/ocp-index.el $(datarootdir)/emacs/site-lisp/

.PHONY: install
install: $(LIBS:=.install) $(PROJECTS:=.install) install-lisp
	@echo
	@echo
	@echo "=== ocp-index installed ==="
	@echo
	@if $$(which emacs >/dev/null); then \
	  tools/emacs-setup.sh $(datarootdir)/emacs/site-lisp; \
	  echo; \
	fi

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

.PHONY: uninstall
uninstall:
	rm -f $(patsubst %,$(mandir)/man1/%.1,$(PROJECTS))
	rm -f $(datarootdir)/emacs/site-lisp/ocp-index.el
	ocp-build uninstall $(OCPBUILD_ARGS) $(LIBS) $(PROJECTS)

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
