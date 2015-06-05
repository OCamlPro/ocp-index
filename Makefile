-include Makefile.config

LIBS = ocp-index-lib
PROJECTS =
ifneq ($(has_cmdliner),no)
  PROJECTS := ocp-index ocp-grep
  ifneq ($(has_lambdaTerm),no)
    PROJECTS := $(PROJECTS) ocp-browser
  endif
endif

OCPBUILD_ARGS =
OCPBUILD_INSTALL_ARGS =

all: $(LIBS) $(PROJECTS) man

ALWAYS:

$(LIBS): ALWAYS ocp-build.root
	ocp-build $@ $(OCPBUILD_ARGS)

$(PROJECTS): ALWAYS ocp-build.root
	ocp-build $@ $(OCPBUILD_ARGS)
	@if [ -x _obuild/$@/$@.asm ]; then cp _obuild/$@/$@.asm ./$@; \
	else cp _obuild/$@/$@.byte ./$@; fi

MANPAGES = $(patsubst %,man/man1/%.1,$(PROJECTS))
.PHONY:man
man: $(MANPAGES)

man/man1/%.1: %
	mkdir -p man/man1
	./$< --help=groff >$@

.PHONY: install
install: $(PROJECTS) man
	opam-installer --prefix $(prefix) ocp-index.install
	@echo
	@echo
	@echo "=== ocp-index installed ==="
	@echo
	@if $$(which emacs >/dev/null); then \
	  tools/emacs-setup.sh $(datarootdir)/emacs/site-lisp; \
	  echo; \
	fi
	@if $$(which vim >/dev/null); then \
	  echo "== Vim configuration =="; \
	  echo "Add the following to your .vimrc:"; \
	  echo "  set rtp+=$(datarootdir)/ocp-index/vim"; \
          echo; \
       fi

.PHONY: clean
clean: ocp-build.root
	ocp-build clean

.PHONY: distclean
distclean:
	rm -rf _obuild man
	rm -f ocp-index
	rm -f Makefile.config
	rm -f ocp-build.root*
	rm -rf config.* aclocal.m4 *.cache configure

.PHONY: uninstall
uninstall:
	opam-installer -u --prefix $(prefix) ocp-index.install

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
