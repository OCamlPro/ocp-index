#!/bin/sh -ue

# Checks that the required emacs packages are installed and gives direction
# for emacs configuration

eldir=

check_eldir () ( [ -e "$eldir/ocp-index.el" ] )

if [ $# -ge 1 ]; then
    eldir=$1; shift
else
    eldir=/usr/local/share/typerex/ocp-index
    check_eldir || eldir=/usr/share/typerex/ocp-index
    check_eldir || {
        $(which opam >/dev/null) &&
        eldir=$(opam config var prefix)/share/typerex/ocp-index
    }
fi

[ $# -eq 0 ] || ( echo "USAGE: $0 [ocp-index.el install dir]" >&2; exit 1 )

if ! check_eldir; then
    echo "Could not find ocp-index.el, please specify the directory where" >&2
    echo "it was installed." >&2
    exit 2
fi

echo "To setup tuareg-mode (or ocaml-mode) to use ocp-index for completion,"
echo "please add the following line to your .emacs :"
echo
echo '  (load-file "'$eldir/ocp-index.el'")'
if ! emacs --batch --eval "(require 'auto-complete)" 2>/dev/null; then
    echo
    echo "WARNING: you do not appear to have 'auto-complete.el' installed,"
    echo "         and it is required for completion in emacs. Check your"
    echo "         distribution for a package named 'auto-complete-el' or"
    echo "         similar."
fi
