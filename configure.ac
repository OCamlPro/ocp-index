AC_INIT(ocp-index,1.1.5)
AC_COPYRIGHT(Copyright 2014 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_CHECK_OCAML_PKG([cmdliner])
if test "$OCAML_PKG_cmdliner" = "no"; then
   AC_MSG_ERROR([Please install cmdliner.])
fi

AC_CHECK_OCAML_PKG([lambda-term])
AC_CHECK_OCAML_PKG([spelll])

AC_CONFIG_FILES(
  Makefile.config
)
AC_OUTPUT

if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"

echo
echo Executables will be installed in ${bindir}
