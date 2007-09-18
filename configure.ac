AC_PREREQ(2.59)

AC_INIT([thrift], [20070917])

AC_CONFIG_AUX_DIR([.])

AM_INIT_AUTOMAKE

AC_ARG_VAR([PY_PREFIX], [Prefix for installing Python modules.
                         (Normal --prefix is ignored for Python because
                         Python has different conventions.)
                         Default = "/usr"])
AS_IF([test "x$PY_PREFIX" == x], [PY_PREFIX="/usr"])

AC_CONFIG_FILES([Makefile])

AC_CONFIG_SUBDIRS([compiler/cpp lib/cpp lib/py if])

AC_OUTPUT
