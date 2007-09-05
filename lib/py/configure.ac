AC_PREREQ(2.59)

AC_INIT([thriftpy], [1.0])

AC_CONFIG_AUX_DIR([.])

AM_INIT_AUTOMAKE

AC_ARG_VAR([PY_PREFIX], [Prefix for installing Python modules.
                         (Normal --prefix is ignored for Python because
                         Python has different conventions.)
                         Default = "/usr"])
AS_IF([test "x$PY_PREFIX" == x], [PY_PREFIX="/usr"])

AM_PATH_PYTHON(2.4,, :)

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
