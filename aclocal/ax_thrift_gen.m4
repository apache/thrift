dnl @synopsis AX_THRIFT_GEN(SHORT_LANGUAGE, LONG_LANGUAGE, DEFAULT)
dnl
dnl Allow a particular language generator to be disabled.
dnl
dnl This macro has poor error handling and is poorly documented.
dnl It is intended only for internal use by the Thrift compiler.
dnl
dnl @author David Reiss <dreiss@facebook.com>
dnl @version 2008-02-20
dnl @license AllPermissive

AC_DEFUN([AX_THRIFT_GEN],
         [
          AC_ARG_ENABLE([gen-$1],
                        AC_HELP_STRING([--enable-gen-$1], [Enable the $2 compiler.  Default is $3]),
                        [ax_thrift_gen_$1="$enableval"],
                        [ax_thrift_gen_$1=$3]
                        )
          dnl I'd like to run the AM_CONDITIONAL here, but automake likes
          dnl all AM_CONDITIONALs to be nice and explicit in configure.ac.
          dnl AM_CONDITIONAL([THRIFT_GEN_$1], [test "$ax_thrift_gen_$1" = "yes"])
         ])
