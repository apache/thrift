# SYNOPSIS
#
#   AX_PROG_MAVEN_VERSION([VERSION],[ACTION-IF-TRUE],[ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   Makes sure that maven supports the version indicated. If true the shell
#   commands in ACTION-IF-TRUE are executed. If not the shell commands in
#   ACTION-IF-FALSE are run. The $MAVEN_VERSION variable will be filled with
#   the detected version.
#
#   This macro uses the $MAVEN variable to perform the check. If $MAVEN is not
#   set prior to calling this macro, the macro will fail.
#
#   Example:
#
#     AC_PATH_PROG([MAVEN],[mvn])
#     AC_PROG_MAVEN_VERSION([3.0.0],[ ... ],[ ... ])
#
#   Searches for Maven, then checks if at least version 3.0.0 is present.
#
# LICENSE
#
#   Copyright (c) 2026 Joshua M. Keyes <joshua.michael.keyes@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 1

AC_DEFUN([AX_PROG_MAVEN_VERSION],[
    AS_IF([test -n "$MAVEN"],[
        ax_maven_version="$1"

        AC_MSG_CHECKING([for maven version])
        maven_version=`$MAVEN --quiet --version`
        AC_MSG_RESULT($maven_version)

           AC_SUBST([MAVEN_VERSION],[$maven_version])

        AX_COMPARE_VERSION([$ax_maven_version],[le],[$maven_version],[
           :
            $2
        ],[
           :
            $3
        ])
    ],[
        AC_MSG_WARN([could not find maven])
        $3
    ])
])