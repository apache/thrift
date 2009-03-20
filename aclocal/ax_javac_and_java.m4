dnl @synopsis AX_JAVAC_AND_JAVA
dnl
dnl Test for the presence of a JDK.
dnl
dnl If "JAVA" is defined in the environment, that will be the only
dnl java command tested.  Otherwise, a hard-coded list will be used.
dnl Similarly for "JAVAC".
dnl
dnl This macro does not currenly support testing for a particular
dnl Java version, the presence of a particular class, testing for
dnl only one of "java" and "javac", or compiling or running
dnl user-provided Java code.
dnl
dnl After AX_JAVAC_AND_JAVA runs, the shell variables "success" and
dnl "ax_javac_and_java" are set to "yes" or "no", and "JAVAC" and
dnl "JAVA" are set to the appropriate commands.
dnl
dnl @category Java
dnl @version 2009-02-09
dnl @license AllPermissive
dnl
dnl Copyright (C) 2009 David Reiss
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.


AC_DEFUN([AX_JAVAC_AND_JAVA],
         [

          dnl Hard-coded default commands to test.
          JAVAC_PROGS="javac,jikes,gcj -C"
          JAVA_PROGS="java,kaffe"

          dnl Allow the user to specify an alternative.
          if test -n "$JAVAC" ; then
            JAVAC_PROGS="$JAVAC"
          fi
          if test -n "$JAVA" ; then
            JAVA_PROGS="$JAVA"
          fi

          AC_MSG_CHECKING(for javac and java)

          echo "public class configtest_ax_javac_and_java { public static void main(String args@<:@@:>@) { } }" > configtest_ax_javac_and_java.java
          success=no
          oIFS="$IFS"

          IFS=","
          for JAVAC in $JAVAC_PROGS ; do
            IFS="$oIFS"

            echo "Running \"$JAVAC configtest_ax_javac_and_java.java\"" >&AS_MESSAGE_LOG_FD
            if $JAVAC configtest_ax_javac_and_java.java >&AS_MESSAGE_LOG_FD 2>&1 ; then

              IFS=","
              for JAVA in $JAVA_PROGS ; do
                IFS="$oIFS"

                echo "Running \"$JAVA configtest_ax_javac_and_java\"" >&AS_MESSAGE_LOG_FD
                if $JAVA configtest_ax_javac_and_java >&AS_MESSAGE_LOG_FD 2>&1 ; then
                  success=yes
                  break 2
                fi

              done

            fi

          done

          rm -f configtest_ax_javac_and_java.java configtest_ax_javac_and_java.class

          if test "$success" != "yes" ; then
            AC_MSG_RESULT(no)
            JAVAC=""
            JAVA=""
          else
            AC_MSG_RESULT(yes)
          fi

          ax_javac_and_java="$success"

          ])
