dnl
dnl The following commonly available Java macros converted to AX prefix 
dnl in support of proper thrift/autoconf extension naming rules
dnl

dnl autoconf M4 macros for Java programs
dnl Copyright Stephane Bortzmeyer <bortzmeyer@pasteur.fr>, 1999
dnl Released under the GPL licence
dnl
dnl A sample configure.in is at the end of this file. Each macro is
dnl documented by comments before its definition. Here is a summary:
dnl
dnl AX_PROG_JAVAC: finds a Java compiler
dnl AX_PROG_JAVA: finds a Java virtual machine
dnl AX_CHECK_CLASS: finds if we have the given class (beware of CLASSPATH!)
dnl AX_CHECK_RQRD_CLASS: finds if we have the given class and stops otherwise


dnl AX_PROG_JAVAC tests an existing Java compiler. It uses the environment
dnl variable JAVAC then tests in sequence various common Java compilers. For
dnl political reasons, it starts with the free ones.
dnl If you want to force a specific compiler:
dnl  - at the configure.in level, set JAVAC=yourcompiler before calling 
dnl    AX_PROG_JAVAC
dnl  - at the configure level, setenv JAVAC
dnl You can use the JAVAC variable in your Makefile.in, with @JAVAC@.
dnl
dnl *Warning*: its success or failure can depend on a proper setting of the
dnl CLASSPATH env. variable.
dnl
dnl TODO: allow to exclude compilers (rationale: most Java programs cannot compile
dnl with some compilers like guavac).

AC_DEFUN([AX_PROG_JAVAC],[
test -z "$JAVAC" && AC_CHECK_PROGS(JAVAC, "gcj -C" guavac javac)
test -z "$JAVAC" && AC_MSG_ERROR([no acceptable Java compiler found in \$PATH])
AX_PROG_JAVAC_WORKS
])


dnl AX_PROG_JAVA tests an existing Java virtual machine. It uses the 
dnl environment variable JAVA then tests in sequence various common Java 
dnl virtual machines. For political reasons, it starts with the free ones.
dnl You *must* call [AX_PROG_JAVAC] before.
dnl If you want to force a specific VM:
dnl  - at the configure.in level, set JAVA=yourvm before calling AX_PROG_JAVA
dnl  - at the configure level, setenv JAVA
dnl You can use the JAVA variable in your Makefile.in, with @JAVA@.
dnl
dnl *Warning*: its success or failure can depend on a proper setting of the
dnl CLASSPATH env. variable.
dnl
dnl TODO: allow to exclude virtual machines (rationale: most Java programs 
dnl cannot run with some VM like kaffe).
dnl
dnl TODO: allow to call this macro without [AX_PROG_JAVAC] before, using a
dnl class included uuencoded as an here document and uudecoded on the fly
dnl (check uudecode first) (rationale: a Java package can have no sources).

AC_DEFUN([AX_PROG_JAVA],[
test -z "$JAVA" && AC_CHECK_PROGS(JAVA, kaffe java)
test -z "$JAVA" && AC_MSG_ERROR([no acceptable Java virtual machine found in \$PATH])
AX_PROG_JAVA_WORKS
])


dnl AX_CHECK_CLASS tests the existence of a given Java class, either in
dnl a jar or in a '.class' file. 
dnl
dnl *Warning*: its success or failure can depend on a proper setting of the
dnl CLASSPATH env. variable.

AC_DEFUN([AX_CHECK_CLASS],[
CLASS=$1
echo -n "Testing if we have Java class $CLASS... "
JAVA_TEST=Test.java
CLASS=`echo $CLASS|sed 's/\./_/g'`
cat << \EOF > $JAVA_TEST
import $1;
public class Test {
}
EOF
if AC_TRY_COMMAND($JAVAC $JAVA_TEST) >/dev/null 2>&1; then
  eval HAVE_$CLASS=yes
  eval HAVE_LAST_CLASS=yes
  echo "yes"
else
  eval HAVE_$CLASS=no
  eval HAVE_LAST_CLASS=no
  echo "no (check config.log, CLASSPATH may be wrong?)"
fi
rm -f $JAVA_TEST
])


dnl AX_CHECK_RQRD_CLASS tests the existence of a given Java class, either in
dnl a jar or in a '.class' file and fails if it doesn't exist.
dnl
dnl *Warning*: its success or failure can depend on a proper setting of the
dnl CLASSPATH env. variable.

AC_DEFUN([AX_CHECK_RQRD_CLASS],[
CLASS=`echo $1|sed 's/\./_/g'`
AX_CHECK_CLASS($1)
if test "$HAVE_LAST_CLASS" = "no"; then
	AC_MSG_ERROR([Required class $1 missing, exiting.])
fi
])


dnl AX_CHECK_CLASSPATH just displays the CLASSPATH, for the edification
dnl of the user.

AC_DEFUN([AX_CHECK_CLASSPATH],[
if test -z "$CLASSPATH"; then
	echo "You have no CLASSPATH, I hope it is good"
else
	echo "You have CLASSPATH $CLASSPATH, hope it is correct"
fi
])


dnl Internal use

AC_DEFUN([AX_PROG_JAVA_WORKS],[
echo -n "Testing if $JAVA works... "
JAVA_TEST=Test.java
CLASS_TEST=Test.class
TEST=Test
changequote(, )dnl
cat << \EOF > $JAVA_TEST
public class Test {
	    public static void main (String args[]) {}
}
EOF
changequote([, ])dnl
if AC_TRY_COMMAND($JAVAC $JAVA_TEST) >/dev/null 2>&1; then
  if AC_TRY_COMMAND($JAVA $TEST) >/dev/null 2>&1; then
    echo "yes"
  else
    echo "no"
    AC_MSG_ERROR(The Java VM $JAVA failed (see config.log, check the CLASSPATH?))
  fi
else
  AC_MSG_ERROR(The Java compiler $JAVAC failed (see config.log, check the CLASSPATH?))
fi
rm -f $JAVA_TEST $CLASS_TEST
]
)


dnl Internal use

AC_DEFUN([AX_PROG_JAVAC_WORKS],[
echo -n "Testing if $JAVAC works... "
JAVA_TEST=Test.java
CLASS_TEST=Test.class
cat << \EOF > $JAVA_TEST
public class Test {
}
EOF
if AC_TRY_COMMAND($JAVAC $JAVA_TEST) >/dev/null 2>&1; then
  echo "yes"
else
  echo "no"
  AC_MSG_ERROR([The Java compiler $JAVAC failed (see config.log, check the CLASSPATH?)])
fi
rm -f $JAVA_TEST $CLASS_TEST
])




dnl TODO: add a AX_CHECK_JAVA_SOURCE where the user can give a complete
dnl Java source to compile or to compile and run.



dnl dnl This is a sample configure.in
dnl dnl Process this file with autoconf to produce a configure script.
dnl dnl Drop the [] around the macro names
dnl
dnl [
dnl
dnl AC_INIT(UnTag.java)
dnl
dnl dnl Checks for programs.
dnl AX_CHECK_CLASSPATH
dnl AX_PROG_JAVAC
dnl AX_PROG_JAVA
dnl
dnl dnl Checks for classes
dnl AX_CHECK_RQRD_CLASS(org.xml.sax.Parser)
dnl AX_CHECK_RQRD_CLASS(com.jclark.xml.sax.Driver)
dnl
dnl AC_OUTPUT(Makefile)
dnl
dnl ]


dnl @synopsis AX_CHECK_JUNIT
dnl
dnl AX_CHECK_JUNIT tests the availability of the Junit testing
dnl framework, and set some variables for conditional compilation
dnl of the test suite by automake.
dnl
dnl If available, JUNIT is set to a command launching the text
dnl based user interface of Junit, @JAVA_JUNIT@ is set to $JAVA_JUNIT
dnl and @TESTS_JUNIT@ is set to $TESTS_JUNIT, otherwise they are set
dnl to empty values.
dnl
dnl You can use these variables in your Makefile.am file like this :
dnl
dnl  # Some of the following classes are built only if junit is available
dnl  JAVA_JUNIT  = Class1Test.java Class2Test.java AllJunitTests.java
dnl
dnl  noinst_JAVA = Example1.java Example2.java @JAVA_JUNIT@
dnl
dnl  EXTRA_JAVA  = $(JAVA_JUNIT)
dnl
dnl  TESTS_JUNIT = AllJunitTests
dnl
dnl  TESTS       = StandaloneTest1 StandaloneTest2 @TESTS_JUNIT@
dnl
dnl  EXTRA_TESTS = $(TESTS_JUNIT)
dnl
dnl  AllJunitTests :
dnl     echo "#! /bin/sh" > $@
dnl     echo "exec @JUNIT@ my.package.name.AllJunitTests" >> $@
dnl     chmod +x $@
dnl
dnl @author Luc Maisonobe
dnl @version $Id: ac_check_junit.ac,v 12.0 2004/11/17 03:43:38 bostic Exp $
dnl
AC_DEFUN([AX_CHECK_JUNIT],[
AC_CACHE_VAL(ax_cv_prog_JUNIT,[
AX_CHECK_CLASS(junit.textui.TestRunner)
if test x"`eval 'echo $ac_cv_class_junit_textui_TestRunner'`" != xno ; then
  ax_cv_prog_JUNIT='$(CLASSPATH_ENV) $(JAVA) $(JAVAFLAGS) junit.textui.TestRunner'
fi])
AC_MSG_CHECKING([for junit])
if test x"`eval 'echo $ax_cv_prog_JUNIT'`" != x ; then
  JUNIT="$ax_cv_prog_JUNIT"
  JAVA_JUNIT='$(JAVA_JUNIT)'
  TESTS_JUNIT='$(TESTS_JUNIT)'
else
  JUNIT=
  JAVA_JUNIT=
  TESTS_JUNIT=
fi
AC_MSG_RESULT($JAVA_JUNIT)
AC_SUBST(JUNIT)
AC_SUBST(JAVA_JUNIT)
AC_SUBST(TESTS_JUNIT)])
