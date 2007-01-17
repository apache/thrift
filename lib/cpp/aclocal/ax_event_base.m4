dnl @synopsis AX_EVENT([MINIMUM-VERSION])
dnl
dnl Test for the libevent libraries of a particular version (or newer)
dnl
dnl If no path to the installed event library is given the macro
dnl searchs under /usr, /usr/local, and /opt, and evaluates the
dnl $EVENT_ROOT environment variable.
dnl
dnl This macro calls:
dnl
dnl   AC_SUBST(EVENT_CPPFLAGS) / AC_SUBST(EVENT_LDFLAGS)
dnl
dnl And sets:
dnl
dnl   HAVE_EVENT
dnl
dnl @category InstalledPackages
dnl @category Cxx
dnl @author Marc Kwiatkowski <marc@facebook.com>
dnl @version 2006-06-15
dnl @license AllPermissive

AC_DEFUN([AX_EVENT_BASE],
[
AC_ARG_WITH([event],
	AS_HELP_STRING([--with-event@<:@=DIR@:>@], [use event (default is yes) - it is possible to specify an alternate root directory for event]),
	[
    if test "$withval" = "no"; then
    		want_event="no"
    elif test "$withval" = "yes"; then
        want_event="yes"
        ac_event_path=""
    else
        want_event="yes"
        ac_event_path="$withval"
    fi
    ],
    [want_event="yes"])

if test "x$want_event" = "xyes"; then
	event_lib_version_req=ifelse([$1], ,1.2.0,$1)
	event_lib_version_req_shorten=`expr $event_lib_version_req : '\([[0-9]]*\.[[0-9]]*\)'`
	event_lib_version_req_major=`expr $event_lib_version_req : '\([[0-9]]*\)'`
	event_lib_version_req_minor=`expr $event_lib_version_req : '[[0-9]]*\.\([[0-9]]*\)'`
	event_lib_version_req_sub_minor=`expr $event_lib_version_req : '[[0-9]]*\.[[0-9]]*\.\([[0-9]]*\)'`
	if test "x$event_lib_version_req_sub_minor" = "x" ; then
	    event_lib_version_req_sub_minor="0"
    	fi
	WANT_EVENT_VERSION=`expr $event_lib_version_req_major \* 10000 \+  $event_lib_version_req_minor \* 100 \+ $event_lib_version_req_sub_minor`
	WANT_EVENT_MAJOR_VERSION=$event_lib_version_req_major
	WANT_EVENT_MINOR_VERSION=$event_lib_version_req_minor
	AC_MSG_CHECKING(for eventlib >= $event_lib_version_req)
	succeeded=no
        
	if test "$ac_event_path" != "" && test -f "$ac_event_path/include/event.h"; then
	    ac_event_include_path=$ac_event_path/include
	    EVENT_CPPFLAGS="-I$ac_event_include_path"
	    EVENT_LDFLAGS="-L$ac_event_path/lib -levent"
            succeeded=yes
	else
	    for ac_event_path_tmp in /usr /usr/local /opt ; do
		if test -f "$ac_event_path_tmp/include/event.h"; then
		    ac_event_include_path=$ac_event_path_tmp/include
		    EVENT_CPPFLAGS="-I$ac_event_include_path"
		    EVENT_LDFLAGS="-L$ac_event_path_tmp/lib -levent"
                    succeeded=yes
		    break;
		fi
	    done
	fi

	if test "$succeeded" != "yes" ; then
            AC_MSG_ERROR([[We could not detect the event libraries (version $event_lib_version_req_shorten or higher). If you have a staged event library (still not installed) please specify \$EVENT_ROOT in your environment and do not give a PATH to --with-event option.  If you are sure you have event installed, then check your version number looking in <event/version.hpp>. See http://randspringer.de/event for more documentation.]])
	else
            AC_SUBST(EVENT_CPPFLAGS)
	    AC_SUBST(EVENT_LDFLAGS)
	    AC_DEFINE(HAVE_EVENT,,[define if the EVENT library is available])
	fi

        CPPFLAGS="$CPPFLAGS_SAVED"
       	LDFLAGS="$LDFLAGS_SAVED"
fi

])
