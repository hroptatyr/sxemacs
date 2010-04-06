dnl sxe-events.m4 -- Event queue and things like that

dnl event drivers
AC_DEFUN([SXE_CHECK_LIBEVENT], [
	AC_DEFUN([SXE_CHECK_LIBEVENT], [])

	SXE_CHECK_HEADERS([event.h])
	SXE_CHECK_LIB_FUNCS([event], [dnl
		event_init event_loop event_add event_del event_once event_dispatch])

	if test "$ac_cv_header_event_h" = "yes" -a \
		"$ac_cv_lib_event___event_init" = "yes" -a \
		"$ac_cv_lib_event___event_loop" = "yes" -a \
		"$ac_cv_lib_event___event_add" = "yes" -a \
		"$ac_cv_lib_event___event_del" = "yes" -a \
		"$ac_cv_lib_event___event_once" = "yes" -a \
		"$ac_cv_lib_event___event_dispatch" = "yes"; then
		AC_DEFINE([HAVE_LIBEVENT], [1],
			[Whether all necessary functions could be found in libevent.])
		sxe_cv_feat_libevent="yes"
		have_libevent="yes"
	else
		sxe_cv_feat_libevent="no"
		have_libevent="no"
	fi
])dnl SXE_CHECK_LIBEVENT

AC_DEFUN([SXE_CHECK_EVENTS], [dnl
	AC_MSG_CHECKING([for event drivers])
	AC_MSG_RESULT([])

	SXE_CHECK_LIBEVENT
	SXE_CHECK_SUFFICIENCY([libevent], [libevent])

	## assume we havent got any of prerequisites
	have_events="no"

	have_events="yes"
])dnl SXE_CHECK_EVENTS

dnl sxe-events.m4 ends here
