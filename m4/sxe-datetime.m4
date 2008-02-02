dnl sxe-datetime.m4 -- Date and time functions
dnl
dnl Copyright (C) 2005, 2006, 2007, 2008 Sebastian Freundt
dnl
dnl Author: Sebastian Freundt <hroptatyr@sxemacs.org>
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions
dnl are met:
dnl
dnl 1. Redistributions of source code must retain the above copyright
dnl    notice, this list of conditions and the following disclaimer.
dnl
dnl 2. Redistributions in binary form must reproduce the above copyright
dnl    notice, this list of conditions and the following disclaimer in the
dnl    documentation and/or other materials provided with the distribution.
dnl
dnl 3. Neither the name of the author nor the names of any contributors
dnl    may be used to endorse or promote products derived from this
dnl    software without specific prior written permission.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
dnl IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
dnl WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
dnl DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
dnl FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
dnl CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
dnl SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
dnl BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
dnl WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
dnl OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
dnl IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
dnl
dnl This file is part of SXEmacs.

AC_DEFUN([SXE_CHECK_TIME_HEADERS], [dnl
	AC_HEADER_TIME
	SXE_CHECK_HEADERS([time.h utime.h])
	SXE_CHECK_HEADERS([sys/time.h sys/timeb.h sys/times.h],
	[have_time=yes])
	if test "$have_time" != "yes"; then
		SXE_CHECK_HEADERS([linux/time.h])
	fi
])dnl SXE_CHECK_TIME_HEADERS

AC_DEFUN([SXE_CHECK_UTIME], [dnl
	## ----------------------------------------------------------------
	## Checking for utime() or utimes().
	## We prefer utime, since it is more standard.
	## Some systems have utime.h but do not declare the struct anyplace,
	## so we use a more sophisticated test for utime than AC_CHECK_FUNCS.
	## ----------------------------------------------------------------
	SXE_CHECK_HEADERS([utime.h])
	AC_MSG_CHECKING([for utime])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if defined HAVE_UTIME_H
#include <utime.h>
#endif
]], [[
struct utimbuf x;
x.actime = x.modtime = 0;
utime ("/", &x);
]])], [dnl
		AC_MSG_RESULT(yes)
		AC_DEFINE([HAVE_UTIME], [1], [Description here!])
		], [dnl
		AC_MSG_RESULT(no)
		dnl We don't have utime(); how about utimes()?
		AC_CHECK_FUNCS([utimes])])
])dnl SXE_CHECK_UTIME

AC_DEFUN([SXE_CHECK_TYPE_STRUCT_TIMEVAL], [dnl

	AC_CHECK_TYPES([struct timeval], [], [],
		[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
		])
	AC_CHECK_MEMBERS([struct timeval.tv_sec, struct timeval.tv_usec], [], [], [
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
		])
])dnl SXE_CHECK_TYPE_STRUCT_TIMEVAL

AC_DEFUN([SXE_CHECK_TYPE_STRUCT_TIMESPEC], [dnl

	AC_CHECK_TYPES([struct timespec], [], [],
		[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
		])
])dnl SXE_CHECK_TYPE_STRUCT_TIMESPEC


AC_DEFUN([SXE_CHECK_TYPE_TIME_T], [dnl

	AC_CHECK_TYPES([time_t], [], [],
		[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
		])
])dnl SXE_CHECK_TYPE_TIME_T

AC_DEFUN([SXE_CHECK_GETTIMEOFDAY], [dnl
	## check for the gettimeofday function
	AC_CHECK_FUNCS([gettimeofday], [], [],
		[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
		])

	if test "$ac_cv_type_struct_timeval" = "yes" \
	     -a "$ac_cv_func_gettimeofday" = "yes"; then
		AC_MSG_CHECKING([whether gettimeofday() accepts one or two arguments])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
			]], [[
struct timeval tm; /* we MUST NOT shadow the global time symbol, fuck it */
gettimeofday(&tm, 0);
			]])],
			[gettimeofday_nargs="2"], [gettimeofday_nargs="1"])
		AC_MSG_RESULT([$gettimeofday_nargs])
		AC_DEFINE_UNQUOTED([GETTIMEOFDAY_NARGS],
			[$gettimeofday_nargs],
			[Number of arguments of the gettimeofday() fun])

		if test "$gettimeofday_nargs" = "1"; then
			## legacy bullshit
			AC_DEFINE([GETTIMEOFDAY_ONE_ARGUMENT], [1],
				[Legacy shit! Use GETTIMEOFDAY_NARGS instead!])
		fi
	fi

	if test "$ac_cv_type_struct_timespec" = "yes" \
	     -a "$ac_cv_func_gettimeofday" = "yes"; then
		AC_MSG_CHECKING([whether gettimeofday() can cope with timespecs])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined(HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  elif define(HAVE_TIME_H)
#    include <time.h>
#  endif
#endif
			]], [[
struct timespec time;
#if GETTIMEOFDAY_NARGS == 1
gettimeofday(&time);
#elif GETTIMEOFDAY_NARGS == 2
gettimeofday(&time, 0);
#else
!@#$% VERY_UNLIKELY_TO_COMPILE )(*&^
#endif
			]])],
			[gettimeofday_handles_struct_timespec=yes],
			[gettimeofday_handles_struct_timespec=no])
		AC_MSG_RESULT([$gettimeofday_handles_struct_timespec])
		AC_DEFINE_UNQUOTED([GETTIMEOFDAY_HANDLES_STRUCT_TIMESPEC],
			[$gettimeofday_handles_struct_timespec],
			[Whether gettimeofday() can be called on a `struct timespec'])
	fi
])dnl SXE_CHECK_GETTIMEOFDAY

AC_DEFUN([SXE_CHECK_DO_GETTIMEOFDAY], [dnl
	AC_CHECK_FUNCS([do_gettimeofday], [], [],
		[
#ifdef HAVE_LINUX_TIME_H
#  include <linux/time.h>
#endif
		])

	if test "$ac_cv_type_struct_timeval" = "yes" \
	     -a "$ac_cv_func_do_gettimeofday" = "yes"; then
		AC_MSG_CHECKING([whether do_gettimeofday accepts one or two arguments])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#ifdef HAVE_LINUX_TIME_H
#  include <linux/time.h>
#endif
]], [[
struct timeval time;
do_gettimeofday(&time, 0);
]])], [do_gettimeofday_nargs="2"], [do_gettimeofday_nargs="1"])
		AC_MSG_RESULT([$do_gettimeofday_nargs])
		AC_DEFINE_UNQUOTED([DO_GETTIMEOFDAY_NARGS],
			[$do_gettimeofday_nargs],
			[Number of arguments of the do_gettimeofday() fun])
	fi
])dnl SXE_CHECK_DO_GETTIMEOFDAY

AC_DEFUN([SXE_CHECK_CURRENT_KERNEL_TIME], [dnl
	AC_CHECK_FUNCS([current_kernel_time], [], [],
		[
#ifdef HAVE_LINUX_TIME_H
#  include <linux/time.h>
#endif
		])

	if test "$ac_cv_type_struct_timespec" = "yes" \
	     -a "$ac_cv_func_current_kernel_time" = "yes"; then
		:
	fi
])dnl SXE_CHECK_CURRENT_KERNEL_TIME

AC_DEFUN([SXE_CHECK_CLOCK_GETTIME], [dnl
	## checks for both clock_gettime() and clock_getres()
	AC_CHECK_LIB([rt], [clock_gettime], [
		AC_DEFINE([HAVE_CLOCK_GETTIME], [1],
			[Whether clock_gettime() is in librt])])
	AC_CHECK_LIB([rt], [clock_getres], [
		AC_DEFINE([HAVE_CLOCK_GETRES], [1],
			[Whether clock_getres() is in librt])])
])dnl SXE_CHECK_CLOCK_GETTIME


dnl recommended interface function, does all the necessary trickery
AC_DEFUN([SXE_CHECK_METRONOME], [dnl
	AC_MSG_CHECKING([for metronome support requisites])
	AC_MSG_RESULT([])

	## assume we havent got any of prerequisites
	have_metronome="no"

	SXE_CHECK_TIME_HEADERS

	## check for utime
	SXE_CHECK_UTIME
	## checks for structure members
	AC_STRUCT_TM
	AC_STRUCT_TIMEZONE

	## check for struct timeval
	SXE_CHECK_TYPE_STRUCT_TIMEVAL
	SXE_CHECK_TYPE_STRUCT_TIMESPEC
	SXE_CHECK_TYPE_TIME_T

	SXE_CHECK_GETTIMEOFDAY
	SXE_CHECK_DO_GETTIMEOFDAY
	SXE_CHECK_CURRENT_KERNEL_TIME
	SXE_CHECK_CLOCK_GETTIME

	if test "$ac_cv_type_struct_timeval" = "yes" \
	     -a "$ac_cv_func_gettimeofday" = "yes"; then
		have_metronome="yes"
	fi

	if test "$ac_cv_type_struct_timespec" = "yes" \
	     -a "$ac_cv_func_do_gettimeofday" = "yes"; then
		have_metronome="yes"
	fi

	if test "$ac_cv_type_struct_timespec" = "yes" \
	     -a "$ac_cv_func_current_kernel_time" = "yes"; then
		have_metronome="yes"
	fi

	if test "$ac_cv_type_struct_timespec" = "yes" \
	     -a "$ac_cv_lib_rt_clock_gettime" = "yes" \
	     -a "$ac_cv_lib_rt_clock_getres" = "yes"; then
		have_metronome="yes"
	fi

	## legacy bullshit
	if test "$ac_cv_type_struct_timeval" = "yes" -a \
		"$ac_cv_member_struct_timeval_tv_sec" = "yes" -a \
		"$ac_cv_member_struct_timeval_tv_usec" = "yes"; then
		AC_DEFINE([HAVE_TIMEVAL], [1],
			[Legacy shit! Use HAVE_STRUCT_TIMEVAL instead!])
	fi
])dnl SXE_CHECK_METRONOME

dnl sxe-datetime.m4 ends here
