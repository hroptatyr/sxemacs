dnl compiler.m4 --- compiler magic
dnl
dnl Copyright (C) 2005, 2006, 2007, 2008 Sebastian Freundt
dnl Copyright (c) 2005 Steven G. Johnson
dnl Copyright (c) 2005 Matteo Frigo
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


## C compiler
AC_DEFUN([SXE_CHECK_CC_VERSION], [dnl
	## ---------------------------------------------------------------
	## Get version information for:
	## C compiler, libc
	## #### should do CC compiler, too, if different from SXEMACS_CC
	## ---------------------------------------------------------------

	compiler_version=""
	gcc_compiler_specs=""
	AC_MSG_CHECKING([for compiler version information])

	dnl First try portable compilers, then crack system secrets
	dnl run through the AC_PROG_CC mill.
	case "$(basename $CC)" in
	dnl GNU cc compiler
	gcc*)
		compiler_version=$($CC --version | head -1)
		gcc_compiler_specs=$($CC -v 2>&1 | sed 's/.* \([[^ ]]\)/\1/' | head -1)
		;;
	dnl The Intel cc compiler
	ic*)
		compiler_version=$($CC -V 2>&1 | head -1)
		;;
	dnl a generic thing called `cc', we just hope that it accepts --version
	cc*)
		compiler_version=$($CC --version 2>&1 | head -1)
		;;
	dnl non-gcc machine-specific magic - contributions welcome
	*)
		case "$ac_cv_build" in
		*-*-aix*   )
			dnl Yes, it's this ugly for AIX...
			realcc=`which $SXEMACS_CC`
			dnl Might be a symlink created by replaceCset command
			if test -L $realcc ; then
				ccdir=`dirname $realcc`
				ccprog=`/bin/ls -l $realcc | sed 's/.* \([[^ ]]\)/\1/'`
				dnl This doesn't handle ../../xlc type
				dnl stuff, but I've not seen one...
				case $ccprog in
				*/*)
					realcc=$ccprog
					;;
				*)
					realcc=$ccdir/$ccprog
					;;
				esac
			fi
			lpp=`lslpp -wqc $realcc | cut -f2 -d:`
			if test ! -z "$lpp" ; then
				lppstr=`lslpp -Lqc $lpp`
				lpplev=`echo "$lppstr" | cut -f3 -d:`
				lppdesc=`echo "$lppstr" | cut -f8 -d:`
			fi
			if test ! -z "$lpplev" ; then
				compiler_version="$lpp $lpplev - $lppdesc"
			fi
			;;

		*-*-solaris*)
			compiler_version=`$SXEMACS_CC -V 2>&1 | head -1`
			;;

		alpha*-dec-osf*)
			compiler_version=`$SXEMACS_CC -V | tr '\n' ' '`
			;;

		mips-sgi-irix*)
			compiler_version=`$SXEMACS_CC -version`
			;;
		*)
			compiler_version=""
			AC_MSG_RESULT([detection failed (please report this)])
			AC_MSG_WARN([No C compiler available nor specified])
			;;
		esac
		;;
	esac

	AC_SUBST([compiler_version])
	AC_MSG_RESULT([$compiler_version])

	dnl Awww, shucks.
	if test -z "$compiler_version"; then
		compiler_version="detection failed (please report this)"
	fi
])dnl SXE_CHECK_CC_VERSION

AC_DEFUN([SXE_CHECK_CC__FGNU89_INLINE], [dnl
	## defines sxe_cc__fgnu89_inline
	AC_MSG_CHECKING([whether $CC supports -fgnu89-inline])
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([on])
	CC="$CC -fgnu89-inline"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]])],
		[sxe_cc__fgnu89_inline="yes"],
		[sxe_cc__fgnu89_inline="no"])
	SXE_RESTORE_LIBS
	AC_MSG_RESULT([$sxe_cc__fgnu89_inline])
	AC_DEFUN([SXE_CHECK_CC__FGNU89_INLINE], [])
])dnl SXE_CHECK_CC__FGNU89_INLINE

AC_DEFUN([SXE_CHECK_CC_GNU89_EXTERN_INLINE], [dnl
	## defines sxe_cc_gnu89_extern_inline
	AC_MSG_CHECKING([whether $CC supports `extern inline'])
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
extern inline void
foobar(void)
{
	return;
}
		]])],
		[sxe_cc_gnu89_extern_inline="yes"],
		[sxe_cc_gnu89_extern_inline="no"])
	SXE_RESTORE_LIBS

	AC_MSG_RESULT([$sxe_cc_gnu89_extern_inline])
])dnl SXE_CHECK_CC_GNU89_EXTERN_INLINE

AC_DEFUN([SXE_CHECK_CC_EXTERN_INLINE_DARWIN], [
	AC_MSG_CHECKING([whether $CC supports `extern inline'])

	case "$compiler_version" in
	gcc*\ 4.0.1* | * )
		# For now we assume all MacOS compilers
		# are not able to handle EXTERN_INLINE
		AC_DEFINE([SXE_STATIC_EXTERN_INLINE], [1],
			[The MacOS gcc does not support extern inline])
		sxe_cc_gnu89_extern_inline="no"
		;;
	esac

	AC_MSG_RESULT([$sxe_cc_gnu89_extern_inline])
])dnl SXE_CHECK_CC_EXTERN_INLINE_DARWIN


AC_DEFUN([SXE_CHECK_CC_GNU89_EXTERN_INLINE_ALLERGY], [dnl
	## defines sxe_cc_gnu89_extern_inline_allergy
	AC_MSG_CHECKING([whether $CC is allergic against gnu89 `extern inline' in C99])
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([on])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
extern inline void
foobar(void)
{
	return;
}
		]])],
		[sxe_cc_gnu89_extern_inline_allergy="no"],
		[sxe_cc_gnu89_extern_inline_allergy="yes"])
	SXE_RESTORE_LIBS
	AC_MSG_RESULT([$sxe_cc_gnu89_extern_inline_allergy])
])dnl SXE_CHECK_CC_GNU89_EXTERN_INLINE_ALLERGY

AC_DEFUN([SXE_CHECK_CC___ATTRIBUTE__GNU_INLINE], [dnl
	## defines sxe_cc___attribute__gnu_inline

	AC_MSG_CHECKING([whether attribute `gnu_inline' is an efficacious medicine])
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([on])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
extern inline void
__attribute__((gnu_inline))
foobar(void)
{
	return;
}
		]])],
		[sxe_cc___attribute__gnu_inline="yes"],
		[sxe_cc___attribute__gnu_inline="no"])
	SXE_RESTORE_LIBS
	AC_MSG_RESULT([$sxe_cc___attribute__gnu_inline])
])dnl SXE_CHECK_CC___ATTRIBUTE__GNU_INLINE

AC_DEFUN([_SXE_CHECK_CC__FGNU89_INLINE_MEDICINE], [dnl
	mytmp_save_CC="$CC"
	CC="$CC -fgnu89-inline"
	save_sxe_cc_blabla="$sxe_cc_gnu89_extern_inline_allergy"
	SXE_CHECK_CC_GNU89_EXTERN_INLINE_ALLERGY
	if test "$save_sxe_cc_blabla" = "yes" -a \
		"$sxe_cc_gnu89_extern_inline_allergy" = "no"; then
		sxe_cc__fgnu89_inline_medicine="yes"
	else
		sxe_cc__fgnu89_inline_medicine="no"
	fi
	CC="$mytmp_save_CC"
	sxe_cc_gnu89_extern_inline_allergy="$save_sxe_cc_blabla"
	AC_MSG_CHECKING([whether flag -fgnu89-inline has been an efficacious medicine])
	AC_MSG_RESULT([$sxe_cc__fgnu89_inline_medicine])
])dnl _SXE_CHECK_CC__FGNU89_INLINE_MEDICINE

AC_DEFUN([SXE_CHECK_CC__FGNU89_INLINE_MEDICINE], [dnl
	## defines sxe_cc__fgnu89_inline_medicine
	AC_REQUIRE([SXE_CHECK_CC__FGNU89_INLINE])
	AC_MSG_CHECKING([whether flag -fgnu89-inline is an efficacious medicine])
	if test "$sxe_cc__fgnu89_inline" = "yes"; then
		AC_MSG_RESULT([])
		_SXE_CHECK_CC__FGNU89_INLINE_MEDICINE
	else
		sxe_cc__fgnu89_inline_medicine="no"
		AC_MSG_RESULT([$sxe_cc__fgnu89_inline_medicine])
	fi
])dnl SXE_CHECK_CC__FGNU89_INLINE_MEDICINE

AC_DEFUN([SXE_CHECK_CC_GNU89_EXTERN_INLINE_MEDICINE], [dnl
	## check if we have a medicine against it
	## defines gnu89_extern_inline_medicine
	AC_REQUIRE([SXE_CHECK_CC__FGNU89_INLINE])
	if test "$sxe_cc_gnu89_extern_inline" = "yes" -a \
		"$sxe_cc_gnu89_extern_inline_allergy" = "yes"; then
		SXE_CHECK_CC___ATTRIBUTE__GNU_INLINE
		SXE_CHECK_CC__FGNU89_INLINE_MEDICINE
	fi
	if test "$sxe_cc__fgnu89_inline_medicine" = "yes"; then
		CC="$CC -fgnu89-inline"
	elif test "$sxe_cc__fgnu89_inline" = "yes"; then
		## use gnu89-inline anyway
		## this is to cope with gcc 4.3.0 which seems to support
		## real C99 extern inlines, which in turn would break
		## our build as we paid no heed at this (yet)
		CC="$CC -fgnu89-inline"
	fi
])dnl SXE_CHECK_CC_GNU89_EXTERN_INLINE_MEDICINE

AC_DEFUN([SXE_CHECK_CC_EXTERN_INLINE], [dnl

	case "$ac_cv_build" in
	*-*-darwin* )
		## don't bother at all ... just invoke the darwin handler
		SXE_CHECK_CC_EXTERN_INLINE_DARWIN
		;;
	*)
		## we simply presume that extern inline is possible first
		SXE_CHECK_CC_GNU89_EXTERN_INLINE
		## check if gcc dislikes gnu89 inlines in c99
		SXE_CHECK_CC_GNU89_EXTERN_INLINE_ALLERGY
		## generally check if we support -fgnu89-inline
		SXE_CHECK_CC__FGNU89_INLINE
	esac

	if test "$sxe_cc_gnu89_extern_inline" = "yes" -a \
		"$sxe_cc_gnu89_extern_inline_allergy" = "yes"; then
		SXE_CHECK_CC_GNU89_EXTERN_INLINE_MEDICINE
	elif test "$sxe_cc__fgnu89_inline" = "yes"; then
		## use gnu89-inline anyway
		## this is to cope with gcc 4.3.0 which seems to support
		## real C99 extern inlines, which in turn would break
		## our build as we paid no heed at this (yet)
		CC="$CC -fgnu89-inline"
	fi
])dnl SXE_CHECK_CC_EXTERN_INLINE

AC_DEFUN([SXE_CHECK_CC_CHAR], [dnl

	## ----------------------------------------------------------------
	## Checking for gnuc va list need in solaris
	## ----------------------------------------------------------------
	if test "$GCC" = "yes" -a "$opsys" = "sol2" ; then
		AC_MSG_CHECKING(for need to define gnuc_va_list)
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <stdarg.h>
#define __GNUC_VA_LIST
#define _VA_LIST_
#define _VA_LIST va_list
typedef void *__gnuc_va_list;
typedef __gnuc_va_list va_list;]],[[1]])], [dnl
		AC_MSG_RESULT(yes)
		AC_DEFINE([NEED_GNUC_VA_LIST], [1], [Description here!])], [dnl
		AC_MSG_RESULT(no)])
	fi

	AC_C_BIGENDIAN
	AC_C_VOLATILE
	AC_C_CONST
	AC_C_INLINE
	SXE_C_TYPEOF
	AC_C_CHAR_UNSIGNED
	AC_TYPE_LONG_DOUBLE  dnl only in >= 2.60
	AC_TYPE_LONG_DOUBLE_WIDER  dnl only in >= 2.60
	AC_C_STRINGIZE
	AC_C_PROTOTYPES

	dnl in case we need the modules
	SXE_LD_EXPORT_DYNAMIC
	dnl in case compiler issues PIE by default which breaks pdump
	SXE_LD_NO_PIE
])dnl SXE_CHECK_CC_CHAR

AC_DEFUN([SXE_CHECK_CC_HACKS], [dnl

	## Identify compilers to enable compiler-specific hacks.
	## Add support for other compilers HERE!
	## GCC is already identified elsewhere.
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
int main ()
{
#if defined __SUNPRO_C
	return 11;
#elif defined __DECC
	return 12;
#elif defined __USLC__ && defined __SCO_VERSION__
	return 13;
#elif defined __INTEL_COMPILER
	return 14;
#else
	return 0;
#endif
}]])], [], [dnl
	case "$?" in
	11)
		echo "You appear to be using the SunPro C compiler."
		__SUNPRO_C=yes
		;;
	12)
		echo "You appear to be using the DEC C compiler."
		__DECC=yes
		;;
	13)
		echo "You appear to be using the SCO C compiler."
		__USLC__=yes
		;;
	14)
		echo "You appear to be using the Intel C++ compiler."
		__ICC=yes
		## Newer versions of icc claim to be GCC
		GCC=no
		;;
	esac], [AS_MESSAGE([cannot cross-compile])])
])dnl SXE_CHECK_CC_HACKS


AC_DEFUN([SXE_DO_CC_HACKS], [dnl
	## -----------------------
	## Compiler-specific hacks
	## -----------------------

	dnl DEC C `-std1' means ANSI C mode
	if test "$__DECC" = "yes"; then
		SXE_APPEND_UNDUP([-std1], [CFLAGS])
	fi

	dnl Some versions of SCO native compiler need -Kalloca
	if test "$__USLC__" = yes; then
		AC_MSG_CHECKING([whether the -Kalloca compiler flag is needed])
		need_kalloca=no
		AC_LINK_IFELSE([AC_LANG_SOURCE([[void *x = alloca(4);]])], [:], [
			SXE_DUMP_LIBS
			CFLAGS="$CFLAGS -Kalloca"
			AC_LINK_IFELSE([AC_LANG_SOURCE([[void *x = alloca(4);]])],
				[ need_kalloca=yes ])
			SXE_RESTORE_LIBS])
		AC_MSG_RESULT([$need_kalloca])
		if test "$need_kalloca" = "yes"; then
			SXE_APPEND_UNDUP([-Kalloca], [c_switch_system])
			SXE_APPEND_UNDUP([-Kalloca], [CFLAGS])
		fi
	fi

	dnl Die if g++
	if test "$CC" = "g++" -o "$SXE_CC" = "g++" ; then
		SXE_DIE("Building with g++ is not supported")
	fi


])dnl SXE_DO_CC_HACKS

AC_DEFUN([SXE_CHECK_CC_NESTED_FUNS], [dnl
	## defines HAVE_NESTED_FUNS

	AC_MSG_CHECKING([whether functions can be nested])
	_SXE_CHECK_CC_NESTED_FUNS
	AC_MSG_RESULT([$sxe_cv_c_nested_funs])

	if test "$sxe_cv_c_nested_funs" != "yes"; then
		## in this case we check if we can do with -fnested-functions
		SXE_CHECK_COMPILER_FLAGS([-fnested-functions])
		if test "$sxe_cv_c_flags__fnested_functions" = "yes"; then
			AC_MSG_CHECKING([whether functions can be nested now])
			_SXE_CHECK_CC_NESTED_FUNS([-fnested-functions])
			AC_MSG_RESULT([$sxe_cv_c_nested_funs])
		fi
	fi

	if test "$sxe_cv_c_nested_funs" = "yes"; then
		AC_DEFINE([HAVE_NESTED_FUNS], [1], [Whether funs can be nested])
	else
		AC_MSG_WARN([Uh oh.])
		AC_MSG_WARN([A look into my crystal ball reveals a broken build.])
		AC_MSG_WARN([Get a compiler that is capable of nested functions.])
	fi
])dnl SXE_CHECK_CC_NESTED_FUNS

AC_DEFUN([_SXE_CHECK_CC_NESTED_FUNS], [dnl
	## defines sxe_cv_c_nested_funs
	## optional arg 1 may be additional CFLAGS
	pushdef([ADDCFLAGS], [$1])

	SXE_DUMP_LIBS
	CFLAGS="${CFLAGS} []ADDCFLAGS[]"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[
		/* we are in main now and we nest another function */
		auto inline int foobar();

		inline int foobar(int counter)
		{
			return counter;
		}

		return foobar(0);
		]])],
		[sxe_cv_c_nested_funs="yes"],
		[sxe_cv_c_nested_funs="no"])
	SXE_RESTORE_LIBS

	popdef([ADDCFLAGS])
])dnl _SXE_CHECK_CC_NESTED_FUNS


AC_DEFUN([SXE_CHECK_BROKEN_GCC], [
dnl This section needs a rewrite.  I think it should just bomb if we
dnl find a gcc that is older than 2.95.3 --SY.
dnl Search for GCC specific build problems we know about

AC_MSG_CHECKING([for buggy gcc versions])
GCC_VERSION=`$CC --version`

case $GCC_VERSION in
2.6.*|2.7.*|2.8.*|2.9* )
	AC_MSG_RESULT([yes])
	AC_MSG_WARN([Don't use medieval compilers])
	AC_MSG_ERROR([Aborting due to known problem])
	;;
3.0.*|3.1.*|3.2.*|3.3.* )
	AC_MSG_RESULT([yes])
	AC_MSG_WARN([You are using an antiquated compiler. Proceed at your own risk.])
	AC_MSG_CHECKING([for other known compiler bugs])
	;;
esac

case `uname -s`:`uname -m`:$GCC_VERSION in
dnl pie-enabled GCC on Linux needs -nopie to build SXEmacs
Linux:i?86:gcc*pie-*)
	;;

dnl egcs 2.90.21 (egcs-1.00 release)
dnl egcs 2.90.29 (egcs-1.0.3 release)
*:sun4*:2.8.1|*:sun4*:egcs-2.90.*)
dnl Don't use -O2 with gcc 2.8.1 and egcs 1.0 under SPARC architectures
dnl without also using `-fno-schedule-insns'.
	case "$CFLAGS" in
	*-O2*|*-O3*)
		case "$CFLAGS" in
		*-fno-schedule-insns*)
			;;
		*)
			AC_MSG_RESULT([yes])
			AC_MSG_WARN([Don't use -O2 with gcc 2.8.1 and egcs 1.0 under SPARC architectures])
			AC_MSG_WARN([without also using -fno-schedule-insns.])
			AC_MSG_ERROR([Aborting due to known problem])
			;;
		esac
		;;
	esac
	;;

dnl egcs-2.91.57 (egcs-1.1 release)
dnl egcs-2.91.66 (egcs-1.1.2 release)
Linux:alpha:egcs-2.91.*)
	AC_MSG_RESULT([yes])
	AC_MSG_WARN([There have been reports of egcs-1.1 not compiling SXEmacs correctly on])
	AC_MSG_WARN([Alpha Linux.  There have also been reports that egcs-1.0.3a is O.K.])
	AC_MSG_ERROR([Aborting due to known problem])
	;;

*:*:* )
	AC_MSG_RESULT([no])
	;;
esac
])dnl SXE_CHECK_BROKEN_GCC


AC_DEFUN([SXE_DEBUGFLAGS], [dnl
	## distinguish between different compilers, no?
	SXE_CHECK_COMPILER_FLAGS([-g])
	SXE_CHECK_COMPILER_FLAGS([-g3])

	AC_PATH_PROG([DBX], [dbx])
	if test -n "$ac_cv_path_DBX"; then
		SXE_CHECK_COMPILER_FLAGS([-gstabs])
		SXE_CHECK_COMPILER_FLAGS([-gstabs3])
		SXE_CHECK_COMPILER_FLAGS([-gxcoff])
		SXE_CHECK_COMPILER_FLAGS([-gxcoff3])
	fi

	AC_PATH_PROG([GDB], [gdb])
	if test -n "$ac_cv_path_GDB"; then
		SXE_CHECK_COMPILER_FLAGS([-ggdb])
		SXE_CHECK_COMPILER_FLAGS([-ggdb3])
	fi

	AC_PATH_PROG([SDB], [sdb])
	if test -n "$ac_cv_path_SDB"; then
		SXE_CHECK_COMPILER_FLAGS([-gcoff])
		SXE_CHECK_COMPILER_FLAGS([-gcoff3])
	fi

	## final evaluation
	debugflags=""
	## gdb
	if test "$sxe_cv_c_flags__ggdb3" = "yes"; then
		debugflags="$debugflags -ggdb3"
	elif test "$sxe_cv_c_flags__ggdb" = "yes"; then
		debugflags="$debugflags -ggdb"
	fi
	## stabs
	if test "$sxe_cv_c_flags__gstabs3" = "yes"; then
		debugflags="$debugflags -gstabs3"
	elif test "$sxe_cv_c_flags__gstabs" = "yes"; then
		debugflags="$debugflags -gstabs"
	fi
	## coff
	if test "$sxe_cv_c_flags__gcoff3" = "yes"; then
		debugflags="$debugflags -gcoff3"
	elif test "$sxe_cv_c_flags__gcoff" = "yes"; then
		debugflags="$debugflags -gcoff"
	fi
	## xcoff
	if test "$sxe_cv_c_flags__gxcoff3" = "yes"; then
		debugflags="$debugflags -gxcoff3"
	elif test "$sxe_cv_c_flags__gxcoff" = "yes"; then
		debugflags="$debugflags -gxcoff"
	fi

	if test -z "debugflags" -a \
		"$sxe_cv_c_flags__g" = "yes"; then
		debugflags="$debugflags -g"
	fi

	SXE_CHECK_COMPILER_FLAGS([-ftime-report])
	SXE_CHECK_COMPILER_FLAGS([-fmem-report])
	SXE_CHECK_COMPILER_FLAGS([-fvar-tracking])
	SXE_CHECK_COMPILER_FLAGS([-save-temps])

	#if test "$sxe_cv_c_flags__ggdb3" = "yes" -a \
	#	"$sxe_cv_c_flags__fvar_tracking" = "yes"; then
	#	debugflags="$debugflags -fvar-tracking"
	#fi

	AC_MSG_CHECKING([for preferred debugging flags])
	AC_MSG_RESULT([${debugflags}])
])dnl SXE_DEBUGFLAGS

AC_DEFUN([SXE_WARNFLAGS], [dnl
	## Calculate warning flags.  We separate the flags for warnings from
	## the other flags because we want to force the warnings to be seen
	## by everyone who doesn't specifically override them.

	## by default we want the -Wall level
	SXE_CHECK_COMPILER_FLAGS([-Wall], [warnflags="-Wall"])

	SXE_CHECK_COMPILER_FLAGS([-qinfo], [
		warnflags="${warnflags} -qinfo"])

	## Yuck, bad compares have been worth at
	## least 3 crashes!
	## Warnings about char subscripts are pretty
	## pointless, though,
	## and we use them in various places.
	if test "${with_maximum_warning_output}" = "yes"; then
		SXE_CHECK_COMPILER_FLAGS([-Wsign-compare], [
			warnflags="$warnflags -Wsign-compare"])
		SXE_CHECK_COMPILER_FLAGS([-Wno-char-subscripts], [
			warnflags="$warnflags -Wno-char-subscripts"])
		SXE_CHECK_COMPILER_FLAGS([-Wundef], [
			warnflags="$warnflags -Wundef"])
	fi

	## too much at the moment, we rarely define protos
	#warnflags="$warnflags -Wmissing-prototypes -Wstrict-prototypes"
	SXE_CHECK_COMPILER_FLAGS([-Wpacked], [
		warnflags="$warnflags -Wpacked"])

	## glibc is intentionally not `-Wpointer-arith'-clean.
	## Ulrich Drepper has rejected patches to fix
	## the glibc header files.
	## we dont care
	SXE_CHECK_COMPILER_FLAGS([-Wpointer-arith], [
		warnflags="$warnflags -Wpointer-arith"])

	SXE_CHECK_COMPILER_FLAGS([-Wshadow], [
		warnflags="$warnflags -Wshadow"])

	## our code lacks declarations almost all the time
	SXE_CHECK_COMPILER_FLAGS([-Wmissing-declarations], [
		warnflags="$warnflags -Wmissing-declarations"])
	SXE_CHECK_COMPILER_FLAGS([-Wmissing-prototypes], [
		warnflags="$warnflags -Wmissing-prototypes"])
	SXE_CHECK_COMPILER_FLAGS([-Wbad-function-cast], [
		warnflags="$warnflags -Wbad-function-cast"])
	SXE_CHECK_COMPILER_FLAGS([-Wcast-qual], [
		warnflags="$warnflags -Wcast-qual"])
	SXE_CHECK_COMPILER_FLAGS([-Wcast-align], [
		warnflags="$warnflags -Wcast-align"])

	## too aggressive innit
	if test "${with_maximum_warning_output}" = "yes"; then
		SXE_CHECK_COMPILER_FLAGS([-Winline], [
			warnflags="$warnflags -Winline"])
	fi

	## warn about incomplete switches
	if test "${with_maximum_warning_output}" = "yes"; then
		SXE_CHECK_COMPILER_FLAGS([-Wswitch], [
			warnflags="$warnflags -Wswitch"])
		SXE_CHECK_COMPILER_FLAGS([-Wswitch-default], [
			warnflags="$warnflags -Wswitch-default"])
		SXE_CHECK_COMPILER_FLAGS([-Wswitch-enum], [
			warnflags="$warnflags -Wswitch-enum"])
	fi

	## Wunused's
	if test "${with_maximum_warning_output}" = "yes"; then
		SXE_CHECK_COMPILER_FLAGS([-Wunused-function], [
			warnflags="$warnflags -Wunused-function"])
		SXE_CHECK_COMPILER_FLAGS([-Wunused-variable], [
			warnflags="$warnflags -Wunused-variable"])
		SXE_CHECK_COMPILER_FLAGS([-Wunused-parameter], [
			warnflags="$warnflags -Wunused-parameter"])
		SXE_CHECK_COMPILER_FLAGS([-Wunused-value], [
			warnflags="$warnflags -Wunused-value"])
		SXE_CHECK_COMPILER_FLAGS([-Wunused], [
			warnflags="$warnflags -Wunused"])
	fi

	## icc
	SXE_CHECK_COMPILER_FLAGS([-Wreorder], [
		warnflags="$warnflags -Wreorder"])
	SXE_CHECK_COMPILER_FLAGS([-Wdeprecated], [
		warnflags="$warnflags -Wdeprecated"])
	SXE_CHECK_COMPILER_FLAGS([-Wnopragma], [
		warnflags="$warnflags -Wnopragma"])

	## for upcoming libev support
	## libev is a warning emitting cow, the developers can't
	## be arsed to fix it, as it turns out
	SXE_CHECK_COMPILER_FLAGS([-fno-strict-aliasing], [
		warnflags="$warnflags -fno-strict-aliasing"])

	## icc specific
	SXE_CHECK_COMPILER_FLAGS([-diag-disable 10237], [dnl
		warnflags="${warnflags} -diag-disable 10237"], [
		SXE_CHECK_COMPILER_FLAGS([-wd 10237], [dnl
			warnflags="${warnflags} -wd 10237"])])

	AC_MSG_CHECKING([for preferred warning flags])
	AC_MSG_RESULT([${warnflags}])
])dnl SXE_WARNFLAGS

AC_DEFUN([SXE_OPTIFLAGS], [dnl
	optiflags=""
	SXE_COMPILER_VENDOR

	case $sxe_cv_c_compiler_vendor in
	dnl (
	dec)
		SXE_CC_MAXOPT_DEC
		;;
	dnl (
	sun)
		SXE_CC_MAXOPT_SUN
		;;
	dnl (
	hp)
		SXE_CC_MAXOPT_HP
		;;
	dnl (
	ibm)
		SXE_CC_MAXOPT_IBM
		;;
	dnl (
	intel)
		SXE_CC_MAXOPT_INTEL
		;;
	dnl (
	gnu)
		SXE_CC_MAXOPT_GNU
		;;
	esac

	AC_MSG_CHECKING([for preferred optimising flags])
	AC_MSG_RESULT([$optiflags])
	if test -z "$optiflags"; then
		echo ""
		echo "********************************************************"
		echo "* WARNING: Don't know the best CFLAGS for this system  *"
		echo "* Use ./configure CFLAGS=... to specify your own flags *"
		echo "* (otherwise, a default of CFLAGS=-O3 will be used)    *"
		echo "********************************************************"
		echo ""
		optiflags="-O3"
	fi

	SXE_CHECK_COMPILER_FLAGS([$optiflags], [:], [
		echo ""
		echo "********************************************************"
		echo "* WARNING: The guessed CFLAGS don't seem to work with  *"
		echo "* your compiler.                                       *"
		echo "* Use ./configure CFLAGS=... to specify your own flags *"
		echo "********************************************************"
		echo ""
		optiflags=""
	])
])dnl SXE_OPTIFLAGS

AC_DEFUN([SXE_FEATFLAGS], [dnl
	## default flags for needed features

	## we need nested functions ... hm, i dont think we do
	## but it's always nice to have them
	SXE_CHECK_COMPILER_FLAGS([-fnested-functions], [
		featflags="$featflags -fnested-functions"])

	## recent gentoos went ballistic again, they compile PIE gcc's
	## but there's no way to turn that misconduct off ...
	## however I've got one report about a working PIE build
	## we'll just check for -nopie here, if it works, we turn it on
	## (and hence PIE off) and hope bug 16 remains fixed
	SXE_CHECK_COMPILER_FLAGS([-nopie],
		[featflags="$featflags -nopie"])

	## icc and gcc related
	## check if some stuff can be staticalised
	## actually requires SXE_WARNFLAGS so warnings would be disabled
	## that affect the outcome of the following tests
	SXE_CHECK_COMPILER_FLAGS([-static-intel], [
		featflags="${featflags} -static-intel"
		XCCLDFLAGS="${XCCLDFLAGS} -static-intel"], [:], [${SXE_CFLAGS}])
	SXE_CHECK_COMPILER_FLAGS([-static-libgcc], [
		featflags="${featflags} -static-libgcc"
		XCCLDFLAGS="${XCCLDFLAGS} -static-libgcc"], [:], [${SXE_CFLAGS}])

	AC_SUBST([XCCLDFLAGS])
])dnl SXE_FEATFLAGS


AC_DEFUN([SXE_COMPILER_VENDOR], [dnl
AC_CACHE_CHECK([for _AC_LANG compiler vendor],
	sxe_cv_[]_AC_LANG_ABBREV[]_compiler_vendor,
	[sxe_cv_[]_AC_LANG_ABBREV[]_compiler_vendor=unknown
	# note: don't check for gcc first since some other compilers define __GNUC__
	for ventest in \
		intel:__ICC,__ECC,__INTEL_COMPILER \
		ibm:__xlc__,__xlC__,__IBMC__,__IBMCPP__ \
		gnu:__GNUC__ sun:__SUNPRO_C,__SUNPRO_CC \
		hp:__HP_cc,__HP_aCC \
		dec:__DECC,__DECCXX,__DECC_VER,__DECCXX_VER \
		borland:__BORLANDC__,__TURBOC__ \
		comeau:__COMO__ \
		cray:_CRAYC \
		kai:__KCC \
		lcc:__LCC__ \
		metrowerks:__MWERKS__ \
		sgi:__sgi,sgi \
		microsoft:_MSC_VER \
		watcom:__WATCOMC__ \
		portland:__PGI; do

		vencpp="defined("$(echo \
			$ventest | cut -d: -f2 | sed "s/,/) || defined(/g")")"
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[
#if !($vencpp)
      thisisanerror;
#endif
			])], [sxe_cv_]_AC_LANG_ABBREV[_compiler_vendor=$(echo $ventest | cut -d: -f1); break; ])
	done
	])
])dnl SXE_COMPILER_VENDOR


AC_DEFUN([SXE_CC_MAXOPT_DEC], [dnl
	optiflags="-newc -w0 -O5 -ansi_alias -ansi_args -fp_reorder -tune host"
	if test "$acx_maxopt_portable" = "no"; then
		optiflags="$optiflags -arch host"
	fi
])dnl SXE_CC_MAXOPT_DEC

AC_DEFUN([SXE_CC_MAXOPT_SUN], [dnl
	optiflags="-native -fast -xO5 -dalign"
	if test "$acx_maxopt_portable" = "yes"; then
		optiflags="$optiflags -xarch=generic"
	fi
])dnl SXE_CC_MAXOPT_SUN

AC_DEFUN([SXE_CC_MAXOPT_HP], [dnl
	optiflags="+Oall +Optrs_ansi +DSnative"
	if test "$acx_maxopt_portable" = "yes"; then
		optiflags="$optiflags +DAportable"
	fi
])dnl SXE_CC_MAXOPT_HP

AC_DEFUN([SXE_CC_MAXOPT_IBM], [dnl
	if test "$acx_maxopt_portable" = "no"; then
		xlc_opt="-qarch=auto -qtune=auto"
	else
		xlc_opt="-qtune=auto"
	fi
	SXE_CHECK_COMPILER_FLAGS([$xlc_opt],
		[optiflags="-O3 -qansialias -w $xlc_opt"],
		[optiflags="-O3 -qansialias -w"
		echo "******************************************************"
		echo "*  You seem to have the IBM  C compiler.  It is      *"
		echo "*  recommended for best performance that you use:    *"
		echo "*                                                    *"
		echo "*    CFLAGS=-O3 -qarch=xxx -qtune=xxx -qansialias -w *"
		echo "*                      ^^^        ^^^                *"
		echo "*  where xxx is pwr2, pwr3, 604, or whatever kind of *"
		echo "*  CPU you have.  (Set the CFLAGS environment var.   *"
		echo "*  and re-run configure.)  For more info, man cc.    *"
		echo "******************************************************"])
])dnl SXE_CC_MAXOPT_IBM

AC_DEFUN([SXE_CC_MAXOPT_INTEL], [dnl
	optiflags="-O3 -ansi_alias"
	if test "$acx_maxopt_portable" = "no"; then
		icc_archflag=unknown
		icc_flags=""
		case "$host_cpu" in
		## (
		i686*|x86_64*)
			# icc accepts gcc assembly syntax,
			# so these should work:
			SXE_CHECK_CPU_PC
			;;
		esac
		if test "$icc_flags" != ""; then
			for flag in $icc_flags; do
				SXE_CHECK_COMPILER_FLAGS([$flag],
					[icc_archflag=$flag; break])
			done
		fi
		AC_MSG_CHECKING([for icc architecture flag])
		AC_MSG_RESULT([$icc_archflag])
		if test "$icc_archflag" != "unknown"; then
			optiflags="$optiflags $icc_archflag"
		fi
	fi
])dnl SXE_CC_MAXOPT_INTEL

AC_DEFUN([SXE_CC_MAXOPT_GNU], [dnl
	## default optimisation flags for gcc on all systems
	## this is the preferred compiler
	optiflags="-O3"

	AC_REQUIRE([SXE_CHECK_SIMD_EXTENSIONS])
	optiflags="$optiflags $simdflags"

	## note that we enable "unsafe" fp optimisation with other compilers, too
	SXE_CHECK_COMPILER_FLAGS([-ftree-vectorize],
		[optiflags="$optiflags -ftree-vectorize"])

	## check for -march and/or -mtune
	SXE_GCC_ARCHFLAG([$acx_maxopt_portable])
	optiflags="$optiflags $archflags"

	## a softer variant of omit-frame-pointer
	SXE_CHECK_COMPILER_FLAGS([-momit-leaf-frame-pointer],
		[optiflags="$optiflags -momit-leaf-frame-pointer"])

	## check for openmp support
	## there are: -fopenmp -xopenmp -openmp -mp -omp -qsmp=omp
	SXE_CHECK_COMPILER_FLAGS([-fopenmp])

	if test "$sxe_cv_c_flags__fopenmp" = "yes" -a "$with_omp"; then
		SXE_MSG_CHECKING([whether it is safe to use -fopenmp])
		case "$compiler_version" in
		gcc*\ 4.4.*)
			optiflags="$optiflags -fopenmp"
			sxe_cv_feat_omp="yes"
			SXE_MSG_RESULT([yes])
			;;
		*)
			SXE_MSG_RESULT([no])
			;;
		esac
	fi

	## these belong to the corresponding MAXOPT macro
dnl	SXE_CHECK_COMPILER_FLAGS([-xopenmp], [
dnl		optiflags="$optiflags -xopenmp"
dnl		sxe_cv_feat_omp="yes"
dnl		])
dnl	SXE_CHECK_COMPILER_FLAGS([-openmp], [
dnl		optiflags="$optiflags -openmp"
dnl		sxe_cv_feat_omp="yes"
dnl		])
dnl	SXE_CHECK_COMPILER_FLAGS([-mp], [
dnl		optiflags="$optiflags -mp"
dnl		sxe_cv_feat_omp="yes"
dnl		])
dnl	SXE_CHECK_COMPILER_FLAGS([-omp], [
dnl		optiflags="$optiflags -omp"
dnl		sxe_cv_feat_omp="yes"
dnl		])
dnl	SXE_CHECK_COMPILER_FLAGS([-qsmp=omp], [
dnl		optiflags="$optiflags -qsmp=omp"
dnl		sxe_cv_feat_omp="yes"
dnl		])
	## add -lgomp to ldflags
	if test "$sxe_cv_feat_omp" = "yes"; then
		LDFLAGS="$LDFLAGS -lgomp"
	fi

	# -malign-double for x86 systems
	SXE_CHECK_COMPILER_FLAGS([-malign-double])
	## won't add this one, since it is causing problems
	##	[optiflags="$optiflags -malign-double"])

	## would be nice to have this but it triggers too many
	## meaningless warnings
dnl	## -fstrict-aliasing for gcc-2.95+
dnl	SXE_CHECK_COMPILER_FLAGS([-fstrict-aliasing],
dnl		[optiflags="$optiflags -fstrict-aliasing"])

	SXE_CHECK_COMPILER_FLAGS([-fearly-inlining],
		[optiflags="$optiflags -fearly-inlining"])

	SXE_CHECK_COMPILER_FLAGS([-fdelete-null-pointer-checks],
		[optiflags="$optiflags -fdelete-null-pointer-checks"])

	SXE_CHECK_COMPILER_FLAGS([-fmodulo-sched],
		[optiflags="$optiflags -fmodulo-sched"])

	SXE_CHECK_COMPILER_FLAGS([-fmudflap])
	SXE_CHECK_COMPILER_FLAGS([-fmudflapth])
	SXE_CHECK_COMPILER_FLAGS([-fmudflapir])
	if test "$sxe_cv_c_flags__fmudflapth" -a \
		"$sxe_cv_c_flags__fmudflapir"; then
		## preferred
		: ##optiflags="$optiflags -fmudflapth -fmudflapir"
	elif test "$sxe_cv_c_flags__fmudflap" -a \
		"$sxe_cv_c_flags__fmudflapir"; then
		: ##optiflags="$optiflags -fmudflap -fmudflapir"
	fi
	SXE_CHECK_COMPILER_FLAGS([-fsection-anchors],
		[optiflags="$optiflags -fsection-anchors"])

	SXE_CHECK_COMPILER_FLAGS([-fbranch-target-load-optimize])
	SXE_CHECK_COMPILER_FLAGS([-fbranch-target-load-optimize2])
	if test "$sxe_cv_c_flags__fbranch_target_load_optimize2" = "yes"; then
		optiflags="$optiflags -fbranch-target-load-optimize2"
	elif test "$sxe_cv_c_flags__fbranch_target_load_optimize" = "yes"; then
		optiflags="$optiflags -fbranch-target-load-optimize"
	fi

	SXE_CHECK_COMPILER_FLAGS([-fgcse],
		[optiflags="$optiflags -fgcse"])
	SXE_CHECK_COMPILER_FLAGS([-fgcse-lm],
		[optiflags="$optiflags -fgcse-lm"])
	SXE_CHECK_COMPILER_FLAGS([-fgcse-sm],
		[optiflags="$optiflags -fgcse-sm"])
	SXE_CHECK_COMPILER_FLAGS([-fgcse-las],
		[optiflags="$optiflags -fgcse-las"])
	SXE_CHECK_COMPILER_FLAGS([-fgcse-after-reload],
		[optiflags="$optiflags -fgcse-after-reload"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-loop-optimizations],
		[optiflags="$optiflags -funsafe-loop-optimizations"])
	SXE_CHECK_COMPILER_FLAGS([-funroll-loops],
		[optiflags="$optiflags -funswitch-loops"])
	SXE_CHECK_COMPILER_FLAGS([-funswitch-loops],
		[optiflags="$optiflags -funswitch-loops"])
	SXE_CHECK_COMPILER_FLAGS([-frename-registers],
		[optiflags="$optiflags -frename-registers"])

	## maths speedup
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fno-signaling-nans],
		[optiflags="$optiflags -fno-signaling-nans"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fno-trapping-math],
		[optiflags="$optiflags -fno-trapping-math"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fno-signed-zeros],
		[optiflags="$optiflags -fno-signed-zeros"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fassociative-math],
		[optiflags="$optiflags -fassociative-math"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fno-rounding-math],
		[optiflags="$optiflags -fno-rounding-math"])
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations -fno-math-errno],
		[optiflags="$optiflags -fno-math-errno"])
	## the same as the previous 5, but sometimes gcc doesn't know'em all
	SXE_CHECK_COMPILER_FLAGS([-funsafe-math-optimizations],
		[optiflags="$optiflags -funsafe-math-optimizations"])
	## another of these
	SXE_CHECK_COMPILER_FLAGS([-ffast-math],
		[optiflags="$optiflags -ffast-math"])
	## and yet some more
	SXE_CHECK_COMPILER_FLAGS([-mrecip],
		[optiflags="$optiflags -mrecip"])
	SXE_CHECK_COMPILER_FLAGS([-msahf],
		[optiflags="$optiflags -msahf"])

	SXE_CHECK_COMPILER_FLAGS([-minline-all-stringops],
		[optiflags="$optiflags -minline-all-stringops"])

])dnl SXE_CC_MAXOPT_GNU


##### http://autoconf-archive.cryp.to/ax_check_compiler_flags.html
## renamed the prefix to SXE_
AC_DEFUN([SXE_CHECK_COMPILER_FLAGS], [dnl
dnl SXE_CHECK_COMPILER_FLAGS(<FLAG>, <ACTION-IF-FOUND>, <ACTION-IF-NOT-FOUND>,
dnl     <ADDITIONAL-FLAGS>)
	AC_MSG_CHECKING([whether _AC_LANG compiler accepts $1])

	dnl Some hackery here since AC_CACHE_VAL can't handle a non-literal varname:
	SXE_LANG_WERROR([push+on])
	AS_LITERAL_IF([$1], [
		AC_CACHE_VAL(AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1), [
			sxe_save_FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
			_AC_LANG_PREFIX[]FLAGS="$4 $1"
			AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
				AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1)="yes",
				AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1)="no")
			_AC_LANG_PREFIX[]FLAGS=$sxe_save_FLAGS])], [
		sxe_save_FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
		_AC_LANG_PREFIX[]FLAGS="$4 $1"
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
			eval AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1)="yes",
			eval AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1)="no")
		_AC_LANG_PREFIX[]FLAGS=$sxe_save_FLAGS])
	eval sxe_check_compiler_flags=$AS_TR_SH(sxe_cv_[]_AC_LANG_ABBREV[]_flags_$1)
	SXE_LANG_WERROR([pop])

	AC_MSG_RESULT([$sxe_check_compiler_flags])
	if test "$sxe_check_compiler_flags" = "yes"; then
		m4_default([$2], :)
	else
		m4_default([$3], :)
	fi
])dnl SXE_CHECK_COMPILER_FLAGS


AC_DEFUN([SXE_CHECK_COMPILER_XFLAG], [dnl
	## if libtool then
	case "${LD}" in
	*"libtool"*)
		SXE_CHECK_COMPILER_FLAGS([-XCClinker], [
			XFLAG="-XCClinker"], [
			XFLAG=""])
		;;
	*"ld"*)
		## no XFLAG needed
		XFLAG=""
		;;
	*)
		SXE_CHECK_COMPILER_FLAGS([-Xlinker], [
			XFLAG="-Xlinker"], [
			XFLAG=""])
		;;
	esac
])dnl SXE_CHECK_COMPILER_XFLAG


AC_DEFUN([SXE_CHECK_CPU], [dnl
	case "$host_cpu" in
	dnl (
	i*86*|x86_64*)
		## pc-style hardware
		SXE_CHECK_CPU_PC
		;;
	dnl (
	sparc*)
		## sparc stuff
		SXE_CHECK_CPU_SPARC
		;;
	dnl (
	alpha*)
		## alpha cruft
		SXE_CHECK_CPU_ALPHA
		;;
	dnl (
	powerpc*)
		## ya know what
		SXE_CHECK_CPU_PPC
		;;
	esac
])dnl SXE_CHECK_CPU

AC_DEFUN([SXE_CHECK_CPU_SPARC], [dnl
	AC_PATH_PROG([PRTDIAG], [prtdiag], [prtdiag],
		[$PATH:/usr/platform/`uname -i`/sbin/:/usr/platform/`uname -m`/sbin/])
	cputype=$((((grep cpu /proc/cpuinfo | cut -d: -f2) ; \
		($PRTDIAG -v |grep -i sparc) ; \
		grep -i cpu "/var/run/dmesg.boot" ) | head -n 1) 2> /dev/null)
	cputype=$(echo "$cputype" | tr -d ' -' |tr $as_cr_LETTERS $as_cr_letters)

	case "$cputype" in
	dnl (
	*ultrasparciv*)
		sxe_gcc_arch="ultrasparc4 ultrasparc3 ultrasparc v9"
		;;
	dnl (
	*ultrasparciii*)
		sxe_gcc_arch="ultrasparc3 ultrasparc v9"
		;;
	dnl (
	*ultrasparc*)
		sxe_gcc_arch="ultrasparc v9"
		;;
	dnl (
	*supersparc*|*tms390z5[[05]]*)
		sxe_gcc_arch="supersparc v8"
		;;
	dnl (
	*hypersparc*|*rt62[[056]]*)
		sxe_gcc_arch="hypersparc v8"
		;;
	dnl (
	*cypress*)
		sxe_gcc_arch=cypress
		;;
	esac
])dnl SXE_CHECK_CPU_SPARC

AC_DEFUN([SXE_CHECK_CPU_ALPHA], [dnl
	case "$host_cpu" in
	dnl (
	alphaev5)
		sxe_gcc_arch="ev5"
		;;
	dnl (
	alphaev56)
		sxe_gcc_arch="ev56"
		;;
	dnl (
	alphapca56)
		sxe_gcc_arch="pca56 ev56"
		;;
	dnl (
	alphapca57)
		sxe_gcc_arch="pca57 pca56 ev56"
		;;
	dnl (
	alphaev6)
		sxe_gcc_arch="ev6"
		;;
	dnl (
	alphaev67)
		sxe_gcc_arch="ev67"
		;;
	dnl (
	alphaev68)
		sxe_gcc_arch="ev68 ev67"
		;;
	dnl (
	alphaev69)
		sxe_gcc_arch="ev69 ev68 ev67"
		;;
	dnl (
	alphaev7)
		sxe_gcc_arch="ev7 ev69 ev68 ev67"
		;;
	dnl (
	alphaev79)
		sxe_gcc_arch="ev79 ev7 ev69 ev68 ev67"
		;;
	esac
])dnl SXE_CHECK_CPU_ALPHA

AC_DEFUN([SXE_CHECK_CPU_PPC], [dnl
	cputype=$(((grep cpu /proc/cpuinfo | head -n 1 | \
		cut -d: -f2 | cut -d, -f1 | sed 's/ //g') ; \
		/usr/bin/machine ; \
		/bin/machine; \
		grep CPU /var/run/dmesg.boot | \
		head -n 1 | cut -d" " -f2) 2> /dev/null)
	cputype=$(echo $cputype | sed -e 's/ppc//g;s/ *//g')

	case "$cputype" in
	dnl (
	*750*)
		sxe_gcc_arch="750 G3"
		;;
	dnl (
	*740[[0-9]]*)
		sxe_gcc_arch="$cputype 7400 G4"
		;;
	dnl (
	*74[[4-5]][[0-9]]*)
		sxe_gcc_arch="$cputype 7450 G4"
		;;
	dnl (
	*74[[0-9]][[0-9]]*)
		sxe_gcc_arch="$cputype G4"
		;;
	dnl (
	*970*)
		sxe_gcc_arch="970 G5 power4"
		;;
	dnl (
	*POWER4*|*power4*|*gq*)
		sxe_gcc_arch="power4 970"
		;;
	dnl (
	*POWER5*|*power5*|*gr*|*gs*)
		sxe_gcc_arch="power5 power4 970"
		;;
	dnl (
	603ev|8240)
		sxe_gcc_arch="$cputype 603e 603"
		;;
	dnl (
	*)
		sxe_gcc_arch="$cputype"
		;;
	esac
	sxe_gcc_arch="$sxe_gcc_arch powerpc"
])dnl SXE_CHECK_CPU_PPC

AC_DEFUN([SXE_CHECK_CPU_PC], [dnl
	## use cpuid codes,
	AC_REQUIRE([SXE_PROC_CPUID])

	case "$sxe_cv_proc_cpuid_00000000" in
	dnl (
	*:756e6547:*:*)
		## Intel
		case "$sxe_cv_proc_cpuid_00000001" in
		dnl (
		*5[[48]]?:*:*:*)
			sxe_gcc_arch="pentium-mmx pentium"
			;;
		dnl (
		*5??:*:*:*)
			sxe_gcc_arch="pentium"
			;;
		dnl (
		*6[[3456]]?:*:*:*)
			sxe_gcc_arch="pentium2 pentiumpro"
			;;
		dnl (
		*6a?:*[[01]]:*:*)
			sxe_gcc_arch="pentium2 pentiumpro"
			;;
		dnl (
		*6a?:*[[234]]:*:*)
			sxe_gcc_arch="pentium3 pentiumpro"
			icc_flags="-xK"
			;;
		dnl (
		*69?:*:*:*)
			sxe_gcc_arch="pentium-m pentium3 pentiumpro"
			icc_flags="-xK"
			;;
		dnl (
		*6d?:*:*:*)
			sxe_gcc_arch="pentium-m pentium3 pentiumpro"
			;;
		dnl (
		*6[[78b]]?:*:*:*)
			sxe_gcc_arch="pentium3 pentiumpro"
			icc_flags="-xK"
			;;
		dnl (
		*6f?:*:*:*)
			## intel core 2 duo
			sxe_gcc_arch="nocona pentium4 pentiumpro"
			## icc flags here
			## icc_flags="??"
			;;
		dnl (
		*6??:*:*:*)
			sxe_gcc_arch="pentiumpro"
			;;
		dnl (
		*f3[[347]]:*:*:*|*f4[1347]:*:*:*)
			icc_flags="-xP -xN -xW -xK"
			case "$host_cpu" in
			dnl (
			x86_64*)
				sxe_gcc_arch="nocona pentium4 pentiumpro"
				;;
			dnl (
			*)
				sxe_gcc_arch="prescott pentium4 pentiumpro"
				;;
			esac
			;;
		dnl (
		*f??:*:*:*)
			icc_flags="-xN -xW -xK"
			sxe_gcc_arch="pentium4 pentiumpro"
			;;
		esac
		;;
	dnl (
	*:68747541:*:*)
		## AMD
		case "$sxe_cv_proc_cpuid_00000001" in
		dnl (
		*5[[67]]?:*:*:*)
			sxe_gcc_arch="k6"
			;;
		dnl (
		*5[[8d]]?:*:*:*)
			sxe_gcc_arch="k6-2 k6"
			;;
		dnl (
		*5[[9]]?:*:*:*)
			sxe_gcc_arch="k6-3 k6"
			;;
		dnl (
		*60?:*:*:*)
			sxe_gcc_arch="k7"
			;;
		dnl (
		*6[[12]]?:*:*:*)
			sxe_gcc_arch="athlon k7"
			;;
		dnl (
		*6[[34]]?:*:*:*)
			sxe_gcc_arch="athlon-tbird k7"
			;;
		dnl (
		*67?:*:*:*)
			sxe_gcc_arch="athlon-4 athlon k7"
			;;
		dnl (
		*6[[68a]]?:*:*:*)
			case "$sxe_cv_proc_cpuid_80000006" in
			dnl (
			*:*:*[[1-9a-f]]??????:*) # (L2 = ecx >> 16) >= 256
				sxe_gcc_arch="athlon-xp athlon-4 athlon k7"
				;;
			dnl (
			*)
				sxe_gcc_arch="athlon-4 athlon k7"
				;;
			esac
			;;
		dnl (
		*f[[4cef8b]]?:*:*:*)
			sxe_gcc_arch="athlon64 k8"
			;;
		dnl (
		*f5?:*:*:*)
			sxe_gcc_arch="opteron k8"
			;;
		dnl (
		*f7?:*:*:*)
			sxe_gcc_arch="athlon-fx opteron k8"
			;;
		dnl (
		*f??:*:*:*)
			sxe_gcc_arch="k8"
			;;
		esac
		;;
	dnl (
	*:746e6543:*:*)
		# IDT
		case "$sxe_cv_proc_cpuid_00000001" in
		dnl (
		*54?:*:*:*)
			sxe_gcc_arch="winchip-c6"
			;;
		dnl (
		*58?:*:*:*)
			sxe_gcc_arch="winchip2"
			;;
		dnl (
		*6[[78]]?:*:*:*)
			sxe_gcc_arch="c3"
			;;
		dnl (
		*69?:*:*:*)
			sxe_gcc_arch="c3-2 c3"
			;;
		esac
		;;
	esac

	## nothing found? => fallback
	if test -z "$sxe_gcc_arch"; then
		## apply a generic strategy
		case "$host_cpu" in
		i586*)
			sxe_gcc_arch="pentium"
			;;
		i686*)
			sxe_gcc_arch="pentiumpro"
			;;
		esac
	fi
])dnl SXE_CHECK_CPU_PC


AC_DEFUN([SXE_GCC_ARCHFLAG], [dnl
	AC_REQUIRE([AC_PROG_CC])
	AC_REQUIRE([AC_CANONICAL_HOST])

	AC_ARG_WITH([gcc-arch], AS_HELP_STRING([--with-gcc-arch=<arch>], [
			use architecture <arch> for gcc -march/-mtune,
			instead of guessing]),
		[sxe_gcc_arch=$withval], [sxe_gcc_arch=yes])

	AC_MSG_CHECKING([for gcc architecture flag])
	AC_MSG_RESULT([])
	AC_CACHE_VAL([sxe_cv_gcc_archflag], [
		## initialise to unknown
		sxe_cv_gcc_archflag="unknown"

		if test "$GCC" = yes; then
			if test "$sxe_gcc_arch" = "yes" -a \
				"$cross_compiling" = "no"; then
				sxe_gcc_arch=""
				SXE_CHECK_CPU
			fi

			if test -n "$sxe_gcc_arch" -a "$sxe_gcc_arch" != "no"; then
				for arch in $sxe_gcc_arch; do
					if test "[]m4_default([$1],yes)" = "yes"; then
						## if we require portable code
						archflags="-mtune=$arch"
						## -mcpu=$arch and m$arch generate
						## nonportable code on every arch except
						## x86.  And some other arches
						## (e.g. Alpha) don't accept -mtune.
						## Grrr.
						case "$host_cpu" in
						dnl (
						i*86|x86_64*)
							archflags="$archflags -mcpu=$arch -m$arch"
							;;
						esac
					else
						archflags="-march=$arch -mcpu=$arch -m$arch"
					fi
					for flag in $archflags; do
						SXE_CHECK_COMPILER_FLAGS([$flag],
							[sxe_cv_gcc_archflag="$flag"; break])
					done
					if test "$sxe_cv_gcc_archflag" != "unknown"; then
						break
					fi
				done
			fi
		fi
		])

	AC_MSG_CHECKING([for gcc architecture flag])
	AC_MSG_RESULT([$sxe_cv_gcc_archflag])
	if test "$sxe_cv_gcc_archflag" = "unknown"; then
		m4_default([$3],:)
	else
		m4_default([$2], [archflags="$sxe_cv_gcc_archflag"])
	fi
])dnl SXE_GCC_ARCHFLAG


#### http://autoconf-archive.cryp.to/sxe_check_define.html
AC_DEFUN([SXE_CHECK_DEFINED], [dnl
	AS_VAR_PUSHDEF([ac_var],[ac_cv_defined_$2])dnl

	AC_CACHE_CHECK([for $1 defined], ac_var,
		AC_TRY_COMPILE($1,[
#ifndef $2
int ok;
#else
choke me
#endif
		],
		AS_VAR_SET(ac_var, [yes]),
		AS_VAR_SET(ac_var, [no])))

	AS_IF([test AS_VAR_GET(ac_var) != "no"], [$3], [$4])
	AS_VAR_POPDEF([ac_var])
])dnl SXE_CHECK_DEFINED

AC_DEFUN([SXE_CHECK_FUNC], [dnl
	AS_VAR_PUSHDEF([ac_var], [ac_cv_func_$2])dnl
	AC_CACHE_CHECK([for $2], ac_var, [
		dnl AC_LANG_FUNC_LINK_TRY
		AC_LINK_IFELSE([AC_LANG_PROGRAM([$1
#undef $2
char $2 ();
			], [
char (*f) () = $2;
return f != $2;
			])], [
			AS_VAR_SET(ac_var, [yes])], [
			AS_VAR_SET(ac_var, [no])])])

	AS_IF([test AS_VAR_GET(ac_var) = "yes"], [$3], [$4])
	AS_VAR_POPDEF([ac_var])
])dnl SXE_CHECK_FUNC



AC_DEFUN([SXE_CHECK_C99_NJSF], [dnl
	dnl If we have a compiler that could do c99 do try to add the flag
	if test "$__GCC3" = "yes" ; then
		SXE_APPEND_UNDUP("-std=c99", c_switch_site)
		AC_MSG_CHECKING([for C99 support])
		save_c_switch_site=$c_switch_site
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
]],[[
return 0;
]])],  [AC_MSG_RESULT(yes)
	have_c99="yes"],
       [c_switch_site=$sace_c_switch_site
		AC_MSG_RESULT(no)
		AC_MSG_WARN([C99 not supported, reverting option append])
		have_c99="no"])
	elif test "$__SUNPRO_C" = "yes" ; then
		AC_MSG_CHECKING([for C99 support])
		save_c_switch_site=$c_switch_site
		SXE_APPEND_UNDUP("-xc99", c_switch_site)
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
]],[[
return 0;
]])],  [AC_MSG_RESULT(yes)
		have_c99="yes"],
       [c_switch_site=$sace_c_switch_site
		AC_MSG_RESULT(no)
		have_c99="no"])
	fi
])dnl SXE_CHECK_C99_NJSF


AC_DEFUN([SXE_C_TYPEOF], [dnl
	dnl check for the typeof extension
	AC_MSG_CHECKING([for typeof])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int i; __typeof__(i) j;
]])], [typeofname="__typeof__"], [dnl else
		AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int i; typeof(i) j;
]])], [typeofname="typeof"], [typeofname=no])])
	AC_MSG_RESULT([$typeofname])
	if test "$typeofname" != "no"; then
		AC_DEFINE_UNQUOTED([TYPEOF], [$typeofname],
			[How to use the typeof extension.])
	fi
])dnl SXE_C_TYPEOF

AC_DEFUN([SXE_C_INLINE], [dnl
	AC_C_INLINE
	if test "$ac_cv_c_inline" != "no" -a "$GCC" = "yes"; then
		SXE_ADD_CRUFT_OBJS([inline.o])
	fi
])dnl SXE_C_INLINE


AC_DEFUN([SXE_CHECK_BROKEN_RESTRICT], [dnl
	AC_MSG_CHECKING([support for restrict keyword])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
extern void f(void*restrict[]);
]])], [have_restrict="yes"], [have_restrict="no"])
	if test "$have_restrict" = "no"; then
		AC_DEFINE_UNQUOTED([restrict], [],
			[Define restrict to nothing])
	fi
	AC_MSG_RESULT([$typeofname])
])dnl SXE_CHECK_BROKEN_RESTRICT

AC_DEFUN([SXE_STACK_FLAGS], [dnl
	## actually this belongs to error-checking stuff
	SXE_CHECK_COMPILER_FLAGS([-fstack-protector])
	if test "${sxe_cv_c_flags__fstack_protector}" = "yes"; then
		case "$opsys" in
		freebsd)
			diagflags="${diagflags} -fstack-protector"
			;;
		*)
			AC_CHECK_LIB([ssp], [__stack_chk_guard])
			if test "${ac_cv_lib_ssp___stack_chk_guard}" = "yes"; then
				diagflags="${diagflags} -fstack-protector"
			fi
			;;
		esac
	fi
])dnl SXE_STACK_FLAGS

dnl recommended interface macros
## compiler wrapper
AC_DEFUN([SXE_CHECK_CC], [dnl

	AC_PROG_CPP
	AC_HEADER_STDC
	AC_PROG_CC([gcc icc cc])
	dnl AC_PROG_CC_STDC
	AC_PROG_CC_C99

	## check for machine and architecture
	SXE_CHECK_MACHARCH

	## check for C compiler characteristics
	SXE_CHECK_CC_VERSION
	if test "$GCC" = "yes"; then
		SXE_CHECK_BROKEN_GCC
	fi
	AC_C_RESTRICT
	if test "$ac_cv_prog_cc_c99" != ""; then
		SXE_CHECK_BROKEN_RESTRICT
	fi

	## check for libc
	SXE_CHECK_LIBC
	SXE_CHECK_LIBC_VERSION
	dnl Non-ordinary link usually requires -lc
	if test "$ordinary_link" = "no" -a -z "$libs_standard"; then
		libs_standard="-lc"
	fi

	SXE_CHECK_CC_HACKS
	SXE_CHECK_CC_CHAR
	SXE_CHECK_CC_NESTED_FUNS

	## Canonicalize the configuration name.
	## Commented out, lets see if anything breaks. --SY.
	## SXE_STRIP_4TH_COMPONENT(ac_cv_build_alias)
	## SXE_STRIP_4TH_COMPONENT(ac_cv_build)
	AC_SUBST([configuration], [$ac_cv_build])

	AM_PROG_CC_C_O

])dnl SXE_CHECK_CC


AC_DEFUN([SXE_CHECK_CFLAGS], [dnl
	dnl #### This may need to be overhauled so that all of SXEMACS_CC's flags
	dnl are handled separately, not just the xe_cflags_warning stuff.

	## diagnostic stuff and error checking
	## this may somehow be in contradiction to optimisation flags later on
	## so we issue the tests right here
	if test "${with_error_checking_stack}" = "yes"; then
		SXE_STACK_FLAGS
	fi
	if test "${with_error_checking_malldbg}" = "yes"; then
		## keep MALLOC_PERTURB_ and friends?
		:
	else
		EXTRA_BATCHENV="MALLOC_PERTURB_= MALLOC_CHECK_="
		AC_SUBST([EXTRA_BATCHENV])
	fi
	SXE_CFLAGS="${SXE_CFLAGS} ${diagflags}"

	## Use either command line flag, environment var, or autodetection
	if test "$with_ridiculously_aggressive_optimisations" = "yes"; then
		CFLAGS=""
		SXE_DEBUGFLAGS
		SXE_WARNFLAGS
		SXE_OPTIFLAGS
		SXE_CFLAGS="$SXE_CFLAGS $debugflags $optiflags $warnflags"

		SXE_FEATFLAGS
		SXE_CFLAGS="$SXE_CFLAGS $featflags"

	elif test "$CFLAGS_uspecified_p" = "no" -o \
		"$ac_test_CFLAGS" != "set"; then
		SXE_DEBUGFLAGS
		SXE_WARNFLAGS

		## the old settings
		## Following values of CFLAGS are known to work well.
		## Should we take debugging options into consideration?
		SXE_CHECK_COMPILER_FLAGS([-xO4], [dnl
			## ah, it's sunos4*
			optiflags="${optiflags} -xO4"], [dnl
			SXE_CHECK_COMPILER_FLAGS([-xO2], [dnl
				## oh, a sol2
				optiflags="${optiflags} -xO2"])])
		SXE_CHECK_COMPILER_FLAGS([-O3], [dnl
			## gcc, icc, decc, et al.
			optiflags="${optiflags} -O3"])

		## xlc specific
		SXE_CHECK_COMPILER_FLAGS([-qnoansialias -qlibansi], [dnl
			optiflags="${optiflags} -qnoansialias -qlibansi"])
		SXE_CHECK_COMPILER_FLAGS([-qro -qmaxmem=20000], [dnl
			optiflags="${optiflags} -qro -qmaxmem=20000"])

		## icc specific
		SXE_CHECK_COMPILER_FLAGS([-inline-level=2], [dnl
			## ah, one of the new flavours, tasty
			optiflags="${optiflags} -inline-level=2"], [dnl
			SXE_CHECK_COMPILER_FLAGS([-Ob2], [dnl
				## deprecated nowadays
				optiflags="${optiflags} -Ob2"])])

		## final check
		if test -z "${optiflags}"; then
			SXE_CHECK_COMPILER_FLAGS([-O], [dnl
				## The only POSIX-approved flag
				optiflags="-O"])
		fi

		SXE_CFLAGS="$SXE_CFLAGS $debugflags $optiflags $warnflags"

		SXE_FEATFLAGS
		SXE_CFLAGS="$SXE_CFLAGS $featflags"
	else
		SXE_CFLAGS="${SXE_CFLAGS} ${USER_CFLAGS}"
		featflags=""
		debugflags=""
		optiflags=""
		warnflags=""
		diagflags=""
	fi

	## unset the werror flag again
	SXE_LANG_WERROR([off])

	CFLAGS="$SXE_CFLAGS"
	AC_MSG_CHECKING([for preferred CFLAGS])
	AC_MSG_RESULT([${SXE_CFLAGS}])

	AC_MSG_NOTICE([
If you wish to ADD your own flags you want to stop here and rerun the
configure script like so:
  configure CFLAGS=<to-be-added-flags>

You can always override the determined CFLAGS, partially or totally,
using
  make -C <directory> CFLAGS=<your-own-flags> [target]
or
  make CFLAGS=<your-own-flags> [target]
respectively

NOTE: -C <directory> option is not available on all systems
		])
])dnl SXE_CHECK_CFLAGS


AC_DEFUN([SXE_CC_LIBRARY_PATH], [dnl
	## arg #1 variable to store name in,
	## if omitted defaults to `sxe_cv_tmp_gcc_library_path'
	pushdef([VAR], ifelse($1,[],[sxe_cv_tmp_gcc_library_path],$1))

	for i in ${LDFLAGS}; do
		case $i in
		-L*)
			__LP=${i:2}:${__LP:-.}
			;;
		*)
			;;
		esac
	done
	SXE_EXPAND_VAR([${libdir}], [__ld])
	[]VAR[]=${__ld:-.}:${__LP:-.}

	popdef([VAR])
])dnl SXE_CC_LIBRARY_PATH


AC_DEFUN([SXE_CC_LIBRARY_LOCATION], [dnl
	## arg #1 the library to seek after
	## arg #2 (optional) checking message
	pushdef([liborig], [$1])
	pushdef([liblink], patsubst(patsubst(liborig, [lib], [-l]), [\.[^.]+$], []))
	pushdef([libname], translit(liborig, [-,.], [___]))
	pushdef([LIBNAME], translit(liborig, [-,.a-z], [___A-Z]))
	pushdef([libchk], ifelse($2,[],[for absolute path of ]liborig,$2))

	## use gcc's -print-file-name
	AC_REQUIRE([SXE_CC_LIBRARY_PATH])
	AC_MSG_CHECKING([]libchk[])
	## not protected against other CCs (yet)!
	sxe_cv_loc_[]libname[]=$(LIBRARY_PATH=${sxe_cv_tmp_gcc_library_path} \
		${CC} -print-file-name=[]liborig[])
	AC_MSG_RESULT(${sxe_cv_loc_[]libname[]})

	[]LIBNAME[]=${sxe_cv_loc_[]libname[]}

	popdef([libchk])
	popdef([liblink])
	popdef([liborig])
	popdef([libname])
	popdef([LIBNAME])
])dnl SXE_CC_LIBRARY_LOCATION


AC_DEFUN([SXE_CHECK_ANON_STRUCTS], [
	AC_MSG_CHECKING([whether C compiler can cope with anonymous structures])
	AC_LANG_PUSH(C)
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
union __test_u {
	int i;
	struct {
		char c;
		char padc;
		short int pads;
	};
};
	]], [[
	union __test_u tmp = {.c = '4'};
	]])], [
		sxe_cv_have_anon_structs="yes"
	], [
		sxe_cv_have_anon_structs="no"
	])
	AC_MSG_RESULT([${sxe_cv_have_anon_structs}])

	if test "${sxe_cv_have_anon_structs}" = "yes"; then
		AC_DEFINE([HAVE_ANON_STRUCTS], [1], [
			Whether c1x anon structs work])
		$1
		:
	else
		$2
		:
	fi
	AC_LANG_POP()
])dnl SXE_CHECK_ANON_STRUCTS

dnl sxe-compiler.m4 ends here
