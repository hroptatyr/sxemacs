dnl sxe-libc.m4 -- libc stuff
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

AC_DEFUN([SXE_CHECK_LIBC], [dnl
	dnl We want feature macros defined here *and* in config.h.in, so that
	dnl the compilation environment at configure time and compile time agree.

	AC_MSG_CHECKING([for GNU libc])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <features.h>]],[[
#if ! (defined __GLIBC__ || defined __GNU_LIBRARY__)
#error Not a GNU libc system :-(
******* ======= ******** &&&&&&&&
#endif
]])], [have_glibc=yes], [have_glibc=no])
	AC_MSG_RESULT($have_glibc)

	dnl I'm tired of pop being broken with GLIBC -slb
	dnl Well. then why not fix fucking pop?
	if test "$have_glibc" = "yes"; then
		AC_DEFINE([_GNU_SOURCE], [1],
			[Enable GNU extensions on systems that have them.])
		AH_VERBATIM([_ALL_SOURCE], [dnl
			/* WTF?! */
			#ifndef _ALL_SOURCE
			# define _ALL_SOURCE	1
			#endif
			])
		AH_VERBATIM([_POSIX_PTHREAD_SEMANTICS], [dnl
			/* WTF?! */
			#ifndef _POSIX_PTHREAD_SEMANTICS
			# define _POSIX_PTHREAD_SEMANTICS	1
			#endif
			])
		AH_VERBATIM([_TANDEM_SOURCE], [dnl
			/* WTF?! */
			#ifndef _TANDEM_SOURCE
			# define _TANDEM_SOURCE	1
			#endif
			])
	fi
	if test "0" = "1"; then
		AC_DEFINE([_ALL_SOURCE], [], [Description])
		AC_DEFINE([_POSIX_PTHREAD_SEMANTICS], [], [Description])
		AC_DEFINE([_TANDEM_SOURCE], [], [Description])
	fi

	dnl We'd like to use vendor extensions, where available.
	dnl We'd like to use functions from the latest Unix98 standards.
	dnl See http://www.opengroup.org/onlinepubs/007908799/xsh/compilation.html
	case "$opsys" in
	sol2)
		AC_DEFINE([__EXTENSIONS__], [1], [Description here!])
		dnl Solaris 2 before 2.5 had some bugs with feature
		dnl test macro interaction.
		if test "$os_release" -ge 510; then
			AC_DEFINE([_POSIX_C_SOURCE], [200112L], [Description here!])
			dnl #### should this be 600?
			AC_DEFINE([_XOPEN_SOURCE], [600], [Description here!])
		elif test "$os_release" -ge 506; then
			AC_DEFINE([_POSIX_C_SOURCE], [199506L], [Description here!])
			dnl #### should this be 600?
			AC_DEFINE([_XOPEN_SOURCE], [500], [Description here!])
		elif test "$os_release" -ge 505; then
			AC_DEFINE([_XOPEN_SOURCE], [500], [Description here!])
			AC_DEFINE([_XOPEN_SOURCE_EXTENDED], [1], [Description here!])
		fi
		;;

	linux)
		AC_DEFINE([_POSIX_C_SOURCE], [199506L], [Description here!])
		dnl #### should this be 600?
		AC_DEFINE([_XOPEN_SOURCE], [500], [Description here!])
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED], [1], [Description here!])
		;;

	freebsd4*)
		AC_DEFINE([_POSIX_C_SOURCE], [199506L], [Description here!])
		dnl #### Do we want these too?  Apparently yes for _XOPEN_SOURCE=500.
		AC_DEFINE([_XOPEN_SOURCE], [500], [Description here!])
		dnl AC_DEFINE(_XOPEN_SOURCE_EXTENDED)
		;;
	esac
])dnl SXE_CHECK_LIBC



AC_DEFUN([SXE_CHECK_LIBC_VERSION], [dnl
	libc_version=""
	AC_MSG_CHECKING(for standard C library version information)
	
	case "$ac_cv_build" in
	*-*-linux*)
		dnl #### who would ever _not_ be running the distro's libc?
		dnl Whose silly question is this? -hroptatyr
		dnl Maybe it would be better to get/augment this info with ldd?
		if test -f /etc/redhat-release ; then
			libc_version=`rpm -q glibc`
		elif test -f /etc/debian_version ; then
			libc_version=`dpkg-query --showformat='${version}' --show libc6`
			libc_version="GNU libc $libc_version (Debian)"
		elif test -f /etc/slackware-version ; then
			libc_version=`/lib/libc.so.6|head -1|cut -d ' ' -f7|tr -d ,`
			libc_version="GNU libc $libc_version (Slackware)"
		dnl need SuSE et al checks here...
		fi
		dnl #### Tested on Debian, does this actually work elsewhere?  ;-)
		dnl #### NO! -hroptatyr
	        dnl if test -z "$libc_version"; then
		dnl   libc_version=`ls /lib/libc-*.so | sed -e 's,/lib/libc-\(.*\)\.so,\1,'`
	        dnl fi

		if test -z "$libc_version"; then
		AC_RUN_IFELSE([AC_LANG_SOURCE([[
int main() { return 0; }]])], [dnl
libc_file_we_use=`$LDD ./conftest | grep libc | sed -e "s/.*=>\(.*\) .*$/\1/"`],
			[],[libc_file_we_use=])
		libc_version=`$libc_file_we_use 2>/dev/null | sed "/version/q" | tr -cd "0-9.()"`
		fi
		;;

	*-*-aix*)
		libc_version="bos.rte.libc `lslpp -Lqc bos.rte.libc | cut -f3 -d:`" 
		;;
	
	*-*-solaris*)
		libc=`pkginfo -l SUNWcsl | grep VERSION: | awk '{print $2}'`
		libc_version="SUNWcsl $libc"
		;;

	mips-sgi-irix*)
		libc_version="IRIX libc `uname -sRm`"
		;;

	alpha*-dec-osf*)
		dnl Another ugly case
		(cd /usr/.smdb.;
			libc_version=` grep -h libc.so *.inv | awk '$9 == "f" {print $12}' | tr '\n' ','`
		)
		;;

	powerpc-apple-darwin*)
		dnl MacOS guys, does this work?
		libc_version="`$LDD /usr/lib/libc.dylib | head -n1 | sed -e 's/.*current version[ ]*\([0-9.]*\).*$/\1/'`"
		;;

	*)
		libc_version=""
		;;
	esac

	AC_MSG_RESULT($libc_version)

	dnl Awww, shucks.
	if test -z "libc_version"; then
		libc_version="detection failed (please report this)"
	fi
])dnl SXE_CHECK_LIBC_VERSION

AC_DEFUN([SXE_CHECK_BASIC_HEADERS], [dnl
	AC_HEADER_DIRENT
	AC_HEADER_STAT
	AC_HEADER_SYS_WAIT
	AC_HEADER_MAJOR
	## check for stdbool, but turn off warnings before
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	AC_HEADER_STDBOOL
	SXE_RESTORE_LIBS

	SXE_CHECK_HEADERS([stddef.h stdlib.h string.h wchar.h])
	SXE_CHECK_HEADERS([fcntl.h float.h inttypes.h limits.h locale.h mach/mach.h malloc.h memory.h unistd.h ulimit.h])
	SXE_CHECK_HEADERS([sys/file.h sys/ioctl.h sys/param.h sys/pstat.h sys/resource.h sys/vlimit.h])
	SXE_CHECK_HEADERS([a.out.h elf.h])
	SXE_CHECK_HEADERS([ctype.h errno.h])
	SXE_CHECK_HEADERS([getopt.h])

	## just to be ubersure
	unset ac_cv_header_stdbool_h
	SXE_CHECK_HEADERS([stdbool.h])
])dnl SXE_CHECK_BASIC_HEADERS


AC_DEFUN([SXE_CHECK_BASIC_TYPES], [dnl
	pushdef([INC_STDBOOL_H], [
#if defined HAVE_STDBOOL_H
# include <stdbool.h>
#endif
		])

	## checks for typedefs
	AC_TYPE_INT16_T
	AC_TYPE_INT32_T
	AC_TYPE_INT8_T
	AC_TYPE_MODE_T
	AC_TYPE_OFF_T
	AC_TYPE_PID_T
	## next one is obsolete, it is safe to assume that RETSIGTYPE
	## is always `void' these days
	dnl AC_TYPE_SIGNAL
	AC_TYPE_SIZE_T
	AC_TYPE_SSIZE_T
	AC_TYPE_UID_T
	AC_TYPE_UINT16_T
	AC_TYPE_UINT32_T
	AC_TYPE_UINT64_T
	AC_TYPE_UINT8_T
	AC_TYPE_UINTMAX_T
	AC_TYPE_UINTPTR_T
	AC_CHECK_TYPES([wchar_t])
	AC_CHECK_TYPE([bool], [], [], INC_STDBOOL_H)
	AC_CHECK_TYPES([long long int])
	AC_CHECK_TYPES([longopts])

	dnl We immediately jump off the cliff when using a cross compiler
	dnl Why?
	AC_CHECK_SIZEOF([char], [0])
	AC_CHECK_SIZEOF([short], [0])
	AC_CHECK_SIZEOF([int], [0])
	AC_CHECK_SIZEOF([long], [0])
	AC_CHECK_SIZEOF([long long int], [0])
	AC_CHECK_SIZEOF([wchar_t], [0])
	AC_CHECK_SIZEOF([void *], [0])
	AC_CHECK_SIZEOF([double], [0])
	AC_CHECK_SIZEOF([long double], [0])
	AC_CHECK_SIZEOF([size_t], [0])
	AC_CHECK_SIZEOF([char *], [0])
	AC_CHECK_SIZEOF([bool], [0], INC_STDBOOL_H)

	## we used to do:
	## if test "$ac_cv_sizeof_short" = 0; then
	## 	echo ""
	## 	echo "*** PANIC *** Configure tests are not working - compiler is broken."
	## 	echo "*** PANIC *** Please examine config.log for compilation errors."
	## 	exit 1
	## fi

	## treat RETSIGTYPE specially since we've kicked the detection
	## routine for it
	AC_DEFINE([RETSIGTYPE], [void], [Return type of signal handler funs])

	popdef([INC_STDBOOL_H])
])dnl SXE_CHECK_BASIC_TYPES


AC_DEFUN([_SXE_CHECK_CTYPE_H_FUN], [dnl
	## arg #1 is the fun
	pushdef([FUN], [$1])
	pushdef([INC_CTYPE_H], [
#ifdef HAVE_CTYPE_H
# include <ctype.h>
#endif
		])

	## naive check
	AC_CHECK_FUNC(FUN)
	AC_CHECK_DECL(FUN)

	## now as they are usually in ctype.h, check there
	if test "$ac_cv_func_[]FUN[]" != "yes"; then
		unset ac_cv_func_[]FUN[]
		AC_CHECK_FUNCS(FUN, [], [], INC_CTYPE_H)
	else
		## just to define them symbols
		AC_CHECK_FUNCS(FUN)
	fi
	if test "$ac_cv_have_decl_[]FUN[]" != "yes"; then
		unset ac_cv_have_decl_[]FUN[]
		AC_CHECK_DECLS(FUN, [], [], INC_CTYPE_H)
	else
		## just to define them symbols
		AC_CHECK_DECLS(FUN)
	fi

	popdef([FUN])
	popdef([INC_CTYPE_H])
])dnl _SXE_CHECK_CTYPE_H_FUN

AC_DEFUN([_SXE_CHECK_CTYPE_H_FUNS], [dnl
	## arg #1 are the funs, space separated
	m4_foreach([_FUN], [$1], [_SXE_CHECK_CTYPE_H_FUN(_FUN)])
])dnl _SXE_CHECK_CTYPE_H_FUNS

AC_DEFUN([SXE_CHECK_BASIC_FUNS], [dnl
	## turn off that Werror thing
	SXE_LANG_WERROR([push+off])

	AC_CHECK_FUNCS([strdup strcat strncat strcmp strncmp strcpy strncpy])
	AC_CHECK_DECLS([strdup, strcat, strncat, strcmp, strncmp, strcpy, strncpy])
	AC_CHECK_FUNCS([strlen stpcpy stpncpy])
	AC_CHECK_DECLS([strlen, stpcpy, stpncpy])
	AC_CHECK_FUNCS([malloc realloc calloc free])
	AC_CHECK_DECLS([malloc, realloc, calloc, free])
	AC_CHECK_FUNCS([getopt getopt_long])
	AC_CHECK_DECLS([getopt, getopt_long])

	_SXE_CHECK_CTYPE_H_FUNS([isalnum, isalpha, isascii, isdigit])
	_SXE_CHECK_CTYPE_H_FUNS([isblank, isspace, ispunct, isgraph])
	_SXE_CHECK_CTYPE_H_FUNS([islower, isupper, isxdigit])
	_SXE_CHECK_CTYPE_H_FUNS([iscntrl, isprint])
])dnl SXE_CHECK_BASIC_FUNS


AC_DEFUN([SXE_CHECK_SIGNALS], [dnl
	AC_CHECK_DECLS([sys_siglist], [], [], [
#include <signal.h>
/* NetBSD declares sys_siglist in unistd.h.  */
#if HAVE_UNISTD_H
# include <unistd.h>
#endif
		])
	AC_CHECK_DECLS([sys_siglist])
])dnl SXE_CHECK_SIGNALS

AC_DEFUN([SXE_CHECK_INTPTR_T], [dnl
	dnl not AC_CHECK_TYPE; lisp.h does hairy conditional typedef
	dnl why not? let's test it...
	SXE_CHECK_HEADERS([inttypes.h])
	AC_CHECK_TYPE([intptr_t], [], [], [
#if defined HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
		])
	if test "$ac_cv_type_intptr_t" = "yes"; then
		AC_DEFINE([HAVE_INTPTR_T_IN_SYS_TYPES_H], [1], [Description here!])
	fi

	dnl if test "$ac_cv_header_inttypes_h" != "yes"; then
	dnl 	AC_MSG_CHECKING([for intptr_t in sys/types.h])
	dnl 	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
	dnl #include <sys/types.h>
	dnl ]], [[
	dnl intptr_t x;
	dnl ]])], [dnl
	dnl 		AC_MSG_RESULT(yes)
	dnl 		AC_DEFINE([HAVE_INTPTR_T_IN_SYS_TYPES_H], [1],
	dnl			[Description here!])
	dnl 		], [dnl
	dnl 		AC_MSG_RESULT(no)])
	dnl fi
])dnl SXE_CHECK_INTPTR_T

dnl sxe-libc.m4 ends here
