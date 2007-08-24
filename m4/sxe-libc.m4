dnl sxe-libc.m4 -- libc stuff

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
		AC_DEFINE([_GNU_SOURCE], [1], [Description here!])
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
	AC_HEADER_STDBOOL
	AC_HEADER_SYS_WAIT
	AC_HEADER_MAJOR

	SXE_CHECK_HEADERS([stddef.h stdlib.h string.h wchar.h])
	SXE_CHECK_HEADERS([fcntl.h float.h inttypes.h limits.h locale.h mach/mach.h malloc.h memory.h unistd.h ulimit.h])
	SXE_CHECK_HEADERS([sys/file.h sys/ioctl.h sys/param.h sys/pstat.h sys/resource.h sys/vlimit.h])
	SXE_CHECK_HEADERS([a.out.h elf.h])
])dnl SXE_CHECK_BASIC_HEADERS


AC_DEFUN([SXE_CHECK_BASIC_TYPES], [dnl
	dnl checks for typedefs
	AC_TYPE_INT16_T  dnl autoconf >=2.60
	AC_TYPE_INT32_T  dnl autoconf >=2.60
	AC_TYPE_INT8_T   dnl autoconf >=2.60
	AC_TYPE_MODE_T
	AC_TYPE_OFF_T
	AC_TYPE_PID_T
	AC_TYPE_SIGNAL
	AC_TYPE_SIZE_T
	AC_TYPE_SSIZE_T  dnl autoconf >=2.60
	AC_TYPE_UID_T
	AC_TYPE_UINT16_T  dnl autoconf >=2.60
	AC_TYPE_UINT32_T  dnl autoconf >=2.60
	AC_TYPE_UINT64_T  dnl autoconf >=2.60
	AC_TYPE_UINT8_T   dnl autoconf >=2.60
	AC_TYPE_UINTMAX_T
	AC_TYPE_UINTPTR_T
	AC_CHECK_TYPES([wchar_t])

	dnl We immediately jump off the cliff when using a cross compiler
	dnl Why?
	AC_CHECK_SIZEOF([char], [0])
	AC_CHECK_SIZEOF([short], [0])
	AC_CHECK_SIZEOF([int], [0])
	AC_CHECK_SIZEOF([long], [0])
	AC_CHECK_SIZEOF([long long], [0])
	AC_CHECK_SIZEOF([wchar_t], [0])
	AC_CHECK_SIZEOF([void *], [0])
	AC_CHECK_SIZEOF([double], [0])
	AC_CHECK_SIZEOF([long double], [0])
	AC_CHECK_SIZEOF([size_t], [0])
	AC_CHECK_SIZEOF([char *], [0])

	## we used to do:
	## if test "$ac_cv_sizeof_short" = 0; then
	## 	echo ""
	## 	echo "*** PANIC *** Configure tests are not working - compiler is broken."
	## 	echo "*** PANIC *** Please examine config.log for compilation errors."
	## 	exit 1
	## fi
])dnl SXE_CHECK_BASIC_TYPES


AC_DEFUN([SXE_CHECK_BASIC_FUNS], [dnl
	AC_CHECK_FUNCS([strdup strcat strncat strcmp strncmp strcpy strncpy])
	AC_CHECK_DECLS([strdup, strcat, strncat, strcmp, strncmp, strcpy, strncpy])
	AC_CHECK_FUNCS([strlen malloc realloc calloc free])
	AC_CHECK_DECLS([strlen, malloc, realloc, calloc, free])
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
