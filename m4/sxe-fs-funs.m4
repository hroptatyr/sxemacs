dnl sxe-fs-funs.m4 -- Functions for file system access

AC_DEFUN([SXE_CHECK_REALPATH], [dnl
	## defines have_realpath

	## for the dirname proto
	AC_CHECK_DECLS([realpath], [], [], [
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
		])
	AC_CHECK_FUNCS([realpath])
	AC_CHECK_LIB([c], [realpath])

	if test "$ac_cv_func_realpath" = "yes"; then
		SXE_CHECK_BROKEN_REALPATH
	elif test "$ac_cv_lib_c_realpath" = "yes"; then
		LDFLAGS="$LDFLAGS -lc"
		SXE_CHECK_BROKEN_REALPATH
	else
		have_realpath=no
	fi

	if test "$sxe_func_realpath_broken" != "no"; then
		SXE_ADD_CORE_OBJS([realpath.o])
	fi
])dnl SXE_CHECK_REALPATH

AC_DEFUN([_SXE_CHECK_REALPATH_RETVAL], [dnl
	## arg #1 is the return type, char* by default
	## arg #2 is the final test,
	##        pass something that is expected to be true here
	##        the return value is stored in a variable `r'
	## defines sxe_func_realpath_returns_<ret_t> and
	## sxe_func_realpath_returns=$1 if true
	pushdef([ret_t], ifelse($1, [], [char*],$1))
	pushdef([resvar_body], [sxe_func_realpath_returns])
	pushdef([resvar], resvar_body[_]translit(ret_t, [ -*], [__X]))
	pushdef([rettest], ifelse($2, [], [[[[r[0] == '/']]]],$2))

	AC_MSG_CHECKING([whether realpath returns ]ret_t)
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

int main()
{
	]]ret_t[[ r;
	char p[8] = "/bin/sh\000";
	char resv[PATH_MAX];
	int res;

	resv[0] = '\0';
	r = realpath(p, resv);
	if (r) {
		res = ((]]rettest[[) == 0);
	} else {
		res = 0;
	}
	return res;
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		resvar_body[=]ret_t
		AC_DEFINE_UNQUOTED([REALPATH_RET_T], ret_t,
			[return type of realpath()])
	fi

	popdef([resvar_body])
	popdef([resvar])
	popdef([ret_t])
	popdef([rettest])
])dnl _SXE_CHECK_REALPATH_RETVAL

AC_DEFUN([_SXE_CHECK_REALPATH_RETVAL_OWNER], [dnl
	## defines sxe_func_realpath_retval_owner,
	## values are either "sys" or "user"
	pushdef([resvar], [sxe_func_realpath_retval_owner])

	## this test is especially critical, because some systems do not
	## allocate memory for the user when the return value is "."
	## instead they point to a static const char somewhere in their
	## guts which, of course, must not be furtherly modified, free'd or
	## anything ... took me fucking ages to find out what's going on
	## so let's drink to the morons responsible for THAT!
	AC_MSG_CHECKING([to whom belongs the object returned by realpath])
        if test "$opsys" = "darwin"; then
           resvar=user
        else
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define FOLLOW_FREE_STRATEGY		1
#define FOLLOW_REALLOC_STRATEGY		1

int main()
{
	void *r;  /* any pointer is just fine */
	char p[8] = "/bin/sh\000";

	r = (void*)realpath(p, NULL);
#if FOLLOW_REALLOC_STRATEGY
	/* reallocation would help already */
	r = realloc(r, 4096);
#endif
#if FOLLOW_FREE_STRATEGY
	/* if r was ours we should be able to free it */
	free(r);
#endif
#if FOLLOW_FREE_STRATEGY || FOLLOW_REALLOC_STRATEGY
	return 0;
#else
	return 1;
#endif
}
		]])],
		resvar[=user],
		resvar[=sys],
		resvar[=sys])
        fi
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "user"; then
		AC_DEFINE([REALPATH_USER_OWNS_RETVAL], [1],
			[Whether the user space owns the retval of realpath()])
	elif test "$[]resvar[]" = "sys"; then
		AC_DEFINE([REALPATH_SYS_OWNS_RETVAL], [1],
			[Whether the system owns the retval of realpath()])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_REALPATH_RETVAL_OWNER

AC_DEFUN([_SXE_CHECK_REALPATH_ON_PROTECTED_MEMORY], [dnl
	## defines sxe_func_realpath_accepts_protmem
	pushdef([resvar], [sxe_func_realpath_accepts_protmem])

	AC_MSG_CHECKING([whether realpath can operate on protected mem blocks])
        if test "$opsys" = "darwin"; then
           resvar=no
        else
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

int main()
{
	char resv[PATH_MAX];
	realpath("/bin/sh", NULL);
	realpath("/bin/sh", resv);
	return 0;
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
        fi
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		AC_DEFINE([REALPATH_ACCEPTS_PROTMEM], [1],
			[Whether realpath() accepts protected memory blocks])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_REALPATH_ON_PROTECTED_MEMORY

AC_DEFUN([_SXE_CHECK_REALPATH_SANE_ON_NON_EXISTENT], [dnl
	## defines sxe_func_realpath_
	pushdef([resvar], [sxe_func_realpath_sane_on_non_existent])

	AC_MSG_CHECKING([whether realpath survives if we pass non-existent stuff])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static char p[24] = "/nobody/has/this/file\000";

int main()
{
	char *r;
	char resv[PATH_MAX];
	r = realpath((char*)p, resv);

	return ((r == NULL) == 0);
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		AC_DEFINE([REALPATH_SANE_ON_NON_EXISTENT], [1],
			[Whether realpath() accepts and handles non-existent files])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_REALPATH_SANE_ON_NON_EXISTENT

AC_DEFUN([SXE_CHECK_BROKEN_REALPATH], [dnl
	## defines 3 vars, look in the sub macros to see which ones
	_SXE_CHECK_REALPATH_ON_PROTECTED_MEMORY
	_SXE_CHECK_REALPATH_SANE_ON_NON_EXISTENT
	_SXE_CHECK_REALPATH_RETVAL
	_SXE_CHECK_REALPATH_RETVAL_OWNER

	AC_MSG_CHECKING([if realpath is considered broken])
	if test "$sxe_func_realpath_returns" = "char*" -a \
		"$sxe_func_realpath_sane_on_non_existent" = "yes" -a \
		"$sxe_func_realpath_retval_owner" = "user"; then
		sxe_func_realpath_broken="no"
	else
		sxe_func_realpath_broken="yes"
	fi
	AC_MSG_RESULT([$sxe_func_realpath_broken])
])dnl SXE_CHECK_BROKEN_REALPATH


AC_DEFUN([SXE_CHECK_DIRNAME], [dnl
	## defines have_dirname

	## of course posix standards are just rough draughts
	## and by no means obliging in any way ...
	## and since we all hate working systems we do our best
	## to break these so called standards wherever we can
	##
	## Passage from coreutils: 
	## In general, we can't use the builtin `dirname' function if available,
	## since it has different meanings in different environments. In some
	## environments the builtin `dirname' modifies its argument.

	## for the dirname proto
	SXE_CHECK_HEADERS([libgen.h])
	AC_CHECK_DECLS([dirname], [], [], [
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif
		])
	AC_CHECK_FUNCS([dirname])	dnl should be part of glibc
	AC_CHECK_LIB([c], [dirname])

	if test "$ac_cv_func_dirname" = "yes"; then
		SXE_CHECK_BROKEN_DIRNAME
	elif test "$ac_cv_lib_c_dirname" = "yes"; then
		LDFLAGS="$LDFLAGS -lc"
		SXE_CHECK_BROKEN_DIRNAME
	else
		have_dirname=no
	fi
])dnl SXE_CHECK_DIRNAME

AC_DEFUN([_SXE_CHECK_DIRNAME_SIDE_EFFECT], [dnl
	## defines sxe_func_dirname_side_effect
	## and DIRNAME_SIDE_EFFECT
	pushdef([resvar], [sxe_func_dirname_side_effect])

	AC_MSG_CHECKING([whether dirname modifies its argument by side-effect])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif

int main()
{
	char p[11] = "somefile\000";
	dirname(p);
	return ((p[0] == '.' && p[1] == '\0') == 0);
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		AC_DEFINE([DIRNAME_SIDE_EFFECT], [1],
			[Whether dirname() operates by side effect])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_DIRNAME_SIDE_EFFECT

AC_DEFUN([_SXE_CHECK_DIRNAME_RETVAL], [dnl
	## arg #1 is the return type, char* by default
	## arg #2 is the final test,
	##        pass something that is expected to be true here
	##        the return value is stored in a variable `r'
	## defines sxe_func_dirname_returns_<ret_t> and
	## sxe_func_dirname_returns=$1 if true
	pushdef([ret_t], ifelse($1, [], [char*],$1))
	pushdef([resvar_body], [sxe_func_dirname_returns])
	pushdef([resvar], resvar_body[_]translit(ret_t, [ -*], [__X]))
	pushdef([rettest], ifelse($2, [], [[[[r[0] == '.' && r[1] == '\000']]]],$2))

	AC_MSG_CHECKING([whether dirname returns ]ret_t)
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif

int main()
{
	]]ret_t[[ r;
	char p[11] = "somefile\000";
	int res;

	r = dirname(p);
	res = ((]]rettest[[) == 0);
	return res;
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		resvar_body[=]ret_t
		AC_DEFINE_UNQUOTED([DIRNAME_RET_T], ret_t,
			[return type of dirname()])
	fi

	popdef([resvar_body])
	popdef([resvar])
	popdef([ret_t])
	popdef([rettest])
])dnl _SXE_CHECK_DIRNAME_RETVAL

AC_DEFUN([_SXE_CHECK_DIRNAME_RETVAL_OWNER], [dnl
	## defines sxe_func_dirname_retval_owner,
	## values are either "sys" or "user"
	pushdef([resvar], [sxe_func_dirname_retval_owner])

        malloc_check=${MALLOC_CHECK_}
        ## Turn off the stupid glibc 2.5 stack trace check. We *know* we may
        ## do something bad here :-) 
        MALLOC_CHECK_=0
	export MALLOC_CHECK_
	## this test is especially critical, because some systems do not
	## allocate memory for the user when the return value is "."
	## instead they point to a static const char somewhere in their
	## guts which, of course, must not be furtherly modified, free'd or
	## anything ... took me fucking ages to find out what's going on
	## so let's drink to the morons responsible for THAT!
	AC_MSG_CHECKING([to whom belongs the object returned by dirname])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif

#define FOLLOW_FREE_STRATEGY		1
#define FOLLOW_REALLOC_STRATEGY		1

int main()
{
	void *r;  /* any pointer is just fine */
	char p[11] = "./somefile\000";

	r = (void*)dirname(p);
#if FOLLOW_REALLOC_STRATEGY
	/* reallocation would help already */
	r = realloc(r, 4096);
#endif
#if FOLLOW_FREE_STRATEGY
	/* if r was ours we should be able to free it */
	free(r);
#endif
#if FOLLOW_FREE_STRATEGY || FOLLOW_REALLOC_STRATEGY
	return 0;
#else
	return 1;
#endif
}
		]])],
		resvar[=user],
		resvar[=sys],
		resvar[=sys])
        if test "${malloc_check}" = "" ; then
		unset MALLOC_CHECK_
        else 
        	MALLOC_CHECK_=${malloc_check}
        fi
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "user"; then
		AC_DEFINE([DIRNAME_USER_OWNS_RETVAL], [1],
			[Whether the user space owns the retval of dirname()])
	elif test "$[]resvar[]" = "sys"; then
		AC_DEFINE([DIRNAME_SYS_OWNS_RETVAL], [1],
			[Whether the system owns the retval of dirname()])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_DIRNAME_RETVAL_OWNER

AC_DEFUN([_SXE_CHECK_DIRNAME_ON_PROTECTED_MEMORY], [dnl
	## defines sxe_func_dirname_accepts_protmem
	pushdef([resvar], [sxe_func_dirname_accepts_protmem])

	AC_MSG_CHECKING([whether dirname can operate on protected mem blocks])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif

int main()
{
	dirname("./somefile");
	return 0;
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		AC_DEFINE([DIRNAME_ACCEPTS_PROTMEM], [1],
			[Whether dirname() accepts protected memory blocks])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_DIRNAME_ON_PROTECTED_MEMORY

AC_DEFUN([_SXE_CHECK_DIRNAME_ON_C99_RESTRICT_MEMORY], [dnl
	## defines sxe_func_dirname_accepts_restrmem
	pushdef([resvar], [sxe_func_dirname_accepts_restrmem])

	AC_MSG_CHECKING([whether dirname can operate on C99 restrict mem blocks])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_STDIO_H
#  inlcude <stdio.h>
#endif
#ifdef HAVE_LIBGEN_H
#  include <libgen.h>
#endif

static char f[11] = "./somefile\000";

int main()
{
	const char *restrict p = &f;
	dirname((char*)p);
	return 0;
}
		]])],
		resvar[=yes],
		resvar[=no],
		resvar[=no])
	AC_MSG_RESULT([$]resvar)

	if test "$[]resvar[]" = "yes"; then
		AC_DEFINE([DIRNAME_ACCEPTS_RESTRMEM], [1],
			[Whether dirname() accepts restricted memory blocks])
	fi

	popdef([resvar])
])dnl _SXE_CHECK_DIRNAME_ON_C99_RESTRICT_MEMORY

AC_DEFUN([SXE_CHECK_BROKEN_DIRNAME], [dnl
	## defines 3 vars, look in the sub macros to see which ones
	_SXE_CHECK_DIRNAME_SIDE_EFFECT
	_SXE_CHECK_DIRNAME_ON_PROTECTED_MEMORY
	_SXE_CHECK_DIRNAME_ON_C99_RESTRICT_MEMORY
	_SXE_CHECK_DIRNAME_RETVAL
	_SXE_CHECK_DIRNAME_RETVAL_OWNER
])dnl SXE_CHECK_BROKEN_DIRNAME


AC_DEFUN([SXE_CHECK_FILE_LOCK], [dnl
	## Determine type of mail locking from configure args and s&m headers
	AC_MSG_CHECKING([for a type of mail spool file locking])
	AC_MSG_RESULT([])

	AC_CHECK_FUNCS([lockf flock locking])
	## The mail_use_xxx variables are set according to the s&m headers.
	if test "$with_mail_locking" != "no" -a \
		"$mail_use_flock" = "yes"; then
		with_mail_locking=flock
	elif test "$with_mail_locking" != "no" -a \
		"$mail_use_lockf" = "yes"; then
		with_mail_locking=lockf
	elif test "$with_mail_locking" != "no" -a \
		"$mail_use_locking" = "yes"; then
		with_mail_locking=locking
	fi

	if test "$with_mail_locking" = "lockf"; then
		AC_DEFINE([MAIL_LOCK_LOCKF], [1], [Description here!])
	elif test "$with_mail_locking" = "flock"; then
		AC_DEFINE([MAIL_LOCK_FLOCK], [1], [Description here!])
	elif test "$with_mail_locking" = "locking"; then
		AC_DEFINE([MAIL_LOCK_LOCKING], [1], [Description here!])
	elif test "$with_mail_locking" = "pop"; then
		with_pop=yes
		with_mail_locking=
	elif test "$with_mail_locking" = "mmdf"; then
		AC_DEFINE([MAIL_LOCK_MMDF], [1], [Description here!])
	else
		with_mail_locking="file"
		AC_DEFINE([MAIL_LOCK_DOT], [1], [Description here!])
	fi

	if test "$with_mail_locking" = "lockf" -a \
		"$ac_cv_func_lockf" != "yes"; then
		SXE_DIE([lockf mail locking requested but not available.])
	elif test "$with_mail_locking" = "flock" -a \
		"$ac_cv_func_flock" != "yes"; then
		SXE_DIE([flock mail locking requested but not available.])
	elif test "$with_mail_locking" = "locking" -a \
		"$ac_cv_func_locking" != "yes"; then
		SXE_DIE([locking mail locking requested but not available.])
	fi
])dnl SXE_CHECK_FILE_LOCK


dnl sxe-fs-funs.m4 ends here
