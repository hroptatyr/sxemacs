dnl sxe-libtool.m4 -- just a quick libtoolish macros
dnl
dnl Copyright (C) 2007, 2008 Sebastian Freundt.
dnl
dnl This file is part of SXEmacs

AC_DEFUN([SXE_CHECK_LIBTOOL], [dnl
	AC_MSG_RESULT([starting libtool investigation...])
	m4_ifdef([LT_INIT], [_SXE_CHECK_LT2], [_SXE_CHECK_LT1])

	if test -n "$export_dynamic_flag_spec"; then
		sxe_cv_export_dynamic=$(\
			echo $(eval echo "$export_dynamic_flag_spec"))
		SXE_APPEND([$sxe_cv_export_dynamic], [LDFLAGS])
	else
		AC_MSG_NOTICE([
Neither -export-dynamic nor equivalent flags are supported by your linker.
Emodules however will reference some symbols dynamically.
We assume that your linker will do what we need, but this assumption
might be wrong as well.
])dnl AC_MSG_NOTICE
	fi

	## cope with libtool's convenience lib/bin concept
	if test -n "$lt_cv_objdir"; then
		## this variable is a #define, too
		LT_OBJDIR="$lt_cv_objdir"
	else
		## hm, probably not the best idea but let's try
		LT_OBJDIR="."
	fi
	## definitely subst that though
	AC_SUBST([LT_OBJDIR])

	## currently there's no official variable for that, but `lt-'
	## seems to be a consistent choice throughout all libtools
	LT_CONVENIENCE_PREFIX="lt-"
	AC_SUBST([LT_CONVENIENCE_PREFIX])
])dnl SXE_CHECK_LIBTOOL

AC_DEFUN([_SXE_CHECK_LT2], [dnl
	LT_CONFIG_LTDL_DIR([libltdl], [recursive])
	LT_PREREQ([2.1])
	LT_INIT([dlopen])
	LTDL_INSTALLABLE
	LTDL_INIT

	LT_LIB_DLLOAD
	LT_LIB_M
	LT_SYS_DLOPEN_DEPLIBS
	LT_SYS_DLSEARCH_PATH
	LT_SYS_MODULE_EXT
	LT_SYS_MODULE_PATH
	LT_SYS_SYMBOL_USCORE
	LT_FUNC_DLSYM_USCORE

	dnl Configure libltdl
	dnl newer libtool2s will do this implicitly, we drop all support
	dnl for the `old' libtool2 stuff as this is available through
	dnl cvs only and we stick with the latest
	dnl AC_CONFIG_SUBDIRS([libltdl])
	AC_CONFIG_MACRO_DIR([libltdl/m4])
])dnl _SXE_CHECK_LT2

m4_ifdef([LT_CONFIG_LTDL_DIR], [], [dnl else
AC_DEFUN([LT_CONFIG_LTDL_DIR], [dnl
	AS_MESSAGE([trying to fake an initialisation of a libltdl subproject])
])dnl LT_CONFIG_LTDL_DIR
])

AC_DEFUN([_SXE_CHECK_LT1], [dnl
	## This overcomes a strange but existent scenario
	## (see ssh horstbox for one)
	## where autoconf is SO new that it can actually only work
	## with libtool2 but, sigh, of course, sigh, libtool-1.stone.age
	## is installed
	LT_CONFIG_LTDL_DIR([libltdl], [recursive])

	AC_LIBLTDL_INSTALLABLE
	dnl AC_LTDL_ENABLE_INSTALL
	dnl Check for dlopen support
	AC_LIBTOOL_DLOPEN
	AC_PROG_LIBTOOL([dlopen])
	AC_LIB_LTDL
	AC_LTDL_SYMBOL_USCORE

	dnl Substitute LTDLINCL and LIBLTDL in the Makefiles
	AC_SUBST(LTDLINCL)
	AC_SUBST(LIBLTDL)
	AC_SUBST([LIBTOOL_DEPS])

	dnl Configure libltdl
	AC_CONFIG_SUBDIRS([libltdl])
])dnl _SXE_CHECK_LT1

AC_DEFUN([SXE_CHECK_LIBLTDL], [dnl
	## make sure the libtool stuff has been run before
	AC_REQUIRE([SXE_CHECK_LIBTOOL])

	AC_CACHE_CHECK([for dynamic loader provided by libltdl],
		[sxe_cv_feat_libltdl], [_SXE_CHECK_LIBLTDL])

	## if the user wants to use the included libltdl, descend
	if test -z "$with_included_ltdl" -a \
		-d "libltdl" -a \
		"$with_module_support" != "no" -a \
		"$sxe_cv_feat_libltdl" != "yes"; then
		with_included_ltdl="yes"
		## assume we have a working ltdl lib afterwards
		sxe_cv_feat_libltdl="yes"
		## also install that pig
		enable_ltdl_install="yes"
		## add libltdl/ to the include path
		CPPFLAGS="$CPPFLAGS ${LTDLINCL}"
		## and assume we've seen ltdl.h
		AC_DEFINE([HAVE_LTDL_H], [1], [Whether ltdl.h is somewhere])
	elif test -d "libltdl" -a \
		"$with_included_ltdl" = "yes"; then
		## the user WANTS to use the included ltdl
		## assume we have a working ltdl lib afterwards
		sxe_cv_feat_libltdl="yes"
		## also install that pig
		enable_ltdl_install="yes"
		## add libltdl/ to the include path
		CPPFLAGS="$CPPFLAGS -I${top_srcdir}/libltdl"
		## and assume we've seen ltdl.h
		AC_DEFINE([HAVE_LTDL_H], [1], [Whether ltdl.h is somewhere])
	else
		with_included_ltdl="no"
	fi
	AM_CONDITIONAL([DESCEND_LIBLTDL], [test "$with_included_ltdl" = "yes"])
])dnl SXE_CHECK_LIBLTDL

AC_DEFUN([_SXE_CHECK_LIBLTDL], [dnl
	AC_MSG_RESULT([])
	SXE_CHECK_LTDL_HEADERS
	SXE_CHECK_LTDL_FUNCS

	if test "$ac_cv_header_ltdl_h" = "yes" -a \
		"$ac_cv_func_lt_dlopen" = "yes" -a \
		"$ac_cv_func_lt_dlclose" = "yes" -a \
		"$ac_cv_type_lt_dlhandle" = "yes" -a \
		"$ac_cv_type_lt_dlinfo" = "yes"; then
		sxe_cv_feat_libltdl="yes"
	else
		sxe_cv_feat_libltdl="no"
	fi
])dnl _SXE_CHECK_LIBLTDL

AC_DEFUN([SXE_CHECK_LTDL_HEADERS], [dnl
	AC_CHECK_HEADERS([ltdl.h])
])dnl SXE_CHECK_LTDL_HEADERS

AC_DEFUN([SXE_CHECK_LTDL_FUNCS], [dnl
	AC_REQUIRE([SXE_CHECK_LIBTOOL])

	SXE_DUMP_LIBS
	LIBS="$LIBS $LIBLTDL"
	_SXE_CHECK_LTDL_FUNCS
	SXE_RESTORE_LIBS

	_SXE_CHECK_LTDL_TYPES
])dnl _SXE_CHECK_LTDL_FUNCS

AC_DEFUN([_SXE_CHECK_LTDL_FUNCS], [dnl
	AC_CHECK_FUNCS([dnl
		lt_dlopen lt_dlopenext dnl
		lt_dlclose lt_dlexit dnl
		lt_dlsym lt_dlgetinfo dnl
		lt_dlfree lt_dlforeach lt_dlhandle_next dnl
		lt_dlinterface_register lt_dlcaller_register lt_dlhandle_fetch dnl
		lt_dladdsearchdir dnl
		lt_dlcaller_set_data lt_dlcaller_get_data dnl
		lt_dlcaller_id dnl
		])

	AC_CHECK_DECLS([lt_dlopen, lt_dlopenext, lt_dlclose, lt_dlexit],
		[], [], [
#ifdef HAVE_LTDL_H
# include <ltdl.h>
#endif
		])
])dnl _SXE_CHECK_LTDL_FUNCS

AC_DEFUN([_SXE_CHECK_LTDL_TYPES], [dnl
	AC_CHECK_TYPES([lt_dlinfo, lt_dlhandle], [], [], [
#ifdef HAVE_LTDL_H
# include <ltdl.h>
#endif
		])
])dnl _SXE_CHECK_LTDL_TYPES

dnl sxe-libtool.m4 ends here
