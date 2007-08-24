dnl sxe-emodffi.m4 -- Everything that extends SXE functionality on the fly

dnl module voodoo
AC_DEFUN([SXE_REGISTER_MODULE], [dnl
	## arg #1 is module name
	pushdef([emodname], [$1])
	pushdef([EMODNAME], [translit([$1], [a-z], [A-Z])])

	AM_CONDITIONAL([DESCEND_]EMODNAME, [dnl
		test "$have_static_modules_[]emodname[]" = "yes" -o \
		"$have_modules_[]emodname[]" = "yes"])
	AC_CONFIG_FILES([modules/]emodname[/Makefile])

	popdef([EMODNAME])
	popdef([emodname])
])

AC_DEFUN([SXE_EMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description (unused atm)
	pushdef([emodname], [$1])
	pushdef([EMODNAME], [translit([$1], [a-z], [A-Z])])

	SXE_REGISTER_MODULE([$1])
	AM_CONDITIONAL([EMOD_]EMODNAME, [dnl
		test "$have_modules_[]emodname[]" = "yes"])

	popdef([EMODNAME])
	popdef([emodname])
])dnl SXE_EMOD

AC_DEFUN([SXE_STATMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description (unused atm)
	pushdef([emodname], [$1])
	pushdef([EMODNAME], [translit([$1], [a-z], [A-Z])])

	SXE_REGISTER_MODULE([$1])
	if test "$have_module_support" = "yes" -a \
		"$have_static_modules_[]emodname[]" = "yes"; then
		AC_DEFINE([USE_STATIC_]EMODNAME, [1], [dnl
			Whether to use the module] emodname [statically])
		SXE_ADD_STATMOD_A([$emodblddir/]emodname[/lib]emodname[.a])
		echo "syms_of_[]emodname[]();" >> emod_static_syms.c
		echo "vars_of_[]emodname[]();" >> emod_static_vars.c
	fi
	AM_CONDITIONAL([STATIC_]EMODNAME, [dnl
		test "$have_static_modules_[]emodname[]" = "yes"])

	popdef([EMODNAME])
	popdef([emodname])
])dnl SXE_STATMOD

AC_DEFUN([SXE_EMOD_STATMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description
	SXE_REGISTER_MODULE([$1])
	pushdef([SXE_REGISTER_MODULE], [])
	SXE_EMOD([$1], [$2])
	SXE_STATMOD([$1], [$2])
	popdef([SXE_REGISTER_MODULE])
])dnl SXE_EMOD_STATMOD



AC_DEFUN([SXE_FFI_TRY_STANDARD_PREFIX], [dnl
	## arg1 what to do in case of success
	## arg2 what to do in case of failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_CHECK_HEADERS([ffi.h])
	if test "$ac_cv_header_ffi_h" != "yes"; then
		have_ffi="no"
		MM_FAIL
	else
		:
		MM_SUCC
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_FFI_TRY_STANDARD_PREFIX

AC_DEFUN([SXE_FFI_TRY_GCC_PREFIX], [dnl
	## arg1 what to do in case of success
	## arg2 what to do in case of failure
	## maybe defines FFI_CPPFLAGS
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_DUMP_LIBS
	if test "$GCC" = "yes"; then
		GCC_FFI_PATH=$(dirname $($CC -print-libgcc-file-name))/include/libffi
		if test -n "${GCC_FFI_PATH}"; then
			FFI_CPPFLAGS="-I${GCC_FFI_PATH}"
		fi
		SXE_APPEND([$FFI_CPPFLAGS], [CPPFLAGS])
	else
		GCC_FFI_PATH=
	fi
	SXE_CHECK_HEADERS([ffi.h])

	if test "$ac_cv_header_ffi_h" != "yes"; then
		have_ffi="no"
		MM_FAIL
	else
		:
		MM_SUCC
	fi
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_FFI_TRY_STANDARD_PREFIX

AC_DEFUN([SXE_CHECK_FFI], [dnl
	SXE_FFI_TRY_STANDARD_PREFIX([:], [unset ac_cv_header_ffi_h])
	if test "$have_ffi" = "no"; then
		SXE_FFI_TRY_GCC_PREFIX([:], [dnl
			AC_MSG_WARN([GNU C FFI not found. Will have to revert path append.])])
	fi

	AC_CHECK_LIB([ffi], [ffi_call], [:], [dnl
		AC_MSG_WARN([Couldn't find a usable libffi, not compiling FFI support])])
	if test "$ac_cv_header_ffi_h" = "yes" -a \
		"$ac_cv_lib_ffi_ffi_call" = "yes"; then
		have_ffi="yes"
	else
		have_ffi="no"
	fi

	AC_SUBST([FFI_CPPFLAGS])
	AC_SUBST([FFI_LDFLAGS])
	AC_SUBST([FFI_LIBS])
])dnl SXE_CHECK_FFI


AC_DEFUN([SXE_CHECK_DLOPEN], [
	have_dlopen=no
	try_on=yes
	SXE_CHECK_HEADERS([dlfcn.h])

	if test "$ac_cv_header_dlfcn_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -lc])
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -ldl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -ldl"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -lsvld])
		LIBS="$ac_save_LIBS -lsvld"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes"; then
		AC_DEFINE([HAVE_DLOPEN], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_DL

AC_DEFUN([SXE_CHECK_SHL_LOAD], [
	dnl Check for HP/UX shl_load
	have_shl_load=no
	try_on=yes
	SXE_CHECK_HEADERS([dl.h])

	if test "$ac_cv_header_dl_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for shl_load in -lc])
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dl.h>
			]],
			[[shl_load ("", 0, 0);]])], [
			AC_MSG_RESULT([yes])
			have_shl_load=yes], [
			AC_MSG_RESULT([no])
			have_shl_load=no])
	fi

	if test "$have_shl_load" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for shl_load in -ldl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -ldld"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dl.h>
			]],
			[[shl_load ("", 0, 0);]])], [
			AC_MSG_RESULT([yes])
			have_shl_load=yes], [
			AC_MSG_RESULT([no])
			have_shl_load=no])
	fi

	if test "$have_shl_load" = "yes"; then
		AC_DEFINE([HAVE_SHL_LOAD], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_SHL_LOAD

AC_DEFUN([SXE_CHECK_LT_DLINIT], [
	dnl Check for libtool's libltdl
	have_lt_dlinit=no
	try_on=yes
	SXE_CHECK_HEADERS([ltdl.h])

	if test "$ac_cv_header_ltdl_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for lt_dlinit in -lltdl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -lltdl"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <ltdl.h>
			]],
			[[lt_dlinit ();]])], [
			AC_MSG_RESULT([yes])
			have_lt_dlinit=yes], [
			AC_MSG_RESULT([no])
			have_lt_dlinit=no])
	fi

	if test "$have_lt_dlinit" = "yes"; then
		AC_DEFINE([HAVE_LTDL], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_LT_DLINIT


dnl sxe-emodffi.m4 ends here
