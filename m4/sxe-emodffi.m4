dnl sxe-emodffi.m4 -- Everything that extends SXE functionality on the fly
dnl
dnl Copyright (C) 2006-2008 Sebastian Freundt
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

AC_DEFUN([SXE_FFI_TRY_PKGCONFIG], [dnl
	## arg1 what to do in case of success
	## arg2 what to do in case of failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	if test "$sxe_cv_prog_pkg_config" = "no" -o -z "$PKG_CONFIG"; then
		AS_MESSAGE([*** pkg-config not found.])
		AS_MESSAGE([*** Cannot check for FFI with pkg-config.])
		sxe_cv_feat_ffi="no"
		MM_FAIL
	else
		FFI_CPPFLAGS=$($PKG_CONFIG --cflags libffi)
		FFI_LIBS=$($PKG_CONFIG --libs libffi)
		SXE_DUMP_LIBS
		CPPFLAGS="$CPPFLAGS $FFI_CPPFLAGS"
		SXE_CHECK_HEADERS([ffi.h])
		if test "$ac_cv_header_ffi_h" = "yes"; then
			AC_CHECK_LIB([ffi], [ffi_call], [dnl
				sxe_cv_feat_ffi="yes"
				have_ffi="yes"], [:])
		fi
		SXE_RESTORE_LIBS
		if test "$ac_cv_header_ffi_h" = "yes" -a \
			"$ac_cv_lib_ffi_ffi_call" = "yes"; then
			MM_SUCC
		fi
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_FFI_TRY_PKGCONFIG

AC_DEFUN([SXE_FFI_TRY_STANDARD_PREFIX], [dnl
	## arg1 what to do in case of success
	## arg2 what to do in case of failure
	## defines sxe_cv_feat_ffi
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_CHECK_HEADERS([ffi.h])
	if test "$ac_cv_header_ffi_h" != "yes"; then
		sxe_cv_feat_ffi="no"
		MM_FAIL
	else
		sxe_cv_feat_ffi="yes"
		MM_SUCC
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_FFI_TRY_STANDARD_PREFIX

AC_DEFUN([_SXE_FFI_TRY_GCC_PREFIX], [dnl
	## arg 1 is the path we should try
	## defines sxe_cv_cc_ffi_path
	## assumes to be called in a (shell) for-loop
	pushdef([_FFI_PATH], [$1])

	sxe_cv_cc_ffi_path=${sxe_cv_cc_ffi_path:-[]_FFI_PATH[]}
	AC_MSG_CHECKING([for ffi headers in])
	if test -n "${sxe_cv_cc_ffi_path}" -a \
		-d "${sxe_cv_cc_ffi_path}"; then
		SXE_CANONICALISE_PATH([sxe_cv_cc_ffi_path])

		AC_MSG_RESULT([${sxe_cv_cc_ffi_path}])

		FFI_CPPFLAGS="-I${sxe_cv_cc_ffi_path}"
		SXE_DUMP_LIBS
		CPPFLAGS="$CPPFLAGS $FFI_CPPFLAGS"
		SXE_CHECK_HEADERS([ffi.h])
		SXE_RESTORE_LIBS
	else
		AC_MSG_RESULT([non-existing directory ${sxe_cv_cc_ffi_path}])
	fi

	if test "$ac_cv_header_ffi_h" != "yes"; then
		unset ac_cv_header_ffi_h
		unset sxe_cv_cc_ffi_path
		FFI_CPPFLAGS=
	else
		break
	fi

	popdef([_FFI_PATH])
])dnl _SXE_FFI_TRY_GCC_PREFIX

AC_DEFUN([SXE_FFI_TRY_GCC_PREFIX], [dnl
	## arg1 what to do in case of success
	## arg2 what to do in case of failure
	## maybe defines FFI_CPPFLAGS
	## defines sxe_cv_feat_ffi
	## assumes CC points to a gcc

	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	for i in \
		pushdef([libffi_a_fn], [$($CC -print-file-name=libffi.a)])dnl
		pushdef([libgcc_a_fn], [$($CC -print-libgcc-file-name)])dnl
		pushdef([cc_version], [$($CC -dumpversion)])dnl
		"$(dirname []libffi_a_fn[])/include/libffi" \
		"$(dirname []libffi_a_fn[])/../include" \
		"$(dirname []libffi_a_fn[])/../[]cc_version[]/include" \
		"$(dirname []libffi_a_fn[])/../../[]cc_version[]/include" \
		"$(dirname []libgcc_a_fn[])/include/libffi" \
		"$(dirname []libgcc_a_fn[])/../include" \
		"$(dirname []libgcc_a_fn[])/../[]cc_version[]/include" \
		"$(dirname []libgcc_a_fn[])/../../[]cc_version[]/include"
		popdef([cc_version])dnl
		popdef([libgcc_a_fn])dnl
		popdef([libffi_a_fn])dnl
	do
		_SXE_FFI_TRY_GCC_PREFIX([$i])
	done

	if test "$ac_cv_header_ffi_h" != "yes"; then
		sxe_cv_feat_ffi="no"
		MM_FAIL
	else
		sxe_cv_feat_ffi="yes"
		MM_SUCC
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_FFI_TRY_GCC_PREFIX

AC_DEFUN([SXE_CHECK_FFI], [dnl
	## defines sxe_cv_feat_ffi (by side effect)
	## defines have_ffi

	SXE_FFI_TRY_PKGCONFIG([ffi_from_pkgconfig=yes], [:])
	if test "$ffi_from_pkgconfig" != "yes"; then
		SXE_FFI_TRY_STANDARD_PREFIX([:], [unset ac_cv_header_ffi_h])
		if test "$sxe_cv_feat_ffi" = "no" -a \
			"$GCC" = "yes"; then
			SXE_FFI_TRY_GCC_PREFIX([:], [dnl
				AC_MSG_WARN([GCC FFI not found.])])
		fi

		AC_CHECK_LIB([ffi], [ffi_call], [:], [dnl
			AC_MSG_WARN([Cannot find a usable libffi, not compiling FFI support])])
		if test "$ac_cv_header_ffi_h" = "yes" -a \
			"$ac_cv_lib_ffi_ffi_call" = "yes"; then
			sxe_cv_feat_ffi="yes"
			have_ffi="yes"
			FFI_LIBS=-lffi
		else
			sxe_cv_feat_ffi="no"
			have_ffi="no"
		fi
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
