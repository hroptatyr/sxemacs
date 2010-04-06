dnl sxemacs.m4 --- macros to detect ourselves
dnl
dnl Copyright (C) 2007 Sebastian Freundt <hroptatyr@sxemacs.org>
dnl
dnl This set of macros are useful for module developers since they
dnl want to check for (a valid version of) SXEmacs.
dnl As long as we do not provide pkgconfig based magics we stick
dnl with this one.
dnl
dnl This file is part of SXEmacs.

AC_DEFUN([AC_CHECK_SXEMACS], [dnl
	## arg #1: minimum version
	## call it like: AC_CHECK_SXEMACS([22.1.7])
	pushdef([REQUIRED_VERSION], ifelse($1,[],[22.1.7],$1))

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	AC_CACHE_CHECK([for sxemacs (>= ]REQUIRED_VERSION[)],
		[sxe_cv_feat_sxemacs], [dnl
		_AC_CHECK_SXEMACS(REQUIRED_VERSION)])

	if test "$sxe_cv_feat_sxemacs" = "yes"; then
		_AC_CHECK_SXEMACS_VERSION
		_AC_CHECK_SXEMACS_CPPFLAGS
		moduledir=$(${PKG_CONFIG} --variable="moduledir" sxemacs)
		sitemoduledir=$(${PKG_CONFIG} --variable="sitemoduledir" sxemacs)
		SXE_CPPFLAGS=${sxe_cv_cppflags}
	fi

	AC_SUBST([moduledir])
	AC_SUBST([sitemoduledir])
	AC_SUBST([SXE_CPPFLAGS])
	popdef([REQUIRED_VERSION])
])dnl AC_CHECK_SXEMACS

AC_DEFUN([_AC_CHECK_SXEMACS], [dnl
	pushdef([REQUIRED_VERSION], [$1])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	if test "$sxe_cv_prog_pkg_config" = "no" -o -z "$PKG_CONFIG"; then
		AS_MESSAGE([*** pkg-config not found.])
		AS_MESSAGE([*** Cannot check for sxemacs.])
		sxe_cv_feat_sxemacs=no
	elif $($PKG_CONFIG --atleast-version=[]REQUIRED_VERSION[] sxemacs); then
		sxe_cv_feat_sxemacs=yes
	else
		sxe_cv_feat_sxemacs=no
	fi

	popdef([REQUIRED_VERSION])
])dnl _AC_CHECK_SXEMACS

AC_DEFUN([_AC_CHECK_SXEMACS_VERSION], [dnl
	AC_CACHE_CHECK([for sxemacs version], [sxe_cv_version], [dnl
		sxe_cv_version=$($PKG_CONFIG --modversion sxemacs)])
])dnl _AC_CHECK_SXEMACS_VERSION

AC_DEFUN([_AC_CHECK_SXEMACS_CPPFLAGS], [dnl
	AC_CACHE_CHECK([for sxemacs includes], [sxe_cv_cppflags], [dnl
		sxe_cv_cppflags=$($PKG_CONFIG --cflags sxemacs)])
])dnl _AC_CHECK_SXEMACS_CPPFLAGS

dnl We only install sxemacs.m4, so any macro we use is
dnl supposed to be defined here
dnl Anyway, we install sxe-pkgconfig.m4 and just define a dummy here
m4_ifndef([SXE_CHECK_PKGCONFIG], [
	AC_DEFUN([SXE_CHECK_PKGCONFIG], [dnl
		AC_MSG_ERROR([Install `m4/sxe-pkgconfig.m4' from the SXEmacs distribution.])
	])dnl SXE_CHECK_PKGCONFIG
])

dnl sxemacs.m4 ends here
