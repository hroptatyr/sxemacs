dnl pkgconfig.m4 --- pkgconfig helpers
dnl
dnl Copyright (C) 2008 Sebastian Freundt
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
dnl
dnl
dnl Commentary:
dnl Be sure to use solely basic stuff that ships with autoconf.
dnl This file must sort of work stand-alone.


AC_DEFUN([SXE_CHECK_PKGCONFIG], [dnl
	AC_CACHE_CHECK([for pkg-config], [sxe_cv_feat_pkgconfig], [
		AC_MSG_RESULT([answer deferred])
		AC_CHECK_PROG([have_pkg_config], [pkg-config], [yes], [no])
		AC_PATH_PROG([PKG_CONFIG], [pkg-config], [echo])
		if test "$have_pkg_config" = "yes" -a -n "$PKG_CONFIG"; then
			sxe_cv_feat_pkgconfig="yes"
		else
			AS_MESSAGE([
*** pkg-config not found.
*** See http://pkgconfig.sourceforge.net
*** Cannot check for various goodies, YOUR fault!])
			have_pkg_config="no"
			PKG_CONFIG="false"
			sxe_cv_feat_pkgconfig="no"
		fi
		AC_MSG_CHECKING([for pkg-config for real this time])
		])
])dnl SXE_CHECK_PKGCONFIG

AC_DEFUN([SXE_PC_CHECK_VERSION_ATLEAST], [dnl
	## arg1 is the module
	## arg2 is the version we want to see
	## defines sxe_cv_pc_<$1>_version and
	## sxe_cv_pc_<$1>_recent_enough_p
	pushdef([mod], [$1])
	pushdef([ver], [$2])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([vervar], [sxe_cv_pc_]shmod[_version])
	pushdef([verrecp], [sxe_cv_pc_]shmod[_recent_enough_p])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([whether ]mod[ is at least ]ver)
	if $PKG_CONFIG --atleast-version []ver[] []mod[]; then
		verrecp[]="yes"
	else
		verrecp[]="no"
	fi
	SXE_MSG_RESULT([$]verrecp[])

	SXE_MSG_CHECKING([for ]mod['s actual version])
	vervar[]=$($PKG_CONFIG --modversion []mod[])
	MOD[_VERSION]=[$]vervar[]
	SXE_MSG_RESULT([$]vervar[])

	AC_SUBST(MOD[_VERSION])

	popdef([verrecp])
	popdef([vervar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
	popdef([ver])
])dnl SXE_PC_CHECK_VERSION

AC_DEFUN([SXE_PC_CHECK_VERSION_ATMOST], [dnl
	## arg1 is the module
	## arg2 is the version we want to see
	## defines sxe_cv_pc_<$1>_version and
	## sxe_cv_pc_<$1>_recent_enough_p
	pushdef([mod], [$1])
	pushdef([ver], [$2])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([vervar], [sxe_cv_pc_]shmod[_version])
	pushdef([veroldp], [sxe_cv_pc_]shmod[_old_enough_p])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([whether ]mod[ is at most ]ver)
	if test "$PKG_CONFIG --max-version []ver[] []mod[]"; then
		veroldp[]="yes"
	else
		veroldp[]="no"
	fi
	SXE_MSG_RESULT([$]veroldp[])

	SXE_MSG_CHECKING([for ]mod['s actual version])
	vervar[]=$($PKG_CONFIG --modversion []mod[])
	MOD[_VERSION]=[$]vervar[]
	SXE_MSG_RESULT([$]vervar[])

	AC_SUBST(MOD[_VERSION])

	popdef([verrecp])
	popdef([vervar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
	popdef([ver])
])dnl SXE_PC_CHECK_VERSION

AC_DEFUN([SXE_PC_CHECK_VERSION], [dnl
	## arg1 is the module
	## defines sxe_cv_pc_<$1>_version
	pushdef([mod], [$1])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([vervar], [sxe_cv_pc_]shmod[_version])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([for ]mod[ version])
	vervar[]=$($PKG_CONFIG --modversion []mod[])
	MOD[_VERSION]=[$]vervar[]
	SXE_MSG_RESULT([$]vervar[])

	AC_SUBST(MOD[_VERSION])

	popdef([vervar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
])dnl SXE_PC_CHECK_VERSION

AC_DEFUN([SXE_PC_CHECK_LIBS], [dnl
	## arg1 is the module
	## defines sxe_cv_pc_<$1>_libs
	pushdef([mod], [$1])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([libvar], [sxe_cv_pc_]shmod[_libs])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([for ]mod[ libraries])
	libvar[]=$($PKG_CONFIG --libs-only-l []mod[])
	MOD[_LIBS]=[$]libvar[]
	SXE_MSG_RESULT([$]libvar[])

	AC_SUBST(MOD[_LIBS])

	popdef([libvar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
])dnl SXE_PC_CHECK_VERSION

AC_DEFUN([SXE_PC_CHECK_LDFLAGS], [dnl
	## arg1 is the module
	## defines sxe_cv_pc_<$1>_ldflags
	pushdef([mod], [$1])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([ldfvar], [sxe_cv_pc_]shmod[_ldflags])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([for ]mod[ ldflags])
	ldfvar[]=$($PKG_CONFIG --libs-only-L []mod[])
	MOD[_LDFLAGS]=[$]ldfvar[]
	SXE_MSG_RESULT([$]ldfvar[])

	AC_SUBST(MOD[_LDFLAGS])

	popdef([ldfvar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
])dnl SXE_PC_CHECK_VERSION

AC_DEFUN([SXE_PC_CHECK_CPPFLAGS], [dnl
	## arg1 is the module
	## defines sxe_cv_pc_<$1>_cppflags
	pushdef([mod], [$1])
	pushdef([shmod], translit([mod], [+-.], [___]))
	pushdef([MOD], translit([shmod], [a-z], [A-Z]))
	pushdef([cfvar], [sxe_cv_pc_]shmod[_cppflags])

	AC_REQUIRE([SXE_CHECK_PKGCONFIG])
	SXE_MSG_CHECKING([for ]mod[ cppflags])
	cfvar[]=$($PKG_CONFIG --cflags []mod[])
	MOD[_CPPFLAGS]=[$]cfvar[]
	SXE_MSG_RESULT([$]cfvar[])

	AC_SUBST(MOD[_CPPFLAGS])

	popdef([cfvar])
	popdef([shmod])
	popdef([MOD])
	popdef([mod])
])dnl SXE_PC_CHECK_VERSION


dnl sxe-pkgconfig.m4 ends here
