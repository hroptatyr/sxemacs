dnl compress.m4 --- compression libs
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


AC_DEFUN([SXE_CHECK_ZLIB], [dnl
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	AC_CHECK_HEADERS([zlib.h])
	AC_CHECK_LIB([z], [gzopen], [:], [:])

	SXE_MSG_CHECKING([for zlib support])
	if test "$ac_cv_header_zlib_h" = "yes" -a \
		"$ac_cv_lib_z_gzopen" = "yes"; then
		sxe_cv_feat_zlib="yes"
		ZLIB_LIBS="-lz"
		AC_DEFINE([HAVE_ZLIB], [1], [Whether zlib is available])
	else
		sxe_cv_feat_zlib="no"
	fi
	SXE_MSG_RESULT([$sxe_cv_feat_zlib])

	AC_SUBST([ZLIB_CPPFLAGS])
	AC_SUBST([ZLIB_LDFLAGS])
	AC_SUBST([ZLIB_LIBS])

	AM_CONDITIONAL([HAVE_ZLIB], [test "$sxe_cv_feat_zlib" = "yes"])

	AC_LANG_POP([C])
])dnl SXE_CHECK_ZLIB

AC_DEFUN([SXE_CHECK_BZLIB], [dnl
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	AC_CHECK_HEADERS([bzlib.h])
	AC_CHECK_LIB([bz2], [BZ2_bzReadOpen], [:], [:])
	AC_CHECK_LIB([bz2], [BZ2_bzWriteOpen], [:], [:])

	SXE_MSG_CHECKING([for bzlib support])
	if test "$ac_cv_header_bzlib_h" = "yes" -a \
		"$ac_cv_lib_bz2_BZ2_bzReadOpen" = "yes" -a \
		"$ac_cv_lib_bz2_BZ2_bzWriteOpen" = "yes"; then
		sxe_cv_feat_bzlib="yes"
		BZLIB_LIBS="-lbz2"
		AC_DEFINE([HAVE_BZLIB], [1], [Whether bz2 lib is available])
	else
		sxe_cv_feat_bzlib="no"
	fi
	SXE_MSG_RESULT([$sxe_cv_feat_bzlib])

	AC_SUBST([BZLIB_CPPFLAGS])
	AC_SUBST([BZLIB_LDFLAGS])
	AC_SUBST([BZLIB_LIBS])

	AM_CONDITIONAL([HAVE_BZLIB], [test "$sxe_cv_feat_bzlib" = "yes"])

	AC_LANG_POP([C])
])dnl SXE_CHECK_BZLIB

AC_DEFUN([SXE_CHECK_LZMA], [dnl
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	AC_CHECK_HEADERS([lzma.h])

	SXE_PC_CHECK_VERSION_ATLEAST([lzma], [4.999])
	SXE_PC_CHECK_CPPFLAGS([lzma])
	SXE_PC_CHECK_LDFLAGS([lzma])
	SXE_PC_CHECK_LIBS([lzma])

	SXE_MSG_CHECKING([for lzma support])
	if test "$ac_cv_header_lzma_h" = "yes" -a \
		"$sxe_cv_pc_lzma_recent_enough_p" = "yes"; then
		sxe_cv_feat_lzma="yes"
		LZMA_LIBS="-llzma"
		AC_DEFINE([HAVE_LZMA], [1], [Whether lzma lib is available])
	else
		sxe_cv_feat_lzma="no"
	fi
	SXE_MSG_RESULT([$sxe_cv_feat_lzma])

	AC_SUBST([LZMA_CPPFLAGS])
	AC_SUBST([LZMA_LDFLAGS])
	AC_SUBST([LZMA_LIBS])

	AM_CONDITIONAL([HAVE_LZMA], [test "$sxe_cv_feat_lzma" = "yes"])

	AC_LANG_POP([C])
])dnl SXE_CHECK_LZMA


dnl sxe-compress.m4 ends here
