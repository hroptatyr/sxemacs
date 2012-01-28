dnl sxe-maths.m4 -- Arithmetics and maths functions
dnl
dnl Copyright (C) 2005 - 2008 Sebastian Freundt
dnl Copyright (c) 2005 - 2008 Nelson Ferreira
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

AC_DEFUN([_SXE_MATH_ASSIGN_IFELSE], [
	dnl call it like so:
	dnl _SXE_MATH_ASSIGN_IFELSE(<type>, <value>, <do-if>, <do-if-not>)
	pushdef([type], [$1])
	pushdef([value], [$2])

	## for math.h and limits.h
	AC_REQUIRE([SXE_CHECK_MATHS_HEADERS])
	AC_REQUIRE([SXE_CHECK_MATHS_TYPES])

	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_MATH_H
# include <math.h>
#endif
#if defined HAVE_VALUES_H
# include <values.h>
#endif
#if defined HAVE_LIMITS_H
# include <limits.h>
#endif
		]], [[]type[ __attribute__((unused)) __test_assign = ]value[];
		])], [$3], [$4])

	popdef([value])
	popdef([type])
])dnl _SXE_MATH_ASSIGN_IFELSE

AC_DEFUN([SXE_CHECK_MATHS_HEADERS], [dnl
	## just check for the bugger, no need to introduce new dependencies
	SXE_CHECK_HEADERS([cfloat.h float.h inttypes.h limits.h math.h ieeefp.h])
	SXE_CHECK_HEADERS([values.h])
	SXE_CHECK_HEADERS([monetary.h])
])dnl SXE_CHECK_MATHS_HEADERS


AC_DEFUN([SXE_CHECK_MATHS_TYPES], [dnl
	AC_CHECK_TYPES([float])
	AC_CHECK_TYPES([float_t])
	AC_CHECK_TYPES([double])
	AC_CHECK_TYPES([double_t])
])dnl SXE_CHECK_MATHS_TYPES

AC_DEFUN([SXE_CHECK_MATHS_SYM], [dnl
	dnl call it like
	dnl SXE_CHECK_MATHS_SYM(<typeof_symbol>, <symbol>, [<if>, <if-not>])
	## defines HAVE_MATHS_<symbol> and
	## TYPEOF_MATHS_<symbol> (always defined) and
	## sxe_cv_maths_<symbol> as well as
	## have_maths_<symbol> (AC_SUBSTituted), and finally
	## sxe_cv_maths_typeof_<symbol> (also AC_SUBSTituted)
	pushdef([typesym], [$1])
	pushdef([sym], [$2])
	pushdef([do_if], [$3])
	pushdef([do_if_not], [$4])

	AC_REQUIRE([SXE_CHECK_MATHS_HEADERS])
	AC_REQUIRE([SXE_CHECK_MATHS_TYPES])

	SXE_MSG_CHECKING([for ]sym[])
	_SXE_MATH_ASSIGN_IFELSE([]typesym[], []sym[], [dnl yes case
		sxe_cv_maths_[]sym[]="yes"
		AC_DEFINE([HAVE_MATHS_]sym[], [1],
			[Whether ]sym[ is defined in math.h])
		AC_DEFINE([TYPEOF_MATHS_]sym[], []typesym[],
			[The type that seems to suit ]sym[])
		sxe_cv_maths_typeof_[]sym[]="[]typesym[]"
		## do as the user pleases
		do_if
		], [dnl no case
		sxe_cv_maths_[]sym[]="no"
		AC_DEFINE([TYPEOF_MATHS_]sym[], [void],
			[The type that seems to suit ]sym[])
		sxe_cv_maths_typeof_[]sym[]="void"
		## do as the user pleases
		do_if_not
		])
	SXE_MSG_RESULT([$sxe_cv_maths_]sym[])
	have_maths_[]sym[]="$sxe_cv_maths_[]sym[]"

	AC_SUBST([have_maths_]sym[])
	AC_SUBST([sxe_cv_maths_typeof_]sym[])

	popdef([do_if])
	popdef([do_if_not])
	popdef([sym])
	popdef([typesym])
])dnl SXE_CHECK_MATHS_SYM


AC_DEFUN([SXE_MATHS_SHRT_MAX], [
	SXE_CHECK_MATHS_SYM([short int], [SHRT_MAX], [$1], [$2])
])dnl SXE_MATHS_SHRT_MAX

AC_DEFUN([SXE_MATHS_SHRT_MIN], [
	SXE_CHECK_MATHS_SYM([short int], [SHRT_MIN], [$1], [$2])
])dnl SXE_MATHS_SHRT_MIN

AC_DEFUN([SXE_MATHS_MAXSHORT], [
	SXE_CHECK_MATHS_SYM([short int], [MAXSHORT], [$1], [$2])
])dnl SXE_MATHS_MAXSHORT

AC_DEFUN([SXE_MATHS_MINSHORT], [
	SXE_CHECK_MATHS_SYM([short int], [MINSHORT], [$1], [$2])
])dnl SXE_MATHS_MINSHORT


AC_DEFUN([SXE_MATHS_INT_MAX], [
	SXE_CHECK_MATHS_SYM([int], [INT_MAX], [$1], [$2])
])dnl SXE_MATHS_INT_MAX

AC_DEFUN([SXE_MATHS_INT_MIN], [
	SXE_CHECK_MATHS_SYM([int], [INT_MIN], [$1], [$2])
])dnl SXE_MATHS_INT_MIN

AC_DEFUN([SXE_MATHS_MAXINT], [
	SXE_CHECK_MATHS_SYM([int], [MAXINT], [$1], [$2])
])dnl SXE_MATHS_MAXINT

AC_DEFUN([SXE_MATHS_MININT], [
	SXE_CHECK_MATHS_SYM([int], [MININT], [$1], [$2])
])dnl SXE_MATHS_MININT


AC_DEFUN([SXE_MATHS_LONG_MAX], [
	SXE_CHECK_MATHS_SYM([long int], [LONG_MAX], [$1], [$2])
])dnl SXE_MATHS_LONG_MAX

AC_DEFUN([SXE_MATHS_LONG_MIN], [
	SXE_CHECK_MATHS_SYM([long int], [LONG_MIN], [$1], [$2])
])dnl SXE_MATHS_LONG_MIN

AC_DEFUN([SXE_MATHS_MAXLONG], [
	SXE_CHECK_MATHS_SYM([long int], [MAXLONG], [$1], [$2])
])dnl SXE_MATHS_MAXLONG

AC_DEFUN([SXE_MATHS_MINLONG], [
	SXE_CHECK_MATHS_SYM([long int], [MINLONG], [$1], [$2])
])dnl SXE_MATHS_MINLONG


AC_DEFUN([SXE_MATHS_FLT_MAX], [
	SXE_CHECK_MATHS_SYM([float], [FLT_MAX], [$1], [$2])
])dnl SXE_MATHS_FLT_MAX

AC_DEFUN([SXE_MATHS_FLT_MIN], [
	SXE_CHECK_MATHS_SYM([float], [FLT_MIN], [$1], [$2])
])dnl SXE_MATHS_FLT_MIN

AC_DEFUN([SXE_MATHS_MAXFLOAT], [
	SXE_CHECK_MATHS_SYM([float], [MAXFLOAT], [$1], [$2])
])dnl SXE_MATHS_MAXFLOAT

AC_DEFUN([SXE_MATHS_MINFLOAT], [
	SXE_CHECK_MATHS_SYM([float], [MINFLOAT], [$1], [$2])
])dnl SXE_MATHS_MINFLOAT

AC_DEFUN([SXE_MATHS_FLT_MAX_EXP], [
	SXE_CHECK_MATHS_SYM([float], [FLT_MAX_EXP], [$1], [$2])
])dnl SXE_MATHS_FLT_MAX_EXP

AC_DEFUN([SXE_MATHS_FLT_MIN_EXP], [
	SXE_CHECK_MATHS_SYM([float], [FLT_MIN_EXP], [$1], [$2])
])dnl SXE_MATHS_FLT_MIN_EXP

AC_DEFUN([SXE_MATHS_FMAXEXP], [
	SXE_CHECK_MATHS_SYM([float], [FMAXEXP], [$1], [$2])
])dnl SXE_MATHS_FMAXEXP

AC_DEFUN([SXE_MATHS_FMINEXP], [
	SXE_CHECK_MATHS_SYM([float], [FMINEXP], [$1], [$2])
])dnl SXE_MATHS_FMINEXP


AC_DEFUN([SXE_MATHS_DBL_MAX], [
	SXE_CHECK_MATHS_SYM([double], [DBL_MAX], [$1], [$2])
])dnl SXE_MATHS_DBL_MAX

AC_DEFUN([SXE_MATHS_DBL_MIN], [
	SXE_CHECK_MATHS_SYM([double], [DBL_MIN], [$1], [$2])
])dnl SXE_MATHS_DBL_MIN

AC_DEFUN([SXE_MATHS_MAXDOUBLE], [
	SXE_CHECK_MATHS_SYM([double], [MAXDOUBLE], [$1], [$2])
])dnl SXE_MATHS_MAXDOUBLE

AC_DEFUN([SXE_MATHS_MINDOUBLE], [
	SXE_CHECK_MATHS_SYM([double], [MINDOUBLE], [$1], [$2])
])dnl SXE_MATHS_MINDOUBLE

AC_DEFUN([SXE_MATHS_DBL_MAX_EXP], [
	SXE_CHECK_MATHS_SYM([double], [DBL_MAX_EXP], [$1], [$2])
])dnl SXE_MATHS_DBL_MAX_EXP

AC_DEFUN([SXE_MATHS_DBL_MIN_EXP], [
	SXE_CHECK_MATHS_SYM([double], [DBL_MIN_EXP], [$1], [$2])
])dnl SXE_MATHS_DBL_MIN_EXP

AC_DEFUN([SXE_MATHS_DMAXEXP], [
	SXE_CHECK_MATHS_SYM([double], [DMAXEXP], [$1], [$2])
])dnl SXE_MATHS_DMAXEXP

AC_DEFUN([SXE_MATHS_DMINEXP], [
	SXE_CHECK_MATHS_SYM([double], [DMINEXP], [$1], [$2])
])dnl SXE_MATHS_DMINEXP


AC_DEFUN([SXE_MATHS_LDBL_MAX], [
	SXE_CHECK_MATHS_SYM([long double], [LDBL_MAX], [$1], [$2])
])dnl SXE_MATHS_LDBL_MAX

AC_DEFUN([SXE_MATHS_LDBL_MIN], [
	SXE_CHECK_MATHS_SYM([long double], [LDBL_MIN], [$1], [$2])
])dnl SXE_MATHS_LDBL_MIN


AC_DEFUN([SXE_MATHS_INFINITY], [
	SXE_CHECK_MATHS_SYM([float_t], [INFINITY], [$1], [$2])
])dnl SXE_MATHS_INFINITY

AC_DEFUN([SXE_MATHS_NAN], [
	SXE_CHECK_MATHS_SYM([float_t], [NAN], [$1], [$2])
])dnl SXE_MATHS_NAN


dnl compound thing
AC_DEFUN([SXE_CHECK_MATHS_VALUES], [dnl
	SXE_MATHS_SHRT_MAX
	SXE_MATHS_SHRT_MIN
	SXE_MATHS_MAXSHORT
	SXE_MATHS_MINSHORT

	SXE_MATHS_INT_MAX
	SXE_MATHS_INT_MIN
	SXE_MATHS_MAXINT
	SXE_MATHS_MININT

	SXE_MATHS_LONG_MAX
	SXE_MATHS_LONG_MIN
	SXE_MATHS_MAXLONG
	SXE_MATHS_MINLONG

	SXE_MATHS_FLT_MAX
	SXE_MATHS_FLT_MIN
	SXE_MATHS_MAXFLOAT
	SXE_MATHS_MINFLOAT
	SXE_MATHS_FLT_MAX_EXP
	SXE_MATHS_FLT_MIN_EXP
	SXE_MATHS_FMAXEXP
	SXE_MATHS_FMINEXP

	SXE_MATHS_DBL_MAX
	SXE_MATHS_DBL_MIN
	SXE_MATHS_MAXDOUBLE
	SXE_MATHS_MINDOUBLE
	SXE_MATHS_DBL_MAX_EXP
	SXE_MATHS_DBL_MIN_EXP
	SXE_MATHS_DMAXEXP
	SXE_MATHS_DMINEXP

	SXE_MATHS_LDBL_MAX
	SXE_MATHS_LDBL_MIN

	SXE_MATHS_INFINITY
	SXE_MATHS_NAN
])dnl SXE_CHECK_MATHS_VALUES


AC_DEFUN([SXE_MATHS_FINITE], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([finite])

	SXE_MSG_CHECKING([for working finite])
	_SXE_MATH_ASSIGN_IFELSE([float], [0.0; finit(__test_assign);],
		[sxe_cv_maths_finite="yes"], [sxe_cv_maths_finite="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_fpclassify])

	if test "$sxe_cv_maths_finite" = "yes"; then
		AC_DEFINE_UNQUOTED([HAVE_MATHS_FINITE], [1],
			[Whether finite() is defined])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATHS_FINITE

AC_DEFUN([SXE_MATHS_FPCLASS], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([fpclass])

	SXE_MSG_CHECKING([for working fpclass])
	_SXE_MATH_ASSIGN_IFELSE([float], [0.0; fpclass(__test_assign);],
		[sxe_cv_maths_fpclass="yes"], [sxe_cv_maths_fpclass="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_fpclass])

	if test "$sxe_cv_maths_fpclass" = "yes"; then
		AC_DEFINE_UNQUOTED([HAVE_MATHS_FPCLASS], [1],
			[Whether fpclass() is defined])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATHS_FPCLASS

AC_DEFUN([SXE_MATHS_FPCLASSIFY], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([fpclassify __fpclassifyf __fpclassify __fpclassifyl])

	SXE_MSG_CHECKING([for working fpclassify])
	_SXE_MATH_ASSIGN_IFELSE([float], [0.0; fpclassify(__test_assign);],
		[sxe_cv_maths_fpclassify="yes"], [sxe_cv_maths_fpclassify="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_fpclassify])

	if test "$sxe_cv_maths_fpclassify" = "yes"; then
		AC_DEFINE_UNQUOTED([HAVE_MATHS_FPCLASSIFY], [1],
			[Whether fpclassify() is defined in math.h])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATHS_FPCLASSIFY

AC_DEFUN([SXE_MATHS_ISINF], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([isinf __isinff __isinf __isinfl])

	SXE_MSG_CHECKING([for working isinf])
	_SXE_MATH_ASSIGN_IFELSE([float], [0.0; isinf(__test_assign);],
		[sxe_cv_maths_isinf="yes"], [sxe_cv_maths_isinf="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_isinf])

	if test "$sxe_cv_maths_isinf" = "yes"; then
		AC_DEFINE_UNQUOTED([HAVE_MATHS_ISINF], [1],
			[Whether isinf() is defined in math.h])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATH_ISINF

AC_DEFUN([SXE_MATHS_ISNAN], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([isnan __isnanf __isnan __isnanl])

	SXE_MSG_CHECKING([for working isnan])
	_SXE_MATH_ASSIGN_IFELSE([float], [0.0; isnan(__test_assign);],
		[sxe_cv_maths_isnan="yes"], [sxe_cv_maths_isnan="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_isnan])

	if test "$sxe_cv_maths_isnan" = "yes"; then
		AC_DEFINE_UNQUOTED([HAVE_MATHS_ISNAN], [1],
			[Whether isnan() is defined in math.h])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATH_ISNAN

AC_DEFUN([SXE_MATHS_SIGNBIT], [
	## could be an ordinary function, at least it should be
	AC_CHECK_FUNCS([signbit __signbitf __signbit __signbitl])

	SXE_MSG_CHECKING([for working signbit])
	_SXE_MATH_ASSIGN_IFELSE([float], [-1.0; signbit(__test_assign);],
		[sxe_cv_maths_signbit="yes"], [sxe_cv_maths_signbit="no"])
	SXE_MSG_RESULT([$sxe_cv_maths_signbit])

	if test "$sxe_cv_maths_signbit" = "yes"; then
		AC_DEFINE([HAVE_MATHS_SIGNBIT], [1],
			[Whether signbit() is defined in math.h])
		$1
	else
		:
		$2
	fi
])dnl SXE_MATH_SIGNBIT


dnl
dnl all the ENT stuff
dnl =================
AC_DEFUN([SXE_CHECK_ENT_MSG], [dnl
	AS_MESSAGE([checking for Enhanced Number Types])
])dnl SXE_CHECK_ENT_MSG

AC_DEFUN([SXE_CHECK_ENT], [dnl
	AC_REQUIRE([SXE_CHECK_ENT_MSG])

	SXE_CHECK_GMP
	dnl SXE_CHECK_BSDMP
	SXE_CHECK_MPFR
	SXE_CHECK_MPC
	SXE_CHECK_ECM

	## instead we define the following
	with_ent_mp="no"
	sxe_cv_feat_bsdmp="no"
])dnl SXE_CHECK_ENT

dnl
dnl GMP
dnl ===
AC_DEFUN([SXE_CHECK_GMP], [dnl
	## call like this SXE_CHECK_GMP([<if-found>], [<if-not-found>])
	## defines HAVE_GMP, HAVE_MPZ, HAVE_MPQ, HAVE_MPF
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for GNU mp multi-precision arithmetics support],
		[sxe_cv_feat_gmp], [_SXE_CHECK_GMP])

	if test "$sxe_cv_feat_gmp" = "yes"; then
		AC_DEFINE([HAVE_GMP], [1],
			[Whether mp arithmetics come from GNU MP (GMP)])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_GMP

AC_DEFUN([_SXE_CHECK_GMP], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $ENT_CPPFLAGS"
	LDFLAGS="$CPPFLAGS $ENT_LDFLAGS"
	LIBS="$LIBS $ENT_LIBS"

	AC_REQUIRE([SXE_CHECK_GMP_HEADERS])
	AC_REQUIRE([SXE_CHECK_GMP_LIBS])
	SXE_RESTORE_LIBS

	if test "$ac_cv_header_gmp_h" = "yes" -a \
		"$ac_cv_lib_gmp___gmpz_init" = "yes" -a \
		"$ac_cv_lib_gmp___gmpq_init" = "yes" -a \
		"$ac_cv_lib_gmp___gmpf_init" = "yes"; then
		sxe_cv_feat_gmp="yes"
		AC_DEFINE([HAVE_MPZ], [1], [Description here!])
		AC_DEFINE([HAVE_MPQ], [1], [Description here!])
		AC_DEFINE([HAVE_MPF], [1], [Description here!])
		GMP_LIBS="-lgmp"
	else
		sxe_cv_feat_gmp="no"
	fi
])dnl _SXE_CHECK_GMP

AC_DEFUN([SXE_CHECK_GMP_HEADERS], [dnl
	AC_CHECK_HEADERS([gmp.h])
])dnl SXE_CHECK_GMP_HEADERS

AC_DEFUN([SXE_CHECK_GMP_LIBS], [dnl
	AC_CHECK_LIB([gmp], [__gmpz_init], [:])
	AC_CHECK_LIB([gmp], [__gmpq_init], [:])
	AC_CHECK_LIB([gmp], [__gmpf_init], [:])
])dnl SXE_CHECK_GMP_LIBS

dnl
dnl BSD-MP
dnl ======
AC_DEFUN([SXE_CHECK_BSDMP], [dnl
	## call like this SXE_CHECK_BSDMP([<if-found>], [<if-not-found>])
	## defines HAVE_BSDMP, HAVE_MPZ
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for BSD mp multi-precision arithmetics support],
		[sxe_cv_feat_bsdmp], [_SXE_CHECK_BSDMP])

	if test "$sxe_cv_feat_bsdmp" = "yes"; then
		AC_DEFINE([HAVE_BSDMP], [1],
			[Whether mp-arithmetics come from BSD mp library])
		AC_DEFINE([HAVE_MPZ], [1], [Description here!])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_BSDMP

AC_DEFUN([_SXE_CHECK_BSDMP], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $ENT_CPPFLAGS"
	LDFLAGS="$CPPFLAGS $ENT_LDFLAGS"
	LIBS="$LIBS $ENT_LIBS"

	AC_REQUIRE([SXE_CHECK_BSDMP_HEADERS])
	AC_REQUIRE([SXE_CHECK_BSDMP_LIBS])
	AC_REQUIRE([SXE_CHECK_BSDMP_FUNCS])
	SXE_RESTORE_LIBS

	if test "$ac_cv_header_mp_h" = "yes" -a \
		"$ac_cv_lib_mp_mp_mfree" = "yes"; then
		AC_DEFINE([MP_PREFIX], [1], [Description here!])
		sxe_cv_feat_bsdmp="yes"
		BSDMP_LIBS="-lmp"
	elif test "$ac_cv_header_mp_h" = "yes" -a \
		"$ac_cv_lib_mp_mfree" = "yes"; then
		sxe_cv_feat_bsdmp="yes"
		BSDMP_LIBS="-lmp"
	else
		sxe_cv_feat_bsdmp="no"
	fi
])dnl _SXE_CHECK_BSDMP

AC_DEFUN([SXE_CHECK_BSDMP_HEADERS], [dnl
	AC_CHECK_HEADERS([mp.h])
])dnl SXE_CHECK_BSDMP_HEADERS

AC_DEFUN([SXE_CHECK_BSDMP_LIBS], [dnl
	AC_CHECK_LIB([mp], [mp_mfree], [:])
	AC_CHECK_LIB([mp], [mfree], [:])
	## also try to find these in -lcrypto
	## we don't do this now because of weird interdependencies
	## with openssl support
])dnl SXE_CHECK_BSDMP_LIBS

AC_DEFUN([SXE_CHECK_BSDMP_FUNCS], [dnl
	AC_CHECK_FUNCS([mp_move move])
])dnl SXE_CHECK_ENT_BSDMP


dnl
dnl MPFR
dnl ====
AC_DEFUN([SXE_CHECK_MPFR], [dnl
	## call like this SXE_CHECK_MPFR([<if-found>], [<if-not-found>])
	## defines HAVE_MPFR
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for mpfr real number support],
		[sxe_cv_feat_mpfr], [_SXE_CHECK_MPFR])

	if test "$sxe_cv_feat_mpfr" = "yes"; then
		AC_DEFINE([HAVE_MPFR], [1], [Whether mpfr support is doable])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_MPFR

AC_DEFUN([_SXE_CHECK_MPFR], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $ENT_CPPFLAGS"
	LDFLAGS="$CPPFLAGS $ENT_LDFLAGS"
	LIBS="$LIBS $ENT_LIBS"

	AC_REQUIRE([SXE_CHECK_MPFR_HEADERS])
	AC_REQUIRE([SXE_CHECK_MPFR_LIBS])
	SXE_RESTORE_LIBS

	if test "$ac_cv_header_mpfr_h" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_init" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_equal_p" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_atan2" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_exp10" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_sec" = "yes"; then
		sxe_cv_feat_mpfr="yes"
		MPFR_LIBS="-lmpfr"
	elif test "$ac_cv_header_mpfr_h" = "yes" -a \
		"$ac_cv_lib_mpfr_mpfr_init" = "yes"; then
		AS_MESSAGE([ENT: MPFR is too old. dnl
Consider http://www.mpfr.org and fetch a later one.])
		AS_MESSAGE([ENT: Also note that the MPFR supplied by GMP dnl
cannot be used with SXEmacs.])
		sxe_cv_feat_mpfr="no"
	else
		sxe_cv_feat_mpfr="no"
	fi
])dnl _SXE_CHECK_MPFR

AC_DEFUN([SXE_CHECK_MPFR_HEADERS], [dnl
	AC_CHECK_HEADERS([mpfr.h])
])dnl SXE_CHECK_MPFR_HEADERS

AC_DEFUN([SXE_CHECK_MPFR_LIBS], [dnl
	AC_CHECK_LIB([mpfr], [mpfr_init], [:])
	## do not accept this old MPFR distributed with GMP
	## to do this, we make sure to find the following functions in
	## libmpfr as they are known to be missing in the GMP-supplied
	## version
	AC_CHECK_LIB([mpfr], [mpfr_equal_p], [:])
	## check for trigonometrical and exponential funs
	AC_CHECK_LIB([mpfr], [mpfr_atan2], [:])
	AC_CHECK_LIB([mpfr], [mpfr_sec], [:])
	AC_CHECK_LIB([mpfr], [mpfr_exp10], [:])
])dnl SXE_CHECK_MPFR

dnl
dnl MPC
dnl ===
AC_DEFUN([SXE_CHECK_MPC], [dnl
	## call like this SXE_CHECK_MPC([<if-found>], [<if-not-found>])
	## defines HAVE_MPC
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_REQUIRE([SXE_CHECK_MPFR])
	AC_CACHE_CHECK([for libmpc complex number support],
		[sxe_cv_feat_mpc], [_SXE_CHECK_MPC])

	if test "$sxe_cv_feat_mpc" = "yes"; then
		AC_DEFINE([HAVE_MPC], [1], [Whether mpc support is available])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_MPC

AC_DEFUN([_SXE_CHECK_MPC], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $ENT_CPPFLAGS"
	LDFLAGS="$CPPFLAGS $ENT_LDFLAGS"
	LIBS="$LIBS $ENT_LIBS"

	AC_REQUIRE([SXE_CHECK_MPC_HEADERS])
	AC_REQUIRE([SXE_CHECK_MPC_LIBS])
	SXE_RESTORE_LIBS

	if test "$ac_cv_lib_mpc_mpc_init" = "yes"; then
		AC_DEFINE([HAVE_MPC_INIT], [1], [Whether simple mpc_init is available])
	fi
	if test "$ac_cv_lib_mpc_mpc_set_ui_fr" = "yes"; then
		AC_DEFINE([HAVE_MPC_SET_UI_FR], [1], [Whether simple mpc_set_ui_fr is available])
	fi
	if test "$ac_cv_lib_mpc_mpc_init" = "yes" -o \
		"$ac_cv_lib_mpc_mpc_init2" = "yes"; then
		mpc_can_be_initted="yes"
	fi
	if test "$ac_cv_header_mpc_h" = "yes" -a \
		"$mpc_can_be_initted" = "yes" -a \
		"$mpc_doth_need_mpfr" = "yes"; then
		sxe_cv_feat_mpc="yes"
		MPC_LIBS="-lmpfr -lmpc"
	elif test "$ac_cv_header_mpc_h" = "yes" -a \
		"$mpc_can_be_initted" = "yes" -a \
		"$mpc_doth_need_mpfr" = "no"; then
		sxe_cv_feat_mpc="yes"
		MPC_LIBS="-lmpc"
	else
		sxe_cv_feat_mpc="no"
	fi
])dnl _SXE_CHECK_MPC

AC_DEFUN([SXE_CHECK_MPC_HEADERS], [dnl
	AC_REQUIRE([SXE_CHECK_MPFR_HEADERS])
	if test "$ac_cv_header_mpfr_h" = "yes"; then
		AC_CHECK_HEADERS([mpc.h], [:], [:], [#include <mpfr.h>])
	else
		## try without mpfr.h, but this is definitely going to fail
		## unless you're a developer of mpc ...
		## ... and in that case: Fix teh MPC build chain, Andreas!!!
		AC_CHECK_HEADERS([mpc.h])
	fi
])dnl SXE_CHECK_MPC_HEADERS

AC_DEFUN([SXE_CHECK_MPC_LIBS], [dnl
	AC_REQUIRE([SXE_CHECK_MPFR_LIBS])
	if test "$ac_cv_lib_mpfr_mpfr_init" = "yes"; then
		AC_CHECK_LIB([mpc], [mpc_init], [:], [:], [-lmpfr])
		AC_CHECK_LIB([mpc], [mpc_init2], [:], [:], [-lmpfr])
		AC_CHECK_LIB([mpc], [mpc_set_ui_fr], [:], [:], [-lmpfr])
		mpc_doth_need_mpfr="yes"
	else
		## try without mpfr.h, but this is definitely going to fail
		## unless you're a developer of mpc ...
		## ... and in that case: Fix the MPC build chain, Andreas!!!
		AC_CHECK_LIB([mpc], [mpc_init], [:])
		AC_CHECK_LIB([mpc], [mpc_init2], [:])
		AC_CHECK_LIB([mpc], [mpc_set_ui_fr], [:])
		mpc_doth_need_mpfr="no"
	fi
])dnl SXE_CHECK_MPC_LIBS


AC_DEFUN([SXE_CHECK_ECM], [dnl
	## call like this SXE_CHECK_ECM([<if-found>], [<if-not-found>])
	## defines HAVE_ECM
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_REQUIRE([SXE_CHECK_GMP])
	AC_CACHE_CHECK([for ecm factorisation support],
		[sxe_cv_feat_ecm], [_SXE_CHECK_ECM])

	if test "$sxe_cv_feat_ecm" = "yes"; then
		AC_DEFINE([HAVE_ECM], [1], [Whether ecm support is doable])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_ECM

AC_DEFUN([_SXE_CHECK_ECM], [dnl
	SXE_DUMP_LIBS
	## ecm often needs GMP and is even more often linked statically
	## so we just pump all the ENT stuff into our slots here
	CPPFLAGS="$CPPFLAGS $ENT_CPPFLAGS"
	LDFLAGS="$CPPFLAGS $ENT_LDFLAGS"
	LIBS="$LIBS $ENT_LIBS"

	AC_REQUIRE([SXE_CHECK_ECM_HEADERS])
	AC_REQUIRE([SXE_CHECK_ECM_LIBS])
	SXE_RESTORE_LIBS

	if test "$ac_cv_header_ecm_h" = "yes" -a \
		"$ac_cv_lib_ecm_ecm_init" = "yes" -a \
		"$ecm_doth_need_gmp" = "yes"; then
		sxe_cv_feat_ecm="yes"
		ECM_LIBS="-lgmp -lecm"
	elif test "$ac_cv_header_ecm_h" = "yes" -a \
		"$ac_cv_lib_ecm_ecm_init" = "yes" -a \
		"$ecm_doth_need_gmp" = "no"; then
		sxe_cv_feat_ecm="yes"
		ECM_LIBS="-lecm"
	else
		sxe_cv_feat_ecm="no"
	fi
])dnl _SXE_CHECK_ECM

AC_DEFUN([SXE_CHECK_ECM_HEADERS], [dnl
	AC_REQUIRE([SXE_CHECK_GMP_HEADERS])
	if test "$ac_cv_header_gmp_h" = "yes"; then
		AC_CHECK_HEADERS([ecm.h], [:], [:], [#include <gmp.h>])
		ecm_doth_need_gmp="yes"
	else
		## try without ... oughta work, too
		AC_CHECK_HEADERS([ecm.h])
		ecm_doth_need_gmp="no"
	fi
])dnl SXE_CHECK_ECM_HEADERS

AC_DEFUN([SXE_CHECK_ECM_LIBS], [dnl
	AC_REQUIRE([SXE_CHECK_GMP_LIBS])
	if test "$ac_cv_lib_gmp___gmpz_init" = "yes"; then
		AC_CHECK_LIB([ecm], [ecm_init], [:], [:], [-lgmp])
		ecm_doth_need_gmp="yes"
	else
		## try without ... could work if the user linked a
		## static gmp into libecm.a
		AC_CHECK_LIB([ecm], [ecm_init], [:])
		ecm_doth_need_gmp="no"
	fi
])dnl SXE_CHECK_ECM_LIBS


AC_DEFUN([SXE_CHECK_CLEAN_FLOATOPS], [dnl
	## call like this SXE_CHECK_CLEAN_FLOATOPS([<if-so>], [<if-not>])
	pushdef([ACTION_IF_TRUE], [$1])
	pushdef([ACTION_IF_FALSE], [$2])

	dnl test if floats are clean
	AC_CHECK_FUNCS([strtof])
	AC_CHECK_FUNCS([strtod])
	AC_CHECK_FUNCS([strtold])
	AC_CHECK_FUNCS([strtol])
	AC_CHECK_FUNCS([strtoll])
	AC_CHECK_FUNCS([strtoq])

	SXE_MATHS_FPCLASSIFY
	SXE_MATHS_FPCLASS
	SXE_MATHS_FINITE
	SXE_MATHS_ISINF
	SXE_MATHS_ISNAN
	SXE_MATHS_SIGNBIT

	floatops_clean_p="no"
	SXE_MSG_CHECKING([for clean float operations])

	AC_DEFINE_UNQUOTED([fpfloat_cand], [$fpfloat], [Temporary.])
	AC_DEFINE_UNQUOTED([fpfloat_cand_double_p],
		[${fpfloat_double_p:-0}], [Temporary.])
	AC_DEFINE_UNQUOTED([fpfloat_cand_long_double_p],
		[${fpfloat_long_double_p:-0}], [Temporary.])

	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
#include <values.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef fpfloat_cand fpfloat;
#define fpfloat_double_p fpfloat_cand_double_p
#define fpfloat_long_double_p fpfloat_cand_long_double_p

#include "$sxe_srcdir/src/ent-inf.h"
#include "$sxe_srcdir/src/ent-strflt.h"

int main(int c, char **v)
{
	fpfloat_cand o4;
	fpfloat_cand surprise;

	surprise = str_to_fpfloat("0.0") + (o4 = str_to_fpfloat("1.4"));

	if (surprise == o4)
#if fpfloat_cand_long_double_p
	{
		fpfloat_cand gtdouble_s = str_to_fpfloat("1.0e+513");
		fpfloat_cand gtdouble_l = 1.0e+513L;
		char         buf[1024];

		if ( ENT_FLOAT_INDEFINITE_P(gtdouble_l) )
			return 0;
		*buf=buf[1023]='\0';
		snprintf(buf,1023,"%Lf",gtdouble_s);
		if ( ! strncasecmp("inf",buf,3 ) )
			return 1;
		if ( ! strncasecmp("nan",buf,3 ) )
			return 1;
		snprintf(buf,1023,"%Lf",gtdouble_l);
		if ( ! strncasecmp("inf",buf,3 ) )
			return 1;
		if ( ! strncasecmp("nan",buf,3 ) )
			return 1;
		return 0;
	}
#else
		return 0;
#endif
	else
		return 1;
}
	]])], [
		./conftest dummy_arg; floatop_status=$?;
		if test "$floatop_status" = "0"; then
			ACTION_IF_TRUE
			AC_DEFINE([HAVE_CLEAN_FLOATOPS], [1], [Description here!])
			floatops_clean_p="yes"
		else
			ACTION_IF_FALSE
			:
		fi],
		[ACTION_IF_FALSE], [ACTION_IF_FALSE])

	SXE_MSG_RESULT([$floatops_clean_p])

	popdef([ACTION_IF_TRUE])
	popdef([ACTION_IF_FALSE])
])dnl SXE_CHECK_CLEAN_FLOATOPS


dnl sxe-maths.m4 ends here
