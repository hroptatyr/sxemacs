dnl sxe-maths.m4 -- Arithmetics and maths functions

AC_DEFUN([_AC_MATH_ASSIGN_IFELSE], [
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
#include <limits.h>
]], [[$1 __test_assign=$2]])], [$3], [$4])
])dnl _AC_MATH_DOUBLE_OP

AC_DEFUN([AC_MATH_DBL_MAX], [
        AC_MSG_CHECKING([for DBL_MAX])
	_AC_MATH_ASSIGN_IFELSE([double], [DBL_MAX], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_DBL_MAX], [1],
			[Whether DBL_MAX is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_DBL_MAX

AC_DEFUN([AC_MATH_DBL_MIN], [
        AC_MSG_CHECKING([for DBL_MIN])
	_AC_MATH_ASSIGN_IFELSE([double], [DBL_MIN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_DBL_MIN], [1],
			[Whether DBL_MIN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_DBL_MIN

AC_DEFUN([AC_MATH_LDBL_MAX], [
        AC_MSG_CHECKING([for LDBL_MAX])
	_AC_MATH_ASSIGN_IFELSE([long double], [LDBL_MAX], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_LDBL_MAX], [1],
			[Whether LDBL_MAX is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_LDBL_MAX

AC_DEFUN([AC_MATH_LDBL_MIN], [
        AC_MSG_CHECKING([for LDBL_MIN])
	_AC_MATH_ASSIGN_IFELSE([long double], [LDBL_MIN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_LDBL_MIN], [1],
			[Whether LDBL_MIN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_LDBL_MIN

AC_DEFUN([AC_MATH_INFINITY], [
        AC_MSG_CHECKING([for INFINITY])
	_AC_MATH_ASSIGN_IFELSE([float], [INFINITY], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_INFINITY], [1],
			[Whether INFINITY is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_INFINITY

AC_DEFUN([AC_MATH_FPCLASSIFY], [
        AC_MSG_CHECKING([for fpclassify])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; fpclassify(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_FPCLASSIFY], [1],
			[Whether isinf() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISINF

AC_DEFUN([AC_MATH_ISINF], [
        AC_MSG_CHECKING([for isinf])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; isinf(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_ISINF], [1],
			[Whether isinf() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISINF

AC_DEFUN([AC_MATH_NAN], [
        AC_MSG_CHECKING([for NAN])
	_AC_MATH_ASSIGN_IFELSE([float], [NAN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_NAN], [1],
			[Whether NAN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_NAN

AC_DEFUN([AC_MATH_ISNAN], [
        AC_MSG_CHECKING([for isnan])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; isnan(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_ISNAN], [1],
			[Whether isnan() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISNAN

dnl
dnl all the ENT stuff
dnl =================
AC_DEFUN([SXE_CHECK_ENT_MSG], [dnl
	AS_MESSAGE([checking for Enhanced Number Types])
])dnl SXE_CHECK_ENT_MSG

AC_DEFUN([SXE_CHECK_ENT], [dnl
	AC_REQUIRE([SXE_CHECK_ENT_MSG])

	SXE_CHECK_GMP
	SXE_CHECK_BSDMP
	SXE_CHECK_MPFR
	SXE_CHECK_MPC
	SXE_CHECK_ECM
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
	AC_CHECK_LIB([mpfr], [mpfr_init])
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

	if test "$ac_cv_header_mpc_h" = "yes" -a \
		"$ac_cv_lib_mpc_mpc_init" = "yes" -a \
		"$mpc_doth_need_mpfr" = "yes"; then
		sxe_cv_feat_mpc="yes"
		MPC_LIBS="-lmpfr -lmpc"
	elif test "$ac_cv_header_mpc_h" = "yes" -a \
		"$ac_cv_lib_mpc_mpc_init" = "yes" -a \
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
		mpc_doth_need_mpfr="yes"
	else
		## try without mpfr.h, but this is definitely going to fail
		## unless you're a developer of mpc ...
		## ... and in that case: Fix teh MPC build chain, Andreas!!!
		AC_CHECK_LIB([mpc], [mpc_init], [:])
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
	AC_CHECK_FUNC(strtod,
		[have_strtod=yes AC_DEFINE([HAVE_STRTOD], [1], [Description here!])],
		[have_strtod=no])
	AC_CHECK_FUNC(strtold,
		[have_strtold=yes AC_DEFINE([HAVE_STRTOLD], [1], [Description here!])],
		[have_strtold=no])
	AC_MATH_FPCLASSIFY([AC_DEFINE([HAVE_FPCLASSIFY], [1], [Description here!])])
	AC_MATH_INFINITY([AC_DEFINE([HAVE_INFINITY], [1], [Description here!])])
	AC_MATH_ISINF([AC_DEFINE([HAVE_ISINF], [1], [Description here!])])
	AC_MATH_NAN([AC_DEFINE([HAVE_NAN], [1], [Description here!])])
	AC_MATH_ISNAN([AC_DEFINE([HAVE_ISNAN], [1], [Description here!])])

	floatops_clean_p="no"
	AC_MSG_CHECKING([for clean float operations])

	AC_DEFINE_UNQUOTED([fpfloat_cand], [$fpfloat], [Temporary.])
	AC_DEFINE_UNQUOTED([fpfloat_cand_double_p],
		[${fpfloat_double_p-0}], [Temporary.])
	AC_DEFINE_UNQUOTED([fpfloat_cand_long_double_p],
		[${fpfloat_long_double_p-0}], [Temporary.])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
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
			floatops_clean_p="yes"
		else
			ACTION_IF_FALSE
			:
		fi],
		[ACTION_IF_FALSE], [ACTION_IF_FALSE])

	AC_MSG_RESULT([$floatops_clean_p])

	popdef([ACTION_IF_TRUE])
	popdef([ACTION_IF_FALSE])
])dnl SXE_CHECK_CLEAN_FLOATOPS


dnl sxe-maths.m4 ends here
