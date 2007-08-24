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


AC_DEFUN([SXE_CHECK_ENT_BSDMP], [dnl
	dnl must be triggered after SXE_CHECK_ENT_GMP
	## check for BSD-MP support
	if test "$have_ent_gmp" = "no" -o \
	     -z "$have_ent_gmp" -o "$with_ent_mp" = "yes"; then
		dnl just for convenience
		for library in "" "-lcrypto"; do
			AC_CHECK_HEADER(mp.h, [
				AC_CHECK_LIB(mp, mp_mfree, have_mp_mfree=yes; break, [
					AC_CHECK_LIB(mp, mfree, have_mfree=yes; break, ,
						$library)], $library)])
		done
		if test "$have_mp_mfree" = "yes"; then
			AC_DEFINE([MP_PREFIX], [1], [Description here!])
			have_ent_mp="yes"
			if test "$library" != ""; then
				SXE_APPEND($library, mp_maybe_libs)
			fi
			AC_CHECK_FUNC(mp_move, [
				AC_DEFINE([HAVE_MP_MOVE], [1], [Description here!])])
		elif test "$have_mfree" = "yes"; then
			have_ent_mp="yes"
			if test "$library" != ""; then
				SXE_APPEND($library, mp_maybe_libs)
			fi
			AC_CHECK_FUNC(move, [
				AC_DEFINE([HAVE_MP_MOVE], [1], [Description here!])])
		else
			have_ent_mp="no"
		fi
	fi
])dnl SXE_CHECK_ENT_BSDMP

AC_DEFUN([SXE_CHECK_ENT_MP], [dnl
	SXE_CHECK_GMP
	SXE_CHECK_ENT_BSDMP

	## evaluate results until here
	if test "$sxe_cv_feat_gmp" = "yes"; then
		have_ent_gmp="yes"
	fi
	if test "$with_ent_gmp" = "yes" -a \
		"$have_ent_gmp" = "yes"; then
		AC_DEFINE([WITH_GMP], [1], [Description here!])
		AC_DEFINE([WITH_MPZ], [1], [Description here!])
		AC_DEFINE([WITH_MPQ], [1], [Description here!])
		AC_DEFINE([WITH_MPF], [1], [Description here!])
		SXE_PREPEND([-lgmp], [ENT_LIBS])
		SXE_ADD_ENT_OBJS([ent-gmp.o])

		## now establish a preference chain:
		## if both gmp and mp are there -> use gmp
		if test "$have_ent_mp" = "yes"; then
			have_ent_mp="veto"
		fi
	fi

	if test "$with_ent_mp" = "yes" && dnl
	   test "$have_ent_mp" = "yes"; then
		AC_DEFINE([WITH_MP], [1], [Description here!])
		AC_DEFINE([HAVE_MPZ], [1], [Description here!])
		SXE_APPEND(-lmp $mp_maybe_libs, ENT_LIBS)
		SXE_ADD_ENT_OBJS([ent-mp.o])
		have_ent_mp=yes
	elif test "$have_ent_mp" = "veto"; then
		AS_MESSAGE([ENT: MP support available, but omitted in favour of GMP.])
		omit_ent_mp_in_favour_of="gmp"
	fi
])dnl SXE_CHECK_ENT_MP

AC_DEFUN([SXE_CHECK_ENT], [dnl
	if test "$with_ent" != "none"; then
		AS_MESSAGE([checking for Enhanced Number Types])
	fi

	SXE_CHECK_ENT_MP
])dnl SXE_CHECK_ENT


dnl
dnl GMP
dnl ===
AC_DEFUN([SXE_CHECK_GMP], [dnl
	## call like this SXE_CHECK_GMP([<if-found>], [<if-not-found>])
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for libgmp rational number support],
		[sxe_cv_feat_gmp], [_SXE_CHECK_GMP])

	if test "$sxe_cv_feat_gmp" = "yes"; then
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
])dnl SXE_CHECK_ENT_GMP

dnl
dnl MPFR
dnl ====
AC_DEFUN([SXE_CHECK_MPFR], [dnl
	## call like this SXE_CHECK_MPFR([<if-found>], [<if-not-found>])
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for mpfr real number support],
		[sxe_cv_feat_mpfr], [_SXE_CHECK_MPFR])

	if test "$sxe_cv_feat_mpfr" = "yes"; then
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
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_REQUIRE([SXE_CHECK_MPFR])
	AC_CACHE_CHECK([for libmpc complex number support],
		[sxe_cv_feat_mpc], [_SXE_CHECK_MPC])

	if test "$sxe_cv_feat_mpc" = "yes"; then
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
		"$ac_cv_lib_mpc_mpc_init" = "yes"; then
		sxe_cv_feat_mpc="yes"
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
	else
		## try without mpfr.h, but this is definitely going to fail
		## unless you're a developer of mpc ...
		## ... and in that case: Fix teh MPC build chain, Andreas!!!
		AC_CHECK_LIB([mpc], [mpc_init], [:])
	fi
])dnl SXE_CHECK_MPC_LIBS


AC_DEFUN([SXE_CHECK_ECM], [dnl
	## call like this SXE_CHECK_ECM([<if-found>], [<if-not-found>])
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_REQUIRE([SXE_CHECK_GMP])
	AC_CACHE_CHECK([for ecm factorisation support],
		[sxe_cv_feat_ecm], [_SXE_CHECK_ECM])

	if test "$sxe_cv_feat_ecm" = "yes"; then
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
		"$ac_cv_lib_ecm_ecm_init" = "yes"; then
		sxe_cv_feat_ecm="yes"
	else
		sxe_cv_feat_ecm="no"
	fi
])dnl _SXE_CHECK_ECM

AC_DEFUN([SXE_CHECK_ECM_HEADERS], [dnl
	AC_REQUIRE([SXE_CHECK_GMP_HEADERS])
	if test "$ac_cv_header_gmp_h" = "yes"; then
		AC_CHECK_HEADERS([ecm.h], [:], [:], [#include <gmp.h>])
	else
		## try without ... oughta work, too
		AC_CHECK_HEADERS([ecm.h])
	fi
])dnl SXE_CHECK_ECM_HEADERS

AC_DEFUN([SXE_CHECK_ECM_LIBS], [dnl
	AC_REQUIRE([SXE_CHECK_GMP_LIBS])
	if test "ac_cv_lib_gmp___gmpz_init" = "yes"; then
		AC_CHECK_LIB([ecm], [ecm_init], [:], [:], [-lgmp])
	else
		## try without ... could work if the user linked a
		## static gmp into libecm.a
		AC_CHECK_LIB([ecm], [ecm_init], [:])
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



dnl SSL detection
dnl =============
dnl since this is also a big chunk of maths we check for it here
AC_DEFUN([SXE_PATH_OPENSSL_BIN], [dnl
	AC_CHECK_PROG([have_openssl_bin], [openssl], [yes], [no])
	AC_PATH_PROG([OPENSSL_BIN], [openssl], [echo])
])dnl SXE_PATH_OPENSSL_BIN

AC_DEFUN([SXE_OPENSSL_VERSION], [dnl
	## assumes SXE_PATH_OPENSSL_BIN has been run already
	AC_MSG_CHECKING([for openssl version])
	if test "$have_openssl_bin" = "yes"; then
		OPENSSL_VERSION=`$OPENSSL_BIN version`
	else
		OPENSSL_VERSION="unknown"
	fi
	AC_MSG_RESULT([$OPENSSL_VERSION])

	AC_MSG_CHECKING([whether OpenSSL version is recent enough])
	## we allow 0.9.7e-?, 0.9.8* and 0.9.9*
	allowed_versions="0.9.7[e-z] 0.9.8* 0.9.9*"
	OPENSSL_SANE_P=no
	for ver in $allowed_versions; do
		echo "$OPENSSL_VERSION" | grep -Gq "$ver" && \
			OPENSSL_SANE_P="yes" && break
	done
	AC_MSG_RESULT([$OPENSSL_SANE_P])
])dnl SXE_OPENSSL_VERSION

AC_DEFUN([SXE_TRY_OPENSSL_HISTORICAL_PREFIX], [dnl
	## ooh, maybe this historical trap to install at /usr/local/ssl
	OPENSSL_CPPFLAGS="-I/usr/local/ssl/include"
	OPENSSL_LDFLAGS="-L/usr/local/ssl/lib"

	## now append these candidates to our c_switch and ld_switch
	SXE_DUMP_LIBS
	SXE_APPEND([$OPENSSL_CPPFLAGS], [CPPFLAGS])
	SXE_APPEND([$OPENSSL_LDFLAGS], [LDFLAGS])

	## check again
	SXE_CHECK_HEADERS([openssl/crypto.h])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])

	## restore
	SXE_RESTORE_LIBS
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto" != "yes yes"; then
		OPENSSL_CPPFLAGS=
		OPENSSL_LDFLAGS=
		openssl_historical_prefix_worked="no"
	else
		openssl_historical_prefix_worked="yes"
	fi
])dnl SXE_TRY_OPENSSL_HISTORICAL_PREFIX

AC_DEFUN([SXE_TRY_OPENSSL_BIN_PREFIX], [dnl
	## use the dirname of the openssl binary to determine the prefix of SSL
	openssl_bindir=`dirname $OPENSSL_BIN`
	openssl_prefix_maybe=`dirname $openssl_bindir`
	OPENSSL_CPPFLAGS="-I$openssl_prefix_maybe/include"
	OPENSSL_LDFLAGS="-L$openssl_prefix_maybe/lib"

	## now append these candidates to our c_switch and ld_switch
	SXE_DUMP_LIBS
	SXE_APPEND([$OPENSSL_CPPFLAGS], [CPPFLAGS])
	SXE_APPEND([$OPENSSL_LDFLAGS], [LDFLAGS])

	## check again
	SXE_CHECK_HEADERS([openssl/crypto.h])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])

	## restore
	SXE_RESTORE_LIBS
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto" != "yes yes"; then
		OPENSSL_CPPFLAGS=
		OPENSSL_LDFLAGS=
		openssl_bin_prefix_worked="no"
	else
		openssl_bin_prefix_worked="yes"
	fi
])dnl SXE_TRY_OPENSSL_BIN_PREFIX

AC_DEFUN([SXE_CHECK_OPENSSL_LOCS], [dnl
	## defines OPENSSL_CPPFLAGS and OPENSSL_LDFLAGS if needed

	dnl Look for these standard header file locations
	OPENSSL_LIBS="-lssl -lcrypto"
	SXE_CHECK_HEADERS([openssl/crypto.h])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto" != "yes yes"; then
		unset ac_cv_header_openssl_crypto_h
		unset ac_cv_lib_crypto_OPENSSL_cleanse
		SXE_TRY_OPENSSL_BIN_PREFIX
		if test "$openssl_bin_prefix_worked" != "yes"; then
			###/* sigh */
			unset ac_cv_header_openssl_crypto_h
			unset ac_cv_lib_crypto_OPENSSL_cleanse
			SXE_TRY_OPENSSL_HISTORICAL_PREFIX
		fi
	else
		## the location was known already, nothing to do now
		:
	fi
])dnl SXE_CHECK_OPENSSL_LOCS

AC_DEFUN([SXE_CHECK_OPENSSL_FEATURES], [dnl
	dnl test for some special purpose stuff in libcrypto
	AC_CHECK_LIB([crypto], [RSA_new], [openssl_no_rsa=no], [openssl_no_rsa=yes])
	AC_CHECK_LIB([crypto], [DSA_new], [openssl_no_dsa=no], [openssl_no_dsa=yes])
	AC_CHECK_LIB([crypto], [EC_KEY_new], [openssl_no_ec=no], [openssl_no_ec=yes])
	AC_CHECK_LIB([crypto], [DH_new], [openssl_no_dh=no], [openssl_no_dh=yes])
	if test "$openssl_no_rsa" = "yes"; then
		AC_DEFINE([OPENSSL_NO_RSA], [1], [Description here!])
	fi
	if test "$openssl_no_dsa" = "yes"; then
		AC_DEFINE([OPENSSL_NO_DSA], [1], [Description here!])
	fi
	if test "$openssl_no_ec" = "yes"; then
		AC_DEFINE([OPENSSL_NO_EC], [1], [Description here!])
	fi
	if test "$openssl_no_dh" = "yes"; then
		AC_DEFINE([OPENSSL_NO_DH], [1], [Description here!])
	fi
	
	dnl check for libssl support
	AC_CHECK_LIB([ssl], [SSL_new], [openssl_ssl=yes], [openssl_ssl=no])
	if test "$openssl_ssl" = "yes"; then
		AC_DEFINE([OPENSSL_SSL], [1], [Description here!])
	fi
])dnl SXE_CHECK_OPENSSL_FEATURES

AC_DEFUN([SXE_CHECK_OPENSSL_FUNCS], [dnl
	SXE_DUMP_LIBS
	LDFLAGS="$LDFLAGS $OPENSSL_LDFLAGS"
	CPPFLAGS="$CPPFLAGS $OPENSSL_CPPFLAGS"
	LIBS="$LIBS $OPENSSL_LIBS"
	AC_CHECK_FUNCS([dnl
		OpenSSL_add_all_digests OpenSSL_add_all_ciphers dnl
		RAND_bytes RAND_query_egd_bytes RAND_status dnl
		EVP_cleanup EVP_MD_CTX_init EVP_DigestInit_ex dnl
		EVP_DigestUpdate EVP_DigestFinal_ex EVP_MD_CTX_cleanup dnl
		HMAC_CTX_init HMAC_Init HMAC_Update HMAC_Final HMAC_CTX_cleanup dnl
		EVP_BytesToKey EVP_CIPHER_CTX_init EVP_EncryptInit dnl
		EVP_EncryptUpdate EVP_EncryptFinal EVP_DecryptInit dnl
		EVP_DecryptUpdate EVP_DecryptFinal EVP_CIPHER_CTX_cleanup dnl
		EVP_PKEY_new RSA_generate_key DSA_generate_parameters dnl
		DSA_generate_key EC_get_builtin_curves dnl
		EC_KEY_new_by_curve_name EC_KEY_generate_key dnl
		EC_KEY_set_private_key EC_KEY_dup dnl
		EVP_SealInit EVP_SealFinal EVP_OpenInit EVP_OpenFinal dnl
		EVP_SignFinal EVP_VerifyFinal dnl
		PEM_read_X509 PEM_read_PUBKEY PEM_read_PrivateKey dnl
		PEM_write_PUBKEY PEM_write_PKCS8PrivateKey dnl
		BIO_new BIO_free BIO_printf BIO_dump BIO_get_callback_arg dnl
		BIO_set_callback BIO_set_callback_arg BIO_read dnl
		SSL_library_init SSL_load_error_strings dnl
		SSLv2_client_method SSLv3_client_method dnl
		SSLv23_client_method TLSv1_client_method dnl
		SSLv2_server_method SSLv3_server_method dnl
		SSLv23_server_method TLSv1_server_method dnl
		SSL_CTX_new SSL_CTX_free SSL_CTX_add_client_CA dnl
		SSL_CTX_load_verify_locations SSL_CTX_use_certificate dnl
		SSL_CTX_use_PrivateKey SSL_CTX_check_private_key dnl
		SSL_CTX_use_certificate_file SSL_CTX_use_PrivateKey_file dnl
		SSL_do_handshake SSL_get_error ssl_verify_cert_chain dnl
		SSL_get_peer_cert_chain SSL_pending SSL_get_certificate dnl
		SSL_get_peer_certificate X509_verify_cert_error_string dnl
		SSL_get_verify_result SSL_get_current_cipher SSL_CIPHER_get_bits])
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_OPENSSL_FUNCS

AC_DEFUN([SXE_CHECK_OPENSSL], [dnl
	AC_MSG_CHECKING([for OpenSSL])
	AC_MSG_RESULT([])

	SXE_PATH_OPENSSL_BIN
	dnl defines OPENSSL_VERSION and OPENSSL_SANE_P
	SXE_OPENSSL_VERSION
	if test "$OPENSSL_SANE_P" = "yes"; then
		SXE_CHECK_OPENSSL_LOCS
		have_openssl=yes
		SXE_CHECK_OPENSSL_FEATURES
		SXE_CHECK_OPENSSL_FUNCS
	fi
])dnl SXE_CHECK_OPENSSL


dnl sxe-maths.m4 ends here
