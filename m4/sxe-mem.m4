dnl sxe-mem.m4 -- memory managers

AC_DEFUN([SXE_CHECK_BDWGC], [dnl
	## the actual check
	AC_REQUIRE([SXE_CHECK_CC_VERSION])

	AC_CACHE_CHECK([for Boehm-Demers-Weiser GC support],
		[sxe_cv_feat_bdwgc], [_SXE_CHECK_BDWGC])

	## quick compiler check
	if test "$with_experimental_features_bdwgc" = "yes"; then
		case "$compiler_version" in
		gcc*\ 3.* | gcc*\ 2.* )
			AC_MSG_NOTICE([
Stop right there, my friend!
The powers that be have deemed you unworthy of succeeding in building
SXEmacs in BDWGC mode with _that_ compiler ($compiler_version)!

It's not that we are going to stop you now but this will definitely fail.
In order to build a BDWGC-flavoured SXEmacs you want to use a GCC C compiler
of the 4th series.

The alternative is likewisely simple, do NOT enable the BDWGC feature,
that is leave --with-experimental-features alone, or explicitly turn off the
BDWGC feature like so: --with-experimental-features=nobdwgc.

I shall now give you a grace period of 32 seconds to consider your answer,
will you (from most recommended to least recommended)
a) upgrade your gcc, or
b) rerun configure not requesting the BDWGC feature, or
c) continue the build and promise not to complain?
				])
			sleep 32
			AC_MSG_NOTICE([
Your time is up and you've made the wrongest decision, possibly, ever!
Well, you can still press C-c (control c, that is).
				])
			sleep 5
			;;
		esac
	fi

	AM_CONDITIONAL([HAVE_BDWGC], [test "$have_bdwgc" = "yes"])
])dnl SXE_CHECK_BDWGC

AC_DEFUN([SXE_CHECK_BDWGC_HEADERS], [dnl
	## defines sxe_cv_bdwgc_headers

	AC_MSG_CHECKING([for bdwgc includes directory])
	if test -n "${with_bdwgc_includes}"; then
		SXE_BDWGC_INCLUDES_PATH([${with_bdwgc_includes}])
	elif test -n "${with_bdwgc}"; then
		SXE_BDWGC_INCLUDES_PATH([${with_bdwgc}])
	else
		## just try ${prefix}/include
		SXE_BDWGC_INCLUDES_PATH([${prefix}])
	fi
	AC_MSG_RESULT([$BDWGC_CPPFLAGS])

	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS ${BDWGC_CPPFLAGS}"
	SXE_CHECK_HEADERS([gc.h])
	SXE_CHECK_HEADERS([gc/gc.h])
	SXE_CHECK_HEADERS([cord.h])
	SXE_CHECK_HEADERS([gc/cord.h])
	SXE_RESTORE_LIBS

	if test "$ac_cv_header_gc_gc_h" = "yes" -o \
		"$ac_cv_header_gc_h" = "yes"; then
		sxe_cv_bdwgc_headers="yes"
	else
		sxe_cv_bdwgc_headers="no"
	fi

	AC_SUBST([BDWGC_CPPFLAGS])
])dnl SXE_CHECK_BDWGC_HEADERS

AC_DEFUN([SXE_CHECK_BDWGC_LIBS], [dnl
	AC_MSG_CHECKING([for bdwgc library])
	if test -n "$with_bdwgc_libs"; then
		SXE_BDWGC_LIBS_PATH([${with_bdwgc_libs}])
	elif test -n "${with_bdwgc}"; then
		SXE_BDWGC_LIBS_PATH([${with_bdwgc}])
	else
		## just try under ${prefix}/lib
		SXE_BDWGC_LIBS_PATH([${prefix}])
	fi
	AC_MSG_RESULT([$BDWGC_LDFLAGS])

	AC_CHECK_LIB([gc], [GC_init], [:], [:])
	AC_CHECK_LIB([gc], [GC_malloc], [:], [:])
	AC_CHECK_LIB([gc], [GC_malloc_atomic], [:], [:])
	AC_CHECK_LIB([gc], [GC_malloc_uncollectable], [:], [:])
	AC_CHECK_LIB([gc], [GC_realloc], [:], [:])
	AC_CHECK_LIB([gc], [GC_strdup], [:], [:])
	AC_CHECK_LIB([gc], [GC_free], [:], [:])

	if test "$ac_cv_lib_gc___GC_init" = "yes" -a \
		"$ac_cv_lib_gc___GC_malloc" = "yes" -a \
		"$ac_cv_lib_gc___GC_malloc_atomic" = "yes" -a \
		"$ac_cv_lib_gc___GC_malloc_uncollectable" = "yes" -a \
		"$ac_cv_lib_gc___GC_realloc" = "yes" -a \
		"$ac_cv_lib_gc___GC_strdup" = "yes" -a \
		"$ac_cv_lib_gc___GC_free" = "yes"; then
		sxe_cv_bdwgc_libs="yes"
		BDWGC_LIBS="-lgc"

	elif test "$ac_cv_lib_gc_GC_init" = "yes" -a \
		"$ac_cv_lib_gc_GC_malloc" = "yes" -a \
		"$ac_cv_lib_gc_GC_malloc_atomic" = "yes" -a \
		"$ac_cv_lib_gc_GC_malloc_uncollectable" = "yes" -a \
		"$ac_cv_lib_gc_GC_realloc" = "yes" -a \
		"$ac_cv_lib_gc_GC_strdup" = "yes" -a \
		"$ac_cv_lib_gc_GC_free" = "yes"; then
		sxe_cv_bdwgc_libs="yes"
		BDWGC_LIBS="-lgc"

	else
		sxe_cv_bdwgc_libs="no"
	fi

	AC_SUBST([BDWGC_LDFLAGS])
	AC_SUBST([BDWGC_LIBS])
])dnl SXE_CHECK_BDWGC_LIBS

AC_DEFUN([_SXE_CHECK_BDWGC], [dnl
	## defines have_bdwgc and sxe_cv_feat_bdwgc
	AC_REQUIRE([SXE_CHECK_BDWGC_HEADERS])
	AC_REQUIRE([SXE_CHECK_BDWGC_LIBS])

	if test "$sxe_cv_bdwgc_headers" = "yes" -a \
		"$sxe_cv_bdwgc_libs" = "yes"; then
		AC_DEFINE([HAVE_BDWGC], [1],
			[Whether all necessary functions could be found in lgc.])
		have_bdwgc="yes"
	else
		have_bdwgc="no"
	fi
	sxe_cv_feat_bdwgc="$have_bdwgc"
])dnl _SXE_CHECK_BDWGC

AC_DEFUN([SXE_BDWGC_INCLUDES_PATH], [dnl
	## arg 1 is /path/to/kant/src
	pushdef([bdwgc_path], [$1])

	if test -d "/[]bdwgc_path[]"; then
		path=/[]bdwgc_path[]
	elif test -d "$(pwd)/[]bdwgc_path[]" -o -d "[]bdwgc_path[]"; then
		## relative thing
		path=$(pwd)/[]bdwgc_path
	elif test -f "/[]bdwgc_path[]"; then
		## PEBKAC
		path=/$(dirname []bdwgc_path[])
	elif test -f "$(pwd)/[]bdwgc_path[]" -o -f "[]bdwgc_path[]"; then
		## still PEBKAC
		path=$(dirname $(pwd)/[]bdwgc_path[])
	elif test -n "[]bdwgc_path[]"; then
		path=[]bdwgc_path[]
	else
		## yuck, path's empty, dunno what to do
		path=
	fi

	if test -d "$path/include" -a \
		-e "$path/include/gc/gc.h" -o \
		-d "$path/include" -a \
		-e "$path/include/gc.h"; then
		## we went to prefix
		path="$path/include"
	elif test -d "$path/../include" -a \
		-e "$path/../include/gc/gc.h" -o \
		-d "$path/../include" -a \
		-e "$path/../include/gc.h"; then
		## we went to directly to the include dir
		path="${path}/../include"
	else
		## whatever
		path=
	fi

	## simple canonicalisation
	if test -n "$path"; then
		SXE_CANONICALISE_PATH([path])
		BDWGC_CPPFLAGS="${BDWGC_CPPFLAGS} -I${path}"
	fi

	popdef([bdwgc_path])
])dnl SXE_BDWGC_INCLUDES_PATH

AC_DEFUN([SXE_BDWGC_LIBS_PATH], [dnl
	## arg 1 is /path/to/libgc.so
	pushdef([bdwgc_path], [$1])

	if test -d "/[]bdwgc_path[]"; then
		path=/[]bdwgc_path[]
	elif test -d "$(pwd)/[]bdwgc_path[]" -o -d "[]bdwgc_path[]"; then
		## relative thing
		path=$(pwd)/[]bdwgc_path
	elif test -f "/[]bdwgc_path[]"; then
		## PEBKAC
		path=/$(dirname []bdwgc_path[])
	elif test -f "$(pwd)/[]bdwgc_path[]" -o -f "[]bdwgc_path[]"; then
		## still PEBKAC
		path=$(dirname $(pwd)/[]bdwgc_path[])
	elif test -n "[]bdwgc_path[]"; then
		path=[]bdwgc_path[]
	else
		## yuck, path's empty, dunno what to do
		path=
	fi

	if test -d "$path/lib" -a -e "$path/lib/libgc.so"; then
		## we went to prefix
		path="$path/lib"
	elif test -d "$path/../lib" -a -e "$path/../lib/libgc.so"; then
		## we went to directly to the lib dir
		path="${path}/../lib"
	else
		## whatever
		path=
	fi

	## simple canonicalisation
	if test -n "$path"; then
		SXE_CANONICALISE_PATH([path])
		BDWGC_LDFLAGS="${BDWGC_LDFLAGS} -L${path}"
	fi

	popdef([bdwgc_path])
])dnl SXE_BDWGC_LIBS_PATH


dnl sxe-mem.m4 ends here
