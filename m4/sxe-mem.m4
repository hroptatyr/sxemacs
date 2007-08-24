dnl sxe-mem.m4 -- memory managers

AC_DEFUN([SXE_CHECK_BDWGC], [dnl
	## the actual check
	AC_CACHE_CHECK([for Boehm-Demers-Weiser GC support],
		[sxe_cv_feat_bdwgc], [_SXE_CHECK_BDWGC])
])dnl SXE_CHECK_BDWGC

AC_DEFUN([SXE_CHECK_BDWGC_HEADERS], [dnl
	SXE_CHECK_HEADERS([gc.h])
])dnl SXE_CHECK_BDWGC_HEADERS

AC_DEFUN([SXE_CHECK_BDWGC_LIBS], [dnl
	SXE_CHECK_LIB_FUNCS([gc], [GC_malloc GC_realloc GC_init])

	if test "$ac_cv_lib_gc___GC_malloc" = "yes"; then
		AC_DEFINE([HAVE_LIBGC_GC_MALLOC], [1],
			[Whether GC_malloc() from libgc is available.])
	fi
	if test "$ac_cv_lib_gc___GC_realloc" = "yes"; then
		AC_DEFINE([HAVE_LIBGC_GC_REALLOC], [1],
			[Whether GC_realloc() from libgc is available.])
	fi
	if test "$ac_cv_lib_gc___GC_init" = "yes"; then
		AC_DEFINE([HAVE_LIBGC_GC_INIT], [1],
			[Whether GC_init() from libgc is available.])
	fi
])dnl SXE_CHECK_BDWGC_LIBS

AC_DEFUN([_SXE_CHECK_BDWGC], [dnl
	## defines have_bdwgc and sxe_cv_feat_bdwgc
	AC_REQUIRE([SXE_CHECK_BDWGC_HEADERS])
	AC_REQUIRE([SXE_CHECK_BDWGC_LIBS])

	if test "$ac_cv_header_gc_h" = "yes" -a \
		"$ac_cv_lib_gc___GC_malloc" = "yes" -a \
		"$ac_cv_lib_gc___GC_realloc" = "yes" -a \
		"$ac_cv_lib_gc___GC_init" = "yes"; then
		AC_DEFINE([HAVE_BDWGC], [1],
			[Whether all necessary functions could be found in lgc.])
		have_bdwgc="yes"
	else
		have_bdwgc="no"
	fi
	sxe_cv_feat_bdwgc="$have_bdwgc"
])dnl _SXE_CHECK_BDWGC


dnl sxe-mem.m4 ends here
