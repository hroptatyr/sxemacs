dnl sxe-linker.m4 -- Linker stuff, esp. dynamic linking
dnl needed for emodules for one


AC_DEFUN([SXE_CHECK_LD_ZFLAG], [dnl
	pushdef([LD_ZFLAG], [$1])
	pushdef([cv_zflag], [sxe_cv_ld__z_]translit(LD_ZFLAG,[-.=],[___]))

	AC_CACHE_CHECK([whether linker supports -z ]LD_ZFLAG[],
		[]cv_zflag[], [_SXE_CHECK_LD_ZFLAG(LD_ZFLAG)])

	popdef([cv_zflag])
	popdef([LD_ZFLAG])
])dnl SXE_CHECK_LD_ZFLAG

AC_DEFUN([_SXE_CHECK_LD_ZFLAG], [dnl
	## arg1 is the flag to check for
	pushdef([LD_ZFLAG], [$1])
	pushdef([cv_zflag], [sxe_cv_ld__z_]translit(LD_ZFLAG,[-.=],[___]))

	if test "$GCC" = "yes"; then
		if test "($CC -Xlinker --help 2>&1 | \
			grep \"-z []LD_ZFLAG[]\" > /dev/null 2>&1 ) "; then
			cv_zflag="yes"
		else
			cv_zflag="no"
		fi
	elif test -n "$LD"; then
		if test "($LD --help 2>&1 | \
			grep \"-z []LD_ZFLAG[]\" > /dev/null 2>&1 )"; then
			cv_zflag="yes"
		else
			cv_zflag="no"
		fi
	else
		cv_zflag="no"
	fi

	popdef([cv_zflag])
	popdef([LD_ZFLAG])
])dnl _SXE_CHECK_LD_ZFLAG

AC_DEFUN([SXE_CHECK_LD_NOCOMBRELOC], [dnl
	SXE_CHECK_LD_ZFLAG([nocombreloc])
])dnl SXE_CHECK_LD_NOCOMBRELOC


AC_DEFUN([SXE_CHECK_LINKER_FLAGS], [dnl
	## relocation
	SXE_CHECK_LD_ZFLAG([combreloc])
	SXE_CHECK_LD_ZFLAG([nocombreloc])
	## symbols
	SXE_CHECK_LD_ZFLAG([defs])
	SXE_CHECK_LD_ZFLAG([muldefs])
	## search paths
	SXE_CHECK_LD_ZFLAG([nodefaultlib])
	## binding policy
	SXE_CHECK_LD_ZFLAG([lazy])
	SXE_CHECK_LD_ZFLAG([now])
])dnl SXE_CHECK_LINKER_FLAGS

AC_DEFUN([SXE_PREPEND_LINKER_FLAG], [dnl
	## a convenience function to add such linker flags to variables
	## arg1 is the flag to add (must include -z if applicable)
	## arg2 is the variable whither to prepend
	pushdef([FLAG], [$1])
	pushdef([__FLAG], [-Wl,]patsubst([$1], [ ], [[,]]))
	pushdef([VAR], [$2])

	[]VAR[]="[]__FLAG[] $[]VAR[]"
	if test "$extra_verbose" = "yes"; then
		echo "    Prepending linker flag \"[]__FLAG[]\" to \$[]VAR[]"
	fi

	popdef([VAR])
	popdef([__FLAG])
	popdef([FLAG])
])dnl SXE_PREPEND_LINKER_FLAG

dnl sxe-linker.m4 ends here
