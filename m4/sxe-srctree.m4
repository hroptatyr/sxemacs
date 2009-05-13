dnl sxe-srctree.m4 -- Checks on source tree and build directory

AC_DEFUN([SXE_COMPUTE_BLDDIR], [dnl
	## Calculate canonical name for sxe_blddir (i.e. current directory).
	## PWD may already be the preferable absolute name for ".",
	## but we can't trust it - it is sometimes inaccurate.
	## defines sxe_blddir
	absolute_pwd=$(pwd);
	if test -n "$PWD" -a $(cd $PWD && pwd) = "$absolute_pwd"; then
		sxe_blddir="$PWD"
	else
		sxe_blddir="$absolute_pwd"
		SXE_CANONICALISE_PATH([sxe_blddir])
	fi
])dnl SXE_COMPUTE_BLDDIR

AC_DEFUN([SXE_COMPUTE_SRCDIR], [dnl
	## Make srcdir absolute, if not already.  It is important to
	## avoid running the path through pwd unnecessary, since pwd can
	## give you automounter prefixes, which can go away.
	##   so? we want to build at last from our srcdir -hrop
	## defines sxe_srcdir
	case "$srcdir" in
	/* )
		sxe_srcdir="$srcdir"
		;;
	. )
		sxe_srcdir="$sxe_blddir"
		;;
	* )
		sxe_srcdir=$(cd $srcdir && pwd)
		SXE_CANONICALISE_PATH([sxe_srcdir])
		;;
	esac
])dnl SXE_COMPUTE_BLDDIR

AC_DEFUN([SXE_CHECK_SRCDIR_HEALTH], [dnl
	## check whether both sxe_blddir and sxe_srcdir have been set
	if test "${sxe_blddir-unset}" = "unset"; then
		## run the computation
		SXE_COMPUTE_BLDDIR
	fi
	if test "${sxe_srcdir-unset}" = "unset"; then
		## run the computation
		SXE_COMPUTE_SRCDIR
	fi

	## check if source dir and build dir coincide
	AC_MSG_CHECKING([if source tree coincides with the build tree])
	if test "$sxe_blddir" = "$sxe_srcdir" -o \
		-f "$sxe_blddir/sxemacs_version.m4"; then
		AC_MSG_RESULT([yes])
		srcdir_equals_blddir_p=yes
	else
		AC_MSG_RESULT([no. Good Boy!])
		srcdir_equals_blddir_p=no
	fi

	## Check if the source directory already has a configured system in it.
	if test "$srcdir_equals_blddir_p" = "no" -a \
		-f "$sxe_srcdir/src/config.h" -o \
		"$srcdir_equals_blddir_p" = "no" -a \
		-e "$sxe_srcdir/.sxemacs.source.tree"; then
		AC_MSG_WARN([The directory tree `$sxe_srcdir' is being used])
		AC_MSG_WARN([as a build directory right now; it has been configured in its own])
		AC_MSG_WARN([right.  To configure in another directory as well, you MUST])
		AC_MSG_WARN([use GNU make.  If you do not have GNU make, then you must])
		AC_MSG_WARN([now do `make distclean' in $sxe_srcdir,])
		AC_MSG_WARN([and then run $progbasename again.])
		extrasub="/^VPATH[[	 ]]*=/c\
vpath %.c $sxe_srcdir/lib-src $sxe_srcdir/src\\n\
vpath %.h $sxe_srcdir/lib-src $sxe_srcdir/src\\n\
vpath %.y $sxe_srcdir/lib-src $sxe_srcdir/src\\n\
vpath %.l $sxe_srcdir/lib-src $sxe_srcdir/src\\n\
vpath %.s $sxe_srcdir/lib-src $sxe_srcdir/src\\n\
vpath %.in $sxe_srcdir/lib-src $sxe_srcdir/src\\n"
	fi

])dnl SXE_CHECK_SRCDIR_HEALTH

dnl sxe-srctree.m4 ends here
