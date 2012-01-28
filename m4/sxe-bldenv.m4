dnl sxe-bldenv.m4 -- Things for/on the build environment

AC_DEFUN([SXE_LD_EXPORT_DYNAMIC], [dnl
	AC_MSG_CHECKING([if linker understands -export-dynamic])
	SXE_DUMP_LIBS
	LDFLAGS="-export-dynamic $LDFLAGS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
]],[[
return 0;
]])],  [AC_MSG_RESULT([yes])
	have_ld_export_dynamic="yes"], [
	AC_MSG_RESULT([no])
	have_ld_export_dynamic="no"])
	SXE_RESTORE_LIBS
])dnl SXE_LD_EXPORT_DYNAMIC


AC_DEFUN([SXE_LD_NO_PIE], [dnl
	AC_MSG_CHECKING([if linker understands -no_pie])
	SXE_DUMP_LIBS
	LDFLAGS="-no_pie $LDFLAGS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
]],[[
return 0;
]])],  [AC_MSG_RESULT([yes])
	have_ld_no_pie="yes"], [
	AC_MSG_RESULT([no])
	have_ld_no_pie="no"])
	SXE_RESTORE_LIBS
])dnl SXE_LD_NO_PIE

dnl SXE_RILLY_COMPUTE_LD_RUN_PATH()
AC_DEFUN([SXE_RILLY_COMPUTE_LD_RUN_PATH], [dnl
	if test "$add_runtime_path" = "yes" -a -n "$dash_r"; then
		## Remove runtime paths from current ld switches
		ld_switch_site=`echo   '' $ld_switch_site   | sed -e 's:^ ::' -e "s/$dash_r[[^ ]]*//g"`
		ld_switch_x_site=`echo '' $ld_switch_x_site | sed -e 's:^ ::' -e "s/$dash_r[[^ ]]*//g"`

		## Fix up Runtime path
		## If LD_RUN_PATH is set in environment, use that.
		## In this case, assume user has set the right value.
		runpath=""
		runpath_dirs=""
		if test -n "$LD_RUN_PATH"; then
			runpath="$LD_RUN_PATH"
		elif test "$GCC" = "yes"; then
			## Compute runpath from gcc's -v output
			ld_switch_run_save="$ld_switch_run"
			ld_switch_run=""
			echo "int main(int argc, char *argv[[]]) {return 0;}" \
				> conftest.c
			xe_runpath_link='${CC-cc} -o conftest -v $CFLAGS '"$xe_ldflags"' conftest.$ac_ext 2>&1 1>/dev/null'
			for arg in `eval "$xe_runpath_link" | grep ' -L'`; do
				case "$arg" in
				P,* | -L* | -R* )
					for dir in `echo '' "$arg" | sed -e 's:^ ::' -e 's/^..//' -e 'y/:/ /'`; do
						SXE_ADD_RUNPATH_DIR(["$dir"])
					done
					;;
				esac
			done
			ld_switch_run="$ld_switch_run_save"
			rm -f conftest*
		else
			## Add all directories with .so files to runpath
			for arg in $ld_switch_site $ld_switch_x_site; do
				case "$arg" in
				-L*)
					SXE_ADD_RUNPATH_DIR(`echo '' "$arg" | sed -e 's:^ ::' -e 's/^-L//'`)
					;;
				esac
			done
			## Sometimes /opt/SUNWdt/lib is the only
			## installed Motif available
			if test "$opsys $need_motif" = "sol2 yes"; then
				xe_runpath_dir="/opt/SUNWdt/lib";
				eval "$xe_add_unique_runpath_dir";
			fi
		fi dnl Compute $runpath

		if test -n "$runpath"; then
			ld_switch_run="${dash_r}${runpath}"
			SXE_PROTECT_LINKER_FLAGS(ld_switch_run)
			if test "$extra_verbose" = "yes"; then
				echo "Setting runpath to $runpath"
			fi
		fi
	fi
])dnl SXE_RILLY_COMPUTE_LD_RUN_PATH

AC_DEFUN([SXE_COMPUTE_LD_RUN_PATH], [dnl
	## --with-site-runtime-libraries (multiple dirs)
	SXE_COLON_TO_SPACE(with_site_runtime_libraries)
	if test -n "$with_site_runtime_libraries"; then
		LD_RUN_PATH="`echo $with_site_runtime_libraries | sed -e 's/  */:/g'`"
		export LD_RUN_PATH
		for path in $with_site_runtime_libraries; do
			dnl SXE_APPEND_UNDUP("-R$path ", $ld_switch_run)
			SXE_APPEND_UNDUP(["-R${path} "], [LDFLAGS])
		done
	fi

	## -------------------------------------
	## Compute runtime library path
	## -------------------------------------

	## Linux systems have dynamic runtime library directories listed in
	## /etc/ld.so.conf.  Since those are used at run time, it seems pretty
	## safe to use them at link time, and less controversial than forcing
	## the run-time to use the link-time libraries.  This also helps avoid
	## mismatches between the link-time and run-time libraries.

	## #### Unfortunately, there are horrible libc4 and libc5 libraries
	## listed in /etc/ld.so.conf on some systems, and including them on
	## the link path leads to linking in utterly broken libc's.
	## There are many clever ways of approaching this problem,
	## but finding one that actually works...

	## if test -z "$LD_RUN_PATH" -a -r "/etc/ld.so.conf"; then
	##   for dir in `cat /etc/ld.so.conf`; do
	##     test -d "$dir" && SXE_APPEND_UNDUP(-L${dir}, ld_switch_system)
	##   done
	##   add_runtime_path=no
	## fi

	if test -n "$add_runtime_path"; then
		:
	elif test "$with_dynamic" = "no"; then
		add_runtime_path=no
	elif test -n "$LD_RUN_PATH"; then
		add_runtime_path=yes
	else
		case "$opsys" in
		sol2 | irix* | *bsd* | decosf* )
			add_runtime_path=yes
			;;
		* )
			add_runtime_path=no
			;;
		esac
	fi

	if test "$add_runtime_path" = "yes"; then
		## Try to autodetect runtime library flag (usually -R),
		## and whether it works (or at least does no harm)
		AC_MSG_CHECKING([for runtime libraries flag])
		case "$opsys" in
		sol2 )
			dash_r="-R"
			;;
		decosf* | linux* | irix*)
			dash_r="-rpath "
			;;
		*)
			dash_r=""
			for try_dash_r in "-R" "-R " "-rpath "; do
				xe_check_libs="${try_dash_r}/no/such/file-or-directory"
				SXE_PROTECT_LINKER_FLAGS(xe_check_libs)
				AC_LINK_IFELSE([AC_LANG_SOURCE([[]])],
					[dash_r="$try_dash_r"])
				xe_check_libs=""
				test -n "$dash_r" && break
			done
			;;
		esac
		if test -n "$dash_r"; then
			AC_MSG_RESULT("${dash_r}")
		else
			AC_MSG_RESULT([NONE])
		fi
	fi

	xe_add_unique_runpath_dir='
	xe_add_p=yes
	for xe_dir in $runpath_dirs; do   dnl Uniquify
		test "$xe_dir" = "$xe_runpath_dir" && xe_add_p=no
	done
	if test "$xe_add_p" = "yes"; then
		test -n "$runpath" && runpath="${runpath}:"
		runpath="${runpath}${xe_runpath_dir}"
		runpath_dirs="$runpath_dirs $xe_runpath_dir"
	fi'

	## t3h real computation
	SXE_RILLY_COMPUTE_LD_RUN_PATH
])dnl SXE_COMPUTE_LD_RUN_PATH


AC_DEFUN([SXE_COMPUTE_SITE_PREFIXES], [dnl
	## ---------------------------------------------------------------
	## Add site and system specific flags to compile and link commands
	## ---------------------------------------------------------------

	dnl --with-site-libraries (multiple dirs)
	SXE_COLON_TO_SPACE(with_site_libraries)
	if test -n "$with_site_libraries"; then
		for arg in $with_site_libraries; do
			case "$arg" in
			-* )
				;;
			* )
				test -d "$arg" || \
				SXE_DIE("Invalid site library \`$arg': no such directory")
				arg="-L${arg}"
				;;
			esac
			SXE_APPEND_UNDUP($arg, ld_switch_site)
		done
	fi

	dnl --with-site-includes (multiple dirs)
	SXE_COLON_TO_SPACE(with_site_includes)
	if test -n "$with_site_includes"; then
		for arg in $with_site_includes; do
			case "$arg" in
			-* )
				;;
			* )
				test -d "$arg" || \
				SXE_DIE("Invalid site include \`$arg': no such directory")
				arg="-I${arg}"
				;;
			esac
			SXE_APPEND_UNDUP($arg, c_switch_site)
			SXE_APPEND_UNDUP($arg, CPPFLAGS)
		done
	fi

	dnl --with-site-prefixes (multiple dirs)
	dnl --with-site-prefixes=dir1:dir2 is a convenient shorthand for
	dnl --with-site-libraries=dir1/lib:dir2/lib
	dnl --with-site-includes=dir1/include:dir2/include
	## Site prefixes take precedence over the standard places, but not over
	## with-site-includes and with-site-libraries.
	SXE_COLON_TO_SPACE(with_site_prefixes)
	if test -n "$with_site_prefixes"; then
		for dir in $with_site_prefixes; do
			lib_dir="${dir}/lib"
			inc_dir="${dir}/include"
			if test ! -d "$dir"; then
				SXE_DIE("Invalid site prefix \`$dir': no such directory")
			elif test ! -d "$lib_dir"; then
				SXE_DIE("Invalid site prefix \`$dir': no such directory \`$lib_dir'")
			else
				if test -d "$inc_dir"; then
					SXE_APPEND_UNDUP(["-I$inc_dir"], [CPPFLAGS])
				fi
				SXE_APPEND_UNDUP(["-L$lib_dir"], [LDFLAGS])
			fi
		done
	fi

	dnl GNU software installs by default into /usr/local/{include,lib}
	if test -d "/usr/local/include" -a -d "/usr/local/lib"; then
		SXE_APPEND_UNDUP(["-L/usr/local/lib"], [LDFLAGS])
		SXE_APPEND_UNDUP(["-I/usr/local/include"], [CPPFLAGS])
	fi

	dnl Extra system-specific library directories - please add to list
	for dir in "/usr/ccs/lib"; do
		dnl test -d "$dir" && SXE_APPEND_UNDUP(-L${dir}, ld_switch_system)
		if test -d "$dir"; then
			SXE_APPEND_UNDUP([-L${dir}], [LDFLAGS])
		fi
	done
])dnl SXE_COMPUTE_SITE_PREFIXES


AC_DEFUN([SXE_EXPLORE_BUILD_ENVIRONMENT], [dnl
dnl AC_ARG_PROGRAM
	AC_CANONICAL_BUILD
	AC_CANONICAL_HOST

	dnl Local paths
	test "x$prefix" = xNONE && prefix=$ac_default_prefix
	# Let make expand exec_prefix.
	if test "x$exec_prefix" = xNONE; then
		exec_prefix='${prefix}'
	else
		AC_DEFINE([EXEC_PREFIX_USER_DEFINED], [1],
			[exec-prefix has been specified on the configure line.])
	fi
	##datarootdir='${prefix}/share'
	if test "x$datadir" != 'x${prefix}/share' && \
	   test "x$datadir" != 'x${datarootdir}'; then
		AC_DEFINE([INFODIR_USER_DEFINED], [1], [Description here!])
		AC_DEFINE([LISPDIR_USER_DEFINED], [1], [Description here!])
		AC_DEFINE([ETCDIR_USER_DEFINED], [1], [Description here!])
		AC_DEFINE([DOCDIR_USER_DEFINED], [1], [Description here!])
	##else
	##	datadir='${prefix}/share'
	fi

	if test "x$libdir" != 'x${exec_prefix}/lib'; then
		AC_DEFINE([ARCHLIBDIR_USER_DEFINED], [1], [Description here!])
		AC_DEFINE([MODULEDIR_USER_DEFINED], [1], [Description here!])
		AC_DEFINE([SITEMODULEDIR_USER_DEFINED], [1], [Description here!])
	fi
	##if test "x$mandir" = 'x${prefix}/man' || \
	##   test "x$mandir" = 'x${datarootdir}/man'; then
	##	mandir='${prefix}/man/man1'
	##fi
	if test "x$infodir" != 'x${prefix}/info' -a \
		"x$infodir" != 'x${datarootdir}/info'; then
		AC_DEFINE([INFODIR_USER_DEFINED], [1], [Description here!])
	elif test "x$infodir" = 'x${prefix}/info'; then
		## use new convention
		infodir='${prefix}/share/info'
	fi

	inststaticdir='${PROGNAME}'
	instvardir='${PROGNAME}-${old_version}'
	instvarsepdir='${PROGNAME}/${old_version}'
	sitemoduledir='${libdir}/${inststaticdir}/${configuration}/site-modules'

	dnl until someone explains what this is for!!!
	dnl AC_SUBST([statedir], [$with_statedir])

	## Calculate canonical name for sxe_blddir (i.e. current directory)
	SXE_COMPUTE_BLDDIR
	## ... and sxe_srcdir
	SXE_COMPUTE_SRCDIR
	## general health checking
	SXE_CHECK_SRCDIR_HEALTH

	## derive emodblddir from this
	emodblddir=$sxe_blddir/modules
	## derivate emodsrcdir from srcdir
	emodsrcdir=$sxe_srcdir/modules
])dnl SXE_EXPLORE_BUILD_ENVIRONMENT

dnl sxe-bldenv.m4 ends here
