dnl sxe-iconv.m4 -- mule and other coding system stuff

dnl MULE stuff
dnl ==========

AC_DEFUN([SXE_CHECK_MULE], [dnl
	AC_MSG_CHECKING([for mule-related features])
	AC_MSG_RESULT([])

	AC_DEFINE([MULE], [1], [Description here!])

	AC_DEFINE([ENCAPSULATE_CHDIR], [], [directory calls encapsulation])
	AC_DEFINE([ENCAPSULATE_MKDIR], [], [directory calls encapsulation])
	AC_DEFINE([ENCAPSULATE_OPENDIR], [], [directory calls encapsulation])
	AC_DEFINE([ENCAPSULATE_READDIR], [], [directory calls encapsulation])
	AC_DEFINE([ENCAPSULATE_RMDIR], [], [directory calls encapsulation])

	if test "$ac_cv_func_eaccess" = "yes"; then
		AC_DEFINE([ENCAPSULATE_EACCESS], [], [file information calls])
	fi
	AC_DEFINE([ENCAPSULATE_ACCESS], [], [file information calls])
	AC_DEFINE([ENCAPSULATE_LSTAT], [], [file information calls])
	AC_DEFINE([ENCAPSULATE_READLINK], [], [file information calls])
	AC_DEFINE([ENCAPSULATE_STAT], [], [file information calls])

	AC_DEFINE([ENCAPSULATE_CHMOD], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_CREAT], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_LINK], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_RENAME], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_SYMLINK], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_UNLINK], [], [file manipulation calls])
	AC_DEFINE([ENCAPSULATE_EXECVP], [], [file manipulation calls])

	MULE=yes
	have_mule=yes
	AC_DEFINE([FILE_CODING], [1], [Description here!])
	SXE_ADD_MULE_OBJS([mule.o mule-ccl.o mule-charset.o file-coding.o])

	dnl Use -lintl to get internationalized strerror for Mule
	SXE_CHECK_HEADERS([libintl.h])
	AC_CHECK_LIB([intl], [strerror], [:], [:])

	AC_MSG_CHECKING([for mule input methods])
	AC_MSG_RESULT([])

	SXE_CHECK_XIM
	SXE_CHECK_XFS
])dnl SXE_CHECK_MULE

AC_DEFUN([SXE_CHECK_XIM], [dnl
	## Do we have the XmIm* routines?  And if so, do we want to use them?
	## XIM seems to be flaky except on Solaris...
	## test -z "$with_xim" -a "$opsys" != "sol2" && with_xim=no
	case "$with_xim" in
	"" | "yes" )
		AC_MSG_CHECKING([for XIM])
		AC_MSG_RESULT([])
		AC_CHECK_LIB([X11], [XOpenIM], [with_xim=xlib], [with_xim=no])
		dnl XIM + Lesstif is not (yet?) usable
		if test "$have_motif $have_lesstif" = "yes no"; then
			AC_CHECK_LIB([Xm], [XmImMbLookupString], [with_xim=motif])
		fi
		;;
	esac

	if test "$with_xim" != "no"; then
		AC_DEFINE([HAVE_XIM], [1], [Description here!])
		if test "$with_xim" = "xlib"; then
			AC_DEFINE([XIM_XLIB], [1], [Description here!])
			SXE_ADD_MULE_OBJS([input-method-xlib.o])
		fi
		if test "$with_xim" = "motif"; then
			AC_DEFINE([XIM_MOTIF], [1], [Description here!])
			need_motif=yes
			SXE_ADD_MULE_OBJS([input-method-motif.o])
		fi
		if test "$with_xim" = "motif"; then
			with_xfs=no
		fi
	fi
])dnl SXE_CHECK_XIM

AC_DEFUN([SXE_CHECK_XFS], [dnl
	dnl "with_xfs" = "yes"
	if test "$with_xfs" = "yes"; then
		AC_MSG_CHECKING([for XFontSet])
		AC_MSG_RESULT([])
		AC_CHECK_LIB([X11], [XmbDrawString], [:], [with_xfs=no])
		if test "$with_xfs" = "yes" -a "$with_menubars" = "lucid"; then
			AC_DEFINE([USE_XFONTSET], [1], [Description here!])
			if test "$with_xim" = "no"; then
				SXE_ADD_MULE_OBJS([input-method-xlib.o])
			fi
		fi
	fi dnl with_xfs
])dnl SXE_CHECK_XFS

AC_DEFUN([SXE_CHECK_WNN], [dnl
	dnl Autodetect WNN
	if test "$with_wnn6" = "yes"; then
		with_wnn=yes # wnn6 implies wnn support
		SXE_CHECK_HEADERS([wnn/jllib.h wnn/commonhd.h])
		if test "$ac_cv_header_wnn_jllib_h" != "yes" -o \
			"$ac_cv_header_wnn_commonhd_h" != "yes"; then
			with_wnn=no
		fi
		dnl gcc 2.97 fixincludes breaks inclusion of wnn/commonhd.h
		dnl Detour to find crypt
		AC_CHECK_FUNCS([crypt])
		if test "$ac_cv_func_crypt" != "yes"; then
			AC_CHECK_LIB([crypt], [crypt])
		fi
		dnl Back to our regularly scheduled wnn hunting
		SXE_CHECK_LIB_FUNCS([wnn wnn4 wnn6 wnn6_fromsrc],
			[jl_dic_list_e jl_fi_dic_list])

		if test "$ac_cv_lib_wnn_jl_dic_list_e" = "yes"; then
			libwnn=wnn
			have_wnn=yes
		elif test "$ac_cv_lib_wnn4_jl_dic_list_e" = "yes"; then
			libwnn=wnn4
			have_wnn=yes
		elif test "$ac_cv_lib_wnn6_jl_dic_list_e" = "yes"; then
			libwnn=wnn6
			have_wnn=yes
		elif test "$ac_cv_lib_wnn6_fromsrc_jl_dic_list_e" = "yes"; then
			libwnn=wnn6_fromsrc
			have_wnn=yes
		else
			have_wnn=no
		fi

		if test "$have_wnn" = "yes"; then
			AC_DEFINE([HAVE_WNN], [1], [Description here!])
			SXE_PREPEND([-l$libwnn], [libs_x])
			SXE_ADD_MULE_OBJS([mule-wnnfns.o])
		fi
		if test "$ac_cv_lib_wnn_jl_dic_list" = "yes" -o \
			"$ac_cv_lib_wnn4_jl_dic_list" = "yes" -o \
			"$ac_cv_lib_wnn6_jl_dic_list" = "yes" -o \
			"$ac_cv_lib_wnn6_fromsrc_jl_dic_list" = "yes"; then
			AC_DEFINE([WNN6], [1], [Description here!])
		fi
	fi
])dnl SXE_CHECK_WNN

AC_DEFUN([SXE_CHECK_CANNA], [dnl
	dnl Autodetect canna
	if test "$with_canna" != "no"; then
		SXE_CHECK_HEADERS([canna/jrkanji.h])
	fi
	if test "$ac_cv_header_canna_jrkanji_h" != "yes"; then
		SXE_DUMP_LIBS
		CPPFLAGS="-I/usr/local/canna/include $CPPFLAGS"
		unset ac_cv_header_canna_jrkanji_h
		SXE_CHECK_HEADERS([canna/jrkanji.h])
		SXE_RESTORE_LIBS
	fi
	if test "$ac_cv_header_canna_jrkanji_h" != "yes"; then
		CPPFLAGS="$CPPFLAGS -I/usr/local/canna/include"
	fi

	have_canna="yes"
	AC_CHECK_HEADER([canna/RK.h], [:], [have_canna=no])
	AC_CHECK_LIB([RKC], [RkBgnBun], [:], [have_canna=no])
	AC_CHECK_LIB([canna], [jrKanjiControl], [:], [have_canna=no])
	if test "$have_canna" = "yes"; then
		AC_DEFINE([HAVE_CANNA], [1], [Description here!])
		SXE_PREPEND([-lcanna -lRKC], [libs_x])
		SXE_ADD_MULE_OBJS([mule-canna.o])

		AC_DEFINE([CANNA2], [], [Description here!])
		AC_DEFINE([CANNA_MULE], [], [Description here!])
		AC_DEFINE([CANNA_PURESIZE], [0], [Description here!])
	else
		AC_DEFINE([CANNA_PURESIZE], [0], [Description here!])
	fi
])dnl SXE_CHECK_CANNA

dnl sxe-iconv.m4 ends here
