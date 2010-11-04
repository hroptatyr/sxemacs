dnl sxe-bldchain-progs.m4 -- Necessary build chain stuff



AC_DEFUN([SXE_PROG_AR], [dnl
	AC_ARG_VAR([AR], [the ar command])
	AC_CHECK_TOOL([AR], [ar], [:], [$PATH])
	if test "$AR" = ":"; then
		AC_PATH_TOOL([USR_CCS_BIN_AR], [ar], [:],
			[$PATH:/usr/ccs/bin])
	fi
	if test "$AR" = ":" -a "$USR_CCS_BIN_AR" = ":"; then
		AC_MSG_ERROR([Uh oh, no ar is rilly bad news.])
	elif test "$AR" = ":"; then
		AR=$USR_CCS_BIN_AR
	fi
	AC_SUBST(AR)
])dnl SXE_PROG_AR


AC_DEFUN([SXE_PROG_BISON], [
	AC_PROG_YACC()
	SXE_MSG_CHECKING([for bison])
	if test "$YACC" != "bison -y"; then
		AC_SUBST([BISON], [:], [location of bison])
		have_bison="no"
		sxe_cv_feat_bison="no"
		dnl AC_MSG_ERROR([bison not found but required])
	else
		AC_SUBST([BISON], [bison], [location of bison])
		have_bison="yes"
		sxe_cv_feat_bison="yes"
	fi
	SXE_MSG_RESULT([${sxe_cv_feat_bison}])

	## check if bison is capable
	if test "$sxe_cv_feat_bison" = "yes"; then
		SXE_MSG_CHECKING([if bison is recent enough])
		sxe_cv_feat_bison_version="$(${BISON} --version | head -n1)"
		case "$sxe_cv_feat_bison_version" in
		*\ 1.875 | *\ 2.*)
			;;
		*)
			have_bison="no"
			;;
		esac
		SXE_MSG_RESULT([${have_bison} (${sxe_cv_feat_bison_version})])
	fi
])dnl SXE_PROG_BISON


AC_DEFUN([SXE_CHECK_AUTOTOOL], [dnl
	## arg 1 tool's binary name
	## arg 2 sxemacs_version.m4's name
	## arg 3 config.h.in description
	pushdef([autotool], [$1])
	pushdef([AUTOTOOL], translit($1, [-a-z], [_A-Z]))
	pushdef([v3rs1on], indir($2))
	pushdef([descr], [$3])

	AC_MSG_CHECKING([for ]autotool[ version])
	AC_MSG_RESULT(v3rs1on)
	AC_DEFINE_UNQUOTED(AUTOTOOL[_VERSION], "v3rs1on", descr)

	popdef([descr])
	popdef([v3rs1on])
	popdef([AUTOTOOL])
	popdef([autotool])
])dnl SXE_CHECK_AUTOTOOL

AC_DEFUN([SXE_CHECK_AUTOTOOLS], [dnl
	## brag about the autotools versions
	m4_ifdef([4UTOCONF_VERSION], [dnl
		SXE_CHECK_AUTOTOOL([autoconf], [4UTOCONF_VERSION],
			[version of autoconf used to build the configure script])])
	m4_ifdef([4UTOHEADER_VERSION], [dnl
		SXE_CHECK_AUTOTOOL([autoheader], [4UTOHEADER_VERSION],
			[version of autoheader used to build the config.h.in])])
	m4_ifdef([4CLOCAL_VERSION], [dnl
		SXE_CHECK_AUTOTOOL([aclocal], [4CLOCAL_VERSION],
			[version of aclocal used to incorporate m4 macros])])
	m4_ifdef([4UTOMAKE_VERSION], [dnl
		SXE_CHECK_AUTOTOOL([automake], [4UTOMAKE_VERSION],
			[version of automake used to build the Makefiles])])
	m4_ifdef([4IBTOOL_VERSION], [dnl
		SXE_CHECK_AUTOTOOL([libtool], [4IBTOOL_VERSION],
			[version of libtool used to provide portable linking])])
])dnl SXE_CHECK_AUTOTOOLS


dnl recommended interface function
AC_DEFUN([SXE_CHECK_BUILDCHAIN], [dnl
	AC_PROG_LN_S
	AC_PROG_AWK
	AC_PROG_EGREP
	AC_PROG_FGREP
	AC_PROG_INSTALL

	SXE_CHECK_AUTOTOOLS
	SXE_CHECK_USER_VARS

dnl 	dnl Soon ...
dnl 	dnl m4_ifdef([LT_INIT], [LT_INIT], [AC_PROG_LIBTOOL])
	AC_PROG_RANLIB
	SXE_PROG_AR
])dnl SXE_CHECK_BUILDCHAIN

dnl recommended interface macro for parser/lexer
AC_DEFUN([SXE_CHECK_PARSER_LEXER], [dnl
	SXE_PROG_BISON
	AC_PROG_LEX
	AM_PROG_LEX
])dnl SXE_CHECK_PARSER_LEXER


dnl sxe-bldchain-progs.m4 ends here
