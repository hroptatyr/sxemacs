dnl sxe-foreign.m4 -- Foreign languages

AC_DEFUN([SXE_LANG_CHECK_XML_CONFIG_BASED], [dnl
	AC_MSG_CHECKING([checking again with xml2-config])
	AC_MSG_RESULT([])

	## hopefully defines XML2_CONFIG
	SXE_SEARCH_CONFIG_PROG([xml2-config])

	if test "$have_xml2_config" = "no" -o -z "$XML2_CONFIG"; then
		AS_MESSAGE([*** xml2-config not found.])
		AS_MESSAGE([*** Cannot check for libxml2.])
		have_xml2_config=no
		XML2_CONFIG=
	fi

	AC_MSG_CHECKING([whether libxml2 is at least 2.0.0])
	if test -n "$XML2_CONFIG"; then
		libxml2_cflags=`$XML2_CONFIG $xml_config_args --cflags`
		libxml2_libs=`$XML2_CONFIG $xml_config_args --libs`
		libxml2_version=`$XML2_CONFIG $xml_config_args --version`
		AC_MSG_RESULT([yes ($libxml2_version)])
	else
		libxml2_version="uncertain"
		AC_MSG_RESULT([no ($libxml2_version)])
		have_libxml2_pkg="uncertain"
	fi
])dnl SXE_LANG_CHECK_XML_CONFIG_BASED

AC_DEFUN([SXE_LANG_CHECK_XML_INCLUDES], [dnl
	## assumes libxml2_cflags is defined
	SXE_DUMP_LIBS
	CPPFLAGS="$libxml2_cflags $CPPFLAGS"
	SXE_CHECK_HEADERS([libxml/tree.h libxml/parser.h libxml/xmlreader.h libxml/xmlwriter.h])
	SXE_RESTORE_LIBS
])dnl SXE_LANG_CHECK_XML_INCLUDES

AC_DEFUN([SXE_LANG_CHECK_XML], [dnl
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([SUCC], [$1])
	pushdef([FAIL], [$2])

	if test -z "$PKG_CONFIG"; then
		SXE_SEARCH_CONFIG_PROG([pkg-config])
	fi

	_SXE_CHECK_pkgconfig_based([libxml2], [libxml-2.0], [2.4.0])
	if test "$have_libxml2_pkg" = "yes"; then
		SXE_LANG_CHECK_XML_INCLUDES
	else
		## try with xml2-config maybe?
		SXE_LANG_CHECK_XML_CONFIG_BASED
		SXE_LANG_CHECK_XML_INCLUDES
	fi

	dnl final check
	AC_MSG_CHECKING([whether libxml2 provides what we need])
	if test "$ac_cv_header_libxml_tree_h" = "yes" -a \
		"$ac_cv_header_libxml_parser_h" = "yes"; then
		have_libxml2="yes"
		LIBXML2_CPPFLAGS=$libxml2_cflags
		LIBXML2_LDFLAGS=$libxml2_libs
		SUCC
	else
		have_libxml2="no"
		LIBXML2_CPPFLAGS=
		LIBXML2_LDFLAGS=
		FAIL
	fi
	AC_MSG_RESULT([$have_libxml2])
	AC_SUBST([LIBXML2_CPPFLAGS])
	AC_SUBST([LIBXML2_LDFLAGS])
	popdef([FAIL])
	popdef([SUCC])
])dnl SXE_LANG_CHECK_XML

dnl sxe-foreign.m4
