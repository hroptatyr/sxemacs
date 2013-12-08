dnl sxe-gui.m4 -- GUI stuff

dnl X cruft
dnl =======

## Oki, now that we really need a more exhaustive way to autodetect
## athena and other widgets we construct a chain.  The elements of
## that chain are simple atomic tests (provided by AC_DEFUNs).
## The chain is defined to be:
##
## neXtaw -> Xaw3d -> XawXpm -> Xaw95  ->  Xaw  ->  Motif  ->  Nirvana
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      ^^^      ^^^^^
##         3d sets                       non-3d   non-athena
##
## Now if the user passes a --with-athena=foo option, we jump
## directly to the according test-suite and in case this fails we jump
## off the cliff.
##
## Attention:
## We shall perform every test-suite twice, once the correct way, and
## once with #include'ing the Xfuncproto header.
## Thanks to the many, many distributions which treat their users
## like braindead zombies!
## Especially, I would like to thank Fedora/RedHat for bringing
## this up. Go on guys, you rock!

define([SXE_XAW_INCLUDES_XFUNCPROTO], [dnl
# if HAVE_XFUNCPROTO_H
# include <X11/Xfuncproto.h>
# endif
])dnl SXE_XAW_INCLUDES_XFUNCPROTO

dnl Check for Xfuncproto first
AC_DEFUN([SXE_XAW_FUNCPROTO], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"
	if test -n "$athena_lib" -a -z "$athena_h_path"; then
		AC_CHECK_HEADER([X11/Xfuncproto.h], [$1])
	fi
	SXE_RESTORE_LIBS
])dnl SXE_XAW_FUNCPROTO

AC_DEFUN([SXE_TEST_THREED_XAW_LIB], [dnl generic 3d library checker
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	if test -z "$athena_lib"; then
		AC_CHECK_LIB([$1], [threeDClassRec], [athena_lib=$1])
	fi
	SXE_RESTORE_LIBS
])dnl SXE_TEST_THREED_XAW_LIB

define([SXE_XAW_INCLUDES_INTRINSIC], [dnl
# if HAVE_X11_INTRINSIC_H
# include <X11/Intrinsic.h>
# include <X11/IntrinsicP.h>
# endif
])dnl SXE_XAW_INCLUDES_INTRINSIC

AC_DEFUN([SXE_TEST_THREED_XAW_INC_INTERNAL], [dnl generic 3d library checker
	## arg1 is the path to the Xaw headers relative to its prefix
	## arg2 is some INCs
	## arg3 is some prefix to the Xaw headers
	pushdef([XAW_PATH], [$1])
	pushdef([XAW_PATH_TL], translit([$1], [/], [_]))
	pushdef([XAW_INCS], [$2])
	pushdef([XAW_PREFIX], [$3])
	pushdef([XAW_PREFIX_TL], translit([$3], [/], [_]))

	if test -n "$athena_lib" -a -z "$athena_h_path"; then
		## The three-d Athena headers are so much more slippery.
		## Curse this `Lets replace standard libraries' thing that they did. :/
		## unset the values before we get our dirty hands on them
		unset "ac_cv_header_[]XAW_PREFIX_TL[]XAW_PATH_TL[]_XawInit_h"
		unset "ac_cv_header_[]XAW_PREFIX_TL[]XAW_PATH_TL[]_ThreeD_h"

		SXE_DUMP_LIBS
		CPPFLAGS="$CPPFLAGS $X_CFLAGS"
		LDFLAGS="$LDFLAGS $X_LIBS"
		SXE_CHECK_HEADERS(XAW_PREFIX[]XAW_PATH[/XawInit.h],
			[], [], XAW_INCS SXE_XAW_INCLUDES_INTRINSIC)
		SXE_CHECK_HEADERS(XAW_PREFIX[]XAW_PATH[/ThreeD.h],
			[], [], XAW_INCS)
		if test "$ac_cv_header_[]XAW_PREFIX_TL[]XAW_PATH_TL[]_XawInit_h" -a \
			"$ac_cv_header_[]XAW_PREFIX_TL[]XAW_PATH_TL[]_ThreeD_h"; then
			athena_h_path="XAW_PREFIX[]XAW_PATH"
		fi
		SXE_RESTORE_LIBS
	fi

	popdef([XAW_PATH])
	popdef([XAW_PATH_TL])
	popdef([XAW_INCS])
	popdef([XAW_PREFIX])
	popdef([XAW_PREFIX_TL])
])dnl SXE_TEST_THREED_XAW_INC_INTERNAL

AC_DEFUN([SXE_TEST_THREED_XAW_INC], [dnl generic 3d library checker
	## arg1 is <dunno>
	## arg2 are some INCs
	pushdef([XAW_INCS], [$2])

	SXE_TEST_THREED_XAW_INC_INTERNAL([$1], XAW_INCS, [X11/])
	SXE_TEST_THREED_XAW_INC_INTERNAL([$1], XAW_INCS)
	SXE_TEST_THREED_XAW_INC_INTERNAL([$1], XAW_INCS, [X11/Xaw/])

	popdef([XAW_INCS])
])dnl SXE_TEST_THREED_XAW_INC

AC_DEFUN([SXE_TEST_THREED_XAW_GENERIC], [dnl
	SXE_TEST_THREED_XAW_LIB([$1], [$2])
	SXE_TEST_THREED_XAW_INC([$1])
	SXE_XAW_FUNCPROTO()
	SXE_TEST_THREED_XAW_INC([$1], [SXE_XAW_INCLUDES_XFUNCPROTO])
])dnl SXE_TEST_THREED_XAW_GENERIC

AC_DEFUN([SXE_TEST_XAW_LIB], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	if test -z "$athena_lib"; then
		AC_CHECK_LIB([Xaw], [XawScrollbarSetThumb], [athena_lib="Xaw"])
	fi
	SXE_RESTORE_LIBS
])dnl SXE_TEST_XAW_LIB

AC_DEFUN([SXE_TEST_XAW_INC], [dnl
	## arg1 are some additional INCLUDES

	## unset the values before we get our dirty hands on them
	unset "ac_cv_header_X11_Xaw_XawInit_h"

	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	if test -n "$athena_lib" -a -z "$athena_h_path"; then
		AC_CHECK_HEADERS([X11/Xaw/XawInit.h],
			[athena_h_path="X11/Xaw"], [],
			[$1 SXE_XAW_INCLUDES_INTRINSIC])
	fi
	SXE_RESTORE_LIBS
])dnl SXE_TEST_XAW_INC

dnl Worker macros:
AC_DEFUN([SXE_TEST_NEXTAW], [SXE_TEST_THREED_XAW_GENERIC([neXtaw], [$1])])
AC_DEFUN([SXE_TEST_XAW3D], [SXE_TEST_THREED_XAW_GENERIC([Xaw3d], [$1])])
AC_DEFUN([SXE_TEST_XAWXPM], [SXE_TEST_THREED_XAW_GENERIC([XawXpm], [$1])])
AC_DEFUN([SXE_TEST_XAW95], [SXE_TEST_THREED_XAW_GENERIC([Xaw95], [$1])])
AC_DEFUN([SXE_TEST_XAW], [dnl
	## check if libXaw is a 3d set actually
	## no, we don't ... it's bullshit
	## if some distro handles it this way, i simply shrug it off.
	##
	## SXE_TEST_THREED_XAW_GENERIC(Xaw, $1)

	## the Xaw chain
	SXE_TEST_XAW_LIB
	SXE_TEST_XAW_INC()
	SXE_XAW_FUNCPROTO()
	SXE_TEST_XAW_INC([SXE_XAW_INCLUDES_XFUNCPROTO])
])dnl SXE_TEST_XAW

AC_DEFUN([SXE_TEST_MOTIF], [dnl
	## autodetect Motif - but only add to libs_x later (if necessary)
	## Use a different function to the earlier test to avoid problems with the
	## internal cache.
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	SXE_CHECK_HEADERS([Xm/Xm.h])

	if test "$ac_cv_header_Xm_Xm_h" = "yes"; then
		AC_CHECK_LIB([Xm], [XmStringCreate],
			[have_motif=yes], [have_motif=no])
	else
		have_motif=no
	fi
	SXE_RESTORE_LIBS

	if test "$have_motif" = "yes"; then
		dnl autodetect lesstif
		AC_MSG_CHECKING([for Lesstif])
		AC_EGREP_CPP([yes],
[#include <Xm/Xm.h>
#ifdef LESSTIF_VERSION
yes
#endif
], [have_lesstif=yes], [have_lesstif=no])
		AC_MSG_RESULT([$have_lesstif])
	fi
])dnl SXE_TEST_MOTIF


AC_DEFUN([SXE_CHECK_XTOOLKITS], [
	AC_MSG_CHECKING([for X11 graphics libraries])
	AC_MSG_RESULT([])

	## firstly, check for the Xlib header
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	AC_CHECK_HEADERS([X11/Xlib.h])

	# Check for XkbKeycodeToKeysym to avoid XKeycodeToKeysym which is deprecated
	AC_CHECK_HEADERS([X11/XKBlib.h])
	if test "$ac_cv_header_X11_XKBlib_h" = "yes"; then
		AC_CHECK_FUNC([XkbKeycodeToKeysym], [acx_xkbkeycodetokeysym=yes])
                if test "$acx_xkbkeycodetokeysym" = "yes"; then
			AC_DEFINE([HAVE_XKBKEYCODETOKEYSYM],[1],[Has XkbKeycodeToKeysym])
		fi
        fi
	SXE_RESTORE_LIBS

	## assume 3d first
	athena_3d=yes
	athena_lib=
	athena_h_path=
	## initialise our have-vars
	have_athena=no
	have_motif=no

	case "$with_athena" in
	## autodetect, that is walk through the chain given above
	"auto" | "")
		AC_MSG_CHECKING([for the Athena widgets])
		AC_MSG_RESULT([])

		## ignite the chain :)
		SXE_TEST_NEXTAW
		SXE_TEST_XAW3D
		SXE_TEST_XAWXPM
		SXE_TEST_XAW95
		SXE_TEST_XAW
		SXE_TEST_MOTIF
		;;

	"3d")
		SXE_TEST_XAW3D
		;;
	"next")
		SXE_TEST_NEXTAW
		;;
	"95")
		SXE_TEST_XAW95
		;;
	"xpm")
		SXE_TEST_XAWXPM
		;;

	## This is the default, old fashioned flat Athena.
	"xaw")
		SXE_TEST_XAW
		;;

	## force the motif chain
	"no")
		SXE_TEST_MOTIF
		;;
	*)
		SXE_DIE(["Unknown Athena widget set \`$with_athena'. This should not happen."])
		;;
	esac

	## Do we actually have a usable Athena widget set? Please?
	if test -n "$athena_lib" -a -n "$athena_h_path"; then
		if test "athena_lib" = "Xaw"; then
			athena_3d=no
		fi
		have_xaw=yes
	else
		have_xaw=no
		athena_3d=no
	fi

	## Finish ensuring that we have values for the various toolkit items.
	## Not all toolkits support all widgets
	## We gotta rewrite several defaults, now that we know that we either
	## have athena or motif
	if test "$have_xaw" = "yes" -a "$have_motif" = "yes"; then
		## nothing's wrong in this case, we use the lucid menubar anyway
		if test "$with_menubars" = "athena"; then
			with_menubars="lucid"
		fi

	elif test "$have_xaw" = "yes" -a "$have_motif" = "no"; then
		with_dialogs="athena"
		with_widgets="athena"
		with_menubars="lucid"

	elif test "$have_xaw" = "no" -a "$have_motif" = "yes"; then
		with_dialogs="motif"
		with_widgets="motif"
		with_menubars="lucid"
		with_scrollbars="lucid"

	elif test "$have_xaw" = "no" -a "$have_motif" = "no"; then
		AC_MSG_ERROR([You seem to have no usable X toolkits!
However, SXEmacs runs fine even without X, but this may not be what
you want.  If you explicitly want this, rerun configure with --without-x])

	else
		SXE_DIE("Now I feel dizzy! I know where the error happened but I won't tell!")
	fi

])dnl SXE_CHECK_XTOOLKITS

AC_DEFUN([SXE_SUFFICIENT_ATHENA_P], [dnl
	## arg #1 is the symbol with all the tk widgets
	pushdef([tmpwidgets], ifelse($1,,[all_widgets],$1))

	all_widgets="$with_menubars $with_scrollbars $with_dialogs $with_toolbars $with_widgets"
	case "$[]tmpwidgets[]" in
	*athena* )
		if test "$have_xaw" != "yes"; then
			SXE_DIE(["Could not find a suitable Athena library to build with."])
		fi

		dnl Add the Lucid widget Athena code
		SXE_APPEND([lwlib-Xaw.o], [lwlib_objs])

		dnl Add the Athena widget library we located earlier
		SXE_PREPEND([-l$athena_lib], [libs_x])

		dnl Tell lwlib where to find the Athena header files.
		dnl Many people have tried to create a `smart' way of doing this,
		dnl but all have failed.  Before changing the following ugly definitions,
		dnl consult the veterans of many a battle.
		AC_DEFINE_UNQUOTED([ATHENA_Scrollbar_h_], ["$athena_h_path/Scrollbar.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_Dialog_h_], ["$athena_h_path/Dialog.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_Form_h_], ["$athena_h_path/Form.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_Command_h_], ["$athena_h_path/Command.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_Label_h_], ["$athena_h_path/Label.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_LabelP_h_], ["$athena_h_path/LabelP.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_Toggle_h_], ["$athena_h_path/Toggle.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_ToggleP_h_], ["$athena_h_path/ToggleP.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_AsciiText_h_], ["$athena_h_path/AsciiText.h"],
			[Description here!])
		AC_DEFINE_UNQUOTED([ATHENA_XawInit_h_], ["$athena_h_path/XawInit.h"],
			[Description here!])

		AC_DEFINE([LWLIB_USES_ATHENA], [1], [Description here!])
		AC_DEFINE([NEED_ATHENA], [1], [Description here!])
		need_athena="yes"

		if test "$athena_3d" = "yes"; then
			AC_DEFINE([HAVE_ATHENA_3D], [1], [Description here!])
		fi
		;;
	esac

	## objects to add?
	case "$with_widgets" in
	athena* )
		SXE_APPEND([xlwradio.o xlwcheckbox.o xlwgauge.o], [lwlib_objs])
		;;
	esac

	## final declarations for our config header
	if test "$with_scrollbars" = "athena"; then
		AC_DEFINE([LWLIB_SCROLLBARS_ATHENA], [1], [Description here!])
	fi
	if test "$with_dialogs" = "athena"; then
		AC_DEFINE([LWLIB_DIALOGS_ATHENA], [1], [Description here!])
	fi

	if test "$athena_3d" = "yes" -a \
		"$with_scrollbars" = "athena"; then
		AC_DEFINE([LWLIB_SCROLLBARS_ATHENA3D], [1], [Description here!])
	fi
	if test "$athena_3d" = "yes" -a \
		"$with_dialogs" = "athena"; then
		AC_DEFINE([LWLIB_DIALOGS_ATHENA3D], [1], [Description here!])
	fi
	case "$with_widgets" in
	athena* )
		AC_DEFINE([LWLIB_WIDGETS_ATHENA], [1], [Description here!])
		;;
	esac

	popdef([tmpwidgets])
])dnl SXE_SUFFICIENT_ATHENA_P

AC_DEFUN([SXE_SUFFICIENT_MOTIF_P], [dnl
	## arg #1 is the symbol with all the tk widgets
	pushdef([tmpwidgets], ifelse($1,,[all_widgets],$1))

	all_widgets="$with_menubars $with_scrollbars $with_dialogs $with_toolbars $with_widgets"
	case "$[]tmpwidgets[]" in
	*motif* )
		AC_DEFINE([LWLIB_USES_MOTIF], [1], [Description here!])
		AC_DEFINE([NEED_MOTIF], [1], [Description here!])
		SXE_APPEND([lwlib-Xm.o], [lwlib_objs])
		need_motif=yes
		;;
	esac

	## objects to add?
	if test "$with_menubars" = "motif"; then
		SXE_APPEND([xlwmenu.o], [lwlib_objs])
	fi

	## final declarations for our config header
	if test "$with_menubars" = "motif"; then
		AC_DEFINE([LWLIB_MENUBARS_MOTIF], [1], [Description here!])
	fi
	if test "$with_scrollbars" = "motif"; then
		AC_DEFINE([LWLIB_SCROLLBARS_MOTIF], [1], [Description here!])
	fi
	if test "$with_dialogs" = "motif"; then
		AC_DEFINE([LWLIB_DIALOGS_MOTIF], [1], [Description here!])
	fi
	if test "$with_widgets" = "motif"; then
		AC_DEFINE([LWLIB_WIDGETS_MOTIF], [1], [Description here!])
	fi

	popdef([tmpwidgets])
])dnl SXE_SUFFICIENT_MOTIF_P

AC_DEFUN([SXE_SUFFICIENT_LUCID_P], [dnl
	## arg #1 is the symbol with all the tk widgets
	pushdef([tmpwidgets], ifelse($1,,[all_widgets],$1))

	all_widgets="$with_menubars $with_scrollbars $with_dialogs $with_toolbars $with_widgets"
	case "$[]tmpwidgets[]" in
	*lucid* )
		AC_DEFINE([NEED_LUCID], [1], [Description here!])
		SXE_APPEND([lwlib-Xlw.o], [lwlib_objs])
		;;
	esac

	## objects to add?
	if test "$with_menubars" = "lucid"; then
		SXE_APPEND([xlwmenu.o], [lwlib_objs])
	fi
	if test "$with_scrollbars" = "lucid"; then
		SXE_APPEND([xlwscrollbar.o], [lwlib_objs])
	fi

	## final declarations for our config header
	if test "$with_menubars" = "lucid"; then
		AC_DEFINE([LWLIB_MENUBARS_LUCID], [1], [Description here!])
	fi
	if test "$with_scrollbars" = "lucid"; then
		AC_DEFINE([LWLIB_SCROLLBARS_LUCID], [1], [Description here!])
	fi

	popdef([tmpwidgets])
])dnl SXE_SUFFICIENT_LUCID_P

AC_DEFUN([SXE_CHECK_XT_DEPENDENCIES], [dnl

	SXE_SUFFICIENT_ATHENA_P
	SXE_SUFFICIENT_MOTIF_P
	SXE_SUFFICIENT_LUCID_P

	all_widgets="$with_menubars $with_scrollbars $with_dialogs $with_toolbars $with_widgets"
	if test "$with_x11" = "yes"; then

		if test "$with_menubars" != "no"; then
			SXE_ADD_SXEUIX11_OBJS([menubar-x.o])
		fi
		if test "$with_scrollbars" != "no"; then
			SXE_ADD_SXEUIX11_OBJS([scrollbar-x.o])
		fi
		if test "$with_dialogs" != "no"; then
			SXE_ADD_SXEUIX11_OBJS([dialog-x.o])
		fi
		if test "$with_toolbars" != "no"; then
			SXE_ADD_SXEUIX11_OBJS([toolbar-x.o])
		fi
		if test "$all_widgets" != "no no no no no"; then
			SXE_ADD_SXEUIX11_OBJS([gui-x.o])
		fi
	fi

])dnl SXE_CHECK_XT_DEPENDENCIES

AC_DEFUN([SXE_CHECK_UI_SUFFICIENCY], [dnl
	## common-to-all stuff
	if test "$with_menubars" != "no"; then
		AC_DEFINE([HAVE_MENUBARS], [1], [Description here!])
		AC_DEFINE([HAVE_POPUPS], [1], [Description here!])
		SXE_ADD_SXEUI_OBJS([menubar.o])
	fi
	if test "$with_scrollbars" != "no"; then
		AC_DEFINE([HAVE_SCROLLBARS], [1], [Description here!])
		SXE_ADD_SXEUI_OBJS([scrollbar.o])
	fi
	if test "$with_dialogs" != "no"; then
		AC_DEFINE([HAVE_DIALOGS], [1], [Description here!])
		AC_DEFINE([HAVE_POPUPS], [1], [Description here!])
		SXE_ADD_SXEUI_OBJS([dialog.o])
	fi
	if test "$with_toolbars" != "no"; then
		AC_DEFINE([HAVE_TOOLBARS], [1], [Description here!])
		SXE_ADD_SXEUI_OBJS([toolbar.o])
	fi
	if test "$with_widgets" != "no"; then
		AC_DEFINE([HAVE_WIDGETS], [1], [Description here!])
		AC_DEFINE([LWLIB_TABS_LUCID], [1], [Description here!])
		SXE_APPEND([xlwtabs.o xlwgcs.o], [lwlib_objs])
	fi
	AC_SUBST([lwlib_objs])
	SXE_SUBST_SXEUI_OBJS
	SXE_SUBST_SXEUITTY_OBJS
	SXE_SUBST_SXEUIX11_OBJS

])dnl SXE_CHECK_UI_SUFFICIENCY

dnl sxe-gui.m4 ends here
