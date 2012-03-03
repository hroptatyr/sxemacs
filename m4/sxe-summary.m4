dnl sxe-summary.m4 -- print a nice summary describing the build
dnl
dnl Copyright (C) 2005 Malcolm Purvis.
dnl Copyright (C) 2005, 2006, 2007 Steve Youngs.
dnl Copyright (C) 2006, 2007, 2008 Sebastian Freundt.
dnl
dnl This file is part of SXEmacs

dnl Start stdout redirection to '| tee -a Installation'
AC_DEFUN([SXE_SUMMARY], [dnl
	pushdef([CODENAME], m4_ifdef([SXEM4CS_CODENAME],
		[SXEM4CS_CODENAME], [unknown]))

(
echo "

SXEmacs $sxemacs_git_version \"[]CODENAME[]\" configured for \`$ac_cv_build'.
"
echo "
Compilation Environment and Installation Defaults:"
echo "  Source code location:              $sxe_srcdir"
echo "  Installation prefix:               $prefix"
echo "  Arch-dependent files go to:        $ARCHLIBDIR"
echo "  Core emodules go to:               $MODULEDIR"
echo "  Core lisp files go to:             $LISPDIR"
echo "  Additional external data goes to:  $ETCDIR"

if test -n "$opsysfile"
then echo "  Operating system description file: \`$opsysfile'"
else echo "  Not using any operating system description file"
fi
if test -n "$machfile"
then echo "  Machine description file:          \`$machfile'"
else echo "  Not using any machine description file"
fi

echo "  Compiler version:                  $compiler_version"
if test -n "$gcc_compiler_specs"; then
  echo "    - GCC specs file:                $gcc_compiler_specs"
fi
echo "    - Compiler command:              $CC $CFLAGS"
echo "    - Global CPPFLAGS:               $CPPFLAGS"
echo "    - Global LDFLAGS:                $LDFLAGS"
echo "    - C flags"
echo "              System:                $c_switch_system"
echo "              General:               $c_switch_general"
echo "              Window system:         $c_switch_window_system"
echo "              All:                   $c_switch_all"
echo "    - Linker flags"
echo "              System:                $ld_switch_system"
echo "              General:               $ld_switch_general"
echo "              Window system:         $ld_switch_window_system"
echo "              All:                   $ld_switch_all"
echo "    - Linked libraries:"
echo "              System:                $ld_libs_system"
echo "              General:               $ld_libs_general"
echo "              Window system:         $ld_libs_window_system"
echo "              All:                   $ld_libs_all"
echo ""
echo "  libc version:                      $libc_version"
echo "  Relocating allocator for buffers:  $with_rel_alloc"
echo "  GNU version of malloc:             ${GNU_MALLOC}${GNU_MALLOC_reason}"
case "$ld_switch_site" in
  *nocombreloc*) echo "  Linking with \`-z nocombreloc'.
    - Consider configuring with --with-pdump." ;;
esac
echo "
Build Options:"

if test -n "$with_site_includes"; then
  echo "  Additional header files:           $with_site_includes"
fi
if test -n "$with_site_libraries"; then
  echo "  Additional libraries:              $with_site_libraries"
fi
if test -n "$with_site_prefixes"; then
  echo "  Additional prefixes:               $with_site_prefixes"
fi
if test -n "$runpath"; then
  echo "  Runtime library search path:       $runpath"
fi

echo "  Runtime behaviour:"
if test "$with_prefix" = "yes"; then
  echo "    - Value of prefix ($prefix) is compiled into the binary."
elif test "$with_prefix" = "no"; then
  echo "    - Value of prefix ($prefix)is not compiled into the binary."
fi
if test "$with_modules" != "no"; then
  echo "
    - Module search path:"
  echo ${MODULE_PATH} | ${AWK-awk} 'BEGIN {RS=":"} { print "       ",[$]1[] }'
fi
echo "
    - Package search path:"
  echo ${PACKAGE_PATH} | ${AWK-awk} 'BEGIN {RS=":"} { print "       ",[$]1[] }'

echo "
Debugging options:"

OG_MULTIARG_SUMMARY([error-checking], [Runtime Error Checking], [  ])

if test "$tmp_enabled" != "None."; then
  echo "    WARNING: ---------------------------------------------------------"
  echo "    WARNING: SXEmacs will run noticeably more slowly as a result."
  echo "    WARNING: Error checking is on by default for SXEmacs beta releases."
  echo "    WARNING: ---------------------------------------------------------"
elif test "${sxemacs_betaname}" != ""; then
  echo "    WARNING: ---------------------------------------------------------"
  echo "    WARNING: This SXEmacs is a beta release."
  echo "    WARNING: By disabling all error checking there will be limited "
  echo "    WARNING: troubleshooting information available."
  echo "    WARNING: ---------------------------------------------------------"
fi
unset tmp_enabled


echo "
Internals:"

if test "$with_dynamic" = "yes"; then
  echo "  How to link external libraries:  dynamic"
elif test "$with_dynamic" = "no"; then
  echo "  How to link external libraries:  static"
else
  echo "  How to link external libraries:  uncertain"
fi

echo "  Foreign Function Interface:"
if test "$have_ffi" = yes; then
  echo "    Compiling in support for FFI."
elif test "$have_ffi" = no; then
  echo "    No support for FFI."
else
  echo "    Good question ... where is it?"
fi

if test "$have_compre" = yes; then
  echo "  Compiled regex caching: yes"
else
  echo "  Compiled regex caching: no"
fi

dnl summary for ENT and ASE
OG_MULTIARG_SUMMARY([ent], [Enhanced Number Types], [  ])
dnl OG_MULTIARG_SUMMARY([ase], [Algebraic Structures based on ENT], [  ])
dnl summary for EF
OG_MULTIARG_SUMMARY([experimental-features], [Experimental Features], [  ])

echo "
Window System:"
if test "$with_x11" = "yes"; then
  echo "  Compiling in support for the X window system:"
  echo "    - X Windows headers location:                 $x_includes"
  echo "    - X Windows libraries location:               $x_libraries"
  if test "$with_xauth" != yes; then
    echo "    - Xau (X authority) not available."
  fi
  if test "$with_xmu" != yes; then
    echo "    - Xmu library not available; substituting equivalent routines."
  fi
  if test "$with_wmcommand" != no; then
    echo "    - Handling WM_COMMAND properly."
  fi
fi
if test "$need_athena" = "yes"; then
  echo "  Compiling in support for the Athena widget set:"
  echo "    - Athena headers location:                    $athena_h_path"
  echo "    - Athena library to link:                     $athena_lib"
fi
case "$with_menubars" in
  lucid ) echo "  Using Lucid menubars." ;;
  motif ) echo "  Using Motif menubars."
	  echo "  *WARNING*  The Motif menubar implementation is currently buggy."
	  echo "             We recommend using the Lucid menubar instead."
	  echo "             Re-run configure with --with-menubars='lucid'." ;;
  * )      echo "  No support for menubars."   ;;
esac
case "$with_scrollbars" in
  lucid  ) echo "  Using Lucid scrollbars."      ;;
  motif  ) echo "  Using Motif scrollbars."      ;;
  athena ) echo "  Using Athena scrollbars."     ;;
  * )      echo "  No support for scrollbars."   ;;
esac
case "$with_dialogs" in
  motif  ) echo "  Using Motif dialog boxes."
	   if test "$unexec" = "unexaix.o"; then if test "`uname -v`" = 4 -a "`uname -r`" -ge 3; then
	     echo "  *WARNING*  The Motif dialog boxes cause problems on AIX 4.3 and higher."
	     echo "             We recommend using the Athena dialog boxes instead."
	     echo "             Install libXaw and re-run configure with --with-dialogs='athena'."
	     echo "             Read the PROBLEMS file for more information."
	   fi; fi ;;
  athena ) echo "  Using Athena dialog boxes."     ;;
  * )      echo "  No support for dialog boxes." ;;
esac
case "$with_widgets" in
  motif  ) echo "  Using Motif native widgets."      ;;
  athena ) echo "  Using Athena native widgets."     ;;
  * )      echo "  No support for native widgets."   ;;
esac
case "$with_toolbars" in
  yes )       echo "  Support for toolbars."         ;;
  no )        echo "  No support for toolbars."      ;;
  * ) ;;
esac

echo "
TTY:"
test "$with_ncurses" = yes && echo "  Compiling in support for ncurses."
test "$with_curses" = yes && echo "  Compiling in support for curses."
test "$with_terminfo" = yes && echo "  Compiling in support for terminfo ."
test "$with_termlib" = yes && echo "  Compiling in support for termlib."
test "$with_termcap" = yes && echo "  Compiling in support for termcap."
test "$with_gpm" = yes && echo "  Compiling in support for GPM (General Purpose Mouse)."

echo "
Databases:"
OG_MULTIARG_SUMMARY([database], [File-based Databases], [  ])

echo "  Compiling in support for further database interfaces:"
if test "$have_ldap" = "yes"; then
  echo $ECHO_N "    - LDAP"$ECHO_C
  if test "$have_ldap_lber" = "yes" -o "$have_ldap_krb" -o "$have_ldap_des"; then
	echo $ECHO_N " (with"$ECHO_C
	test "$have_ldap_lber" = "yes" && echo $ECHO_N " lber"$ECHO_C
	test "$have_ldap_krb" = "yes" && echo $ECHO_N " krb"$ECHO_C
	test "$have_ldap_des" = "yes" && echo $ECHO_N " des"$ECHO_C
	echo $ECHO_N ")"$ECHO_C
  fi
  echo "."
fi
if test "$have_postgresql" = "yes"; then
  echo $ECHO_N "    - PostgreSQL"$ECHO_C
  test "$have_postgresqlv7" = yes && \
	echo $ECHO_N " (V7 bindings)"$ECHO_C
  echo "."
fi

echo "
Media:"
dnl
OG_MULTIARG_SUMMARY([image], [Image Formats], [  ])
OG_MULTIARG_SUMMARY([sound], [Audio Outputs], [  ])
test "$with_pulseaudio" = yes && echo "    PulseAudio has been enabled.  Good luck!"
OG_MULTIARG_SUMMARY([media], [Media Stream Handlers], [  ])

dnl old stuff ... originally these were elifs
dnl if test "$with_x11" = yes; then
dnl   echo "    WARNING: -----------------------------------------------------------"
dnl   echo "    WARNING: Compiling without XPM image support."
dnl   if test "$xpm_problem" != ""; then
dnl     echo "    Reason: $xpm_problem"
dnl   fi
dnl   echo "    WARNING: You should strongly consider installing XPM."
dnl   echo "    WARNING: Otherwise toolbars and other graphics will look suboptimal."
dnl   echo "    WARNING: -----------------------------------------------------------"
dnl fi
dnl if test "$window_system" != "none"; then
dnl   echo "    WARNING: -----------------------------------------------------------"
dnl   echo "    WARNING: Compiling without PNG image support."
dnl   if test "$png_problem" != ""; then
dnl     echo "    Reason: $png_problem"
dnl   fi
dnl   echo "    WARNING: You should strongly consider installing the PNG libraries."
dnl   echo "    WARNING: Otherwise certain images and glyphs may not display."
dnl   echo "    WARNING: -----------------------------------------------------------"
dnl fi

echo "
Cryptography:"
if test "$have_openssl $with_openssl" = "yes yes"; then
  echo "  Compiling in support for OpenSSL ciphers and digests."
  echo "    - Submodules: RAND MD HMAC CIPHER HYBRID SIGN" \
  `test "$openssl_no_rsa" = no && echo "RSA"` \
  `test "$openssl_no_dsa" = no && echo "DSA"` \
  `test "$openssl_no_ec" = no && echo "EC"` \
  `test "$openssl_no_dh" = no && echo "DH"` \
  "PEM" \
  `test "$openssl_ssl" = yes && echo "SSL"`
fi

echo "
Internationalization:"
test "$with_mule" = yes && echo "  Compiling in support for Mule (multi-lingual Emacs)."
test "$with_file_coding" = yes && echo "  Compiling in support for file coding."
test "$with_xim" != no && echo "  Compiling in support for XIM (X11R5+ I18N input method)."
test "$with_xim" = motif && echo "    - Using Motif to provide XIM support."
test "$with_xim" = xlib && echo "    - Using raw Xlib to provide XIM support."
test "$with_xfs" = yes && echo "    - Using XFontSet to provide bilingual menubar."
test "$with_canna" = yes && echo "  Compiling in support for Canna on Mule."
if test "$with_wnn" = yes; then
  echo "  Compiling in support for the WNN input method on Mule."
  test "$with_wnn6" = yes && echo "    - Using WNN version 6."
fi

echo "
Mail:"
test "$with_pop" = yes && echo "  Compiling in support for POP mail retrieval."
test "$with_kerberos" = yes && echo "  Compiling in support for Kerberos POP authentication."
test "$with_hesiod" = yes && echo "  Compiling in support for Hesiod POP server access."
test -n "$with_mail_locking" && echo "  Compiling in support for \"$with_mail_locking\" mail spool file locking method."

echo "
Modules:"
OG_MULTIARG_SUMMARY([modules], [Dynamic Shared Object Modules], [  ])
OG_MULTIARG_SUMMARY([static-modules], [Static Modules], [  ])

echo "
Other Features:"
test "$with_ipv6_cname" = no && echo "  Inhibiting IPv6 canonicalization at startup."
test "$with_socks" = yes && echo "  Compiling in support for SOCKS."
test "$with_dnet" = yes && echo "  Compiling in support for DNET."
if test "$with_regex_malloc" = no; then
  echo "  WARNING: -----------------------------------------------------------"
  echo "  Using alloca to allocate the failure stack."
  echo "  It may be impossible to detect stack exhaustion, and you will crash."
  echo "  Do NOT use this build of SXEmacs for ordinary work."
  echo "  WARNING: -----------------------------------------------------------"
fi
test "$with_pdump" = yes && echo "  Using the new portable dumper."
test "$with_debug" = yes && echo "  Compiling in support for extra debugging code."
test "$usage_tracking" = yes && echo "  Compiling in support for active usage tracking (Sun internal)."
echo ""

## explanation for flags
echo "Footnotes:"
echo "  + means not requested but enabled"
echo "  * means requested but disabled"
echo ""

## bogus configure opts
if test -n "$bogus_cmd_opts"; then
	echo "The following option were unrecognised and will be ignored:
----------------------------------------------------------"
	echo "$bogus_cmd_opts"
	echo ""
	echo "I'm tempted to remove the entire directory now ..."
	echo "... fortunately I think I'm in a good mood, so have a nice build anyway."
else
	echo "No bogus options. Have a nice build :)"
fi
echo ""
) | tee -a Installation
dnl echo "The above configure report is appended to \"Installation\" file."

	popdef([CODENAME])
])dnl SXE_SUMMARY

dnl sxe-summary.m4 ends here
