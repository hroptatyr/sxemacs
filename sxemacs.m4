dnl sxemacs.m4 --- build chain macros
dnl
dnl Copyright (C) 1998, 1999 J. Kean Johnston.
dnl Copyright (C) 2005, 2006 Sebastian Freundt
dnl
dnl Author: J. Kean Johnston <jkj@sco.com>, based on work in libtool.
dnl Author: Sebastian Freundt <hroptatyr@sxemacs.org>
dnl
dnl This file is part of SXEmacs.
dnl
dnl There are several things we care about here. First, we need to find
dnl out how we create an executable that has its symbols exported, so
dnl that dynamically loaded modules have access to the internal XEmacs
dnl symbols. This is stored in ``ld_dynamic_link_flags'' and is used
dnl in the main Makefile.
dnl Next, we need to know how we compile actual shared libraries, and
dnl the objects in them.  For these purposes, we need to determine the
dnl C compiler flags used to produce shared objects (``dll_cflags''),
dnl what linker to use to create the final shared object that will be
dnl loaded (``dll_ld'') and the flags to pass to that linker
dnl (``dll_ldflags''). This information is used by ellcc to build up
dnl the command line when compiling modules. We build up two other commands
dnl for extremely weird systems where special things need to be done.
dnl The first is ``dll_ldo'', which is the flag used to specify the output
dnl file name, and the second is ``dll_post'' which is inserted after the
dnl list of objects.
dnl After all of this, we should be able to:
dnl    $(CC) $(CFLAGS) $(dll_cflags) -c module.c
dnl to produce a single shared object
dnl And then:
dnl   $(dll_ld) $(dll_ldflags) $(dll_ldo) module.ell module.o $(dll_post)
dnl to create the loadable shared library.
dnl
dnl NOTE: In the code below, where I have modified things to work with
dnl XEmacs, we use $canonical instead of libtool's $host, and we use
dnl $internal_configuration instead of $host_alias. To make typing
dnl shorter we assign these to $xehost and $xealias

AC_DEFUN([SXE_CHECK_DLOPEN], [
	have_dlopen=no
	try_on=yes
	AC_CHECK_HEADERS([dlfcn.h])

	if test "$ac_cv_header_dlfcn_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -lc])
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -ldl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -ldl"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for dlopen in -lsvld])
		LIBS="$ac_save_LIBS -lsvld"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dlfcn.h>
			]],
			[[dlopen ("", 0);]])], [
			AC_MSG_RESULT([yes])
			have_dlopen=yes], [
			AC_MSG_RESULT([no])
			have_dlopen=no])
	fi

	if test "$have_dlopen" = "yes"; then
		AC_DEFINE([HAVE_DLOPEN], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_DL

AC_DEFUN([SXE_CHECK_SHL_LOAD], [
	dnl Check for HP/UX shl_load
	have_shl_load=no
	try_on=yes
	AC_CHECK_HEADERS([dl.h])

	if test "$ac_cv_header_dl_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for shl_load in -lc])
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dl.h>
			]],
			[[shl_load ("", 0, 0);]])], [
			AC_MSG_RESULT([yes])
			have_shl_load=yes], [
			AC_MSG_RESULT([no])
			have_shl_load=no])
	fi

	if test "$have_shl_load" = "yes" -o "$try_on" = "no"; then
		try_on=no
	else
		AC_MSG_CHECKING([for shl_load in -ldl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -ldld"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <dl.h>
			]],
			[[shl_load ("", 0, 0);]])], [
			AC_MSG_RESULT([yes])
			have_shl_load=yes], [
			AC_MSG_RESULT([no])
			have_shl_load=no])
	fi

	if test "$have_shl_load" = "yes"; then
		AC_DEFINE([HAVE_SHL_LOAD], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_SHL_LOAD

AC_DEFUN([SXE_CHECK_LT_DLINIT], [
	dnl Check for libtool's libltdl
	have_lt_dlinit=no
	try_on=yes
	AC_CHECK_HEADERS([ltdl.h])

	if test "$ac_cv_header_ltdl_h" != "yes"; then
		try_on=no
	else
		AC_MSG_CHECKING([for lt_dlinit in -lltdl])
		ac_save_LIBS="$LIBS"
		LIBS="$LIBS -lltdl"
		AC_LINK_IFELSE([AC_LANG_PROGRAM(
			[[
			#include <ltdl.h>
			]],
			[[lt_dlinit ();]])], [
			AC_MSG_RESULT([yes])
			have_lt_dlinit=yes], [
			AC_MSG_RESULT([no])
			have_lt_dlinit=no])
	fi

	if test "$have_lt_dlinit" = "yes"; then
		AC_DEFINE([HAVE_LTDL], [1], [Description here!])
		have_dl=yes
	else
		LIBS="$ac_save_LIBS"
	fi
])dnl SXE_CHECK_LT_DLINIT

AC_DEFUN([SXE_SHLIB_STUFF],[
dll_ld=
dll_ldflags=
dll_cflags=
dll_post=
dll_ldo="-o"
ld_dynamic_link_flags=
xehost=$ac_cv_build
xealias=$ac_cv_build_alias

AC_MSG_CHECKING([how to build dynamic libraries for ${xehost}])
AC_MSG_RESULT([])
# Transform *-*-linux* to *-*-linux-gnu*, to support old configure scripts.
case "$xehost" in
*-*-linux-gnu*) ;;
*-*-linux*) xehost=`echo $xehost | sed 's/^\(.*-.*-linux\)\(.*\)$/\1-gnu\2/'`
esac

xehost_cpu=`echo $xehost | sed 's/^\([[^-]]*\)-\([[^-]]*\)-\(.*\)$/\1/'`
xehost_vendor=`echo $xehost | sed 's/^\([[^-]]*\)-\([[^-]]*\)-\(.*\)$/\2/'`
xehost_os=`echo $xehost | sed 's/^\([[^-]]*\)-\([[^-]]*\)-\(.*\)$/\3/'`

case "$xehost_os" in
aix3*)
  # AIX sometimes has problems with the GCC collect2 program.  For some
  # reason, if we set the COLLECT_NAMES environment variable, the problems
  # vanish in a puff of smoke.
  if test "${COLLECT_NAMES+set}" != set; then
    COLLECT_NAMES=
    export COLLECT_NAMES
  fi
  ;;
esac

# Now see if the compiler is really GCC.
if test "$GCC" = "yes"; then
  XEGCC=yes
else
  AC_MSG_CHECKING([whether we are using GNU C])
  AC_EGREP_CPP(yes,[
#ifdef __GNUC__
  yes;
#endif
],XEGCC=yes, XEGCC=no)
  AC_MSG_RESULT([${XEGCC}])
fi

AC_MSG_CHECKING([how to produce PIC code])
wl=

can_build_shared=yes
if test "$XEGCC" = yes -o "$__ICC" = yes; then
  wl='-Wl,'

  case "$xehost_os" in
  aix[[3-9]]* | irix[[5-9]]* | osf[[3-9]])
    # PIC is the default for these OSes.
    ;;

  *darwin*) dll_cflags='-dynamic'
    ;;

  os2*)
    # We can build DLLs from non-PIC.
    ;;
  amigaos*)
    # FIXME: we need at least 68020 code to build shared libraries, but
    # adding the `-m68020' flag to GCC prevents building anything better,
    # like `-m68040'.
    dll_cflags='-m68020 -resident32 -malways-restore-a4'
    ;;
  *)
    dll_cflags='-fPIC'
    ;;
  esac
else
  # PORTME Check for PIC flags for the system compiler.
  case "$xehost_os" in
  hpux9* | hpux1[[0-9]]*)
    # Is there a better link_static_flag that works with the bundled CC?
    wl='-Wl,'
    dll_cflags='+Z'
    ;;

  irix[[5-9]]*)
    wl='-Wl,'
    # PIC (with -KPIC) is the default.
    ;;

  os2*)
    # We can build DLLs from non-PIC.
    ;;

  osf[[3-9]]*)
    # All OSF/1 code is PIC.
    wl='-Wl,'
    ;;

  aix[[3-9]]*)
    # All AIX code is PIC.
    wl='-Wl,'
    ;;

  sco3.2v5*)
    dll_cflags='-belf -Kpic'
    wl='-Wl,'
    ;;

  unixware*)
    dll_cflags="-KPIC"
    wl="-Wl,"
    ;;

  sysv4*)
    dll_cflags="-KPIC"
    wl="-Wl,"
    ;;

  sysv5*)
    dll_cflags="-KPIC"
    wl="-Wl,"
    ;;

  solaris2*)
    dll_cflags='-KPIC'
    wl='-Wl,'
    ;;

  sunos4*)
    dll_cflags='-PIC'
    wl='-Qoption ld '
    ;;

  uts4*)
    dll_cflags='-pic'
    ;;

  *)
    can_build_shared=no
    ;;
  esac
fi

if test -n "$dll_cflags"; then
  AC_MSG_RESULT([${dll_cflags}])

  # Check to make sure the dll_cflags actually works.
  AC_MSG_CHECKING([if PIC flag ${dll_cflags} really works])
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS $dll_cflags -DPIC"
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([[int x=0;]])],[
    # On HP-UX, the stripped-down bundled CC doesn't accept +Z, but also
    # reports no error.  So, we need to grep stderr for (Bundled).
    if grep '(Bundled)' config.log >/dev/null; then
      AC_MSG_RESULT(no)
      can_build_shared=no
      dll_cflags=
    else
      AC_MSG_RESULT(yes)
    fi], [AC_MSG_RESULT(no)
    can_build_shared=no
    dll_cflags=])
  CFLAGS="$save_CFLAGS"
else
  AC_MSG_RESULT(none)
fi

dnl
dnl Now comes the LD trickery. We do things differently to libtool here.
dnl I believe that libtool is incorrect in trying to drive the linker
dnl directly. This can cause considerable problems if the module you are
dnl compiling has C++ or other static initializers. If we use ld directly,
dnl we don't end up with the crt stuff being linked in, and we don't end up
dnl with any .init or .fini sections (or the moral equivalent thereof).
dnl gcc takes great care to do this properly when invoked in -shared
dnl mode, and we really do want this behavior. Perhaps the libtool folks
dnl are not aware that any SVR4 based dynamic loader will automatically
dnl execute code in the .init section before dlopen() returns. This is
dnl vital, as the module may have been compiled to rely on that behavior.
dnl
dnl So, having said all of that, we diverge from libtool significantly
dnl here. We want to try and use the C compiler as much as possible. Only
dnl if the C compiler itself cannot create shared libraries do we try to
dnl find the linker.
dnl
dnl The other advantage to my scheme is that it removes the dependancy
dnl on a given compiler version remaining static with relation to the
dnl version of XEmacs. With the libtool way, it picks up the linker that
dnl gcc uses, which can be the internal collect2 that comes with gcc.
dnl If the user ever changes their compiler version, the paths will no
dnl longer be correct, and ellcc will break. This is clearly unacceptable.
dnl By using the compiler driver on the path, we don't have this problem.
dnl If that is not clear, consider that gcc -print-prog-name=ld can
dnl produce something along the lines of:
dnl   /usr/local/lib/gcc-lib/OS-NAME/GCC-VERSION/ld
dnl If you ever change GCC versions, then that path no longer exists.
dnl
dnl So, we change the check order here. We first check to see if we are
dnl using GCC, and if so, we see if -shared works. If it does, great.
dnl If we are not using gcc, but the system C compiler can produce
dnl shared objects, we try that. Only if all of that fails do we revert
dnl back to the libtool ld trickery.
dnl
dnl We don't do ANY of this if we can't produce shared objects.
dnl
if test "$can_build_shared" = "yes"; then
cc_produces_so=no
xldf=
xcldf=
AC_MSG_CHECKING([if C compiler can produce shared libraries])
if test "$XEGCC" = yes -o "$__ICC" = yes; then
  case "$xehost_os" in
    *darwin*)
      xcldf='-bundle'
      xldf='-bundle -undefined suppress -flat_namespace'
      ;;
    *)
      xcldf="-shared"
      xldf="-shared"
      ;;
  esac
else # Not using GCC
  case "$xehost_os" in
    aix[[3-9]]*)
      xldf="-bE:ELLSONAME.exp -H512 -T512 -bhalt:4 -bM:SRE -bnoentry -lc"
      xcldf="${wl}-bE:ELLSONAME.exp ${wl}-H512 ${wl}-T512 ${wl}-bhalt:4 ${wl}-bM:SRE ${wl}-bnoentry ${wl}-lc"
      ;;

    freebsd2* | netbsd* | openbsd*)
      xldf="-Bshareable"
      xcldf="${wl}-Bshareable"
      ;;

    freebsd3*)
      xcldf="-shared"
      ;;

    hpux*)
      xldf="-b +s"
      xcldf="${wl}-b ${wl}+s"
      ;;

    irix[[5-9]]* | osf[[3-9]]*)
      xcldf="${wl}-shared"
      xldf="-shared"
      ;;

    sco3.2v5* | unixware* | sysv5* | sysv4* | solaris2* | solaris7* | uts4*)
      xcldf="-G"
      xldf="-G"
      ;;

    sunos4*)
      xcldf="${wl}-assert ${wl}pure-text ${wl}-Bstatic"
      xldf="-assert pure-text -Bstatic"
      ;;
  esac
fi # End if if we are using gcc

if test -n "$xcldf"; then
  save_LDFLAGS=$LDFLAGS
  save_LIBS=$LIBS
  save_xe_libs=$xe_libs
  LDFLAGS="$xcldf $LDFLAGS"
  LIBS=
  xe_libs=
  ac_link='${CC-cc} -o conftest $CFLAGS '"$xe_cppflags $xe_ldflags"' conftest.$ac_ext '"$xe_libs"' 1>&AS_MESSAGE_LOG_FD'
  AC_LINK_IFELSE([AC_LANG_SOURCE([[int x=0;]])],cc_produces_so=yes,cc_produces_so=no)
  LDFLAGS=$save_LDFLAGS
  LIBS=$save_LIBS
  xe_libs=$save_xe_libs
  ac_link='${CC-cc} -o conftest $CFLAGS '"$xe_cppflags $xe_ldflags"' conftest.$ac_ext '"$xe_libs"' 1>&AS_MESSAGE_LOG_FD'
else
  cc_produces_so=no
fi
AC_MSG_RESULT([${cc_produces_so}])

LTLD=$LD
if test -z "$LTLD"; then
  ac_prog=ld
  if test "$XEGCC" = yes; then
    # Check if gcc -print-prog-name=ld gives a path.
    AC_MSG_CHECKING([for ld used by GCC])
    ac_prog=`($CC -print-prog-name=ld) 2>&5`
    case "$ac_prog" in
    # Accept absolute paths.
    /*)
      if test -z "$LTLD"; then
#        case "$ac_prog" in
#          *gcc-lib*) LTLD="$CC"
#                     ;;
#          *)
         LTLD="$ac_prog"
#                     ;;
#        esac
      fi
      ;;
    "")
      # If it fails, then pretend we aren't using GCC.
      ac_prog=ld
      ;;
    *)
      # If it is relative, then search for the first ld in PATH.
      with_gnu_ld=unknown
      ;;
    esac
  else
    AC_MSG_CHECKING([for GNU ld])
  fi

  if test -z "$LTLD"; then
    IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
    for ac_dir in $PATH; do
      test -z "$ac_dir" && ac_dir=.
      if test -f "$ac_dir/$ac_prog"; then
        LTLD="$ac_dir/$ac_prog"
        # Check to see if the program is GNU ld.  I'd rather use --version,
        # but apparently some GNU ld's only accept -v.
        # Break only if it was the GNU/non-GNU ld that we prefer.
        if "$LTLD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
          xe_gnu_ld=yes
        else
          xe_gnu_ld=no
        fi
      fi
    done
    IFS="$ac_save_ifs"
  fi

  if test -n "$LTLD"; then
    AC_MSG_RESULT([${LTLD}])
  else
    AC_MSG_RESULT(no)
  fi

  if test -z "$LTLD" -a "$cc_produces_so" = no; then
    AC_MSG_ERROR(no acceptable linker found in \$PATH)
    exit 1
  fi
fi

dnl
dnl Order of the tests changed somewhat to prevent repetition
dnl
ld_dynamic_link_flags=

# Check to see if it really is or isn't GNU ld.
AC_MSG_CHECKING([if the linker is GNU ld])
# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LTLD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  xe_gnu_ld=yes
else
  xe_gnu_ld=no
fi
AC_MSG_RESULT([${xe_gnu_ld}])

case "$xehost_os" in
  amigaos* | sunos4*)
    # On these operating systems, we should treat GNU ld like the system ld.
    gnu_ld_acts_native=yes
    ;;
  *)
    gnu_ld_acts_native=no
    ;;
esac

if test "$cc_produces_so" = "yes"; then
  dll_ld=$CC
  case "$xehost_os" in
    *darwin*)
      dnl On Darwin, we test with xcldf, but we use xldf
      dll_ldflags=$xldf 
      ;;
    *) 
      dll_ldflags=$xcldf
      ;;
  esac
  can_build_shared=yes
  ld_shlibs=yes
else
  # OK - only NOW do we futz about with ld.
  # See if the linker supports building shared libraries.
  AC_MSG_CHECKING([whether the linker supports shared libraries])
  dll_ld=$CC
  dll_ldflags=$LDFLAGS
  ld_shlibs=yes
  can_build_shared=yes
  if test "$xe_gnu_ld" = yes && test "$gnu_ld_acts_native" != yes; then
    # See if GNU ld supports shared libraries.
    if $LTLD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      dll_ld=$CC
      dll_ldflags="-shared"
      ld_shlibs=yes
    else
      ld_shlibs=no
    fi
  else
    # PORTME fill in a description of your system's linker (not GNU ld)
    case "$xehost_os" in
    aix3*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    aix[[4-9]]*)
      dll_ldflags=$xcldf
      ;;

    # FreeBSD 2.2.[012] allows us to include c++rt0.o to get C++ constructor
    # support.  Future versions do this automatically, but an explicit c++rt0.o
    # doesn't break anything, and helps significantly (at the cost of a little
    # extra space).
    freebsd2.2*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      dll_post="/usr/lib/c++rt0.o"
      ;;

    # Unfortunately, older versions of FreeBSD 2 don't have this feature.
    freebsd2*)
      dll_ld=$LTLD
      dll_ldflags="-Bshareable"
      ;;

    # FreeBSD 3, at last, uses gcc -shared to do shared libraries.
    freebsd3*)
      dll_ldflags="-shared"
      ;;

    hpux*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    irix[[5-9]]*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    netbsd*)
      # Tested with NetBSD 1.2 ld
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    openbsd*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    osf3* | osf4*)
      dll_ld=$LTLD
      dll_ldflags=$xldf
      ;;

    # For both SCO and Solaris we MAY want to have LDFLAGS include -z text
    sco3.2v5* | unixware* | sysv5* | sysv4* | solaris2* | solaris7*)
      dll_ld=$LTLD
      case "$dll_ld" in
        *gcc*) dll_ldflags="-shared"
               dll_ld=$CC
               ;;
        *)     dll_ldflags="-G"
               ;;
      esac
      ;;

    sunos4*)
      if test "$XEGCC" = yes; then
        dll_ld=$CC
      else
        dll_ld=$LTLD
      fi
      dll_ldflags=$xldf
      ;;

    uts4*)
      dll_ld=$LTLD
      dll_ldflags="-G"
      ;;

    bsdi*)
      dll_ldflags="-r"
      dll_ld="shlicc2"
      ;;

    *)
      ld_shlibs=no
      can_build_shared=no
      ;;
    esac
  fi
  AC_MSG_RESULT([${ld_shlibs}])
  if test "$ld_shlibs" = "no"; then
    can_build_shared=no
  fi
fi # End of if cc_produces_so = no

dnl
dnl Last thing, check how to get a linked executable to have its symbols
dnl exported, so that the modules have access to them.
dnl
dnl XEmacs FIXME - we need to set ld_dynamic_link_flags properly for
dnl most of these systems, which was missing from libtool. I know they
dnl all have a way of doing this, but someone needs to look at this
dnl for each OS and make sure it is correct. Remember that the arguments
dnl are passed when temacs is linked, this is NOT for modules. The sole
dnl purpose of the argument is to get the internal XEmacs symbols exposed
dnl for modules to use. This means that the COMPILER (and NOT the linker)
dnl is most often used to create temacs, so arguments to the linker will
dnl usually need to be prefix with ${wl} or some other such thing.
dnl

if test "$xe_gnu_ld" = yes; then
  if test "$ld_shlibs" = yes; then
    ld_dynamic_link_flags="${wl}-export-dynamic"
  fi
fi

if test -z "$ld_dynamic_link_flags"; then
  case "$xehost_os" in
  aix[[3-9]]*)
    ld_dynamic_link_flags=
    ;;

  *darwin*)
    ld_dynamic_link_flags=
    ;;

  freebsd2.2*)
    ld_dynamic_link_flags=
    ;;

  freebsd2*)
    ld_dynamic_link_flags=
    ;;

  freebsd3*)
    ld_dynamic_link_flags=
    ;;

  hpux*)
    ld_dynamic_link_flags="${wl}-E"
    ;;

  irix[[5-9]]*)
    ld_dynamic_link_flags=
    ;;

  netbsd*)
    ld_dynamic_link_flags=
    ;;

  openbsd*)
    ld_dynamic_link_flags=
    ;;

  osf3* | osf4*)
    ld_dynamic_link_flags=
    ;;

  solaris2* | solaris7*)
    ld_dynamic_link_flags=
    ;;

  sco3.2v5* | unixware* | sysv5* | sysv4*)
    ld_dynamic_link_flags="${wl}-Bexport"
    ;;

  sunos4*)
    ld_dynamic_link_flags=
    ;;

  uts4*)
    ld_dynamic_link_flags=
    ;;

  bsdi*)
    ld_dynamic_link_flags=
    ;;

  esac
fi # End of if -z ld_dynamic_link_flags
fi # End of if test "$can_build_shared" = "yes"

AC_SUBST(dll_ld)
AC_SUBST(dll_cflags)
AC_SUBST(dll_ldflags)
AC_SUBST(dll_post)
AC_SUBST(dll_ldo)
AC_SUBST(ld_dynamic_link_flags)
])dnl


dnl -------------------------------------------------------------------------
dnl Local macros

AC_DEFUN([SXE_USAGE_ERROR],
[(echo "$progname: Usage error:"
echo " " $1
echo "  Use \`$progname --help' to show usage.") >&2 && exit 1])

dnl SXE_PRINT_VAR(var var ...)  prints values of shell variables
AC_DEFUN([SXE_PRINT_VAR],[for var in patsubst([$1],[[
]+],[ ]); do eval "echo \"$var = '\$$var'\""; done])

dnl SXE_ADD_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_OBJS], [
	extra_objs="$extra_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. uncertain) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_OBJS

dnl SXE_ADD_CORE_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_CORE_OBJS], [
	libsxecore_objs="$libsxecore_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. CORE) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_CORE_OBJS
AC_DEFUN([SXE_SUBST_CORE_OBJS], [AC_SUBST(libsxecore_objs)])

dnl SXE_ADD_ENT_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_ENT_OBJS], [
	libent_objs="$libent_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. ENT) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_ENT_OBJS
AC_DEFUN([SXE_SUBST_ENT_OBJS], [AC_SUBST(libent_objs)])

dnl SXE_ADD_MM_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_MM_OBJS], [
	libmm_objs="$libmm_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. MM) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_MM_OBJS
AC_DEFUN([SXE_SUBST_MM_OBJS], [AC_SUBST(libmm_objs)])

dnl SXE_ADD_MULE_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_MULE_OBJS], [
	libmule_objs="$libmule_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. MULE) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_MULE_OBJS
AC_DEFUN([SXE_SUBST_MULE_OBJS], [AC_SUBST(libmule_objs)])

dnl SXE_ADD_DB_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_DB_OBJS], [
	libdb_objs="$libdb_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. DB) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_DB_OBJS
AC_DEFUN([SXE_SUBST_DB_OBJS], [AC_SUBST(libdb_objs)])

dnl SXE_ADD_MEMALLOC_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_MEMALLOC_OBJS], [
	libmemalloc_objs="$libmemalloc_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. MEMALLOC) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_MEMALLOC_OBJS
AC_DEFUN([SXE_SUBST_MEMALLOC_OBJS], [AC_SUBST(libmemalloc_objs)])

dnl SXE_ADD_SXEUI_OBJS(foo.o ...)
AC_DEFUN([SXE_ADD_SXEUI_OBJS], [
	libsxeui_objs="$libsxeui_objs [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. SXE UI) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_SXEUI_OBJS
AC_DEFUN([SXE_SUBST_SXEUI_OBJS], [AC_SUBST(libsxeui_objs)])

dnl SXE_ADD_STATMOD_A(foo.o ...)
AC_DEFUN([SXE_ADD_STATMOD_A], [
	statmod_archives="$statmod_archives [$1]" && \
	if test "$extra_verbose" = "yes"; then
		echo "    sxemacs (cat. static module) will be linked with \"[$1]\""
	fi
])dnl SXE_ADD_STATMOD_A
AC_DEFUN([SXE_SUBST_STATMOD_A], [AC_SUBST(statmod_archives)])

dnl SXE_APPEND(value, varname)
AC_DEFUN([SXE_APPEND],
[[$2]="$[$2] [$1]" && dnl
 if test "$extra_verbose" = "yes"; then echo "    Appending \"[$1]\" to \$[$2]"; fi])

dnl SXE_PREPEND(value, varname)
AC_DEFUN([SXE_PREPEND],
[[$2]="[$1] $[$2]" && dnl
 if test "$extra_verbose" = "yes"; then echo "    Prepending \"[$1]\" to \$[$2]"; fi])

dnl SXE_DIE(message)
AC_DEFUN([SXE_DIE], [{ echo "Error:" $1 >&2; exit 1; }])

dnl SXE_CHECK_FEATURE_DEPENDENCY(feature1, feature2)
AC_DEFUN([SXE_CHECK_FEATURE_DEPENDENCY],
[if test "$with_$1 $with_$2" = "yes no"; then
	SXE_USAGE_ERROR("--with-$1 requires --with-$2")
elif test "$with_$2" = "no" ; then with_$1=no
elif test "$with_$1" = "yes"; then with_$2=yes
fi
])

dnl SXE_STRIP_4TH_COMPONENT(var)
dnl Changes i986-pc-linux-gnu to i986-pc-linux, as God (not RMS) intended.
AC_DEFUN([SXE_STRIP_4TH_COMPONENT],
[$1=`echo "$$1" | sed '[s/^\([^-][^-]*-[^-][^-]*-[^-][^-]*\)-.*$/\1/]'`])

dnl Do our best to deal with automounter brokenness
dnl SXE_CANONICALISE_PATH(varname)
AC_DEFUN([SXE_CANONICALISE_PATH],
[if test -d "/net"; then
  if test -d "/tmp_mnt/net"; then tdir="tmp_mnt/net"; else tdir="tmp_mnt"; fi
  $1=`echo "[$]$1" | \
   sed -e "s|^${tdir}/|/net/|" -e "s|^/a/|/net/|" -e "s|^/amd/|/net/|"`
fi])dnl

dnl SXE_PROTECT_LINKER_FLAGS(shell_var)
AC_DEFUN([SXE_PROTECT_LINKER_FLAGS], [
if test "$GCC" = "yes"; then
  set x $[$1]; shift; [$1]=""
  while test -n "[$]1"; do
    case [$]1 in
      -L  | -l  | -u               ) [$1]="$[$1] [$]1 [$]2"; shift ;;
      -L* | -l* | -u* | -Wl* | -pg ) [$1]="$[$1] [$]1" ;;
      -Xlinker* ) ;;
      * ) [$1]="$[$1] -Xlinker [$]1" ;;
    esac
    shift
  done
fi])dnl


dnl Allow use of either ":" or spaces for lists of directories
AC_DEFUN([SXE_COLON_TO_SPACE],
  [case "$[$1]" in *:* [)] [$1]="`echo '' $[$1] | sed -e 's/^ //' -e 's/:/ /g'`";; esac])dnl


dnl SXE_ADD_RUNPATH_DIR(directory)
AC_DEFUN([SXE_ADD_RUNPATH_DIR],[{
xe_runpath_dir=$1
dnl SXE_PRINT_VAR(ld_switch_site ld_switch_x_site runpath xe_runpath_dir LD_RUN_PATH xe_ldflags)
  test "$xe_runpath_dir" != "/lib"     -a \
	"$xe_runpath_dir" != "/usr/lib" -a \
	-n "`ls ${xe_runpath_dir}/*.s[[ol]] 2>/dev/null`" && \
  eval "$xe_add_unique_runpath_dir"
}])dnl

dnl SXE_COMPUTE_RUNPATH()
AC_DEFUN([SXE_COMPUTE_RUNPATH],[
if test "$add_runtime_path" = "yes" -a -n "$dash_r"; then
  dnl Remove runtime paths from current ld switches
  ld_switch_site=`echo   '' $ld_switch_site   | sed -e 's:^ ::' -e "s/$dash_r[[^ ]]*//g"`
  ld_switch_x_site=`echo '' $ld_switch_x_site | sed -e 's:^ ::' -e "s/$dash_r[[^ ]]*//g"`
  dnl SXE_PRINT_VAR(ld_switch_site ld_switch_x_site)

  dnl Fix up Runtime path
  dnl If LD_RUN_PATH is set in environment, use that.
  dnl In this case, assume user has set the right value.
  runpath="" runpath_dirs=""
  if test -n "$LD_RUN_PATH"; then
    runpath="$LD_RUN_PATH"
  elif test "$GCC" = "yes"; then
    dnl Compute runpath from gcc's -v output
    ld_switch_run_save="$ld_switch_run"; ld_switch_run=""
    echo "int main(int argc, char *argv[[]]) {return 0;}" > conftest.c
    xe_runpath_link='${CC-cc} -o conftest -v $CFLAGS '"$xe_ldflags"' conftest.$ac_ext 2>&1 1>/dev/null'
    for arg in `eval "$xe_runpath_link" | grep ' -L'`; do
      case "$arg" in P,* | -L* | -R* )
        for dir in `echo '' "$arg" | sed -e 's:^ ::' -e 's/^..//' -e 'y/:/ /'`; do
          SXE_ADD_RUNPATH_DIR("$dir")
        done ;;
      esac
    done
    ld_switch_run="$ld_switch_run_save"
    rm -f conftest*
  else
    dnl Add all directories with .so files to runpath
    for arg in $ld_switch_site $ld_switch_x_site; do
      case "$arg" in -L*) SXE_ADD_RUNPATH_DIR(`echo '' "$arg" | sed -e 's:^ ::' -e 's/^-L//'`);; esac
    done
    dnl Sometimes /opt/SUNWdt/lib is the only installed Motif available
    if test "$opsys $need_motif" = "sol2 yes"; then
      xe_runpath_dir="/opt/SUNWdt/lib";
      eval "$xe_add_unique_runpath_dir";
    fi
  fi dnl Compute $runpath

  if test -n "$runpath"; then
    ld_switch_run="${dash_r}${runpath}"
    SXE_PROTECT_LINKER_FLAGS(ld_switch_run)
    test "$extra_verbose" = "yes" && echo "Setting runpath to $runpath"
  fi
fi
])dnl

dnl The construct foo=`echo $w1 $w2 $w3` fails on some systems if $w1 = -e or -n
dnl So we use the following instead.
dnl SXE_SPACE(var, words)
AC_DEFUN([SXE_SPACE],[
T=""
for W in $2; do if test -z "$T"; then T="$W"; else T="$T $W"; fi; done
$1="$T"
])dnl SXE_SPACE

AC_DEFUN([SXE_CHECK_BROKEN_GCC], [
dnl This section needs a rewrite.  I think it should just bomb if we
dnl find a gcc that is older than 2.95.3 --SY.
dnl Search for GCC specific build problems we know about

AC_MSG_CHECKING([for buggy gcc versions])
GCC_VERSION=`$CC --version`

case $GCC_VERSION in
2.6.*|2.7.*|2.8.* )
	AC_MSG_RESULT([yes])
	AC_MSG_WARN([Don't use medieval compilers])
	AC_MSG_ERROR([Aborting due to known problem])
	;;
esac

case `uname -s`:`uname -m`:$GCC_VERSION in
dnl pie-enabled GCC on Linux needs -nopie to build SXEmacs
Linux:i?86:gcc*pie-*)
	case "$CFLAGS" in
	*-nopie*)
		;;
	*)
		AC_MSG_RESULT([yes])
		AC_MSG_WARN([pie extension detected... disabling])
		CFLAGS="-nopie $CFLAGS"
		SXE_CFLAGS="-nopie $SXE_CFLAGS"
		;;
	esac
	;;

dnl egcs 2.90.21 (egcs-1.00 release)
dnl egcs 2.90.29 (egcs-1.0.3 release)
*:sun4*:2.8.1|*:sun4*:egcs-2.90.*)
dnl Don't use -O2 with gcc 2.8.1 and egcs 1.0 under SPARC architectures
dnl without also using `-fno-schedule-insns'.
	case "$CFLAGS" in
	*-O2*|*-O3*)
		case "$CFLAGS" in
		*-fno-schedule-insns*)
			;;
		*)
			AC_MSG_RESULT([yes])
			AC_MSG_WARN([Don't use -O2 with gcc 2.8.1 and egcs 1.0 under SPARC architectures])
			AC_MSG_WARN([without also using -fno-schedule-insns.])
			AC_MSG_ERROR([Aborting due to known problem])
			;;
		esac
		;;
	esac
	;;

dnl egcs-2.91.57 (egcs-1.1 release)
dnl egcs-2.91.66 (egcs-1.1.2 release)
Linux:alpha:egcs-2.91.*)
	AC_MSG_RESULT([yes])
	AC_MSG_WARN([There have been reports of egcs-1.1 not compiling SXEmacs correctly on])
	AC_MSG_WARN([Alpha Linux.  There have also been reports that egcs-1.0.3a is O.K.])
	AC_MSG_ERROR([Aborting due to known problem])
	;;

*:*:* )
	AC_MSG_RESULT([no])
	;;
esac
])dnl SXE_CHECK_BROKEN_GCC


AC_DEFUN([MYAC_PROG_BISON], [
	AC_PROG_YACC()
	AC_MSG_CHECKING(for bison)
	if test "$YACC" != "bison -y"; then
		AC_MSG_RESULT(no)
		dnl AC_MSG_ERROR([bison not found but required])
	else
		AC_MSG_RESULT(bison)
		AC_SUBST(BISON,[bison],[location of bison])
	fi
])

AC_DEFUN([_AC_MATH_ASSIGN_IFELSE], [
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
#include <limits.h>
]], [[$1 __test_assign=$2]])], [$3], [$4])
])dnl _AC_MATH_DOUBLE_OP

AC_DEFUN([AC_MATH_DBL_MAX], [
        AC_MSG_CHECKING([for DBL_MAX])
	_AC_MATH_ASSIGN_IFELSE([double], [DBL_MAX], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_DBL_MAX], [1],
			[Whether DBL_MAX is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_DBL_MAX

AC_DEFUN([AC_MATH_DBL_MIN], [
        AC_MSG_CHECKING([for DBL_MIN])
	_AC_MATH_ASSIGN_IFELSE([double], [DBL_MIN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_DBL_MIN], [1],
			[Whether DBL_MIN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_DBL_MIN

AC_DEFUN([AC_MATH_LDBL_MAX], [
        AC_MSG_CHECKING([for LDBL_MAX])
	_AC_MATH_ASSIGN_IFELSE([long double], [LDBL_MAX], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_LDBL_MAX], [1],
			[Whether LDBL_MAX is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_LDBL_MAX

AC_DEFUN([AC_MATH_LDBL_MIN], [
        AC_MSG_CHECKING([for LDBL_MIN])
	_AC_MATH_ASSIGN_IFELSE([long double], [LDBL_MIN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_LDBL_MIN], [1],
			[Whether LDBL_MIN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_LDBL_MIN

AC_DEFUN([AC_MATH_INFINITY], [
        AC_MSG_CHECKING([for INFINITY])
	_AC_MATH_ASSIGN_IFELSE([float], [INFINITY], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_INFINITY], [1],
			[Whether INFINITY is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_INFINITY

AC_DEFUN([AC_MATH_FPCLASSIFY], [
        AC_MSG_CHECKING([for fpclassify])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; fpclassify(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_FPCLASSIFY], [1],
			[Whether isinf() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISINF

AC_DEFUN([AC_MATH_ISINF], [
        AC_MSG_CHECKING([for isinf])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; isinf(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_ISINF], [1],
			[Whether isinf() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISINF

AC_DEFUN([AC_MATH_NAN], [
        AC_MSG_CHECKING([for NAN])
	_AC_MATH_ASSIGN_IFELSE([float], [NAN], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_NAN], [1],
			[Whether NAN is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_NAN

AC_DEFUN([AC_MATH_ISNAN], [
        AC_MSG_CHECKING([for isnan])
	_AC_MATH_ASSIGN_IFELSE([float], [0.0; isnan(__test_assign);], [dnl yes case
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED([HAVE_MATH_ISNAN], [1],
			[Whether isnan() is defined in math.h])
		$1], [dnl no case
		AC_MSG_RESULT(no)
		$2])
])dnl AC_MATH_ISNAN


dnl MM tests
dnl ========

AC_DEFUN([SXE_DUMP_LIBS], [
	save_c_switch_site="$c_switch_site"
	save_LIBS="$LIBS"
	save_ac_c_werror_flag=$ac_c_werror_flag
])dnl SXE_DUMP_LIBS

AC_DEFUN([SXE_RESTORE_LIBS], [
	LIBS="$save_LIBS"
	c_switch_site="$save_c_switch_site"
	ac_c_werror_flag="$save_ac_c_werror_flag"
])dnl SXE_RESTORE_LIBS

AC_DEFUN([SXE_SEARCH_CONFIG_PROG], [
	## arg #1 program to search
	pushdef([config_prog], [$1])
	pushdef([have_config_prog], [have_]translit([$1], [-], [_]))
	pushdef([CONFIG_PROG], translit([$1], [-a-z], [_A-Z]))
	AC_CHECK_PROG(have_config_prog, config_prog, [yes], [no])
	AC_PATH_PROG(CONFIG_PROG, config_prog, [echo])
])dnl SXE_SEARCH_CONFIG_PROG

AC_DEFUN([_SXE_MM_CHECK_pkgconfig_based], [
	## assumes $PKG_CONFIG is defined
	## arg #1: MM param name
	## arg #2: lib to check
	## arg #3: version of that lib
	## arg #4: funs to check
	## arg #5: headers to check
	## arg #6: success action
	## arg #7: failure action

	pushdef([MM_LIB], [$1])
	pushdef([MM_SUCC], [$6])
	pushdef([MM_FAIL], [$7])

	AC_MSG_CHECKING([for ]MM_LIB[ support])
	AC_MSG_RESULT([])

	if test "$have_pkg_config" = "no" -o -z "$PKG_CONFIG"; then
		AS_MESSAGE([*** pkg-config not found. See http://pkgconfig.sourceforge.net])
		AS_MESSAGE([*** Cannot check for ]MM_LIB[.])
		have_pkg_config=no
		PKG_CONFIG=
		MM_FAIL
	fi

	pushdef([MM_MOD], [$2])
	pushdef([MM_MOD_REQUIRED_VERSION], [$3])
	pushdef([MM_MOD_FUNS], [$4])
	pushdef([MM_MOD_HDRS], [$5])
	AC_MSG_CHECKING([whether ]MM_MOD[ is at least ]MM_MOD_REQUIRED_VERSION)
	if test -n "$PKG_CONFIG" && \
	     $PKG_CONFIG --atleast-version MM_MOD_REQUIRED_VERSION MM_MOD; then
		actual_version=`$PKG_CONFIG --modversion []MM_MOD[]`
		AC_MSG_RESULT([yes ($actual_version)])
		AC_MSG_CHECKING([for ]mm_lib[ libraries])
		MM_MOD_LIBS_L=`$PKG_CONFIG --libs-only-l []MM_MOD[]`
		MM_MOD_LIBS=`echo $MM_MOD_LIBS_L | sed -e "s/-l//g"`
		MM_MOD_LIB=`echo $MM_MOD_LIBS | sed -e "s/ .*$//"`
		MM_MOD_OTHER_LIBS=`echo $MM_MOD_LIBS_L | sed -e "s/^[^ ]*//"`
		AC_MSG_RESULT([$MM_MOD_LIBS])

		## backup our site flags
		SXE_DUMP_LIBS
		MM_MOD_LIBS_SITE=`$PKG_CONFIG --libs-only-L []MM_MOD[]`
		MM_MOD_HDRS_SITE=`$PKG_CONFIG --cflags-only-I []MM_MOD[]`
		SXE_PREPEND([$MM_MOD_HDRS_SITE], [c_switch_site])
		SXE_PREPEND([$MM_MOD_LIBS_SITE], [ld_switch_site])

		MM_SUCC
		AC_CHECK_HEADERS([]MM_MOD_HDRS[], [:], [MM_FAIL])
		for i in MM_MOD_FUNS; do
			AC_CHECK_LIB([$MM_MOD_LIB], [$i], [:], [MM_FAIL],
				[$MM_MOD_LIBS_L])
		done

		## restore old values
		SXE_RESTORE_LIBS
	elif -n "$PKG_CONFIG"; then
		actual_version=`$PKG_CONFIG --modversion []MM_MOD[]`
		AC_MSG_RESULT([no ($actual_version)])
	else
		AC_MSG_RESULT([uncertain])
	fi
	popdef([MM_LIB])
	popdef([MM_MOD_FUNS])
	popdef([MM_MOD_HDRS])
	popdef([MM_MOD_REQUIRED_VERSION])
	popdef([MM_MOD])
	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl _SXE_MM_CHECK_pkgconfig_based

AC_DEFUN([SXE_MM_CHECK_XPM], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xpm support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	SXE_PREPEND([-I$x_includes], [c_switch_site])
	SXE_PREPEND([-L$x_libraries], [ld_switch_site])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	AC_CHECK_HEADERS([X11/xpm.h], [:], [MM_FAIL])

	AC_MSG_CHECKING(for Xpm (more recent than 3.4f))
	xe_check_libs=-lXpm
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#define XPM_NUMBERS
#include <X11/xpm.h>
int main(int c, char **v)
{
	return c == 1 ? 0 :
		XpmIncludeVersion != XpmLibraryVersion() ? 1 :
		XpmIncludeVersion < 30406 ? 2 : 0 ;
}]])], [./conftest dummy_arg; xpm_status=$?;
		if test "$xpm_status" = "0"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])
			MM_FAIL
			if test "$xpm_status" = "1"; then
				AC_MSG_WARN([dnl
Xpm library version and header file version don't match!
I have disabled xpm on your behalf.])
			elif test "$xpm_status" = "2"; then
				AC_MSG_WARN([dnl
Xpm library version is too old!
I have disabled xpm on your behalf.])
			else
				AC_MSG_WARN([dnl
Internal xpm detection logic error!])
			fi
		fi], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL])
	xe_check_libs=
	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XPM

AC_DEFUN([SXE_MM_CHECK_XFACE], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xface support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	SXE_PREPEND([-I$x_includes], [c_switch_site])
	SXE_PREPEND([-L$x_libraries], [ld_switch_site])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	AC_CHECK_HEADERS([compface.h], [:], [MM_FAIL])
	AC_CHECK_LIB([compface], [UnGenFace], [:], [MM_FAIL])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XFACE

AC_DEFUN([SXE_MM_CHECK_GIF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for gif support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_GIF

AC_DEFUN([SXE_MM_CHECK_JPEG], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for jpeg support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	SXE_PREPEND([-I$x_includes], [c_switch_site])
	SXE_PREPEND([-L$x_libraries], [ld_switch_site])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	AC_CHECK_HEADERS([jpeglib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([jpeg], [jpeg_destroy_decompress], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_JPEG

AC_DEFUN([SXE_MM_CHECK_PNG], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for PNG support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	SXE_PREPEND([-I$x_includes], [c_switch_site])
	SXE_PREPEND([-L$x_libraries], [ld_switch_site])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	AC_CHECK_HEADERS([png.h], [:], [MM_FAIL])
	AC_CHECK_FUNC([pow], [:], [MM_FAIL]) dnl someone explain?
	AC_CHECK_LIB([png], [png_read_image], [:], [MM_FAIL], [$INFLATE_LIB])

	AC_MSG_CHECKING(for workable png version information)
	xe_check_libs="-lpng $INFLATE_LIB"
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <png.h>
int main(int c, char **v)
{
	if (c == 1)
		return 0;
	if (strcmp(png_libpng_ver, PNG_LIBPNG_VER_STRING) != 0)
		return 1;
	return (PNG_LIBPNG_VER < 10002) ? 2 : 0 ;
}]])], [./conftest dummy_arg; png_status=$?;
		if test "$png_status" = "0"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])
			MM_FAIL
			if test "$png_status" = "1"; then
				AC_MSG_WARN([dnl
PNG library version and header file don't match!
I have disabled PNG support on your behalf.])
			elif test "$png_status" = "2"; then
				AC_MSG_WARN([dnl
PNG library version too old (pre 1.0.2)!
I have disabled PNG support on your behalf.])
			fi
		fi], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL])
	xe_check_libs=

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_PNG

AC_DEFUN([SXE_MM_CHECK_TIFF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for TIFF support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	SXE_PREPEND([-I$x_includes], [c_switch_site])
	SXE_PREPEND([-L$x_libraries], [ld_switch_site])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	AC_CHECK_HEADERS([tiffio.h], [:], [MM_FAIL])
	AC_CHECK_LIB([tiff], [TIFFClientOpen], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_TIFF

AC_DEFUN([SXE_MM_SEARCH_INFLATE], [
	dnl Too many stupid linkers can't detect cascaded lib dependencies
	dnl  until runtime. So we always search for libz compression support.
	AC_SEARCH_LIBS([inflate], [c z gz], [
		if test "$ac_cv_lib_c_inflate" = "yes"; then
			INFLATE_LIB="c"
		elif test "$ac_cv_lib_z_inflate" = "yes"; then
			INFLATE_LIB="z"
		elif test "$ac_cv_lib_gz_inflate" = "yes"; then
			INFLATE_LIB="gz"
		fi], [INFLATE_LIB=])
	if test -n "$INFLATE_LIB"; then
		SXE_PREPEND([$INFLATE_LIB], [MM_LIBS])
	fi
])dnl SXE_MM_SEARCH_INFLATE

AC_DEFUN([SXE_MM_CHECK_SNDFILE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([sndfile], [sndfile], [1.0.12], [dnl
		sf_open sf_close sf_readf_short sf_readf_int dnl
		sf_readf_float sf_seek sf_open_virtual],
		[sndfile.h], [$1], [$2])
])dnl SXE_MM_CHECK_SNDFILE

AC_DEFUN([SXE_MM_CHECK_FFMPEG], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([ffmpeg], [libavformat], [49.0.0], [dnl
		av_open_input_file av_close_input_file av_find_stream_info dnl
		url_fopen av_probe_input_format avcodec_find_decoder dnl
		avcodec_open av_read_frame av_seek_frame av_register_all dnl
		avcodec_decode_audio], [avformat.h], [$1], [$2])
])dnl SXE_MM_CHECK_FFMPEG

AC_DEFUN([SXE_MM_CHECK_SOX], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_SEARCH_CONFIG_PROG([libst-config])

	AC_MSG_CHECKING([for SoX support])
	AC_MSG_RESULT([])

	if test "$have_libst_config" = "no" -o -z "$LIBST_CONFIG"; then
		AS_MESSAGE([*** libst-config not found.])
		AS_MESSAGE([*** Cannot check for SoX.])
		have_libst_config=no
		LIBST_CONFIG=
		MM_FAIL
	fi

	if test "$have_libst_config" = "yes"; then
		SXE_DUMP_LIBS
		sox_c_switch="`$LIBST_CONFIG --cflags`"
		sox_libs="-L`$LIBST_CONFIG --libdir` `$LIBST_CONFIG --libs`"
		sox_libs="$sox_libs -lst"
		SXE_APPEND([$sox_c_switch], [c_switch_site])
		dnl SXE_PREPEND([$sox_libs], [LIBS])

		AC_CHECK_HEADERS([st.h])

		echo "void cleanup(void) {}" > cleanup.c
		$CC -c -o cleanup.o cleanup.c

		dnl we need 12.17.9 with st_open_read
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <st.h>
]], [[
st_open_read((const char*)"", (const st_signalinfo_t*)NULL, (const char*)"");
]])], [MM_SUCC], [sox_too_old=yes])
		AC_CHECK_LIB([st], [st_close], [:], [MM_FAIL], [cleanup.o $sox_libs])
		AC_CHECK_LIB([st], [st_read], [:], [MM_FAIL], [cleanup.o $sox_libs])
		AC_CHECK_LIB([st], [st_seek], [:], [MM_FAIL], [cleanup.o $sox_libs])

		## clean up our cleanup snack
		rm -f cleanup.c cleanup.o

		## restore anything
		SXE_RESTORE_LIBS
	fi

	if test "$sox_too_old" = "yes"; then
		AS_MESSAGE([*** Detected sox, but it is too old.])
		AS_MESSAGE([*** Consider upgrading, see http://sox.sourceforge.net])
		MM_FAIL
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_SOX

AC_DEFUN([SXE_MM_CHECK_MAD], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for mad support])
	AC_MSG_RESULT([])

	MM_SUCC
	AC_CHECK_HEADERS([mad.h], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_frame], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_buffer], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_decode], [:], [MM_FAIL])

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_MAD

AC_DEFUN([SXE_MM_CHECK_OSS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for OSS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_FAIL
	AC_CHECK_HEADERS([machine/soundcard.h sys/soundcard.h linux/soundcard.h],
		[MM_SUCC])
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_OSS

AC_DEFUN([SXE_MM_CHECK_PULSE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([pulse], [libpulse], [0.9], [dnl
		pa_mainloop_new pa_threaded_mainloop_new pa_mainloop_iterate dnl
		pa_threaded_mainloop_lock pa_threaded_mainloop_unlock dnl
		pa_mainloop_get_api pa_threaded_mainloop_get_api dnl
		pa_mainloop_free pa_threaded_mainloop_free dnl
		pa_threaded_mainloop_stop dnl
		pa_context_new pa_context_get_state pa_context_is_pending dnl
		pa_context_disconnect dnl
		pa_operation_new pa_operation_unref dnl
		pa_stream_new pa_stream_get_state pa_stream_write dnl
		pa_stream_set_state_callback pa_stream_set_write_callback dnl
		pa_stream_unref pa_stream_connect_playback pa_stream_disconnect],
		[pulse/pulseaudio.h], [$1], [$2])
])dnl SXE_MM_CHECK_PULSE

AC_DEFUN([SXE_MM_CHECK_JACK], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([jack], [jack], [0.98.0], [dnl
		jack_client_open jack_get_ports jack_port_register dnl
		jack_set_process_callback jack_set_error_function dnl
		jack_on_shutdown jack_activate jack_connect jack_disconnect dnl
		jack_client_close jack_port_get_buffer], [jack/jack.h], [$1], [$2])
])dnl SXE_MM_CHECK_JACK

AC_DEFUN([SXE_MM_CHECK_AO], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([ao], [ao], [0.6], [dnl
		ao_initialize ao_driver_id ao_default_driver_id dnl
		ao_open_live ao_close ao_shutdown ao_play], [ao/ao.h], [$1], [$2])
])dnl SXE_MM_CHECK_AO

AC_DEFUN([SXE_MM_CHECK_ARTS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_SEARCH_CONFIG_PROG([artsc-config])

	AC_MSG_CHECKING([for aRts support])
	AC_MSG_RESULT([])

	if test "$have_artsc_config" = "no" -o -z "$ARTSC_CONFIG"; then
		AS_MESSAGE([*** artsc-config not found.])
		AS_MESSAGE([*** Cannot check for aRts.])
		have_artsc_config=no
		ARTSC_CONFIG=
		MM_FAIL
	fi

	if test "$have_artsc_config" = "yes"; then
		ARTS_VERSION=`$ARTSC_CONFIG --arts-version`
		ARTS_MAJOR_VERSION=`echo $ARTS_VERSION | awk -F. '{print $1}'`
		ARTS_MINOR_VERSION=`echo $ARTS_VERSION | awk -F. '{print $2}'`

		dnl since we are not using most of the arts features, it suffices
		dnl to have a version 1.x (x >= 0)
		dnl if test "$ARTS_MAJOR_VERSION" -eq 1 -a \
		dnl   "$ARTS_MINOR_VERSION" -ge 0; then

		SXE_DUMP_LIBS
		arts_c_switch="`$ARTSC_CONFIG --cflags`"
		arts_libs="-L`$ARTSC_CONFIG --libs`"
		SXE_APPEND([$arts_c_switch], [c_switch_site])
		SXE_PREPEND([$arts_libs], [LIBS])

		MM_SUCC
		AC_CHECK_HEADERS([artsc.h], [:], [MM_FAIL])
		AC_CHECK_LIB([artsc], [arts_init], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_free], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_write], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_play_stream], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_stream_set], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_close_stream], [:], [MM_FAIL], [$arts_libs])

		## restore anything
		SXE_RESTORE_LIBS
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ARTS

AC_DEFUN([SXE_MM_CHECK_ESD], [
	## arg #1: action on success
	## arg #2: action on failure
	SXE_SEARCH_CONFIG_PROG([esd-config])

	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for ESD support])
	AC_MSG_RESULT([])

	if test "$have_esd_config" = "no" -o -z "$ESD_CONFIG"; then
		AS_MESSAGE([*** esd-config not found.])
		AS_MESSAGE([*** Cannot check for ESD.])
		have_esd_config=no
		ESD_CONFIG=
		MM_FAIL
	fi

	if test "$have_esd_config" = "yes"; then
		SXE_DUMP_LIBS
		esd_c_switch="`$ESD_CONFIG --cflags`"
		esd_libs="-L`$ESD_CONFIG --libs`"
		SXE_APPEND([$esd_c_switch], [c_switch_site])
		SXE_PREPEND([$esd_libs], [LIBS])

		MM_SUCC
		AC_CHECK_HEADERS([esd.h], [:], [MM_FAIL])
		AC_CHECK_LIB([esd], [esd_play_stream], [:], [MM_FAIL], [$esd_libs])

		## restore anything
		SXE_RESTORE_LIBS
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ESD

AC_DEFUN([SXE_MM_CHECK_ALSA], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for ALSA support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_SUCC
	ac_c_werror_flag=$save_ac_c_werror_flag
	AC_CHECK_HEADERS([alsa/input.h alsa/output.h alsa/global.h], [:], [MM_FAIL])
	AC_CHECK_HEADERS([alsa/conf.h], [:], [MM_FAIL], [[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/global.h>
]])
	AC_CHECK_HEADERS([alsa/pcm.h alsa/error.h alsa/version.h], [:], [MM_FAIL], [[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/global.h>
#include <alsa/conf.h>
]])

	ac_c_werror_flag=
	AC_MSG_CHECKING(for alsa version)
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/conf.h>
#include <alsa/global.h>
#include <alsa/pcm.h>
#include <alsa/error.h>
#include <alsa/version.h>
int main(int c, char *v[]) 
{
	fprintf(stdout, SND_LIB_VERSION_STR);
	return 0;
}]])], dnl [./conftest; alsa_subminor=$?],[alsa_subminor=$?],[alsa_subminor=0]
	[alsa_ver=`./conftest`], [:], [alsa_ver=])

	case "$alsa_ver" in
	0.*.* | 1.0.3* | 1.0.9* )
		AC_MSG_RESULT([ (known to break)])
		AC_MSG_WARN([Your ALSA version is _KNOWN_ to fail! Do not say we have not warned you!])
		;;
	1.0.2* | 1.0.4* | 1.0.5* | 1.0.6* | 1.0.7* | 1.0.8* ) 
		AC_MSG_RESULT([ (suspicious to break)])
		AC_MSG_WARN([Your ALSA version has not been tested. Do not be surprised if it fails!])
		;;
	1.0.10* | 1.0.11* | 1.0.12* | 1.0.13* | 1.0.14* )
		AC_MSG_RESULT([ (sane)])
		;;
	* )
		AC_MSG_RESULT([ (unknown)])
		AC_MSG_WARN([Your ALSA version is unknown and hence not supported!])
		MM_FAIL
		;;
	esac

	AC_CHECK_LIB([asound], [snd_pcm_open], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_close], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_free], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_any], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_access], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_free], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_channels], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_format], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_channels], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_format], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_rate_near], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_prepare], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_writei], [:], [MM_FAIL])

	## restore anything
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ALSA

AC_DEFUN([SXE_MM_CHECK_NAS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for NAS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	AC_CHECK_HEADERS([audio/audiolib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuOpenServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCloseServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCreateFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStartFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStopFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuScanForTypedEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuDispatchEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetIOErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuWriteElement], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElements], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuRegisterEventHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElementParameters], [:], [MM_FAIL])

	dnl If the nas library does not contain the error jump point,
	dnl then we force safer behavior.
	AC_EGREP_HEADER([AuXtErrorJump], [audio/Xtutil.h], [], [old_nas=yes])
	if test "$old_nas" = "yes"; then
		AC_DEFINE([NAS_NO_ERROR_JUMP], [1], [Description here!])
	fi

	## restore anything
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_NAS


dnl Option group miracle
dnl ====================
dnl helper funs
AC_DEFUN([OG_ARG_POSSVAL], [dnl
[Possible arguments:]ifelse($2,,,[ for $2])[]dnl
ifelse(dnl
$1,[og_any], [dnl
[ anything]],
$1,[og_file], [dnl
[ ]any file names],
$1,,[dnl
[ ]either `yes' or `no'],[dnl
[ ]one of [`]patsubst([$1],[ ],['[,] `])[']])])

AC_DEFUN([OG_OPTION_GROUP], [dnl
AC_ARG_WITH(xyzoptiongroup,[
[$1]
patsubst([$1],[.],[=])])])

pushdef([OG_ERROR_ADDITIONAL_VERBOSITY], [dnl
- If you do not see or know what the option --with-$1 is all about
  consider to consult $sxe_srcdir/configure --help.
- Also SXEmacs comes with a sophisticated set of default values which
  run out of the box on most machines so retry your configure line
  WITHOUT this option at all.
- If you used to pass the option `--with-$1=$withval' and it formerly
  has been accepted please consult $sxe_srcdir/configure --help and perhaps
  update your scripts.
- If you are absolutely sure that `$withval' _should_ work[,] try again!
  .oO(and see me barf again, too)
])

dnl borrowed from the openldap crew, slightly modified
dnl --------------------------------------------------------------------
dnl Restricted form of AC_ARG_WITH that limits user options
dnl
dnl $1 = option name
dnl $2 = help-string
dnl $3 = default value, can be auto or none
dnl $4 = default value documentation
dnl $5 = allowed values [yes no] if empty, can be [og_any]
dnl $6 = actions in case option was set (old AC_ARG_WITH arg #3)
dnl $7 = actions in case option was not set (old AC_ARG_WITH arg #4)
AC_DEFUN([OG_ARG_WITH], [# Option Group --with-$1
	## announce this option as valid option
	sxe_with_options="$sxe_with_options $1"

	AC_ARG_WITH($1,[
[$2]ifelse($5,[og_any],,[
AS_HELP_STRING([], (OG_ARG_POSSVAL($5)))])[]ifelse($4,,,[
AS_HELP_STRING([], [@<:@Default: $4@:>@])])],[
	ifelse($5,og_any,,[
		og_arg=invalid
		for og_val in ifelse($5,,[yes no],[$5]) ; do
			if test "$withval" = "$og_val" ; then
				og_arg="$og_val"
			fi
		done
		if test "$og_arg" = "invalid" ; then
			AS_MESSAGE(Wait a moment ... Are you nuts?)
			if test "`echo $withval | grep ','`"; then
				AC_MSG_ERROR([dnl
Bad multiple value `$withval' for --with-$1.
ifelse($2,[og_any],,[
AS_ESCAPE(OG_ARG_POSSVAL($5,[--with-$1]), [`])
(and remember do NOT specify 2 or 3 or 4 or more values, ONE is enough! Believe me!)

AS_ESCAPE(OG_ERROR_ADDITIONAL_VERBOSITY($1), [`])])])

			elif test "$withval" = "foo"; then
				AC_MSG_ERROR([dnl
--with-$1=foo?! In your dreams, man!
			])
			else
				AC_MSG_ERROR([dnl
Bad value `$withval' for --with-$1.
ifelse($5,[og_any],,[
AS_ESCAPE(OG_ARG_POSSVAL($5,[--with-$1]), [`])
(and nothing else, and especially not `$withval'!)

AS_ESCAPE(OG_ERROR_ADDITIONAL_VERBOSITY($1), [`])])])
			fi
		fi])
	[with_]translit([$1],[-],[_])="$withval"
	$6
],[dnl
	[with_]translit([$1],[-],[_])=ifelse($3,,"",$3,none,"",$3,auto,"",["$3"])
	$7])dnl
dnl AC_MSG_RESULT([Option Group --with-$1 $og_with_$1])
# end --with-$1
])dnl

dnl helper funs for multi arg case
dnl ==============================
dnl $1 - possible option values
dnl $2 - option name
AC_DEFUN([OG_MULTIARG_POSSVAL], [dnl
[Possible arguments:]ifelse($2,,,[ for $2])[]dnl
ifelse(dnl
$1,[og_any], [dnl
[ anything]],
$1,[og_file], [dnl
[ ]any file names],
$1,,[dnl
[ ]either `all' or `none'],[dnl
[ ]`all' or `none' as the first argument[,] followed by dnl
any (comma-separated) combination out of [`(no)]patsubst([$1],[ ],['[,] `(no)])['].])
])dnl OG_MULTIARG_POSSVAL

AC_DEFUN([OG_MULTIARG_MORE_ON_POSSVALS], [dnl
The `no' prefix hereby means not to include the item in question. dnl
E.g. combinations like `all[,]nofoo[,]nobar' are allowed to select dnl
everything but foo and bar. dnl
Later options in the chain override earlier ones. dnl
If `all' or `none' (as first argument) in the chain are omitted dnl
all items are supposed to be specified relative to the default value.])

dnl
dnl $1 = option name
dnl $2 = default value, can be auto, all, none, or any combination of $3
dnl $3 = allowed values, can be [og_any]
AC_DEFUN([OG_MULTIARG_PROCESS], [dnl
	pushdef([og_group], [translit([$1],[-],[_])])
	new_default=
        for og_val in $2 `echo "$withval" | sed -e 's/,/ /g'`; do
	        og_arg=invalid
		case "$og_val" in
		dnl all and none are only permitted as the first in the list.
		no | none )		new_default=no ;;
		yes | all | auto )	new_default=yes ;;
		esac
		ifelse($3,,,[
			case "$og_val" in
			translit([$3],[ ],[|]) )
				og_arg=valid
				eval with_[]og_group[]_${og_val}=yes;;
			[no]patsubst([$3],[ ],[|no]) )
				og_arg=valid
				eval `echo $og_val | sed -e 's/^no/with_[]og_group[]_/'`=no
			esac
		])
		if test -n "$new_default"; then
dnl 			for og_item in "[$3]"; do
dnl 				with_[]og_group[]_$og_item="$new_default"
dnl 			done
			with_[]og_group[]_[]patsubst([$3],[ ],[=$new_default; with_[]og_group[]_])[]=$new_default
				new_default=
				og_arg=valid
			fi
		if test "$og_arg" = "invalid" ; then
			AS_MESSAGE(Wait a moment ... dnl
Find a coffee factory! Drink it! And now listen:)
			AC_MSG_ERROR(dnl
Bad value `$og_val' for --with-$1.

m4_text_wrap(OG_MULTIARG_POSSVAL($3, [--with-$1])
OG_MULTIARG_MORE_ON_POSSVALS)

OG_ERROR_ADDITIONAL_VERBOSITY($1)
)
		fi
	done
        popdef([og_group])
])

dnl automatically generated help string
AC_DEFUN([OG_MULTIARG_HELP_STRINGS], [dnl
	pushdef([OG_MULTIARG_ITEM], AS_HELP_STRING([], - $[1] for $[3]))
	pushdef([OG_MULTIARG_MUTEX], [])
AS_HELP_STRING([], [Explanation of the items:])
$1
	popdef([OG_MULTIARG_MUTEX])
	popdef([OG_MULTIARG_ITEM])
])dnl OG_MULTIARG_HELP_STRINGS

dnl multiple arg form of OG_ARG_WITH, that is it accepts any (comma separated)
dnl combination of args
dnl
dnl $1 = option name
dnl $2 = help-string
dnl $3 = default value, can be auto or none
dnl $3 = default value documentation
dnl $5 = allowed values [yes no] if empty, can be [og_any]
dnl $6 = actions in case option was set (old AC_ARG_WITH arg #3)
dnl $7 = actions in case option was not set (old AC_ARG_WITH arg #4)
dnl improved version
AC_DEFUN([OG_MULTIARG_WITH], [# Option Group --with-$1 (multiarg)
	## announce this option as valid option
	sxe_with_options="$sxe_with_options $1"

	pushdef([og_group], [translit([$1],[-],[_])])
        pushdef([og_DefVal],ifelse($3,,auto,$3))
	pushdef([OG_MULTIARG_ITEM], dnl
		_OG_MULTIARG_ITEM($1, $[1], $[2], $[3]))
	pushdef([OG_MULTIARG_MUTEX], dnl
		_OG_MULTIARG_MUTEX($1, $[1], $[2], $[3], $[4]))
	$5
	popdef([OG_MULTIARG_ITEM])
	popdef([OG_MULTIARG_MUTEX])
	pushdef([OG_MULTIARG_ITEM], $[1])
	pushdef([OG_MULTIARG_MUTEX], [])
	pushdef([og_items],
		patsubst(
			patsubst(
				patsubst(
					patsubst([$5], [[	 ]+], []),
			[
], [ ]), [^ +], []), [ +$], [])) dnl autoconf is soooo brilliant, is it not? :)
	popdef([OG_MULTIARG_MUTEX])
	popdef([OG_MULTIARG_ITEM])

	pushdef([OG_HELP_STRING], [AS_HELP_STRING($[1], $[2])])
	pushdef([og_desc], [dnl
patsubst([$2], [^[ 	]AS_HELP_STRING], [AS_HELP_STRING])])
	pushdef([og_helps], [dnl
patsubst(patsubst(patsubst([OG_MULTIARG_HELP_STRINGS($5)], [^[ 	]+], []), [[ 	]+$], []), [^[ 	]*
], [])])

	dnl I personally shoot everybody who fiddles with the whitespace here!!!!
        AC_ARG_WITH($1, [
og_desc[]ifelse($5,[og_any],,[
AS_HELP_STRING([], OG_MULTIARG_POSSVAL(og_items))
AS_HELP_STRING([], OG_MULTIARG_MORE_ON_POSSVALS)
og_helps])[]ifelse($4,,,[dnl
AS_HELP_STRING([], [@<:@Default: $4@:>@])])],[

	## now process all supplied options (prepend the default opts)
	OG_MULTIARG_PROCESS($1,og_DefVal,og_items)
	$6
],[
	withval=
	with_[]og_group[]="og_DefVal"
	OG_MULTIARG_PROCESS($1,og_DefVal,og_items)
	$7
	# end --with-$1
])

	dnl AC_MSG_RESULT([Option Group --with-$1 $og_with_$1])
	popdef([og_desc])
	popdef([og_helps])
	popdef([og_items])
	popdef([og_DefVal])
	popdef([og_group])
])dnl

dnl Print a summary for OG_MULTIARG_WITH options
dnl
dnl $1 = option name
dnl $2 = headline for option
AC_DEFUN([OG_MULTIARG_SUMMARY], [# Option Group --with-$1 (multiarg)
	tmp_enabled=
	tmp_enabled_verb=
	tmp_disabled=
	tmp_omitted=
	tmp_omitted_verb=
	pushdef([og_group], [translit([$1],[-],[_])])
	pushdef([og_items], [ifelse([$4],[],[$sxe_og_[]og_group[]_items],[$4])])
	pushdef([og_mutices], [$sxe_og_[]og_group[]_mutices])
	pushdef([indent], [ifelse([$3],[],[],[$3])])

        for og_val in og_items; do
		if eval "test \"\$have_[]og_group[]_${og_val}\" = \"yes\""; then
			if eval "test \"\$with_[]og_group[]_${og_val}\" \
			   != \"yes\""; then
				tmp_enabled="$tmp_enabled $og_val+"
			else
				tmp_enabled="$tmp_enabled $og_val"
			fi

			if eval "test -n \
			   \"\$sxe_og_[]og_group[]_${og_val}_mutexgroup\""; then
				tmp_mtx_grp=$(eval "echo \$sxe_og_[]og_group[]_${og_val}_mutexgroup")
				tmp_mtx_grp_desc=$(eval "echo \$sxe_og_[]og_group[]_${tmp_mtx_grp}_mutexdesc")
				tmp_enabled_verb="$tmp_enabled_verb
echo \"[]indent[  ]  + ${tmp_mtx_grp} (${tmp_mtx_grp_desc})\"
echo \"[]indent[  ]    (X) \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc)\""
				tmp_enabled_verb="$tmp_enabled_verb
eval \"\${sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb}\""
				tmp_mtx_grp=
				tmp_mtx_grp_desc=

			else
				tmp_enabled_verb="$tmp_enabled_verb
echo \"[]indent[  ]  - \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc)\""
			fi
		elif eval "test \"\$with_[]og_group[]_${og_val}\" = \"yes\" \
			-a -n \"\$omit_[]og_group[]_${og_val}_in_favour_of\" \
			-a -n \"\$sxe_og_[]og_group[]_${og_val}_mutexgroup\""; then
			tmp_omitted="$tmp_omitted $og_val"
			tmp_mtx_grp=$(eval "echo \$sxe_og_[]og_group[]_${og_val}_mutexgroup")

			eval sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb="\"\$sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb
echo \\\"[]indent[  ]    ( ) \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc) available but omitted\\\"\""
			tmp_mtx_grp=
		elif eval "test \"\$with_[]og_group[]_${og_val}\" = \"yes\""; then
			tmp_disabled="$tmp_disabled $og_val*"
		else
			tmp_disabled="$tmp_disabled $og_val"
		fi
	done

	if test -z "$tmp_enabled"; then
		tmp_enabled="None."
	fi
	if test -z "$tmp_disabled"; then
		tmp_disabled="None."
	fi
	if test -z "$tmp_omitted"; then
		tmp_omitted="None."
	fi

	echo "indent[$2]:"
	echo "indent  Enabled [$2]: $tmp_enabled"
	eval "$tmp_enabled_verb"
	if test -n "$sxe_og_[]og_group[]_mutices"; then
		echo "indent  Omitted [$2]: $tmp_omitted"
		dnl eval "$tmp_omitted_verb"
	fi
	echo "indent  Disabled [$2]: $tmp_disabled"

	popdef([indent])
	popdef([og_mutices])
	popdef([og_items])
	popdef([og_group])
])

AC_DEFUN([_OG_MULTIARG_ITEM], [dnl
	# $1 option group name,
	# $2 item name
	# $3 short description
	# $4 long description
	pushdef([og_group], [translit([$1],[-],[_])])
	sxe_og_[]og_group[]_items="$sxe_og_[]og_group[]_items $2"
	sxe_og_[]og_group[]_[]$2[]_short="[$3]"
	sxe_og_[]og_group[]_[]$2[]_desc="[$4]"
	popdef([og_group])
])

AC_DEFUN([_OG_MULTIARG_MUTEX], [dnl
	# $1 option group name,
	# $2 mutex name
	# $3 mutex description
	# $4 item names in descending order of favouriteness
	pushdef([og_group], [translit([$1],[-],[_])])
	sxe_og_[]og_group[]_mutices="$sxe_og_[]og_group[]_mutices $2"
	sxe_og_[]og_group[]_[]$2[]_mutexdesc="[$3]"
	sxe_og_[]og_group[]_[]$2[]_mutexitems="[$4]"
	for og_item in [$4]; do
		## bind each mutex item to name the mutex group it belongs to
		eval "sxe_og_[]og_group[]_${og_item}_mutexgroup=\"[$2]\""
	done
	popdef([og_group])
])

AC_DEFUN([OG_CHECK_OPTION], [## arg #1 is with or enable, arg #2 the option
	pushdef([og_group], [$1])
	__foundp="nil"
	for __opt in $sxe_[]og_group[]_options; do
		if test "[$2]" = "$__opt"; then
			__foundp="t"
			break
		fi
	done
	if test "$__foundp" != "nil"; then
		[$3]
	else
		[$4]
	fi
	popdef([og_group])
])

AC_DEFUN([SXE_EMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description (unused atm)
	pushdef([emodname], [$1])
	pushdef([EMODNAME], [translit([$1], [a-z], [A-Z])])

	if test "$with_module_support" = "yes" -a \
		"$with_modules_[]emodname[]" = "yes"; then
		have_modules_[]emodname[]="yes"
	fi
	AM_CONDITIONAL([EMOD_]EMODNAME, [dnl
		test "$with_modules_[]emodname[]" = "yes"])
	AM_CONDITIONAL([DESCEND_]EMODNAME, [dnl
		test "$with_static_modules_[]emodname[]" = "yes" -o \
		"$with_modules_[]emodname[]" = "yes"])

	popdef([EMODNAME])
	popdef([emodname])
])dnl SXE_EMOD

AC_DEFUN([SXE_STATMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description (unused atm)
	pushdef([emodname], [$1])
	pushdef([EMODNAME], [translit([$1], [a-z], [A-Z])])

	if test "$with_module_support" = "yes" -a \
		"$with_static_modules_[]emodname[]" = "yes"; then
		have_static_modules_[]emodname[]="yes"
		AC_DEFINE([USE_STATIC_]EMODNAME, [1], [dnl
			Whether to use the module] emodname [statically])
		SXE_ADD_STATMOD_A([$emodblddir/]emodname[/lib]emodname[.a])
	fi
	AM_CONDITIONAL([STATIC_]EMODNAME, [dnl
		test "$with_static_modules_[]emodname[]" = "yes"])
	AM_CONDITIONAL([DESCEND_]EMODNAME, [dnl
		test "$with_static_modules_[]emodname[]" = "yes" -o \
		"$with_modules_[]emodname[]" = "yes"])

	popdef([EMODNAME])
	popdef([emodname])
])dnl SXE_STATMOD

AC_DEFUN([SXE_EMOD_STATMOD], [dnl
	## arg #1 is module name
	## arg #2 is short description		
	SXE_EMOD([$1], [$2])
	SXE_STATMOD([$1], [$2])
])dnl SXE_EMOD_STATMOD
