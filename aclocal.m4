dnl aclocal.m4 --- Dynamically linked library support for XEmacs
dnl Copyright (C) 1998, 1999 J. Kean Johnston.
dnl Author: J. Kean Johnston <jkj@sco.com>, based on work in libtool.
dnl This file is part of XEmacs.

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

AC_DEFUN(XE_SHLIB_STUFF,[
dll_ld=
dll_ldflags=
dll_cflags=
dll_post=
dll_ldo="-o"
ld_dynamic_link_flags=
xehost=$canonical
xealias=$internal_configuration

AC_CHECKING([how to build dynamic libraries for ${xehost}])
# Transform *-*-linux* to *-*-linux-gnu*, to support old configure scripts.
case "$xehost" in
*-*-linux-gnu*) ;;
*-*-linux*) xehost=`echo $xehost | sed 's/^\(.*-.*-linux\)\(.*\)$/\1-gnu\2/'`
esac

changequote(<<, >>)dnl
xehost_cpu=`echo $xehost | sed 's/^\([^-]*\)-\([^-]*\)-\(.*\)$/\1/'`
xehost_vendor=`echo $xehost | sed 's/^\([^-]*\)-\([^-]*\)-\(.*\)$/\2/'`
xehost_os=`echo $xehost | sed 's/^\([^-]*\)-\([^-]*\)-\(.*\)$/\3/'`
changequote([, ])dnl

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
  AC_MSG_CHECKING(checking whether we are using GNU C)
  AC_EGREP_CPP(yes,[
#ifdef __GNUC__
  yes;
#endif
],XEGCC=yes, XEGCC=no)
  AC_MSG_RESULT([${XEGCC}])
fi

AC_MSG_CHECKING(how to produce PIC code)
wl=

can_build_shared=yes
if test "$XEGCC" = yes -o "$__ICC" = yes; then
  wl='-Wl,'

  case "$xehost_os" in
  aix[[3-9]]* | irix[[5-9]]* | osf[[3-9]])
    # PIC is the default for these OSes.
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
  *cygwin* | *mingw* )
    # PIC is the default
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
  AC_TRY_COMPILE(,[int x=0;],[
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
dnl if the C compiler itself cannot create shared libraries to we try to
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
AC_MSG_CHECKING(if C compiler can produce shared libraries)
if test "$XEGCC" = yes -o "$__ICC" = yes; then
  xcldf="-shared"
  xldf="-shared"
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
  ac_link='${CC-cc} -o conftest $CFLAGS '"$xe_cppflags $xe_ldflags"' conftest.$ac_ext '"$xe_libs"' 1>&AC_FD_CC'
  AC_TRY_LINK(,[int x=0;],cc_produces_so=yes,cc_produces_so=no)
  LDFLAGS=$save_LDFLAGS
  LIBS=$save_LIBS
  xe_libs=$save_xe_libs
  ac_link='${CC-cc} -o conftest $CFLAGS '"$xe_cppflags $xe_ldflags"' conftest.$ac_ext '"$xe_libs"' 1>&AC_FD_CC'
else
  cc_produces_so=no
fi
AC_MSG_RESULT([${cc_produces_so}])

LTLD=$LD
if test -z "$LTLD"; then
  ac_prog=ld
  if test "$XEGCC" = yes; then
    # Check if gcc -print-prog-name=ld gives a path.
    AC_MSG_CHECKING(for ld used by GCC)
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
    AC_MSG_CHECKING(for GNU ld)
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
AC_MSG_CHECKING(if the linker is GNU ld)
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
  dll_ldflags=$xcldf
  can_build_shared=yes
  ld_shlibs=yes
else
  # OK - only NOW do we futz about with ld.
  # See if the linker supports building shared libraries.
  AC_MSG_CHECKING(whether the linker supports shared libraries)
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

