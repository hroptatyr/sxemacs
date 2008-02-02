dnl sxe-arch.m4 -- Architecture specific stuff

AC_DEFUN([SXE_CHECK_MACHARCH], [dnl
dnl ------------------------------
dnl Determine the s&m files to use
dnl ------------------------------
dnl Given the configuration name, set machfile and opsysfile to the
dnl names of the m/*.h and s/*.h files we should use.

dnl If you add support for a new configuration, add code to this
dnl switch statement to recognize your configuration name and select
dnl the appropriate operating system and machine description files.

dnl You would hope that you could choose an m/*.h file pretty much
dnl based on the machine portion of the configuration name, and an s-
dnl file based on the operating system portion.  However, it turns out
dnl that each m/*.h file is pretty manufacturer-specific - for
dnl example, apollo.h, hp9000s300.h, mega68k, news.h, and tad68k are
dnl all 68000 machines; mips.h, pmax.h, and news-risc are all MIPS
dnl machines.  So we basically have to have a special case for each
dnl configuration name.

dnl As far as handling version numbers on operating systems is
dnl concerned, make sure things will fail in a fixable way.  If
dnl /etc/MACHINES says nothing about version numbers, be
dnl prepared to handle anything reasonably.  If version numbers
dnl matter, be sure /etc/MACHINES says something about it.

dnl Eric Raymond says we should accept strings like "sysvr4" to mean
dnl "System V Release 4"; he writes, "The old convention encouraged"
dnl "confusion between `system' and `release' levels'."

machine='' opsys=''
LDD="ldd"

dnl Straightforward machine determination
case "$ac_cv_build" in
	sparc-*-*        ) machine=sparc ;;
	alpha*-*-*       ) machine=alpha ;;
	vax-*-*          ) machine=vax ;;
	mips-dec-*       ) machine=pmax ;;
	mips-sgi-irix6*  ) machine=iris6d ;;
	mips-sgi-*       ) machine=iris4d ;;
	mips*-linux      ) machine=mips ;;
	romp-ibm-*       ) machine=ibmrt ;;
	rs6000-ibm-aix*  ) machine=ibmrs6000 ;;
	powerpc-ibm-aix* ) machine=ibmrs6000 ;;
	powerpc*-*       ) machine=powerpc ;;
	hppa-*-*         ) machine=hp800 ;;
	m88k-dg-*        ) machine=aviion ;;
	m68*-sony-*      ) machine=news ;;
	mips-sony-*      ) machine=news-risc ;;
	clipper-*        ) machine=clipper ;;
	arm*             ) machine=arm ;;
	ns32k-*          ) machine=ns32000 ;;
esac

dnl Straightforward OS determination
case "$ac_cv_build" in
	*-*-linux*    ) opsys=linux ;;
	*-*-netbsd*   ) opsys=netbsd ;;
	*-*-openbsd*	) opsys=openbsd ;;
	*-*-nextstep* ) opsys=nextstep ;;
	*-*-vms       ) opsys=vms ;;

	dnl DEC OSF
	*-dec-osf1.3 | *-dec-osf2* ) opsys=decosf1-3 ;;
	*-dec-osf1.2 | *-dec-osf1* ) opsys=decosf1-2 ;;
	*-dec-osf3.[[2-9]]         ) opsys=decosf3-2 ;;
	*-dec-osf3*                ) opsys=decosf3-1 ;;
	*-dec-osf[[4-9]]*          ) opsys=decosf4-0 ;;

	dnl DEC Ultrix
	*-*-ultrix[[0-3]].* | *-*-ultrix4.0* ) opsys=bsd4-2 ;;
	*-*-ultrix4.[[12]]* ) opsys=bsd4-3 ;;
	*-*-ultrix* )         opsys=ultrix4-3 ;;

	dnl AIX
	*-*-aix3.1*     ) opsys=aix3-1   ;;
	*-*-aix3.2.5    ) opsys=aix3-2-5 ;;
	*-*-aix3*       ) opsys=aix3-2   ;;
	*-*-aix4.0*     ) opsys=aix4	   ;;
	*-*-aix4.1*     ) opsys=aix4-1   ;;
	*-*-aix[[4-9]]* ) opsys=aix4-2   ;;

	dnl Other generic OSes
	*-gnu* )			opsys=gnu    ;;
	*-*-bsd4.[[01]] )		opsys=bsd4-1 ;;
	*-*-bsd4.2 )			opsys=bsd4-2 ;;
	*-*-bsd4.3 )			opsys=bsd4-3 ;;
	*-*-aos4.2 )			opsys=bsd4-2 ;;
	*-*-aos*   )			opsys=bsd4-3 ;;
	*-*-sysv0    | *-*-sysvr0 )	opsys=usg5-0 ;;
	*-*-sysv2    | *-*-sysvr2 )	opsys=usg5-2 ;;
	*-*-sysv2.2  | *-*-sysvr2.2 )	opsys=usg5-2-2 ;;
	*-*-sysv3*   | *-*-sysvr3* )	opsys=usg5-3 ;;
	*-*-sysv4.1* | *-*-sysvr4.1* )opsys=usg5-4 NON_GNU_CPP=/usr/lib/cpp ;;
	*-*-sysv4.[[2-9]]* | *-sysvr4.[[2-9]]* )
		if test -z "$NON_GNU_CPP" ; then
			for prog in "/usr/ccs/lib/cpp" "/lib/cpp"; do
				if test -f "$prog"; then
					NON_GNU_CPP="$prog"
					break
				fi
			done
		fi
		opsys=usg5-4-2 ;;
	*-sysv4* | *-sysvr4* )	opsys=usg5-4 ;;
	*-*-mach_bsd4.3* )          opsys=mach-bsd4-3 ;;
esac

case "$ac_cv_build" in

  dnl NetBSD ports
  *-*-netbsd* )
    case "ac_cv_build" in
      i[[3-9]]86-*-netbsd*) machine=intel386 ;;
      hp300-*-netbsd* | amiga-*-netbsd* | sun3-*-netbsd* | mac68k-*-netbsd* | da30-*-netbsd* | m68k-*-netbsd* )
                      dnl Yes, this is somewhat bogus.
                      machine=hp9000s300 ;;
      pc532-*-netbsd* | ns32k-*-netbsd* )  machine=ns32000 ;;
      pmax-*-netbsd*  | mips-*-netbsd*  )  machine=pmax ;;
    esac
  ;;

  dnl OpenBSD ports
  *-*-openbsd* )
    case "${ac_cv_build}" in
      i386-*-openbsd*)		machine=intel386 ;;
      m68k-*-openbsd*)		machine=hp9000s300 ;;
      mipsel-*-openbsd*)	machine=pmax ;;
     esac
   ;;

  dnl Acorn RISCiX:
  arm-acorn-riscix1.1* ) machine=acorn opsys=riscix1-1 ;;
  arm-acorn-riscix1.2* | arm-acorn-riscix ) machine=acorn opsys=riscix1-2 ;;

  dnl Alliant machines
  fx80-alliant-* ) machine=alliant4     opsys=bsd4-2 ;;
  i860-alliant-* ) machine=alliant-2800 opsys=bsd4-3 ;;

  dnl Altos 3068
  m68*-altos-sysv* ) machine=altos opsys=usg5-2 ;;

  dnl Amdahl UTS
  580-amdahl-sysv* ) machine=amdahl opsys=usg5-2-2 ;;

  dnl Apollo, Domain/OS
  m68*-apollo-* ) machine=apollo opsys=bsd4-3 ;;

  dnl AT&T 3b2, 3b5, 3b15, 3b20
  we32k-att-sysv* ) machine=att3b opsys=usg5-2-2 ;;
  dnl AT&T 3b1 - The Mighty Unix PC!
  m68*-att-sysv* ) machine=7300 opsys=usg5-2-2 ;;

  dnl Bull machines
  rs6000-bull-bosx* ) machine=ibmrs6000 opsys=aix3-2 ;; # dpx20
  m68*-bull-sysv3*  ) machine=dpx2      opsys=usg5-3 ;; # dpx2
  m68*-bull-sysv2*  ) machine=sps7      opsys=usg5-2 ;; # sps7

  dnl CCI 5/32, 6/32 -- see "Tahoe".

  dnl Celerity
  celerity-celerity-bsd* ) machine=celerity opsys=bsd4-2 ;;

  dnl Convex
  *-convex-bsd* | *-convex-convexos* )
    machine=convex opsys=bsd4-3
    NON_GNU_CPP="cc -E -P"
  ;;

  dnl Cubix QBx/386
  i[[3-9]]86-cubix-sysv* ) machine=intel386 opsys=usg5-3 ;;

  dnl Darwin, a.k.a. MacOS X (based on Mach and Freebsd)
  *-*-darwin*)
    opsys=darwin
    LDD="otool -XL"
    ;;
  dnl Data General AViiON Machines
  i586-dg-dgux*R4*   | i586-dg-dgux5.4.4* ) machine=aviion opsys=dgux5-4r4 ;;
  m88k-dg-dgux5.4R3* | m88k-dg-dgux5.4.3* ) opsys=dgux5-4r3 ;;
  m88k-dg-dgux5.4R2* | m88k-dg-dgux5.4.2* ) opsys=dgux5-4r2 ;;
  m88k-dg-dgux* 			  ) opsys=dgux	   ;;

  dnl Motorola Delta machines
  m68k-motorola-sysv* | m68000-motorola-sysv* ) machine=delta opsys=usg5-3 ;;
  m88k-motorola-sysv4* )
    dnl jbotte@bnr.ca says that UNIX_System_V <hostName> 4.0 R40V4.3 m88k mc88110
    dnl needs POSIX_SIGNALS and therefore needs usg5-4-2.
    dnl I hope there are not other 4.0 versions for this machine
    dnl which really need usg5-4 instead.
    machine=delta88k opsys=usg5-4-2
  ;;
  m88k-motorola-sysv* | m88k-motorola-m88kbcs* ) machine=delta88k opsys=usg5-3 ;;

  dnl Dual machines
  m68*-dual-sysv*    ) machine=dual opsys=usg5-2   ;;
  m68*-dual-uniplus* ) machine=dual opsys=unipl5-2 ;;

  dnl Encore machines
  ns16k-encore-bsd* ) machine=ns16000 opsys=umax ;;

  dnl Gould Power Node and NP1
  pn-gould-bsd4.2* ) machine=gould     opsys=bsd4-2 ;;
  pn-gould-bsd4.3* ) machine=gould     opsys=bsd4-3 ;;
  np1-gould-bsd* )   machine=gould-np1 opsys=bsd4-3 ;;

  dnl Harris ecx or gcx running CX/UX (Series 1200, Series 3000)
  m68k-harris-cxux* ) machine=nh3000 opsys=cxux ;;
  dnl Harris power pc NightHawk running Power UNIX (Series 6000)
  powerpc-harris-powerunix ) machine=nh6000 opsys=powerunix NON_GNU_CPP="cc -Xo -E -P" ;;

  dnl Honeywell XPS100
  xps*-honeywell-sysv* ) machine=xps100 opsys=usg5-2 ;;

  dnl HP 9000 series 200 or 300
  m68*-hp-bsd* ) machine=hp9000s300 opsys=bsd4-3 ;;

  dnl HP-UX
  *-hp-hpux* )
    dnl Figure out machine and opsys orthogonally
    case "$ac_cv_build" in
      m68*  ) machine=hp9000s300 ;;
      hppa* ) machine=hp800      ;;
    esac

    case "$ac_cv_build" in
      *-hp-hpux7*  )  opsys=hpux   ;;
      *-hp-hpux8*  )  opsys=hpux8  ;;
      *-hp-hpux9*  )  opsys=hpux9  ;;
      *-hp-hpux10* )  opsys=hpux10 ;;
      *-hp-hpux11* )  opsys=hpux11 ;;
      *            )  opsys=hpux   ;;
    esac

    dnl HP has a broken "strcat"
    case "$opsys" in hpux9 | hpux10 ) SXE_ADD_OBJS(strcat.o) ;; esac

    if test "$opsys" = "hpux10" -o "$opsys" = "hpux11"; then \
	ansi_flag="-Ae"; else ansi_flag="-Aa"; fi
    NON_GNU_CC="cc $ansi_flag" NON_GNU_CPP="cc $ansi_flag -E"

    case "$ac_cv_build" in *-hp-hpux*shr* ) opsys="${opsys}-shr" ;; esac
  ;;

  dnl Orion machines
  orion-orion-bsd*   ) machine=orion    opsys=bsd4-2 ;;
  clipper-orion-bsd* ) machine=orion105 opsys=bsd4-2 ;;

  dnl IBM machines
  i[[3-9]]86-ibm-aix1.1* ) machine=ibmps2-aix opsys=usg5-2-2 ;;
  i[[3-9]]86-ibm-aix1.[[23]]* | i[[3-9]]86-ibm-aix* ) machine=ibmps2-aix opsys=usg5-3 ;;
  i370-ibm-aix*) machine=ibm370aix opsys=usg5-3 ;;
  romp-ibm-aos*    ) opsys=bsd4-3 ;;
  romp-ibm-bsd*    ) opsys=bsd4-3 ;;
  romp-ibm-mach*   ) opsys=mach-bsd4-3 ;;

  dnl Integrated Solutions "Optimum V"
  m68*-isi-bsd4.2* ) machine=isi-ov opsys=bsd4-2 ;;
  m68*-isi-bsd4.3* ) machine=isi-ov opsys=bsd4-3 ;;

  dnl Intel 386 machines where we do care about the manufacturer
  i[[3-9]]86-intsys-sysv* ) machine=is386 opsys=usg5-2-2 ;;

  dnl Prime EXL
  i[[3-9]]86-prime-sysv* ) machine=i386 opsys=usg5-3 ;;

  dnl Sequent Symmetry running Dynix
  i[[3-9]]86-sequent-bsd* ) machine=symmetry opsys=bsd4-3 ;;

  dnl Sequent Symmetry running DYNIX/ptx
  i[[3-9]]86-sequent-ptx* ) machine=sequent-ptx opsys=ptx NON_GNU_CPP="/lib/cpp" ;;

  dnl Unspecified sysv on an ncr machine defaults to svr4.2.
  dnl (Plain usg5-4 does not turn on POSIX signals, which we need.)
  i[[3-9]]86-ncr-sysv* ) machine=ncr386 opsys=usg5-4-2 ;;

  dnl Intel Paragon OSF/1
  i860-intel-osf1* ) machine=paragon opsys=osf1 NON_GNU_CPP=/usr/mach/lib/cpp ;;

  dnl Intel 860
  i860-*-sysv4* ) machine=i860 opsys=usg5-4 NON_GNU_CC="/bin/cc" NON_GNU_CPP="/usr/ccs/lib/cpp" ;;

  dnl Masscomp machines
  m68*-masscomp-rtu* ) machine=masscomp opsys=rtu ;;

  dnl Megatest machines
  m68*-megatest-bsd* ) machine=mega68 opsys=bsd4-2 ;;

  dnl Workstations sold by MIPS
  dnl This is not necessarily all workstations using the MIPS processor -
  dnl Irises are produced by SGI, and DECstations by DEC.
  mips-mips-usg* ) machine=mips4 ;;
  mips-mips-riscos4 )
    machine=mips4
    NON_GNU_CC="cc -systype bsd43"
    NON_GNU_CPP="cc -systype bsd43 -E"
    case "$ac_cv_build" in
      mips-mips-riscos4* ) opsys=bsd4-3  ;;
      mips-mips-riscos5* ) opsys=riscos5 ;;
    esac
  ;;
  mips-mips-bsd* ) machine=mips opsys=bsd4-3 ;;
  mips-mips-*    ) machine=mips opsys=usg5-2-2 ;;

  dnl NeXT
  m68*-next-* | m68k-*-nextstep* ) machine=m68k opsys=nextstep ;;

  dnl The complete machine from National Semiconductor
  ns32k-ns-genix* ) machine=ns32000 opsys=usg5-2 ;;

  dnl NCR machines
  m68*-ncr-sysv2* | m68*-ncr-sysvr2* ) machine=tower32   opsys=usg5-2-2 ;;
  m68*-ncr-sysv3* | m68*-ncr-sysvr3* ) machine=tower32v3 opsys=usg5-3 ;;

  dnl Nixdorf Targon 31
  m68*-nixdorf-sysv* ) machine=targon31 opsys=usg5-2-2 ;;

  dnl Nu (TI or LMI)
  m68*-nu-sysv* ) machine=nu opsys=usg5-2 ;;

  dnl Plexus
  m68*-plexus-sysv* ) machine=plexus opsys=usg5-2 ;;

  dnl Pyramid machines
  pyramid-pyramid-bsd* ) machine=pyramid opsys=bsd4-2 ;;

  dnl Sequent Balance
  ns32k-sequent-bsd4.2* ) machine=sequent opsys=bsd4-2 ;;
  ns32k-sequent-bsd4.3* ) machine=sequent opsys=bsd4-3 ;;

  dnl Siemens Nixdorf
  mips-siemens-sysv* | mips-sni-sysv*)
    machine=mips-siemens opsys=usg5-4
    NON_GNU_CC=/usr/ccs/bin/cc
    NON_GNU_CPP=/usr/ccs/lib/cpp
  ;;

  dnl NEC
  mips-nec-sysv*)
    machine=mips-nec
    NON_GNU_CC=/usr/ccs/bin/cc
    NON_GNU_CPP=/usr/ccs/lib/cpp
  ;;

  dnl Silicon Graphics machines
  dnl Iris 2500 and Iris 2500 Turbo (aka the Iris 3030)
  m68*-sgi-iris3.5* ) machine=irist opsys=iris3-5 ;;
  m68*-sgi-iris3.6* | m68*-sgi-iris*) machine=irist opsys=iris3-6 ;;
  dnl Iris 4D
  mips-sgi-irix3.*    ) opsys=irix3-3 ;;
  mips-sgi-irix4.*    ) opsys=irix4-0 ;;
  mips-sgi-irix6*     ) opsys=irix6-0 ;;
  mips-sgi-irix5.1*   ) opsys=irix5-1 ;;
  mips-sgi-irix5.2*   ) opsys=irix5-2 ;;
  mips-sgi-irix5.*    ) opsys=irix5-3 ;;
  mips-sgi-irix*      ) opsys=irix5-0 ;;

  dnl SONY machines
  *-sony-newsos[[34]]* | *-sony-news[[34]]* ) opsys=bsd4-3 ;;
  *-sony-news* ) opsys=newsos5 ;;

  dnl Stride
  m68*-stride-sysv* ) machine=stride opsys=usg5-2 ;;

  dnl Suns
  *-*-solaris* | *-*-sunos* | *-sun-mach* | *-sun-bsd* )
    dnl Hardware type
    case "$ac_cv_build" in
      m68*-sunos1* )	         machine=sun1     ;;
      m68*-sunos2* )	         machine=sun2     ;;
      m68* )		         machine=sun3     ;;
      i*86*-sun-sunos[[34]]* )   machine=sun386   ;;
      i*86-*-* )	         machine=intel386 ;;
      rs6000* )                  machine=rs6000   ;;
    esac

    dnl Make $canonical even more so.
    case "$ac_cv_build" in *-sunos5*)
      ac_cv_build=`echo $ac_cv_build | sed -e s/sunos5/solaris2/`;;
    esac

    case "$ac_cv_build" in
      *-solaris* )
	opsys=sol2
	os_release_major=`uname -r | sed -e 's/^\([[0-9]]\{1,\}\)\.\([[0-9]]\{1,\}\).*/\1/'`
	os_release_minor=`uname -r | sed -e 's/^\([[0-9]]\{1,\}\)\.\([[0-9]]\{1,\}\).*/\2/'`
	case "$os_release_minor" in [[0-9]])
	  os_release_minor="0${os_release_minor}";;
	esac
	os_release="${os_release_major}${os_release_minor}"
	AC_DEFINE_UNQUOTED([OS_RELEASE], [$os_release], [Description here!]) ;;

      dnl The last Sun386 ran 4.0.
      i*86-*-sunos4*      ) opsys=sunos4-0	;;
      *-sunos4.0*	  ) opsys=sunos4-0	;;
      *-sunos4.1.2*	  ) opsys=sunos4-1-2	;;
      *-sunos4.1.3*	  ) opsys=sunos4-1-3	;;
      *-sunos4.1.[[4-9]]* ) opsys=sunos4-1-4	;;
      *-sunos4* | *-sunos ) opsys=sunos4-1	;;
      *-mach*		  ) opsys=mach-bsd4-3	;;
      *			  ) opsys=bsd4-2	;;
    esac

    case "$ac_cv_build" in *-sunos4*shr* ) opsys="${opsys}-shr" ;; esac
  ;;

  dnl Tadpole 68k
  m68*-tadpole-sysv* ) machine=tad68k opsys=usg5-3 ;;

  dnl Tahoe machines
  tahoe-tahoe-bsd4.2* ) machine=tahoe opsys=bsd4-2 ;;
  tahoe-tahoe-bsd4.3* ) machine=tahoe opsys=bsd4-3 ;;

  dnl Tandem Integrity S2
  mips-tandem-sysv* ) machine=tandem-s2 opsys=usg5-3 ;;

  dnl Tektronix XD88
  m88k-tektronix-sysv3* ) machine=tekxd88 opsys=usg5-3 ;;

  dnl Tektronix 16000 box (6130?)
  ns16k-tektronix-bsd* ) machine=ns16000 opsys=bsd4-2 ;;
  dnl Tektronix 4300
  dnl src/m/tek4300.h hints that this is a m68k machine.
  m68*-tektronix-bsd* ) machine=tek4300 opsys=bsd4-3 ;;

  dnl Titan P2 or P3
  titan-titan-sysv* ) machine=titan opsys=usg5-3 ;;

  dnl Ustation E30 (SS5E)
  m68*-unisys-uniplus* ) machine=ustation opsystem=unipl5-2 ;;

  dnl Vaxen.
  vax-dec-* )
    case "$ac_cv_build" in
      *-sysv[[01]]* | *-sysvr[[01]]* ) 	opsys=usg5-0 ;;
      *-sysv2* | *-sysvr2* )		opsys=usg5-2 ;;
      *-mach* )				opsys=mach-bsd4-3 ;;
    esac
  ;;

  dnl Whitechapel MG1
  ns16k-whitechapel-* ) machine=mg1 ;;

  dnl Wicat
  m68*-wicat-sysv* ) machine=wicat opsys=usg5-2 ;;

  dnl Intel 386 machines where we do not care about the manufacturer
  i[[3-9]]86-*-* )
    machine=intel386
    case "$ac_cv_build" in
      *-isc1.* | *-isc2.[[01]]* ) opsys=386-ix ;;
      *-isc2.2* )		opsys=isc2-2 ;;
      *-isc4.0* )		opsys=isc4-0 ;;
      *-isc4.* )		opsys=isc4-1
				GCC_TEST_OPTIONS=-posix
				NON_GCC_TEST_OPTIONS=-Xp
				;;
      *-isc* )			opsys=isc3-0 ;;
      *-esix5* )		opsys=esix5r4 NON_GNU_CPP=/usr/lib/cpp ;;
      *-esix* )			opsys=esix ;;
      *-mach* )			opsys=mach-bsd4-3 ;;
      *-xenix* )		opsys=xenix ;;
      *-sco3.2v4* )		opsys=sco4 NON_GNU_CPP=/lib/cpp  ;;
      *-bsd386* | *-bsdi1* )	opsys=bsd386 ;;
      *-bsdi4* )		opsys=bsdos4 ;;
      *-bsdi3* )		opsys=bsdos3 ;;
      *-bsdi2.1* )		opsys=bsdos2-1 ;;
      *-bsdi2* )		opsys=bsdos2 ;;
      *-sco3.2v5* )		opsys=sco5 ;;
      *-sysv5* )		opsys=sco7 ;;
      *-386bsd* )	        opsys=386bsd ;;
      *-freebsd* )		opsys=freebsd ;;
      *-nextstep* )		opsys=nextstep ;;
    esac
  ;;

  dnl Linux/68k
  m68k-*-linux* ) machine=m68k opsys=linux ;;

esac

dnl Initialize machine from $canonical if not in our database above.
test -z "$machine" && machine=`echo $ac_cv_build | sed 's/-.*$//'`

dnl Initialize opsys from `uname -s` if not in our database above.
test -z "$opsys"   && opsys=`uname -s | tr ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz`

dnl Use configure-time autodetection if s&m not available
if test -r "${sxe_srcdir}/src/m/${machine}.h"; then
	machfile="m/${machine}.h"
	AC_DEFINE_UNQUOTED([config_machfile], ["$machfile"], [Description here!])
else
	echo "SXEmacs has no builtin knowledge of \`$machine' machines."
	echo "Using configure-time autodetection only."
fi

if test -r "${sxe_srcdir}/src/s/${opsys}.h"; then
	opsysfile="s/${opsys}.h"
	AC_DEFINE_UNQUOTED([config_opsysfile], ["$opsysfile"], [Description here!])
else
	echo "SXEmacs has no builtin knowledge of \`$opsys' operating systems."
	echo "Using configure-time autodetection only."
fi
])dnl SXE_CHECK_MACHARCH


dnl CPP_to_sh(CPP_SYMBOL, SH_VAR, DEFAULT_VALUE)
define([CPP_to_sh],
[[#]ifndef [$1]
[#]define [$1]ifelse([$3],,, [ "$3"])
[#]endif
configure___ [$2]=[$1]
])dnl CPP_to_sh

dnl CPP_boolean_to_sh(CPP_SYMBOL, SH_VAR)
define([CPP_boolean_to_sh],
[[#]ifdef [$1]
configure___ [$2]=yes
[#]else
configure___ [$2]=no
[#]endif
])dnl CPP_boolean_to_sh

AC_DEFUN([SXE_EXTRACT_MACHARCH_INFO], [dnl
dnl It is not important that this name contain the PID; you cannot run
dnl two configures in the same directory and have anything work
dnl anyway.
tempcname="conftest.c"

cat > $tempcname < confdefs.h
cat >> $tempcname <<EOF
#define NOT_C_CODE
#define C_SWITCH_SITE
#define C_SWITCH_X_SITE
#define LD_SWITCH_SITE
#define LD_SWITCH_X_SITE
#define LD_SWITCH_X_SITE_AUX
#define OS_RELEASE $os_release

#ifdef config_opsysfile
#include "$sxe_srcdir/src/$opsysfile"
#endif

#ifdef config_machfile
#include "$sxe_srcdir/src/$machfile"
#endif

CPP_to_sh([LIBS_MACHINE], libs_machine)
CPP_to_sh([LIBS_SYSTEM],  libs_system)
CPP_to_sh([LIBS_TERMCAP], libs_termcap)
CPP_to_sh([LIB_STANDARD], libs_standard)

CPP_to_sh(OBJECTS_MACHINE, objects_machine)
CPP_to_sh(OBJECTS_SYSTEM,  objects_system)

CPP_to_sh(C_SWITCH_MACHINE,   c_switch_machine)
CPP_to_sh(C_SWITCH_SYSTEM,    c_switch_system)

CPP_to_sh(LD_SWITCH_MACHINE,  ld_switch_machine)
CPP_to_sh(LD_SWITCH_SYSTEM,   ld_switch_system)

CPP_to_sh(UNEXEC, unexec)

CPP_to_sh(SYSTEM_TYPE, system_type)

CPP_to_sh(LD_SWITCH_SHARED, ld_switch_shared, -c)

#define ORDINARY_LD "\$(CC) \$(CFLAGS)"
configure___ ordinary_ld=ORDINARY_LD

#ifdef ORDINARY_LINK
#define LD ORDINARY_LD
#else /* no ORDINARY LINK */
#ifdef COFF_ENCAPSULATE
#define LD "\$(CC) -nostdlib"
#else /* not COFF_ENCAPSULATE */
#ifdef LINKER
#define LD LINKER
#else /* ! defined (LINKER) */
#define LD "ld"
#endif /* ! defined (LINKER) */
#endif /* ! defined (COFF_ENCAPSULATE) */
#endif /* not ORDINARY_LINK */
configure___ ld=LD

CPP_to_sh(LIB_GCC, lib_gcc)
CPP_to_sh(LD_TEXT_START_ADDR, ld_text_start_addr)

#if ! defined (ORDINARY_LINK) && !defined (START_FILES)
#ifdef NO_REMAP
#ifdef COFF_ENCAPSULATE
#define START_FILES "pre-crt0.o /usr/local/lib/gcc-crt0.o"
#else /* ! defined (COFF_ENCAPSULATE) */
#define START_FILES "pre-crt0.o /lib/crt0.o"
#endif /* ! defined (COFF_ENCAPSULATE) */
#else /* ! defined (NO_REMAP) */
#define START_FILES "ecrt0.o"
#endif /* ! defined (NO_REMAP) */
#endif /* no ORDINARY_LINK */
#ifndef START_FILES
#define START_FILES
#endif
configure___ start_files=START_FILES

CPP_boolean_to_sh(ORDINARY_LINK, ordinary_link)
CPP_boolean_to_sh(SYSTEM_MALLOC, system_malloc)
CPP_boolean_to_sh(TERMINFO, have_terminfo)
dnl The MAIL_USE_xxx variables come from the s&m headers
CPP_boolean_to_sh(MAIL_USE_FLOCK, mail_use_flock)
CPP_boolean_to_sh(MAIL_USE_LOCKF, mail_use_lockf)
CPP_boolean_to_sh(MAIL_USE_LOCKING, mail_use_locking)
EOF

dnl The value of CPP is a quoted variable reference, so we need to do this
dnl to get its actual value...
CPP=$(eval "echo $CPP $CPPFLAGS")
define(TAB, [	])dnl
eval $($CPP -Isrc $tempcname \
	| sed -n -e "s/[[ TAB]]*=[[ TAB\"]]*/='/" -e "s/[[ TAB\"]]*\$/'/" -e "s/^configure___//p")

rm $tempcname

])dnl SXE_EXTRACT_MACHARCH_INFO


dnl sxe-arch.m4 ends here
