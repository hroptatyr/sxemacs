/* Synched up with: FSF 19.31. */

/* s/ file for netbsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef BSD

#ifndef NOT_C_CODE
#include <sys/param.h>
#include <sys/exec.h>
#endif				/* C_CODE */

/* For mem-limits.h.  */
#define BSD4_2

#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* netbsd uses OXTABS instead of the expected TAB3.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define LIBS_DEBUG
/* -lutil is not needed for NetBSD >0.9.  */
/* #define LIBS_SYSTEM -lutil */
/* XEmacs change */
#define LIBS_TERMCAP "-ltermcap"

#define NEED_ERRNO

#if 0				/* mrb */
#ifndef NO_SHARED_LIBS
/* These definitions should work for either dynamic or static linking,
   whichever is the default for `cc -nostdlib'.  */
/* but they probably don't, and life's too short - jrg@doc.ic.ac.uk
   ask for no shared libs if you have 0.9 */
/* mrb -- ORDINARY_LINK works just fine... */
#define LD_SWITCH_SYSTEM "-e start"
#define START_FILES "pre-crt0.o /usr/lib/crt0.o"
#define RUN_TIME_REMAP
#else
#define START_FILES "crt0.o"

#endif				/* not NO_SHARED_LIBS */
#endif				/* 0 - mrb */

#define HAVE_TEXT_START		/* No need to define `start_of_text'.  */
#define ORDINARY_LINK

/* As of this writing (Netbsd 1.5 was just released), Netbsd is
   converting from a.out to elf - x86 and Sparc are using ELF.
   But we're clever and let the compiler tell us which one to use.  */

/*
 * everything is pdump now. --SY
 * #ifdef __ELF__
 * #define UNEXEC "unexelf.o"
 * #else
 * #define UNEXEC "unexfreebsd.o"
 * #endif
 */
#undef UNEXEC

#if 0
/* Try to make this work for both 0.9 and >0.9.  */
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
/* #define N_TRELOFF(x) N_RELOFF(x) */
/* the 1.0 way.. */
#endif				/* 0 */

#define N_RELOFF(x) N_TRELOFF(x)

#define NO_MATHERR

#define AMPERSAND_FULL_NAME
