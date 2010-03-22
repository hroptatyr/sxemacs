/* Synched up with: FSF 19.31. */

/* s/ file for freebsd system.  */

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.
   You can inhibit them on newer systems if you wish
   by defining NO_SHARED_LIBS.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h. */
#define BSD4_2

/* These aren't needed, since we have getloadavg.  */
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define INTERRUPTIBLE_OPEN

#define LIBS_DEBUG
/* FreeBSD 2.2 or later */
#ifndef __FreeBSD_version
#include <osreldate.h>
#endif
#if __FreeBSD_version >= 199701 && __FreeBSD_version < 600006
#define LIBS_SYSTEM "-lutil -lxpg4"
#else
#define LIBS_SYSTEM "-lutil"
#endif

#ifndef NOT_C_CODE
#ifdef BSD			/* fixing BSD define */
#undef BSD
#endif
#include <sys/param.h>
/* Kludge to work around setlocale(LC_ALL,...) not working after 01/1997 */
#if __FreeBSD_version >= 199701 && __FreeBSD_version < 226000
#ifdef HAVE_X_WINDOWS
#include <X11/Xlocale.h>
#define setlocale(locale_category, locale_spec) setlocale(LC_CTYPE, locale_spec)
#endif				/* HAVE X */
#endif				/* FreeBSD >= 199701 && < 226000 */
#endif				/* C code */

#define LIBS_TERMCAP "-ltermcap"

#ifdef __ELF__			/* since from 3.0-CURRENT(maybe 19980831 or later) */
#ifndef NOT_C_CODE
#include <stddef.h>
#endif
#define LD_SWITCH_SYSTEM
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtbegin.o
/*
 * everything is pdump now. --SY
 * #define UNEXEC "unexelf.o"
 */
#undef UNEXEC
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o /usr/lib/crtn.o
#define LINKER "$(CC) -nostdlib"
#undef LIB_GCC
#define LIB_GCC

#else				/* not __ELF__ */

#ifndef NO_SHARED_LIBS
#if 0				/* mrb */
#define LIB_GCC "-lgcc"
#define LD_SWITCH_SYSTEM "-dc -dp -e start"
#define START_FILES "pre-crt0.o /usr/lib/crt0.o"
#else				/* mrb */
#define ORDINARY_LINK
#undef LIB_GCC
#undef LD_SWITCH_SYSTEM
#undef START_FILES
#endif				/* mrb */

#define HAVE_TEXT_START		/* No need to define `start_of_text'. */
/* #define UNEXEC "unexfreebsd.o" */
#define RUN_TIME_REMAP

#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif
#else				/* NO_SHARED_LIBS */
#ifdef __FreeBSD__		/* shared libs are available, but the user prefers
				   not to use them.  */
#define LD_SWITCH_SYSTEM "-Bstatic"
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#endif				/* __FreeBSD__ */
#endif				/* NO_SHARED_LIBS */

#endif				/* not __ELF__ */

			/* #define NO_TERMIO *//* detected in configure */
#define DECLARE_GETPWUID_WITH_UID_T

/* freebsd uses OXTABS instead of the expected TAB3. */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* Needed to avoid hanging when child process writes an error message
   and exits -- enami tsugutomo <enami@ba2.so-net.or.jp>.  */
#define vfork fork
