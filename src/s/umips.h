/* Synched up with: FSF 19.31. */

/* Definitions file for GNU Emacs running on Mips operating system.
   That system can emulate either BSD or Sys V, in either case with changes.
   If BSD is defined, we assume BSD is being emulated; otherwise, Sys V.  */

#ifdef BSD
#include "bsd4-3.h"

#define C_SWITCH_SYSTEM "-systype bsd43"
#define LD_SWITCH_SYSTEM "-systype bsd43"
#undef LIBS_SYSTEM
#define LIBS_DEBUG
#define START_FILES "pre-crt0.o /lib/crt1.o"
#define LIB_STANDARD "-lc /usr/lib/crtn.o"

#define COFF
#define TERMINFO
#undef MAIL_USE_FLOCK		/* Someone should check this.  */

#else				/* not BSD */

#define DONT_DEFINE_NO_REMAP	/* `static' hack not needed */
#include "usg5-2-2.h"

#undef LIBS_SYSTEM
#define LIBS_DEBUG
#define START_FILES "pre-crt0.o /usr/lib/crt1.o"
#define LIB_STANDARD "-lbsd -lc /usr/lib/crtn.o"
/* #define LIBS_TERMCAP -lcurses */

#define C_SWITCH_SYSTEM "-I/usr/include/bsd"

/* Don't try to use SIGIO even though it is defined.  */
#define BROKEN_SIGIO

/* Describe special kernel features.  */

#if defined(emacs)
#include <bsd/sys/time.h>
#endif

/* The `select' in the system won't work for pipes,
   so don't use it.  */
#define BROKEN_SELECT

#define HAVE_PTYS

/* ??? */
#define IRIS

#endif				/* not BSD */

/* High order bit must be stripped off nlist return values */
#define FIXUP_KERNEL_SYMBOL_ADDR(NL)  (NL)[0].n_value &= 0x7fffffff;
