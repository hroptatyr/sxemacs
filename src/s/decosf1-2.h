/* Synched up with: Not in FSF. */

#define OSF1

#ifndef NOT_C_CODE
#include <sys/param.h>
#endif

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#ifdef __GNUC__
#define C_SWITCH_SYSTEM "-D_BSD"
#else
#define C_SWITCH_SYSTEM "-std -D_BSD"
#endif
#define LIBS_SYSTEM	"-lbsd"
#define SYSTEM_MALLOC

/* This to get rid of the -X that ymakefile inserts */
#undef LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM

/* XEmacs change */
#define GMALLOC_NEEDS_SBRK_DECL
