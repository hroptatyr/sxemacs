/* Synched up with: FSF 19.31. */

/* I don't care if this doesn't do more than including bsd4-3.h;
   Mach is not bsd4-3 and the moment you forget it chances are that
   you're in deep shit.  */

#include "bsd4-3.h"

/* The rest of this stuff is XEmacs additions. */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "mach"

/* Don't send signals to subprocesses by "typing" special chars at them. */
#undef SIGNALS_VIA_CHARACTERS

/* XEmacs change */
/* unistd.h defines _POSIX_VERSION, which leads some things to believe
   that _POSIX_PATH_MAX should be defined.  Unfortunately, it isn't. */
#ifndef NOT_C_CODE
#include <sys/param.h>
#define _POSIX_PATH_MAX MAXPATHLEN
#endif

#ifndef NOT_C_CODE
typedef int pid_t;
/* XEmacs change */
typedef unsigned short mode_t;
#endif				/* NOT_C_CODE */

#if (defined(i386) || defined(ibmrt))
  /* use drem() instead of fmod() -- this is a BUG in the compiler runtime. */
# define USE_DREM
#endif
