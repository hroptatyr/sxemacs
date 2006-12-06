/* Synched up with: FSF 19.31. */

/* s- file for Interactive (ISC) Unix version 3.0 on the 386.  */

#include "isc2-2.h"

/* This appears on 3.0, presumably as part of what SunSoft call X2. */
#undef NO_X_DESTROY_DATABASE

/* People say that using -traditional causes lossage with `const',
   so we might as well try getting rid of -traditional.  */
#undef C_SWITCH_SYSTEM

/* TIOCGWINSZ isn't broken; you just have to know where to find it.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_SIOCTL

/* We need either _XOPEN_SOURCE or _POSIX_SOURCE to import the posix
   signal symbols; might as well use _XOPEN_SOURCE.  Defining _SYSV3
   ensures that we don't lose the traditional symbols as a side effect
   from this or __STDC__ being defined.  */
#define C_SWITCH_SYSTEM "-D_XOPEN_SOURCE -D_SYSV3"

#ifdef __GNUC__			/* Currently we use -lcposix only with gcc */
/* This works around a bug in ISC 4.0 and 3.0; it fails
   to clear the "POSIX process" flag on an exec.
   It won't be needed for 4.1.  */
#define EXTRA_INITIALIZE __setostype (0)
#endif
