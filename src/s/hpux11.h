/* Synched up with: FSF 19.31. */

/* System description file for hpux version 11.  */

#include "hpux9-shr.h"

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX10
#define HPUX11
/* AlainF 20-Jul-1996 -- fixes for 10.10, untested for 10.0x */
/* Fix kernel file name for 10.10 and later */
#undef KERNEL_FILE
#define KERNEL_FILE "/stand/vmunix"

/* XEmacs: -lcurses includes a broken select() call on some 10.X systems. */
/* #undef LIBS_TERMCAP */
/* #define LIBS_TERMCAP "-ltermcap" */
