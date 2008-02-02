/* Synched up with: FSF 19.31. */

/* machine description file for the Tandem Integrity S2.  */

#include "mips.h"

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-3"  */

/* This overrides some of the usual support for the mips and system V.3.  */

/* The operating system apparently defines TIOCGETC
   but it doesn't work.  */
#undef BROKEN_TIOCGETC

/* rs@ai.mit.edu said this was necessary for it to work.  However, some
   user of this machine ought to try to get subprocesses to work.  */
#define NO_SUBPROCESSES

/* Correct some library file names.  */
#define START_FILES "pre-crt0.o /usr/lib/crt1.o1.31"
#define LIB_STANDARD "-lbsd -lc /usr/lib/crtn.o1.31"
