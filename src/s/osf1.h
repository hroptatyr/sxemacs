/* Synched up with: FSF 19.31. */

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	"-D_BSD"
#define LIBS_SYSTEM	"-lbsd"

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF
