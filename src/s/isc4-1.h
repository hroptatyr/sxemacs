/* Synched up with: FSF 19.31. */

#include "isc3-0.h"

/* ISC 4.1 has renamed __setostype, but also has fixed the bug
   for which we needed to call it; so just do nothing.  uddeborg@carmen.se.  */
#undef EXTRA_INITIALIZE

#define ISC4_1

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-linet"

/* uddeborg@carmen.se recommends the rest of this file.  */

/* A special startup file is used when compiling with Posix. */
#define START_FILES "pre-crt0.o /lib/crtp1.o"

/* -lPW is only needed if not using Gcc. */
#undef LIB_STANDARD
#if defined (__GNUC__)
#  define LIB_STANDARD "-lcposix -lc /lib/crtn.o"
#else /* !__GNUC__ */
#  define LIB_STANDARD "-lPW -lcposix -lc /lib/crtn.o"
#endif /* !__GNUC__ */

/* We have Posix termios. */
#define HAVE_TERMIOS
/* According to template.h HAVE_TERMIO and HAVE_TERMIOS shouldn't be */
/* defined at the same time. */
#undef HAVE_TERMIO
