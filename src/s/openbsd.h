/* s/ file for openbsd systems. */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* OpenBSD alpha has no shared libs yet. */
#if defined (__alpha__) && !defined (__ELF__)
#define NO_SHARED_LIBS
#endif

/* Get the rest of the stuff from NetBSD */
#include "netbsd.h"
