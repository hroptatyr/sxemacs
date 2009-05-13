/* Synched up with: FSF 19.31. */

/* Definitions for ESIX System V 4.0.4, a variant of V.4 for the 386.  */
/* Redone by zircon!joe@uunet.uu.net (Joe Kelsey).  */

#include "usg5-4.h"

#define SYSTEM_MALLOC 1
#if defined (HAVE_XFREE386)
# undef LIB_STANDARD
# define LIB_STANDARD "-lc"
#else
# undef LIB_STANDARD
# ifdef ORDINARY_LINK
#   define LIB_STANDARD "-lnsl -lns -lelf /usr/ucblib/libucb.a"
# else
#   define LIB_STANDARD "-lnsl -lns -lelf /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o"
# endif
