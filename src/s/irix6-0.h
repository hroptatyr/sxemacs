/* Synched up with: FSF 19.31. */

#include "irix5-3.h"

/* Irix 6.2 doesn't need -lw */
#undef NEED_LIBW

#undef memmove /* Use the standard system memmove() */
