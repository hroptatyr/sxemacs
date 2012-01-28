/* Synched up with: Not in FSF. */

#include "sunos4-1.h"

/* Rodney Peck <rpeck@nas.nasa.gov>  19940608
   the /usr/lib/libresolv.a in sunos 4.1.1 not really a static library and
   will cause linker errors looking for _GLOBAL_OFFSET_TABLE_.  The resolver
   routines are in libc however so XEmacs will link without the -lresolv
   */
#undef LIBS_SYSTEM
