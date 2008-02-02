/* Synched up with: Not in FSF. */

#include "irix5-2.h"

/* by Hayden Schultz <haydens@ll.mit.edu> for XEmacs: */

/*
 * Limit does work in 5.3
 */
#ifdef ULIMIT_BREAK_VALUE
#undef ULIMIT_BREAK_VALUE
#endif
