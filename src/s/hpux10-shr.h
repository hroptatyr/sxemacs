/* Synched up with: FSF 19.31. */

/* For building XEmacs under HPUX 10.0 with dynamic libraries. */

#define ORDINARY_LINK

/* XEmacs change */
/* Only support for hp9000s300 currently */
#if !defined(__hp9000s300)
#define HPUX_USE_SHLIBS
#endif				/* !hp9000s300 */

/* XEmacs: */
/* Don't tell the linker to link statically */
#ifdef NOT_C_CODE
#define START_FILES
#define LINKER "$(CC)"
#endif				/* THIS IS YMAKEFILE */

/* get call to brk() when rerunning XEmacs */
#define RUN_TIME_REMAP

#include "hpux10.h"

/* We must turn off -g since it forces -static.  */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
