/* Synched up with: FSF 19.31. */

/* For building XEmacs under HPUX 9.0 with dynamic libraries. */

#define ORDINARY_LINK

/* XEmacs change */
/* Only support for hp9000s700 currently */
#if !defined(__hp9000s300)
/* #ifndef USE_GCC */
#define HPUX_USE_SHLIBS
/* #endif */
#endif				/* !hp9000s300 */

/* XEmacs: */
/* Don't tell the linker to link statically */
#ifdef NOT_C_CODE
#define START_FILES
#define LINKER "$(CC)"
#endif				/* THIS IS YMAKEFILE */

/* get call to brk() when rerunning XEmacs */
/* #ifndef USE_GCC */
#define RUN_TIME_REMAP
/* #endif */

#include "hpux9.h"

#if 0				/* No longer needed, since in current GCC -g no longer does that.  */
/* We must turn off -g since it forces -static.  */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
#endif
