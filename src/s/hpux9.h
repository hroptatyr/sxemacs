/* Synched up with: FSF 19.31. */

/* System description file for hpux version 9.  */

#include "hpux8.h"

#define HPUX9

/* If Emacs doesn't seem to work when built to use GNU malloc, you
   probably need to get the latest patches to the HP/UX compiler.
   See `etc/MACHINES' for more information.  */
#if 0
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC
#endif

/* cc1: warning: `-g' not supported by this configuration of GCC
   #### Still needs to be fixed in a more general way... */
#if 0
#ifdef __GNUC__
#undef  C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
#endif

#ifndef __GNUC__
/* Make room for enough symbols, so dispnew.c does not fail.  */
/* XEmacs: cognot@ensg.u-nancy.fr: C_SWITCH_SYSTEM already defined in hpux8.h,
			   -D_BSD makes hp CC choke on process.c
#define C_SWITCH_SYSTEM "-Wp,-H200000 -D_BSD"
*/
#undef C_SWITCH_SYSTEM
#ifdef __hp9000s300
#define C_SWITCH_SYSTEM "-Aa -D_HPUX_SOURCE"
#else
#define C_SWITCH_SYSTEM "-Ae -Wp,-H100000 +Olibcalls"
#endif
/* XEmacs: commented out
#else
#define C_SWITCH_SYSTEM "-D_BSD"
*/
#endif

/* XEmacs: apparently rint() is totally broken in HPUX 9. */
#undef HAVE_RINT

/* XEmacs: avoid using -lcurses, to make the binary portable from 9.X to 10.X */
#undef LIBS_TERMCAP
#define LIBS_TERMCAP "-ltermcap"

/* mrb */
#undef LD_SWITCH_SYSTEM
