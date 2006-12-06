/* Synched up with: Not in FSF. */

#include "sco5.h"

/* XEmacs change. */
#undef LINKER
#undef C_OPTIMIZE_SWITCH
#undef C_DEBUG_SWITCH
#undef C_SWITCH_SYSTEM

#ifndef __GNUC__
#define LINKER "cc -belf -dy -Xc"
#define C_OPTIMIZE_SWITCH "-O3 -Xc"
#define C_DEBUG_SWITCH "-g -Xc"
#define C_SWITCH_SYSTEM "-belf -D_NO_STATIC -D_SCO_ELF"
#else
#define LINKER "gcc -melf -Xc"
#define C_OPTIMIZE_SWITCH "-O99 -m486 -fomit-frame-pointer -Xc"
#define C_DEBUG_SWITCH "-g -Xc"
#define C_SWITCH_SYSTEM "-melf -D_NO_STATIC -D_SCO_ELF"
#endif
