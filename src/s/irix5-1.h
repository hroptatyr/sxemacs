/* Synched up with: Not in FSF. */

/* by Hayden Schultz <haydens@ll.mit.edu> for XEmacs */

#ifndef IRIX5_1_H
#define IRIX5_1_H

#include "irix5-0.h"

/* #### Questionable defines. */
#define IRIX
#define IRIS

/* XEmacs change */
#if 0
#define LD_SWITCH_SYSTEM "-elf -_SYSTYPE_SVR4 -require_dynamic_link _rld_new_interface -no_unresolved -Wx,-G 0 -L. -L./lwlib -g0 -call_shared -transitive_link"
#endif

/* By Tor Arntsen <tor@spacetec.no> for XEmacs.
   With the following kludge the above LD_SWITCH_SYSTEM will still work just 
   fine even with USE_GCC, and additional tweaking of config.h or ymakefile 
   is avoided. */
#ifdef NOT_C_CODE
# ifdef USE_GCC
#  undef LINKER
#  undef LIB_GCC
#  define LINKER "ld"
#  define LIB_GCC "`gcc --print`"
#  endif
#endif

#endif /* IRIX5_1_H */
