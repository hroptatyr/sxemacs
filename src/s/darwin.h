/* s/ file for Darwin/MacOS X systems */

#define SYSTEM_TYPE "darwin"

/* Defines this as a BSD system, used in fakemail, for example */
#ifndef NOT_C_CODE
#include <sys/param.h>
#endif

/* The builtin malloc is broken in the sense that replacing it
   with a user implementation is impossible. */
#define SYSTEM_MALLOC

#define HAVE_PTYS

/* TAB3 is defined in Unix98, but darwin doesn't have it. 
   OXTABS is the traditional BSD equivalent. */
#define TAB3 OXTABS
