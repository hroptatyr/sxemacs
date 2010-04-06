/* Synched up with: FSF 19.31. */

#include "sunos4-1.h"

#if 0
/* XEmacs: FSF 19.31 removes this.  Let's just comment it out. */
/* TERMIOS is broken under SunOS??

   Someone says: This causes failure in process_send_signal (tcgetattr
   loses) and may also cause hanging at Emacs startup when parent is
   not a job control shell.  */
/* murray@chemical-eng.edinburgh.ac.uk says this works, and avoids
   the problem of spurious ^M in subprocess output.  */
#undef HAVE_TERMIOS
#endif				/* 0 */

#if 0
/* XEmacs: FSF 19.31 mistakenly reenables this. */
/* jik@gza.com says this works now.  */
/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC
#endif				/* 0 */

/* barrie@calvin.demon.co.uk says memmove is missing.  */
#ifndef SYSTEM_MALLOC
#define MEMMOVE_MISSING
#endif
