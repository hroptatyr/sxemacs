/* Synched up with: Not in FSF. */

#include "decosf1-3.h"

/* It seems that read() and write() are affected, but not open() and
   close() */

#define INTERRUPTIBLE_IO

/* XEmacs change from "Andrew G. Cohen, 617-353-6051" <cohen@andy.bu.edu> */

/* XEmacs: moved the following four declarations from decosf3-2.h, as
   suggested by srivasta@pilgrim.umass.edu (Manoj Srivastava) */

/* apparently this breaks things under OSF 3.2 */
#undef GMALLOC_NEEDS_SBRK_DECL

/* XEmacs: from Stephen Carney <carney@gvc.dec.com> */

/* #### why the hell is configure so broken on this system? */
#ifndef HAVE_SELECT
#define HAVE_SELECT 1
#endif

#define BROKEN_SIGIO

#ifndef TERMINFO
#define TERMINFO
#endif
