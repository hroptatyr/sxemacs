/* Synched up with: FSF 19.31. */

#include "irix5-1.h"

/* as of version 5.2, irix no longer uses flock, 
   according to jackr@wpd.sgi.com.  */
#undef MAIL_USE_FLOCK 

#if 0 /* XEmacs is smarter about dealing with C-g in select */
/* C-g in select is not handled properly with restartable
   system calls.  So don't use them.  */
#undef SA_RESTART
#endif /* 0 */

/* schoepf@goofy.zdv.Uni-Mainz.de reports he needed -lw with X11R6
   on Irix 5.3.  I don't know which Irix version that need starts with.  */
#define NEED_LIBW
