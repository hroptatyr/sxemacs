/* Synched up with: FSF 19.31. (Split off from sunos4-shr.h.) */

/* This file permits building Emacs with a shared libc on Sunos 4.
   To make this work, you must completely replace your C shared library
   using one of the SunOS 4.1.x jumbo replacement patches from Sun.
   Here are the patch numbers for Sunos 4.1.3:
   100890-10   SunOS 4.1.3: domestic libc jumbo patch
   100891-10   SunOS 4.1.3: international libc jumbo patch  */


#include "sunos4-0.h"

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

/*  Misleading!  Actually gets loaded after crt0.o */
#undef START_FILES
#define START_FILES "pre-crt0.o"

/*
 *  Kludge!  can't get at symbol "start" in std crt0.o
 *  Who the #$%&* decided to remove the __ characters!
 *  Someone needs to fix this in sysdep.c  with an #ifdef BROKEN_START in
 * sysdep.c.  We do not use this address so any value should do really.  Still
 *  may need it in the future?
 */
#define BROKEN_START
#ifndef TEXT_START
#define TEXT_START 0x2020
#endif

#undef UNEXEC
#define UNEXEC	"unexsunos4.o"
#ifndef RUN_TIME_REMAP
#define RUN_TIME_REMAP
#endif
#define ORDINARY_LINK
#define SUNOS4_SHARED_LIBRARIES

#undef LD_SWITCH_SYSTEM

#undef	SYSTEM_MALLOC
#ifndef GNU_MALLOC
#define	GNU_MALLOC
#endif
#ifndef REL_ALLOC
#define	REL_ALLOC
#endif

#undef USE_DL_STUBS

#ifndef HAVE_X11R6
/* With X11R5 it was reported that linking -lXmu dynamically
   did not work.  With X11R6, it does work; and since normally
   only the dynamic libraries are available, we should use them.  */
#define LIBXMU "-Bstatic -lXmu -Bdynamic"

#endif  /* not HAVE_X11R6 */
