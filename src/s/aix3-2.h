/* Synched up with: FSF 19.31. */

/* s- file for building Emacs on AIX 3.2.  */

#include "aix3-1.h"

#define AIX3_2

/* No need to define this--the header files indicate X11R4,
   and that's supposedly what 3.2 will come with.  */
#undef SPECIFY_X11R4

#ifndef __GNUC__
/* Some programs in src produce warnings saying certain subprograms
   are to comples and need a MAXMEM value greater than 2000 for
   additional optimization.  --nils@exp-math.uni-essen.de */
/* XEmacs change: maxmem=-1 means unlimited.  Suggested by
   dkeller@VNET.IBM.COM */
#if 0				/* mrb */
#define C_SWITCH_SYSTEM "-ma -qmaxmem=-1"
#endif				/* mrb */
#else
/* Otherwise, XEmacs is just too big ... */
#define C_SWITCH_SYSTEM "-mminimal-toc"
#endif

#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif
/* Adrian Colley <Adrian.Colley@three.serpentine.com> says this is needed.  */
#ifndef NOT_C_CODE
#ifndef AIX4
#pragma alloca
#endif
#endif

/* With this defined, a gcc-compiled Emacs crashed in realloc under AIX
   3.2, and a cc-compiled Emacs works with this undefined.
   --karl@cs.umb.edu.  */
#undef SYSTEM_MALLOC

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
/* XEmacs change:  no evidence of this in XEmacs */
#if 0
#ifndef __GNUC__
#define C_SWITCH_DEBUG "-g"
#define C_SWITCH_OPTIMIZE
#endif
#endif

/* The character-composition stuff is broken in X11R5.
   Even with XIMStatusNothing aliased to XIMStatusNone,
   tranle@intellicorp.com (Minh Tran-Le) reports that enabling
   the internationalization code causes the modifier keys C, M and Shift
   to beep after a mouse click.  */
#define X11R5_INHIBIT_I18N
