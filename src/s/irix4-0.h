/* Synched up with: FSF 19.31. */

#include "irix3-3.h"

#define USG5_3
#define IRIX4

#undef NEED_SIOCTL

/* use K&R C */
/* XEmacs change -- use ANSI, not K&R */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM "-xansi"
#endif

/* SGI has all the fancy wait stuff, but we can't include sys/wait.h
   because it defines BIG_ENDIAN and LITTLE_ENDIAN (ugh!.)  Instead
   we'll just define WNOHANG right here.
   (An implicit decl is good enough for wait3.)  */
/* [XEmacs: Now that we don't use BIG_ENDIAN/LITTLE_ENDIAN, it's safe
   to include wait.h.  Should something change here?] */

#define WNOHANG		0x1

/* jpff@maths.bath.ac.uk reports `struct exception' is not defined
   on this system, so inhibit use of matherr.  */
#define NO_MATHERR
