/* Synched up with: Mule 2.0.  Not in FSF. */

/* Definitions file for GNU Emacs running on NEC's EWS-UX/V(Rel4.2) 
						   (System V Release 4.2) */

#include "usg5-4-2.h"

#ifndef nec_ews
#define nec_ews
#endif
#ifndef nec_ews_svr4
#define nec_ews_svr4
#endif

#define XOS_NEEDS_TIME_H
#define HAVE_CLOCK

#ifdef __STDC__
#define MKDIR_PROTOTYPE int mkdir(char *dpath, mode_t dmode)
#endif

#ifndef __GNUC__
#define C_DEBUG_SWITCH "-O  -KOlimit=2000 -ZXNd=5000"
#endif
