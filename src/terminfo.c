/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30. */

#include <config.h>

#include <string.h>

/* Every little bit of this God-damned file has caused all manner
   of headaches due to inconsistent and incorrect header files
   on one system or other, and we don't currently need anything here,
   so just comment the whole damn lot out!!! */

#ifndef HAVE_TERMIOS

#ifdef AIX
#include <termio.h>
#endif /* AIX */

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.
*/

#include CURSES_H_FILE
/* Sun, in their infinite lameness, supplies (possibly) broken headers
   even under Solaris.  GCC feels it necessary to correct things by
   supplying its own headers.  Unfortunately, if you build GCC under
   one version of Solaris and then upgrade your Solaris, you may get
   screwed because Sun in their continuing lameness changes curses.h
   in such a way that the "fixed" GCC headers are now broken. (GCC
   is equally lame in that it supplies "fixed" headers for curses.h
   but not term.h.) However, it seems to work to just not include
   term.h under Solaris, so we try that.  KLUDGE! */
#if !(defined (__GNUC__) && defined (SOLARIS2))
#include TERM_H_FILE
#endif

extern void *xmalloc (int size);

#if 0 /* If this isn't declared somewhere, too bad */
extern char * tparm (const char *string, int arg1, int arg2, int arg3,
                     int arg4, int arg5, int arg6, int arg7, int arg8,
                     int arg9);
#endif
/* XEmacs: renamed this function because just tparam() conflicts with
   ncurses (We don't use this function anyway!) */
char *
emacs_tparam (const char *string, char *outstring, int len, int arg1,
	      int arg2, int arg3, int arg4, int arg5, int arg6, int arg7,
	      int arg8, int arg9)
{
  char *temp;

  temp = (char *) tparm (string, arg1, arg2, arg3, arg4, arg5, arg6, arg7,
			 arg8, arg9);
  if (outstring == 0)
    outstring = (char *) xmalloc (strlen (temp) + 1);
  strcpy (outstring, temp);
  return outstring;
}

#endif /* not HAVE_TERMIOS */
