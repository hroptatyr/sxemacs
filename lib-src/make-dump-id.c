/* Generate a unique dump-id for use with the portable dumper.
   Copyright (C) 2000 Olivier Galibert, Martin Buchholz

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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include "../src/systime.h"

#ifdef WIN32_NATIVE
#include <sys/timeb.h>

/* Emulate gettimeofday (Ulrich Leodolter, 1/11/95).  */
void 
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  struct _timeb tb;
  _ftime (&tb);

  tv->tv_sec = tb.time;
  tv->tv_usec = tb.millitm * 1000L;
  if (tz) 
    {
      tz->tz_minuteswest = tb.timezone;	/* minutes west of Greenwich  */
      tz->tz_dsttime = tb.dstflag;	/* type of dst correction  */
    }
}
#endif

/* Generates an (extremely) pseudo random number for the dump-id */
static unsigned int
generate_dump_id (void)
{
  EMACS_TIME thyme;
  EMACS_GET_TIME (thyme);

  return (unsigned int) (EMACS_SECS (thyme) ^ EMACS_USECS (thyme));
}

int
main (int argc, char *argv[])
{
  FILE *f;

  if ((f = fopen ("dump-id.c", "w")) == NULL)
    {
      perror ("open dump-id.c");
      return EXIT_FAILURE;
    }

  fprintf (f, "unsigned int dump_id = %uU;\n", generate_dump_id ());

  if ((fclose (f)) != 0)
    {
      perror ("close dump-id.c");
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
