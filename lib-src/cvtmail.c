/* Copyright (C) 1985, 1993, 1994 Free Software Foundation
This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.28. */

/* cvtmail:
 * Program to convert oldstyle goslings emacs mail directories into
 * gnu-rmail format.  Program expects a directory called Messages to
 * exist in your home directory, containing individual mail messages in
 * separate files in the standard gosling emacs mail reader format.
 *
 * Program takes one argument: an output file.  This file will contain
 * all the messages in Messages directory, in berkeley mail format.
 * If no output file is mentioned, messages are put in ~/OMAIL.
 *
 * In order to get rmail to read the messages, the resulting file must
 * be mv'ed to ~/mbox, and then have rmail invoked on them.
 *
 * Author: Larry Kolodney, 1985
 */


#include <config.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void *xmalloc (size_t);
static void *xrealloc (void *, size_t);
static void skip_to_lf (FILE *stream);
static void fatal (const char *s1, const char *s2);
static void error (const char *s1, const char *s2);

int
main (int argc, char *argv[])
{
  char *hd;
  char *md;
  char *mdd;
  char *mfile;
  char *cf;
  int cflen;
  FILE *mddf;
  FILE *mfilef;
  FILE *cff;
  char pre[10];
  char name[14];
  int c;

  hd = getenv ("HOME");

  md = (char *) xmalloc (strlen (hd) + 10);
  strcpy (md, hd);
  strcat (md, "/Messages");

  mdd = (char *) xmalloc (strlen (md) + 11);
  strcpy (mdd, md);
  strcat (mdd, "/Directory");

  cflen = 100;
  cf = (char *) xmalloc (cflen);

  mddf = fopen (mdd, "r");
  if (argc > 1)
    mfilef = fopen (argv[1], "w");
  else
    {
      mfile = (char *) xmalloc (strlen (hd) + 7);
      strcpy (mfile, hd);
      strcat (mfile, "/OMAIL");
      mfilef = fopen (mfile, "w");
    }
  skip_to_lf (mddf);
  while (fscanf (mddf, "%4c%14[0123456789]", pre, name) != EOF)
    {
      int comp_len = strlen (md) + strlen (name) + 2;
      if (cflen < comp_len)
	{
	  cflen = strlen (md) + strlen (name) + 2;
	  cf = (char *) xrealloc (cf, cflen);
	}
      strcpy (cf, md);
      strcat (cf,"/");
      strcat (cf, name);
      cff = fopen (cf, "r");
      while ((c = getc(cff)) != EOF)
	putc (c, mfilef);
      putc ('\n', mfilef);
      skip_to_lf (mddf);
     fclose (cff);
    }
  fclose (mddf);
  fclose (mfilef);
  return 0;
}

static void
skip_to_lf (FILE *stream)
{
  register int c;
  while ((c = getc(stream)) != '\n')
    ;
}

static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

static void *
xrealloc (void *ptr, size_t size)
{
  void *result = realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Print error message and exit.  */

static void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (1);
}

static void
error (const char *s1, const char *s2)
{
  fprintf (stderr, "cvtmail: ");
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}
