/* Functions for memory limit warnings.
   Copyright (C) 1990, 1992 Free Software Foundation, Inc.

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

#ifdef emacs
#include <config.h>
#include "lisp.h"
#endif

#include <stddef.h>
#ifndef emacs
typedef size_t SIZE;
typedef void *POINTER;
#define EXCEEDS_LISP_PTR(x) 0
#endif

#include "mem-limits.h"

/*
  Level number of warnings already issued.
  0 -- no warnings issued.
  1 -- 75% warning already issued.
  2 -- 85% warning already issued.
  3 -- 95% warning issued; keep warning frequently.
*/
static int warnlevel;

/* Function to call to issue a warning;
   0 means don't issue them.  */
static void (*warn_function) (const char *);

/* Get more memory space, complaining if we're near the end. */

static void
check_memory_limits (void)
{
  extern POINTER (*__morecore) (ptrdiff_t size);

  POINTER cp;
  unsigned long five_percent;
  unsigned long data_size;
  void (*save_warn_fun) (const char *);

  if (lim_data == 0)
    get_lim_data ();
  five_percent = lim_data / 20;

  /* Find current end of memory and issue warning if getting near max */
  cp = (char *) (*__morecore) (0);
  data_size = (char *) cp - (char *) data_space_start;

  if (warn_function)
    {
      /* temporarily reset the warn_function to 0 or we will get infinite
	 looping. */
      save_warn_fun = warn_function;
      warn_function = 0;
      switch (warnlevel)
	{
	case 0:
	  if (data_size > five_percent * 15)
	    {
	      warnlevel++;
	      (*save_warn_fun) ("Warning: past 75% of memory limit");
	    }
	  break;

	case 1:
	  if (data_size > five_percent * 17)
	    {
	      warnlevel++;
	      (*save_warn_fun) ("Warning: past 85% of memory limit");
	    }
	  break;

	case 2:
	  if (data_size > five_percent * 19)
	    {
	      warnlevel++;
	      (*save_warn_fun) ("Warning: past 95% of memory limit");
	    }
	  break;

	default:
	  (*save_warn_fun) ("Warning: past acceptable memory limits");
	  break;
	}
      warn_function = save_warn_fun;
    }

  /* If we go down below 70% full, issue another 75% warning
     when we go up again.  */
  if (data_size < five_percent * 14)
    warnlevel = 0;
  /* If we go down below 80% full, issue another 85% warning
     when we go up again.  */
  else if (warnlevel > 1 && data_size < five_percent * 16)
    warnlevel = 1;
  /* If we go down below 90% full, issue another 95% warning
     when we go up again.  */
  else if (warnlevel > 2 && data_size < five_percent * 18)
    warnlevel = 2;

  if (EXCEEDS_LISP_PTR (cp))
    {
      if (warn_function)
	{
	  /* temporarily reset the warn_function to 0 or we will get infinite
	     looping. */
	  save_warn_fun = warn_function;
	  warn_function = 0;
	  (*save_warn_fun)
	    ("Warning: memory in use exceeds lisp pointer size");
	  warn_function = save_warn_fun;
	}
    }
}

/* Cause reinitialization based on job parameters;
   also declare where the end of pure storage is. */

void
memory_warnings (void *start, void (*warnfun) (const char *))
{
  extern void (* __after_morecore_hook) (void);	/* From gmalloc.c */

  if (start)
    data_space_start = (char*) start;
  else
    data_space_start = start_of_data ();

#ifndef _NO_MALLOC_WARNING_
  warn_function = warnfun;
  __after_morecore_hook = check_memory_limits;
#endif
}
