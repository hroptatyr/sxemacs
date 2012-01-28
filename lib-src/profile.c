/* profile.c --- generate periodic events for profiling of Emacs Lisp code.
 Copyright (C) 1992, 1994 Free Software Foundation, Inc.

 Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>

 This file is part of SXEmacs.

 SXEmacs is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 SXEmacs is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.28. */
/* #### Not sure if this is needed for XEmacs. */

/**
 **  To be run as an emacs process. Input string that starts with:
 **    'z' -- resets the watch (to zero).
 **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
 **    'q' -- exit.
 **
 **  abstraction : a stopwatch
 **  operations: reset_watch, get_time
 */
#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "../src/systime.h"

static struct timeval TV1, TV2;
static int watch_not_started = 1;	/* flag */
static char time_string[64];

/* Reset the stopwatch to zero.  */

static void reset_watch(void)
{
	EMACS_GET_TIME(TV1);
	watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<micro-seconds>
   If reset_watch was not called yet, exit.  */

static char *get_time(void)
{
	int sz;
	if (watch_not_started)
		exit(1);	/* call reset_watch first ! */
	EMACS_GET_TIME(TV2);
	if (TV1.tv_usec > TV2.tv_usec) {
		TV2.tv_usec += 1000000;
		TV2.tv_sec--;
	}
	sz = snprintf(time_string, sizeof(time_string), "%lu.%06lu",
		      (unsigned long)TV2.tv_sec - TV1.tv_sec,
		      (unsigned long)TV2.tv_usec - TV1.tv_usec);
	assert(sz >= 0 && (size_t)sz < sizeof(time_string));
	return time_string;
}

int main(int argc, char *argv[])
{
	int c;
	while ((c = getchar()) != EOF) {
		switch (c) {
		case 'z':
			reset_watch();
			break;
		case 'p':
			puts(get_time());
			break;
		case 'q':
			exit(0);
		default:
			break;
		}
		/* Anything remaining on the line is ignored.  */
		while (c != '\n' && c != EOF)
			c = getchar();
	}
	return 1;
}
