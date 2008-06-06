/* Unexec function for Solaris 2.x
   Copyright (C) 1994 Sun Microsystems, Inc.

This file is part of SXEmacs

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


/* Synched up with: Not in FSF. */

/* #pragma ident "@(#) $Id: unexsol2.c,v 1.3 1997/10/13 03:35:33 steve Exp $" */

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#define DYNODUMP_SO	"../dynodump/dynodump.so"
#define DYNODUMP_SYM	"dynodump"

int unexec(char *new_name, char *old_name, unsigned int data_start,
	   unsigned int bss_start, unsigned int entry_address);
int
unexec(char *new_name, char *old_name, unsigned int data_start,
       unsigned int bss_start, unsigned int entry_address)
{
	void *handle;
	void (*func) (const char *file);

	if ((handle = dlopen(DYNODUMP_SO, RTLD_LAZY)) == NULL) {
		fprintf(stderr, "unexec(): dlopen(%s): %s\n",
			(char *)DYNODUMP_SO, dlerror());
		exit(1);
	}

	if ((func =
	     (void (*)(const char *))dlsym(handle, DYNODUMP_SYM)) == NULL) {
		fprintf(stderr, "unexec(): dlsym(%s): %s \n",
			(char *)DYNODUMP_SYM, dlerror());
		exit(1);
	}

	(*func) (new_name);

	dlclose(handle);

	return 0;
}
