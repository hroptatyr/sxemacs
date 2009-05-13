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

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

int unexec(char *new_name, char *old_name, unsigned int data_start,
	   unsigned int bss_start, unsigned int entry_address);
int
unexec(char *new_name, char *old_name, unsigned int data_start,
       unsigned int bss_start, unsigned int entry_address)
{
	if (dldump(0, new_name, RTLD_MEMORY) != 0) {
		fprintf(stderr, "unexec(): dldump(%s): %s \n",
			new_name, dlerror());
		exit(1);
	}

	return 0;
}
