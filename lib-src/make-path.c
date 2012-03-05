/* Make all the directories along a path.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

/* This program works like mkdir, except that it generates
   intermediate directories if they don't exist.  This is just like
   the `mkdir -p' command on most systems; unfortunately, the mkdir
   command on some of the purer BSD systems (like Mt. Xinu) don't have
   that option. */

#ifdef emacs
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

char *prog_name;

static int touchy_mkdir(char *path)
{
	struct stat buf;

	/* Try to make it.  If PATH exists, EEXIST will be the
	   return. Anyway on error but isn't a directory,
	   this will signal an error.  */
	if (mkdir(path, 0777) < 0 && errno != EEXIST ) {
		int serrno = errno; /* save errno because stat may
				       change it... */

		/* If PATH already exists and is a directory, return success.  */
		if (stat(path, &buf) >= 0 && (buf.st_mode & S_IFMT) == S_IFDIR)
			return 0;

		fprintf(stderr, "%s: %s", prog_name, strerror(serrno));
		return 1;
	}

	return 0;
}

int main(int argc, char *argv[])
{
	prog_name = *argv;

	for (argc--, argv++; argc > 0; argc--, argv++) {
		char *path = *argv;
		int i;

		/* Stop at each slash in path and try to create the directory.
		   Skip any initial slash.  */
		for (i = (path[0] == '/') ? 1 : 0; path[i]; i++)
			if (path[i] == '/') {
				path[i] = '\0';
				if (touchy_mkdir(path) < 0)
					goto next_pathname;
				path[i] = '/';
			}

		touchy_mkdir(path);

	      next_pathname:
		;
	}

	return 0;
}
