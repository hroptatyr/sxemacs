/*
 * realpath.c -- canonicalize pathname by removing symlinks
 * Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
 *

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

#include <config.h>
#include "lisp.h"
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined (HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif

#include <sys/stat.h>		/* for S_IFLNK */

/* First char after start of absolute filename. */
#define ABS_START(name) (name + ABS_LENGTH (name))

#define ABS_LENGTH(name) (IS_DIRECTORY_SEP (*name) ? 1 : 0)
#define system_readlink readlink


#if !defined (HAVE_GETCWD) && defined (HAVE_GETWD)
#undef getcwd
#define getcwd(buffer, len) getwd (buffer)
#endif

#ifndef PATH_MAX
# if defined (_POSIX_PATH_MAX)
#  define PATH_MAX _POSIX_PATH_MAX
# elif defined (MAXPATHLEN)
#  define PATH_MAX MAXPATHLEN
# else
#  define PATH_MAX 1024
# endif
#endif

#define MAX_READLINKS 32

char *xrealpath(const char *path, char *restrict resolved_path);
char *xrealpath(const char *path, char *restrict resolved_path)
{
	char copy_path[PATH_MAX];
	char *new_path = resolved_path;
	char *max_path;
#if defined (S_IFLNK)
	int readlinks = 0;
	char link_path[PATH_MAX];
	int n;
	int abslen = ABS_LENGTH(path);
#endif

	/* Make a copy of the source path since we may need to modify it. */
	strncpy(copy_path, path, sizeof(copy_path)-1);
	copy_path[sizeof(copy_path)-1]='\0';
	path = copy_path;
	max_path = copy_path + PATH_MAX - 2;

	/* If it's a relative pathname use getcwd for starters. */
	if (abslen == 0) {
		getcwd(new_path, PATH_MAX - 1);
		new_path += strlen(new_path);
		if (!IS_DIRECTORY_SEP(new_path[-1]))
			*new_path++ = DIRECTORY_SEP;
	} else {
		/* Copy first directory sep. */
		strncpy(new_path, path, abslen);
		new_path += abslen;
		path += abslen;
	}

	/* Expand each slash-separated pathname component. */
	while (*path != '\0') {
		/* Ignore stray "/". */
		if (IS_DIRECTORY_SEP(*path)) {
			path++;
			continue;
		}

		if (*path == '.') {
			/* Ignore ".". */
			if (path[1] == '\0' || IS_DIRECTORY_SEP(path[1])) {
				path++;
				continue;
			}

			/* Handle ".." */
			if (path[1] == '.' &&
			    (path[2] == '\0' || IS_DIRECTORY_SEP(path[2]))) {
				path += 2;

				/* Ignore ".." at root. */
				if (new_path == ABS_START(resolved_path))
					continue;

				/* Handle ".." by backing up. */
				--new_path;
				while (!IS_DIRECTORY_SEP(new_path[-1]))
					--new_path;
				continue;
			}
		}

		/* Safely copy the next pathname component. */
		while (*path != '\0' && !IS_DIRECTORY_SEP(*path)) {
			if (path > max_path) {
				errno = ENAMETOOLONG;
				return NULL;
			}
			*new_path++ = *path++;
		}

#if defined (S_IFLNK)
		/* See if latest pathname component is a symlink. */
		*new_path = '\0';
		n = system_readlink(resolved_path, link_path, PATH_MAX - 1);

		if (n < 0) {
			/* EINVAL means the file exists but isn't a symlink. */
			if (errno != EINVAL)
				return NULL;
		} else {
			/* Protect against infinite loops. */
			if (readlinks++ > MAX_READLINKS) {
				errno = ELOOP;
				return NULL;
			}

			/* Note: readlink doesn't add the null byte. */
			link_path[n] = '\0';

			if (ABS_LENGTH(link_path) > 0)
				/* Start over for an absolute symlink. */
				new_path =
				    resolved_path + ABS_LENGTH(link_path) - 1;
			else
				/* Otherwise back up over this component. */
				for (--new_path; !IS_DIRECTORY_SEP(*new_path);
				     --new_path)
					assert(new_path > resolved_path);

			/* Safe sex check. */
			if (strlen(path) + n >= PATH_MAX) {
				errno = ENAMETOOLONG;
				return NULL;
			}

			/* Insert symlink contents into path. */
			strcat(link_path, path);
			strcpy(copy_path, link_path);
			path = copy_path;
		}
#endif				/* S_IFLNK */
		*new_path++ = DIRECTORY_SEP;
	}

	/* Delete trailing slash but don't whomp a lone slash. */
	if (new_path != ABS_START(resolved_path)
	    && IS_DIRECTORY_SEP(new_path[-1]))
		new_path--;

	/* Make sure it's null terminated. */
	*new_path = '\0';

	return resolved_path;
}
