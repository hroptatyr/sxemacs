/*
 * realpath.c -- canonicalize pathname by removing symlinks
 * Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
 *

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

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined (HAVE_SYS_PARAM_H) && !defined (WIN32_NATIVE)
#include <sys/param.h>
#endif

#ifdef WIN32_NATIVE
#include <direct.h>
#endif

#include <sys/stat.h>			/* for S_IFLNK */

#if defined(WIN32_NATIVE) || defined(CYGWIN)
#define WIN32_FILENAMES
#endif

/* First char after start of absolute filename. */
#define ABS_START(name) (name + ABS_LENGTH (name))

#if defined (WIN32_NATIVE)
/* Length of start of absolute filename. */
# define ABS_LENGTH(name) (win32_abs_start (name))
static int win32_abs_start (const char * name);
/* System dependent version of readlink. */
# define system_readlink win32_readlink
#else
# ifdef CYGWIN
#  ifdef WIN32_FILENAMES
#   define ABS_LENGTH(name) (win32_abs_start (name))
static int win32_abs_start (const char * name);
#  else
#   define ABS_LENGTH(name) (IS_DIRECTORY_SEP (*name) ? \
                             (IS_DIRECTORY_SEP (name[1]) ? 2 : 1) : 0)
#  endif
#  define system_readlink cygwin_readlink
# else
#  define ABS_LENGTH(name) (IS_DIRECTORY_SEP (*name) ? 1 : 0)
#  define system_readlink readlink
# endif /* CYGWIN */
#endif /* WIN32_NATIVE */

#if defined (WIN32_NATIVE) || defined (CYGWIN)
#include "syswindows.h"
/* Emulate readlink on win32 - finds real name (i.e. correct case) of
   a file. UNC servers and shares are lower-cased. Directories must be
   given without trailing '/'. One day, this could read Win2K's
   reparse points. */
static int
win32_readlink (const char * name, char * buf, int size)
{
  WIN32_FIND_DATA find_data;
  HANDLE dir_handle = NULL;
  int len = 0;
  int err = 0;
  const char* lastname;
  int count = 0;
  const char* tmp;
  char* res = NULL;
  
  assert (*name);
  
  /* Sort of check we have a valid filename. */
  /* #### can we have escaped shell operators in a Windows filename? */
  if (strpbrk (name, "|<>\"") || strlen (name) >= MAX_PATH)
    {
      errno = EIO;
      return -1;
    }
  /* #### can we have escaped wildcards in a Windows filename? */
  else if (strpbrk (name, "*?"))
    {
      errno = EINVAL;		/* this valid path can't be a symlink */
      return -1;
    }
  
  /* Find start of filename */
  lastname = name + strlen (name);
  while (lastname > name && !IS_DIRECTORY_SEP (lastname[-1]))
    --lastname;

  /* Count slashes in unc path */
  if (ABS_LENGTH (name) == 2)
    for (tmp = name; *tmp; tmp++)
      if (IS_DIRECTORY_SEP (*tmp))
	count++;

  if (count >= 2 && count < 4)
    {
      /* UNC server or share name: just copy lowercased name. */
      res = find_data.cFileName;
      for (tmp = lastname; *tmp; tmp++)
	*res++ = tolower (*tmp);
      *res = '\0';
    }
  else
    dir_handle = FindFirstFile (name, &find_data);

  if (res || dir_handle != INVALID_HANDLE_VALUE)
    {
      if ((len = strlen (find_data.cFileName)) < size)
	{
	  if (strcmp (lastname, find_data.cFileName) == 0)
	    /* Signal that the name is already OK. */
	    err = EINVAL;
	  else
	    memcpy (buf, find_data.cFileName, len + 1);
	}
      else
	err = ENAMETOOLONG;
      if (!res) FindClose (dir_handle);
    }
  else
    err = ENOENT;

  errno = err;
  return err ? -1 : len;
}
#endif /* WIN32_NATIVE || CYGWIN */

#ifdef CYGWIN
/* Call readlink and try to find out the correct case for the file. */
static int
cygwin_readlink (const char * name, char * buf, int size)
{
  int n = readlink (name, buf, size);
  if (n < 0 && errno == EINVAL)
    {
      /* The file may exist, but isn't a symlink. Try to find the
         right name. */
      char* tmp = alloca (cygwin_posix_to_win32_path_list_buf_size (name));
      cygwin_posix_to_win32_path_list (name, tmp);
      n = win32_readlink (tmp, buf, size);
    }
  return n;
}
#endif /* CYGWIN */

#ifdef WIN32_FILENAMES
#ifndef ELOOP
#define ELOOP 10062 /* = WSAELOOP in winsock.h */
#endif
/* Length of start of absolute filename. */
static int 
win32_abs_start (const char * name)
{
  if (isalpha (*name) && IS_DEVICE_SEP (name[1])
      && IS_DIRECTORY_SEP (name[2]))
    return 3;
  else if (IS_DIRECTORY_SEP (*name))
    return IS_DIRECTORY_SEP (name[1]) ? 2 : 1;
  else 
    return 0;
}
#endif /* WIN32_NATIVE */

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

char * xrealpath (const char *path, char resolved_path []);
char *
xrealpath (const char *path, char resolved_path [])
{
  char copy_path[PATH_MAX];
  char *new_path = resolved_path;
  char *max_path;
#if defined (S_IFLNK) || defined (WIN32_NATIVE)
  int readlinks = 0;
  char link_path[PATH_MAX];
  int n;
  int abslen = ABS_LENGTH (path);
#endif

  /* Make a copy of the source path since we may need to modify it. */
  strcpy (copy_path, path);
  path = copy_path;
  max_path = copy_path + PATH_MAX - 2;

  if (0)
    ;
#ifdef WIN32_FILENAMES
  /* Check for c:/... or //server/... */
  else if (abslen == 3 || abslen == 2)
    {
      /* Make sure drive letter is lowercased. */
      if (abslen == 3) {
	*new_path = tolower (*path);
	new_path++;
	path++;
	abslen--;
      }
      /* Coerce directory chars. */
      while (abslen-- > 0) {
	if (IS_DIRECTORY_SEP (*path))
	  *new_path++ = DIRECTORY_SEP;
	else
	  *new_path++ = *path;
	path++;
      }
    }
#endif
#ifdef WIN32_NATIVE
  /* No drive letter, but a beginning slash? Prepend drive letter. */
  else if (abslen == 1)
    {
      getcwd (new_path, PATH_MAX - 1);
      new_path += 3;
      path++;
    }
  /* Just a path name, prepend the current directory */
  else if (1)
    {
      getcwd (new_path, PATH_MAX - 1);
      new_path += strlen (new_path);
      if (!IS_DIRECTORY_SEP (new_path[-1]))
	*new_path++ = DIRECTORY_SEP;
    }
#else
  /* If it's a relative pathname use getcwd for starters. */
  else if (abslen == 0)
    {
      getcwd (new_path, PATH_MAX - 1);
      new_path += strlen (new_path);
      if (!IS_DIRECTORY_SEP (new_path[-1]))
	*new_path++ = DIRECTORY_SEP;
    }
  else
    {
      /* Copy first directory sep. May have two on cygwin. */
      strncpy (new_path, path, abslen);
      new_path += abslen;
      path += abslen;
    }
#endif
  /* Expand each slash-separated pathname component. */
  while (*path != '\0')
    {
      /* Ignore stray "/". */
      if (IS_DIRECTORY_SEP (*path))
	{
	  path++;
	  continue;
	}

      if (*path == '.')
	{
	  /* Ignore ".". */
	  if (path[1] == '\0' || IS_DIRECTORY_SEP (path[1]))
	    {
	      path++;
	      continue;
	    }

	  /* Handle ".." */
	  if (path[1] == '.' &&
	      (path[2] == '\0' || IS_DIRECTORY_SEP (path[2])))
	    {
	      path += 2;

	      /* Ignore ".." at root. */
	      if (new_path == ABS_START (resolved_path))
		continue;

	      /* Handle ".." by backing up. */
	      --new_path;
	      while (!IS_DIRECTORY_SEP (new_path[-1]))
		--new_path;
	      continue;
	    }
	}

      /* Safely copy the next pathname component. */
      while (*path != '\0' && !IS_DIRECTORY_SEP (*path))
	{
	  if (path > max_path)
	    {
	      errno = ENAMETOOLONG;
	      return NULL;
	    }
	  *new_path++ = *path++;
	}

#if defined (S_IFLNK) || defined (WIN32_NATIVE)
      /* See if latest pathname component is a symlink. */
      *new_path = '\0';
      n = system_readlink (resolved_path, link_path, PATH_MAX - 1);

      if (n < 0)
	{
	  /* EINVAL means the file exists but isn't a symlink. */
#ifdef CYGWIN
	  if (errno != EINVAL && errno != ENOENT)
#else
	  if (errno != EINVAL) 
#endif
	    return NULL;
	}
      else
	{
	  /* Protect against infinite loops. */
	  if (readlinks++ > MAX_READLINKS)
	    {
	      errno = ELOOP;
	      return NULL;
	    }

	  /* Note: readlink doesn't add the null byte. */
	  link_path[n] = '\0';
	  
	  if (ABS_LENGTH (link_path) > 0)
	    /* Start over for an absolute symlink. */
	    new_path = resolved_path + ABS_LENGTH (link_path) - 1;
	  else
	    /* Otherwise back up over this component. */
	    for (--new_path; !IS_DIRECTORY_SEP (*new_path); --new_path)
	      assert (new_path > resolved_path);

	  /* Safe sex check. */
	  if (strlen(path) + n >= PATH_MAX)
	    {
	      errno = ENAMETOOLONG;
	      return NULL;
	    }

	  /* Insert symlink contents into path. */
	  strcat(link_path, path);
	  strcpy(copy_path, link_path);
	  path = copy_path;
	}
#endif /* S_IFLNK || WIN32_NATIVE */
      *new_path++ = DIRECTORY_SEP;
    }

  /* Delete trailing slash but don't whomp a lone slash. */
  if (new_path != ABS_START (resolved_path) && IS_DIRECTORY_SEP (new_path[-1]))
    new_path--;

  /* Make sure it's null terminated. */
  *new_path = '\0';

  return resolved_path;
}
