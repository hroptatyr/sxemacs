/*
   Copyright (C) 1995 Free Software Foundation, Inc.

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


/* Synched up with: Not really in FSF. */

#ifndef INCLUDED_sysdir_h_
#define INCLUDED_sysdir_h_

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef SYSV_SYSTEM_DIR
# include <dirent.h>
#elif defined (NONSYSTEM_DIR_LIBRARY)
# include "ndir.h"
#else
# include <sys/dir.h>
#endif				/* not NONSYSTEM_DIR_LIBRARY */

#ifdef SYSV_SYSTEM_DIR
# define DIRENTRY struct dirent
#else				/* not SYSV_SYSTEM_DIR */
# define DIRENTRY struct direct
#endif

/* The d_nameln member of a struct dirent includes the '\0' character
   on some systems, but not on others.  What's worse, you can't tell
   at compile-time which one it will be, since it really depends on
   the sort of system providing the filesystem you're reading from,
   not the system you are running on.  Paul Eggert
   <eggert@bi.twinsun.com> says this occurs when Emacs is running on a
   SunOS 4.1.2 host, reading a directory that is remote-mounted from a
   Solaris 2.1 host and is in a native Solaris 2.1 filesystem.

   (and Solaris 2 doesn't have a d_nameln member at all!  Posix.1
   doesn't specify it -- mrb)

   Since applying strlen to the name always works, we'll just do that.  */
#define NAMLEN(p) strlen (p->d_name)

#define DIRENTRY_NONEMPTY(p) ((p)->d_ino)

/* encapsulation: directory calls */

#ifdef ENCAPSULATE_CHDIR
int sys_chdir(const char *path);
#endif
#if defined (ENCAPSULATE_CHDIR) && !defined (DONT_ENCAPSULATE)
# undef chdir
# define chdir sys_chdir
#endif
#if !defined (ENCAPSULATE_CHDIR) && defined (DONT_ENCAPSULATE)
# define sys_chdir chdir
#endif

#ifdef ENCAPSULATE_MKDIR
int sys_mkdir(const char *path, mode_t mode);
#endif
#if defined (ENCAPSULATE_MKDIR) && !defined (DONT_ENCAPSULATE)
# undef mkdir
# define mkdir sys_mkdir
#endif
#if !defined (ENCAPSULATE_MKDIR) && defined (DONT_ENCAPSULATE)
# define sys_mkdir mkdir
#endif

#ifdef ENCAPSULATE_OPENDIR
DIR *sys_opendir(const char *filename);
#endif
#if defined (ENCAPSULATE_OPENDIR) && !defined (DONT_ENCAPSULATE)
# undef opendir
# define opendir sys_opendir
#endif
#if !defined (ENCAPSULATE_OPENDIR) && defined (DONT_ENCAPSULATE)
# define sys_opendir opendir
#endif

#ifdef ENCAPSULATE_READDIR
DIRENTRY *sys_readdir(DIR * dirp);
#endif
#if defined (ENCAPSULATE_READDIR) && !defined (DONT_ENCAPSULATE)
# undef readdir
# define readdir sys_readdir
#endif
#if !defined (ENCAPSULATE_READDIR) && defined (DONT_ENCAPSULATE)
# define sys_readdir readdir
#endif

#ifdef ENCAPSULATE_CLOSEDIR
int sys_closedir(DIR * dirp);
#endif
#if defined (ENCAPSULATE_CLOSEDIR) && !defined (DONT_ENCAPSULATE)
# undef closedir
# define closedir sys_closedir
#endif
#if !defined (ENCAPSULATE_CLOSEDIR) && defined (DONT_ENCAPSULATE)
# define sys_closedir closedir
#endif

#ifdef ENCAPSULATE_RMDIR
int sys_rmdir(const char *path);
#endif
#if defined (ENCAPSULATE_RMDIR) && !defined (DONT_ENCAPSULATE)
# undef rmdir
# define rmdir sys_rmdir
#endif
#if !defined (ENCAPSULATE_RMDIR) && defined (DONT_ENCAPSULATE)
# define sys_rmdir rmdir
#endif

#endif				/* INCLUDED_sysdir_h_ */
