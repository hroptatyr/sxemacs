/* Utility and Unix shadow routines for XEmacs on MS Windows.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.


   Geoff Voelker (voelker@cs.washington.edu) 7-29-94 */

/* Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au> */
/* Sync'ed with Emacs 19.34.6 by Marc Paquette <marcpa@cam.org> */

#include <config.h>
#define getwd _getwd
#include "lisp.h"
#undef getwd
#include "buffer.h"

#include "systime.h"
#include "syssignal.h"
#include "sysproc.h"
#include "sysfile.h"
#include "syspwd.h"
#include "sysdir.h"

#include "syswindows.h"

#include "nt.h"
#include "ntheap.h"


extern Lisp_Object Vmswindows_downcase_file_names;
#if 0
extern Lisp_Object Vwin32_generate_fake_inodes;
#endif
extern Lisp_Object Vmswindows_get_true_file_attributes;

Fixnum nt_fake_unix_uid;

static char startup_dir[ MAXPATHLEN ];

/* Get the current working directory.  */
char *
getwd (char *dir)
{
#if 0
  if (GetCurrentDirectory (MAXPATHLEN, dir) > 0)
    return dir;
  return NULL;
#else
  /* Emacs doesn't actually change directory itself, and we want to
     force our real wd to be where emacs.exe is to avoid unnecessary
     conflicts when trying to rename or delete directories.  */
  strcpy (dir, startup_dir);
  return dir;
#endif
}

/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char the_passwd_name[PASSWD_FIELD_SIZE];
static char the_passwd_passwd[PASSWD_FIELD_SIZE];
static char the_passwd_gecos[PASSWD_FIELD_SIZE];
static char the_passwd_dir[PASSWD_FIELD_SIZE];
static char the_passwd_shell[PASSWD_FIELD_SIZE];

static struct passwd the_passwd = 
{
  the_passwd_name,
  the_passwd_passwd,
  0,
  0,
  0,
  the_passwd_gecos,
  the_passwd_dir,
  the_passwd_shell,
};

uid_t
getuid (void) 
{
  return nt_fake_unix_uid;
}

uid_t 
geteuid (void) 
{ 
  return nt_fake_unix_uid;
}

gid_t
getgid (void) 
{ 
  return the_passwd.pw_gid;
}

gid_t
getegid (void) 
{ 
  return getgid ();
}

struct passwd *
getpwuid (uid_t uid)
{
  if (uid == nt_fake_unix_uid)
    {
      the_passwd.pw_gid = the_passwd.pw_uid = uid;
      return &the_passwd;
    }
  else
    return NULL;
}

struct passwd *
getpwnam (const char *name)
{
  struct passwd *pw;
  
  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (stricmp (name, pw->pw_name))
    return NULL;

  return pw;
}

void
init_user_info (void)
{
  /* This code is pretty much of ad hoc nature. There is no unix-like
     UIDs under Windows NT. There is no concept of root user, because
     all security is ACL-based. Instead, let's use a simple variable,
     nt-fake-unix-uid, which would allow the user to have a uid of
     choice. --kkm, 02/03/2000 */
#if 0
  /* Find the user's real name by opening the process token and
     looking up the name associated with the user-sid in that token.

     Use the relative portion of the identifier authority value from
     the user-sid as the user id value (same for group id using the
     primary group sid from the process token). */

  char            user_sid[256], name[256], domain[256];
  DWORD           length = sizeof (name), dlength = sizeof (domain), trash;
  HANDLE          token = NULL;
  SID_NAME_USE    user_type;

  if (OpenProcessToken (GetCurrentProcess (), TOKEN_QUERY, &token)
      && GetTokenInformation (token, TokenUser,
			      (PVOID) user_sid, sizeof (user_sid), &trash)
      && LookupAccountSid (NULL, *((PSID *) user_sid), name, &length,
			   domain, &dlength, &user_type))
    {
      strcpy (the_passwd.pw_name, name);
      /* Determine a reasonable uid value. */
      if (stricmp ("administrator", name) == 0)
	{
	  the_passwd.pw_uid = 0;
	  the_passwd.pw_gid = 0;
	}
      else
	{
	  SID_IDENTIFIER_AUTHORITY * pSIA;

	  pSIA = GetSidIdentifierAuthority (*((PSID *) user_sid));
	  /* I believe the relative portion is the last 4 bytes (of 6)
	     with msb first. */
	  the_passwd.pw_uid = ((pSIA->Value[2] << 24) +
			       (pSIA->Value[3] << 16) +
			       (pSIA->Value[4] << 8)  +
			       (pSIA->Value[5] << 0));
	  /* restrict to conventional uid range for normal users */
	  the_passwd.pw_uid = the_passwd.pw_uid % 60001;

	  /* Get group id */
	  if (GetTokenInformation (token, TokenPrimaryGroup,
				   (PVOID) user_sid, sizeof (user_sid), &trash))
	    {
	      SID_IDENTIFIER_AUTHORITY * pSIA;

	      pSIA = GetSidIdentifierAuthority (*((PSID *) user_sid));
	      the_passwd.pw_gid = ((pSIA->Value[2] << 24) +
				   (pSIA->Value[3] << 16) +
				   (pSIA->Value[4] << 8)  +
				   (pSIA->Value[5] << 0));
	      /* I don't know if this is necessary, but for safety... */
	      the_passwd.pw_gid = the_passwd.pw_gid % 60001;
	    }
	  else
	    the_passwd.pw_gid = the_passwd.pw_uid;
	}
    }
  /* If security calls are not supported (presumably because we
       are running under Windows 95), fallback to this. */
  else if (GetUserName (name, &length))
    {
      strcpy (the_passwd.pw_name, name);
      if (stricmp ("administrator", name) == 0)
	the_passwd.pw_uid = 0;
      else
	the_passwd.pw_uid = 123;
      the_passwd.pw_gid = the_passwd.pw_uid;
    }
  else
    {
      strcpy (the_passwd.pw_name, "unknown");
      the_passwd.pw_uid = 123;
      the_passwd.pw_gid = 123;
    }

  if (token)
    CloseHandle (token);
#else
  /* Obtain only logon id here, uid part is moved to getuid */
  char name[256];
  DWORD length = sizeof (name);
  if (GetUserName (name, &length))
    strcpy (the_passwd.pw_name, name);
  else
    strcpy (the_passwd.pw_name, "unknown");
#endif

  /* Ensure HOME and SHELL are defined. */
#if 0
  /*
   * With XEmacs, setting $HOME is deprecated.
   */
  if (getenv ("HOME") == NULL)
    putenv ("HOME=c:/");
#endif

  /* Set dir from environment variables. */
  strcpy (the_passwd.pw_dir, (char *)get_home_directory());
  /* We used to set pw_shell here, but the order is wrong (SHELL gets
     init in callproc.c, called later in the init process) and pw_shell
     is not used anywhere. */
}

/* Normalize filename by converting all path separators to
   the specified separator.  Also conditionally convert upper
   case path name components to lower case.  */

static void
normalize_filename (char *fp, char path_sep)
{
  char sep;
  char *elem;

  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so filenames can be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if (fp[1] == ':' && *fp >= 'A' && *fp <= 'Z')
    {
      *fp += 'a' - 'A';
      fp += 2;
    }

  if (NILP (Vmswindows_downcase_file_names))
    {
      while (*fp)
	{
	  if (*fp == '/' || *fp == '\\')
	    *fp = path_sep;
	  fp++;
	}
      return;
    }

  sep = path_sep;		/* convert to this path separator */
  elem = fp;			/* start of current path element */

  do {
    if (*fp >= 'a' && *fp <= 'z')
      elem = 0;			/* don't convert this element */

    if (*fp == 0 || *fp == ':')
      {
	sep = *fp;		/* restore current separator (or 0) */
	*fp = '/';		/* after conversion of this element */
      }

    if (*fp == '/' || *fp == '\\')
      {
	if (elem && elem != fp)
	  {
	    *fp = 0;		/* temporary end of string */
	    _strlwr (elem);	/* while we convert to lower case */
	  }
	*fp = sep;		/* convert (or restore) path separator */
	elem = fp + 1;		/* next element starts after separator */
	sep = path_sep;
      }
  } while (*fp++);
}

/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (char *p)
{
  normalize_filename (p, '/');
}

/* Destructively turn slashes into backslashes.  */
void
unixtodos_filename (char *p)
{
  normalize_filename (p, '\\');
}

/* Remove all CR's that are followed by a LF.
   (From msdos.c...probably should figure out a way to share it,
   although this code isn't going to ever change.)  */
int
crlf_to_lf (int n, unsigned char *buf, unsigned *lf_count)
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0a)
	(*lf_count)++;
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    {
      if (*buf == 0x0a)
	(*lf_count)++;
    *np++ = *buf++;
    }
  return np - startp;
}

/* Parse the root part of file name, if present.  Return length and
    optionally store pointer to char after root.  */
static int
parse_root (char * name, char ** pPath)
{
  char * start = name;

  if (name == NULL)
    return 0;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      /* skip past drive specifier */
      name += 2;
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      int slashes = 2;
      name += 2;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  name++;
	}
      while ( *name );
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }

  if (pPath)
    *pPath = name;

  return name - start;
}

/* Get long base name for name; name is assumed to be absolute.  */
static int
get_long_basename (char * name, char * buf, int size)
{
  WIN32_FIND_DATA find_data;
  HANDLE dir_handle;
  int len = 0;
#ifdef PIGSFLY
  char *p;

  /* If the last component of NAME has a wildcard character, 
     return it as the basename.  */
  p = name + strlen (name);
  while (*p != '\\' && *p != ':' && p > name) p--;
  if (p > name) p++;
  if (strchr (p, '*') || strchr (p, '?'))
    {
      if ((len = strlen (p)) < size)
	memcpy (buf, p, len + 1);
      else
	len = 0;
      return len;
    }
#endif

  dir_handle = FindFirstFile (name, &find_data);
  if (dir_handle != INVALID_HANDLE_VALUE)
    {
      if ((len = strlen (find_data.cFileName)) < size)
	memcpy (buf, find_data.cFileName, len + 1);
      else
	len = 0;
      FindClose (dir_handle);
    }
  return len;
}

/* Get long name for file, if possible (assumed to be absolute).  */
BOOL
win32_get_long_filename (char * name, char * buf, int size)
{
  char * o = buf;
  char * p;
  char * q;
  char full[ MAX_PATH ];
  int len;

  len = strlen (name);
  if (len >= MAX_PATH)
    return FALSE;

  /* Use local copy for destructive modification.  */
  memcpy (full, name, len+1);
  unixtodos_filename (full);

  /* Copy root part verbatim.  */
  len = parse_root (full, &p);
  memcpy (o, full, len);
  o += len;
  size -= len;

  do
    {
      q = p;
      p = strchr (q, '\\');
      if (p) *p = '\0';
      len = get_long_basename (full, o, size);
      if (len > 0)
	{
	  o += len;
	  size -= len;
	  if (p != NULL)
	    {
	      *p++ = '\\';
	      if (size < 2)
		return FALSE;
	      *o++ = '\\';
	      size--;
	      *o = '\0';
	    }
	}
      else
	return FALSE;
    }
  while (p != NULL && *p);

  return TRUE;
}


/* Routines that are no-ops on NT but are defined to get Emacs to compile.  */

#if 0 /* #### We do not need those, do we? -kkm */
int 
unrequest_sigio (void) 
{ 
  return 0;
}

int 
request_sigio (void) 
{ 
  return 0;
}
#endif /* 0 */

#define REG_ROOT "SOFTWARE\\XEmacs\\XEmacs"

LPBYTE 
nt_get_resource (char *key, LPDWORD lpdwtype)
{
  LPBYTE lpvalue;
  HKEY hrootkey = NULL;
  DWORD cbData;
  
  /* Check both the current user and the local machine to see if 
     we have any resources.  */
  
  if (RegOpenKeyEx (HKEY_CURRENT_USER, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS 
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL 
	  && RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
	  return (lpvalue);
	}

      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  if (RegOpenKeyEx (HKEY_LOCAL_MACHINE, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;
	
      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS &&
	  (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL &&
	  RegQueryValueEx (hrootkey, key, NULL, lpdwtype, lpvalue, &cbData) == ERROR_SUCCESS)
	{
	  return (lpvalue);
	}
	
      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  return (NULL);
}

void
init_environment (void)
{
  /* Check for environment variables and use registry if they don't exist */
  {
    int i;
    LPBYTE lpval;
    DWORD dwType;

    static char * env_vars[] = 
    {
      "HOME",
      "emacs_dir",
      "EMACSLOADPATH",
      "EMACSDEBUGPATHS",
      "SHELL",
      "CMDPROXY",
      "EMACSDATA",
      "EMACSPATH",
      "EMACSPACKAGEPATH",
      "EMACSLOCKDIR",
      "INFOPATH"
    };
#if defined (HEAP_IN_DATA) && !defined(PDUMP)
    cache_system_info ();
#endif
    for (i = 0; i < countof (env_vars); i++) 
      {
	if (!getenv (env_vars[i]) &&
	    (lpval = nt_get_resource (env_vars[i], &dwType)) != NULL)
	  {
	    if (dwType == REG_EXPAND_SZ)
	      {
		char buf1[500], buf2[500];

		ExpandEnvironmentStrings ((LPSTR) lpval, buf1, 500);
		_snprintf (buf2, 499, "%s=%s", env_vars[i], buf1);
		putenv (strdup (buf2));
	      }
	    else if (dwType == REG_SZ)
	      {
		char buf[500];
		  
		_snprintf (buf, 499, "%s=%s", env_vars[i], lpval);
		putenv (strdup (buf));
	      }

	    xfree (lpval);
	  }
      }
  }

  /* Another special case: on NT, the PATH variable is actually named
     "Path" although cmd.exe (perhaps NT itself) arranges for
     environment variable lookup and setting to be case insensitive.
     However, Emacs assumes a fully case sensitive environment, so we
     need to change "Path" to "PATH" to match the expectations of
     various elisp packages.  We do this by the sneaky method of
     modifying the string in the C runtime environ entry.

     The same applies to COMSPEC.  */
  {
    char ** envp;

    for (envp = environ; *envp; envp++)
      if (_strnicmp (*envp, "PATH=", 5) == 0)
	memcpy (*envp, "PATH=", 5);
      else if (_strnicmp (*envp, "COMSPEC=", 8) == 0)
	memcpy (*envp, "COMSPEC=", 8);
  }

  /* Remember the initial working directory for getwd, then make the
     real wd be the location of emacs.exe to avoid conflicts when
     renaming or deleting directories.  (We also don't call chdir when
     running subprocesses for the same reason.)  */
  if (!GetCurrentDirectory (MAXPATHLEN, startup_dir))
    abort ();

  {
    char *p;
    char modname[MAX_PATH];

    if (!GetModuleFileName (NULL, modname, MAX_PATH))
      abort ();
    if ((p = strrchr (modname, '\\')) == NULL)
      abort ();
    *p = 0;

    SetCurrentDirectory (modname);
  }

  init_user_info ();
}

#ifndef HAVE_X_WINDOWS
/* X11R6 on NT provides the single parameter version of this command. */

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

#endif /* HAVE_X_WINDOWS */

/* ------------------------------------------------------------------------- */
/* IO support and wrapper functions for Win32 API. */
/* ------------------------------------------------------------------------- */

/* Place a wrapper around the MSVC version of ctime.  It returns NULL
   on network directories, so we handle that case here.  
   (Ulrich Leodolter, 1/11/95).  */
char *
sys_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : "Sun Jan 01 00:00:00 1970");
}

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */

#ifndef HAVE_X_WINDOWS
void
sys_sleep (int seconds)
{
  Sleep (seconds * 1000);
}
#endif

/* #### This is an evil dirty hack. We must get rid of it.
   Word "munging" is not in XEmacs lexicon. - kkm */

/* Internal MSVC data and functions for low-level descriptor munging */
#if (_MSC_VER == 900)
extern char _osfile[];
#endif
extern int __cdecl _set_osfhnd (int fd, long h);
extern int __cdecl _free_osfhnd (int fd);

/* parallel array of private info on file handles */
filedesc fd_info [ MAXDESC ];

typedef struct volume_info_data {
  struct volume_info_data * next;

  /* time when info was obtained */
  DWORD     timestamp;

  /* actual volume info */
  char *    root_dir;
  DWORD     serialnum;
  DWORD     maxcomp;
  DWORD     flags;
  char *    name;
  char *    type;
} volume_info_data;

/* Global referenced by various functions.  */
static volume_info_data volume_info;

/* Vector to indicate which drives are local and fixed (for which cached
   data never expires).  */
static BOOL fixed_drives[26];

/* Consider cached volume information to be stale if older than 10s,
   at least for non-local drives.  Info for fixed drives is never stale.  */
#define DRIVE_INDEX( c ) ( (c) <= 'Z' ? (c) - 'A' : (c) - 'a' )
#define VOLINFO_STILL_VALID( root_dir, info )		\
  ( ( isalpha (root_dir[0]) &&				\
      fixed_drives[ DRIVE_INDEX (root_dir[0]) ] )	\
    || GetTickCount () - info->timestamp < 10000 )

/* Cache support functions.  */

/* Simple linked list with linear search is sufficient.  */
static volume_info_data *volume_cache = NULL;

static volume_info_data *
lookup_volume_info (char * root_dir)
{
  volume_info_data * info;

  for (info = volume_cache; info; info = info->next)
    if (stricmp (info->root_dir, root_dir) == 0)
      break;
  return info;
}

static void
add_volume_info (char * root_dir, volume_info_data * info)
{
  info->root_dir = xstrdup (root_dir);
  info->next = volume_cache;
  volume_cache = info;
}


/* Wrapper for GetVolumeInformation, which uses caching to avoid
   performance penalty (~2ms on 486 for local drives, 7.5ms for local
   cdrom drive, ~5-10ms or more for remote drives on LAN).  */
volume_info_data *
GetCachedVolumeInformation (char * root_dir)
{
  volume_info_data * info;
  char default_root[ MAX_PATH ];

  /* NULL for root_dir means use root from current directory.  */
  if (root_dir == NULL)
    {
      if (GetCurrentDirectory (MAX_PATH, default_root) == 0)
	return NULL;
      parse_root (default_root, &root_dir);
      *root_dir = 0;
      root_dir = default_root;
    }

  /* Local fixed drives can be cached permanently.  Removable drives
     cannot be cached permanently, since the volume name and serial
     number (if nothing else) can change.  Remote drives should be
     treated as if they are removable, since there is no sure way to
     tell whether they are or not.  Also, the UNC association of drive
     letters mapped to remote volumes can be changed at any time (even
     by other processes) without notice.
   
     As a compromise, so we can benefit from caching info for remote
     volumes, we use a simple expiry mechanism to invalidate cache
     entries that are more than ten seconds old.  */

#if 0
  /* No point doing this, because WNetGetConnection is even slower than
     GetVolumeInformation, consistently taking ~50ms on a 486 (FWIW,
     GetDriveType is about the only call of this type which does not
     involve network access, and so is extremely quick).  */

  /* Map drive letter to UNC if remote. */
  if ( isalpha( root_dir[0] ) && !fixed[ DRIVE_INDEX( root_dir[0] ) ] )
    {
      char remote_name[ 256 ];
      char drive[3] = { root_dir[0], ':' };

      if (WNetGetConnection (drive, remote_name, sizeof (remote_name))
	  == NO_ERROR)
	/* do something */ ;
    }
#endif

  info = lookup_volume_info (root_dir);

  if (info == NULL || ! VOLINFO_STILL_VALID (root_dir, info))
  {
    char  name[ 256 ];
  DWORD     serialnum;
  DWORD     maxcomp;
  DWORD     flags;
    char  type[ 256 ];

    /* Info is not cached, or is stale. */
    if (!GetVolumeInformation (root_dir,
			       name, sizeof (name),
			       &serialnum,
			       &maxcomp,
			       &flags,
			       type, sizeof (type)))
      return NULL;

    /* Cache the volume information for future use, overwriting existing
       entry if present.  */
    if (info == NULL)
      {
	info = (volume_info_data *) xmalloc (sizeof (volume_info_data));
	add_volume_info (root_dir, info);
      }
    else
      {
	free (info->name);
	free (info->type);
      }

    info->name = xstrdup (name);
    info->serialnum = serialnum;
    info->maxcomp = maxcomp;
    info->flags = flags;
    info->type = xstrdup (type);
    info->timestamp = GetTickCount ();
  }

  return info;
}

/* Get information on the volume where name is held; set path pointer to
   start of pathname in name (past UNC header\volume header if present).  */
int
get_volume_info (const char * name, const char ** pPath)
{
  char temp[MAX_PATH];
  char *rootname = NULL;  /* default to current volume */
  volume_info_data * info;

  if (name == NULL)
    return FALSE;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      rootname = temp;
      temp[0] = *name++;
      temp[1] = *name++;
      temp[2] = '\\';
      temp[3] = 0;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      char *str = temp;
      int slashes = 4;
      rootname = temp;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  *str++ = *name++;
	}
      while ( *name );

      *str++ = '\\';
      *str = 0;
    }

  if (pPath)
    *pPath = name;
    
  info = GetCachedVolumeInformation (rootname);
  if (info != NULL)
    {
      /* Set global referenced by other functions.  */
      volume_info = *info;
      return TRUE;
    }
  return FALSE;
}

/* Determine if volume is FAT format (ie. only supports short 8.3
   names); also set path pointer to start of pathname in name.  */
int
is_fat_volume (const char * name, const char ** pPath)
{
  if (get_volume_info (name, pPath))
    return (volume_info.maxcomp == 12);
  return FALSE;
}

/* Map filename to a legal 8.3 name if necessary. */
const char *
map_win32_filename (const char * name, const char ** pPath)
{
  static char shortname[MAX_PATH];
  char * str = shortname;
  char c;
  const char * path;
  const char * save_name = name;

  if (is_fat_volume (name, &path)) /* truncate to 8.3 */
    {
      REGISTER int left = 8;	/* maximum number of chars in part */
      REGISTER int extn = 0;	/* extension added? */
      REGISTER int dots = 2;	/* maximum number of dots allowed */

      while (name < path)
	*str++ = *name++;	/* skip past UNC header */

      while ((c = *name++))
        {
	  switch ( c )
	    {
	    case '\\':
	    case '/':
	      *str++ = '\\';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case ':':
	      *str++ = ':';
	      extn = 0;		/* reset extension flags */
	      dots = 2;		/* max 2 dots */
	      left = 8;		/* max length 8 for main part */
	      break;
	    case '.':
	      if ( dots )
	        {
		  /* Convert path components of the form .xxx to _xxx,
		     but leave . and .. as they are.  This allows .emacs
		     to be read as _emacs, for example.  */

		  if (! *name ||
		      *name == '.' ||
		      IS_DIRECTORY_SEP (*name))
		    {
		      *str++ = '.';
		      dots--;
		    }
		  else
		    {
		      *str++ = '_';
		      left--;
		      dots = 0;
		    }
		}
	      else if ( !extn )
	        {
		  *str++ = '.';
		  extn = 1;		/* we've got an extension */
		  left = 3;		/* 3 chars in extension */
		}
	      else
	        {
		  /* any embedded dots after the first are converted to _ */
		  *str++ = '_';
		}
	      break;
	    case '~':
	    case '#':			/* don't lose these, they're important */
	      if ( ! left )
		str[-1] = c;		/* replace last character of part */
	      /* FALLTHRU */
	    default:
	      if ( left )
	        {
		  *str++ = tolower (c);	/* map to lower case (looks nicer) */
		  left--;
		  dots = 0;		/* started a path component */
		}
	      break;
	    }
	}
      *str = '\0';
    }
  else
    {
      strcpy (shortname, name);
      unixtodos_filename (shortname);
    }

  if (pPath)
    *pPath = shortname + (path - save_name);

  return shortname;
}


/* Emulate the Unix directory procedures opendir, closedir, 
   and readdir.  We can't use the procedures supplied in sysdep.c,
   so we provide them here.  */

struct direct dir_static;       /* simulated directory contents */
static HANDLE dir_find_handle = INVALID_HANDLE_VALUE;
static int    dir_is_fat;
static char   dir_pathname[MAXPATHLEN+1];
static WIN32_FIND_DATA dir_find_data;

DIR *
opendir (const char *filename)
{
  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we defer the open until read time.  */

  if (!(dirp = xnew_and_zero(DIR)))
    return NULL;
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    return NULL;

  dirp->dd_fd = 0;
  dirp->dd_loc = 0;
  dirp->dd_size = 0;

  strncpy (dir_pathname, map_win32_filename (filename, NULL), MAXPATHLEN);
  dir_pathname[MAXPATHLEN] = '\0';
  dir_is_fat = is_fat_volume (filename, NULL);

  return dirp;
}

int
closedir (DIR *dirp)
{
  BOOL retval;

  /* If we have a find-handle open, close it.  */
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    {
      retval = FindClose (dir_find_handle);
      dir_find_handle = INVALID_HANDLE_VALUE;
    }
  xfree (dirp);
  if (retval)
    return 0;
  else
    return -1;
}

struct direct *
readdir (DIR *dirp)
{
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  if (dir_find_handle == INVALID_HANDLE_VALUE)
    {
      char filename[MAXNAMLEN + 3];
      int ln;

      strcpy (filename, dir_pathname);
      ln = strlen (filename) - 1;
      if (!IS_DIRECTORY_SEP (filename[ln]))
	strcat (filename, "\\");
      strcat (filename, "*");

      dir_find_handle = FindFirstFile (filename, &dir_find_data);

      if (dir_find_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }
  else
    {
      if (!FindNextFile (dir_find_handle, &dir_find_data))
	return NULL;
    }
  
  /* Emacs never uses this value, so don't bother making it match
     value returned by xemacs_stat().  */
  dir_static.d_ino = 1;
  
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;
  
  dir_static.d_namlen = strlen (dir_find_data.cFileName);
  strcpy (dir_static.d_name, dir_find_data.cFileName);
  if (dir_is_fat)
    _strlwr (dir_static.d_name);
  else if (!NILP (Vmswindows_downcase_file_names))
    {
      REGISTER char *p;
      for (p = dir_static.d_name; *p; p++)
	if (*p >= 'a' && *p <= 'z')
	  break;
      if (!*p)
	_strlwr (dir_static.d_name);
    }
  
  return &dir_static;
}

#if 0
/* #### Have to check if all that sad story about '95 is true - kkm */
int
sys_rename (const char * oldname, const char * newname)
{
  char temp[MAX_PATH];
  DWORD attr;

  /* MoveFile on Win95 doesn't correctly change the short file name
     alias in a number of circumstances (it is not easy to predict when
     just by looking at oldname and newname, unfortunately).  In these
     cases, renaming through a temporary name avoids the problem.

     A second problem on Win95 is that renaming through a temp name when
     newname is uppercase fails (the final long name ends up in
     lowercase, although the short alias might be uppercase) UNLESS the
     long temp name is not 8.3.

     So, on Win95 we always rename through a temp name, and we make sure
     the temp name has a long extension to ensure correct renaming.  */

  strcpy (temp, map_win32_filename (oldname, NULL));

  if (GetVersion () & 0x80000000)
    {
      char * p;

      if (p = strrchr (temp, '\\'))
	p++;
      else
	p = temp;
      /* Force temp name to require a manufactured 8.3 alias - this
	 seems to make the second rename work properly. */
      strcpy (p, "_rename_temp.XXXXXX");
      sys_mktemp (temp);
      if (rename (map_win32_filename (oldname, NULL), temp) < 0)
	return -1;
    }

  /* Emulate Unix behavior - newname is deleted if it already exists
     (at least if it is a file; don't do this for directories).
     However, don't do this if we are just changing the case of the file
     name - we will end up deleting the file we are trying to rename!  */
  newname = map_win32_filename (newname, NULL);

  /* TODO: Use GetInformationByHandle (on NT) to ensure newname and temp
     do not refer to the same file, eg. through share aliases.  */
  if (stricmp (newname, temp) != 0
      && (attr = GetFileAttributes (newname)) != -1
      && (attr & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      _chmod (newname, 0666);
      _unlink (newname);
    }

  return rename (temp, newname);
}
#endif /* 0 */

static FILETIME utc_base_ft;
static long double utc_base;
static int init = 0;

#if 0

time_t
convert_time (FILETIME ft)
{
  long double ret;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  if (CompareFileTime (&ft, &utc_base_ft) < 0)
    return 0;

  ret = (long double) ft.dwHighDateTime * 4096 * 1024 * 1024 + ft.dwLowDateTime;
  ret -= utc_base;
  return (time_t) (ret * 1e-7);
}
#else

static LARGE_INTEGER utc_base_li;

time_t
convert_time (FILETIME uft)
{
  time_t ret;
#ifndef MAXLONGLONG
  SYSTEMTIME st;
  struct tm t;
  FILETIME ft;
  TIME_ZONE_INFORMATION tzi;
  DWORD tzid;
#else
  LARGE_INTEGER lft;
#endif

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);

      utc_base_li.LowPart = utc_base_ft.dwLowDateTime;
      utc_base_li.HighPart = utc_base_ft.dwHighDateTime;

      init = 1;
    }

#ifdef MAXLONGLONG

  /* On a compiler that supports long integers, do it the easy way */
  lft.LowPart = uft.dwLowDateTime;
  lft.HighPart = uft.dwHighDateTime;
  ret = (time_t) ((lft.QuadPart - utc_base_li.QuadPart) / 10000000);

#else

  /* Do it the hard way using mktime. */
  FileTimeToLocalFileTime(&uft, &ft);
  FileTimeToSystemTime (&ft, &st);
  tzid = GetTimeZoneInformation (&tzi);
  t.tm_year = st.wYear - 1900;
  t.tm_mon = st.wMonth - 1;
  t.tm_mday = st.wDay;
  t.tm_hour = st.wHour;
  t.tm_min = st.wMinute;
  t.tm_sec = st.wSecond;
  t.tm_isdst = (tzid == TIME_ZONE_ID_DAYLIGHT);
  /* st.wMilliseconds not applicable */
  ret = mktime(&t);
  if (ret == -1)
    {
      ret = 0;
    }

#endif

  return ret;
}
#endif
#if defined(MINGW) && CYGWIN_VERSION_DLL_MAJOR <= 21
#undef LowPart
#undef HighPart
#endif

#if 0
/* in case we ever have need of this */
void
convert_from_time_t (time_t time, FILETIME * pft)
{
  long double tmp;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp = (long double) time * 1e7 + utc_base;
  pft->dwHighDateTime = (DWORD) (tmp / (4096.0 * 1024 * 1024));
  pft->dwLowDateTime = (DWORD) (tmp - pft->dwHighDateTime);
}
#endif

#if 0
/* No reason to keep this; faking inode values either by hashing or even
   using the file index from GetInformationByHandle, is not perfect and
   so by default Emacs doesn't use the inode values on Windows.
   Instead, we now determine file-truename correctly (except for
   possible drive aliasing etc).  */

/*  Modified version of "PJW" algorithm (see the "Dragon" compiler book). */
static unsigned
hashval (const unsigned char * str)
{
  unsigned h = 0;
  while (*str)
    {
      h = (h << 4) + *str++;
      h ^= (h >> 28);
    }
  return h;
}

/* Return the hash value of the canonical pathname, excluding the
   drive/UNC header, to get a hopefully unique inode number. */
static DWORD
generate_inode_val (const char * name)
{
  char fullname[ MAX_PATH ];
  char * p;
  unsigned hash;

  /* Get the truly canonical filename, if it exists.  (Note: this
     doesn't resolve aliasing due to subst commands, or recognize hard
     links.  */
  if (!win32_get_long_filename ((char *)name, fullname, MAX_PATH))
    abort ();

  parse_root (fullname, &p);
  /* Normal Win32 filesystems are still case insensitive. */
  _strlwr (p);
  return hashval (p);
}

#endif

/* #### aichner@ecf.teradyne.com reported that with the library
   provided stat/fstat, (file-exist "d:\\tmp\\") =>> nil,
   (file-exist "d:\\tmp") =>> t, when d:\tmp exists. Whenever
   we opt to use non-encapsulated stat(), this should serve as
   a compatibility test. --kkm */

/* Since stat is encapsulated on Windows NT, we need to encapsulate
   the equally broken fstat as well. FSFmacs also provides its own
   utime. Is that necessary here too? */
int
mswindows_fstat (int desc, struct stat * buf)
{
  HANDLE fh = (HANDLE) _get_osfhandle (desc);
  BY_HANDLE_FILE_INFORMATION info;
  DWORD fake_inode;
  int permission;

  switch (GetFileType (fh) & ~FILE_TYPE_REMOTE)
    {
    case FILE_TYPE_DISK:
      buf->st_mode = _S_IFREG;
      if (!GetFileInformationByHandle (fh, &info))
	{
	  errno = EACCES;
	  return -1;
	}
      break;
    case FILE_TYPE_PIPE:
      buf->st_mode = _S_IFIFO;
      goto non_disk;
    case FILE_TYPE_CHAR:
    case FILE_TYPE_UNKNOWN:
    default:
      buf->st_mode = _S_IFCHR;
    non_disk:
      memset (&info, 0, sizeof (info));
      info.dwFileAttributes = 0;
      info.ftCreationTime = utc_base_ft;
      info.ftLastAccessTime = utc_base_ft;
      info.ftLastWriteTime = utc_base_ft;
    }

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
  else
    {
      buf->st_nlink = (short) info.nNumberOfLinks;
      /* Might as well use file index to fake inode values, but this
	 is not guaranteed to be unique unless we keep a handle open
	 all the time (even then there are situations where it is
	 not unique).  Reputedly, there are at most 48 bits of info
      (on NTFS, presumably less on FAT). */
      fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
    }

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = (unsigned short) (fake_inode ^ (fake_inode >> 16));
  else
    buf->st_ino = (unsigned short) fake_inode;

  /* consider files to belong to current user */
  buf->st_uid = 0;
  buf->st_gid = 0;

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_rdev = info.dwVolumeSerialNumber;

  buf->st_size = info.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (info.ftLastWriteTime);
  buf->st_atime = convert_time (info.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (info.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* MSVC stat function can't cope with UNC names and has other bugs, so
   replace it with our own.  This also allows us to calculate consistent
   inode values without hacks in the main Emacs code. */
int
mswindows_stat (const char * path, struct stat * buf)
{
  char * name;
  WIN32_FIND_DATA wfd;
  HANDLE fh;
  DWORD fake_inode;
  int permission;
  int len;
  int rootdir = FALSE;
  int errm;

  if (path == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  name = (char *) map_win32_filename (path, &path);
  /* must be valid filename, no wild cards */
  if (strchr (name, '*') || strchr (name, '?'))
    {
      errno = ENOENT;
      return -1;
    }

  /* Remove trailing directory separator, unless name is the root
     directory of a drive or UNC volume in which case ensure there
     is a trailing separator. */
  len = strlen (name);
  rootdir = (path >= name + len - 1
	     && (IS_DIRECTORY_SEP (*path) || *path == 0));
  name = strcpy ((char *)alloca (len + 2), name);
  errm = SetErrorMode (SEM_FAILCRITICALERRORS
		       | SEM_NOOPENFILEERRORBOX);
  if (rootdir)
    {
      if (!IS_DIRECTORY_SEP (name[len-1]))
	strcat (name, "\\");

      if (GetDriveType (name) < 2)
	{
	  SetErrorMode (errm);
	  errno = ENOENT;
	  return -1;
	}
      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      strcpy (wfd.cFileName, name);
    }
  else
    {
      if (IS_DIRECTORY_SEP (name[len-1]))
	name[len - 1] = 0;

      /* (This is hacky, but helps when doing file completions on
	 network drives.)  Optimize by using information available from
	 active readdir if possible.  */
      if (dir_find_handle != INVALID_HANDLE_VALUE &&
	  (len = strlen (dir_pathname)),
	  strnicmp (name, dir_pathname, len) == 0 &&
	  IS_DIRECTORY_SEP (name[len]) &&
	  stricmp (name + len + 1, dir_static.d_name) == 0)
	{
	  /* This was the last entry returned by readdir.  */
	  wfd = dir_find_data;
	}
      else
	{
	  fh = FindFirstFile (name, &wfd);
	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      SetErrorMode (errm);
	      errno = ENOENT;
	      return -1;
	    }
	  FindClose (fh);
	}
    }

  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
  else if (!NILP (Vmswindows_get_true_file_attributes))
    {
      /* This is more accurate in terms of getting the correct number
	 of links, but is quite slow (it is noticeable when Emacs is
	 making a list of file name completions). */
      BY_HANDLE_FILE_INFORMATION info;

      /* No access rights required to get info.  */
      fh = CreateFile (name, 0, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL,
		       OPEN_EXISTING, 0, NULL);

      if (GetFileInformationByHandle (fh, &info))
	{
	  switch (GetFileType (fh))
	    {
	    case FILE_TYPE_DISK:
	      buf->st_mode = _S_IFREG;
	      break;
	    case FILE_TYPE_PIPE:
	      buf->st_mode = _S_IFIFO;
	      break;
	    case FILE_TYPE_CHAR:
	    case FILE_TYPE_UNKNOWN:
	    default:
	      buf->st_mode = _S_IFCHR;
	    }
	  buf->st_nlink = (short) info.nNumberOfLinks;
	  /* Might as well use file index to fake inode values, but this
	     is not guaranteed to be unique unless we keep a handle open
	     all the time (even then there are situations where it is
	     not unique).  Reputedly, there are at most 48 bits of info
	     (on NTFS, presumably less on FAT). */
	  fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
	  CloseHandle (fh);
	}
      else
	{
	  SetErrorMode (errm);
	  errno = EACCES;
	  return -1;
	}
    }
  else
    {
      /* Don't bother to make this information more accurate.  */
      buf->st_mode = _S_IFREG;
      buf->st_nlink = 1;
      fake_inode = 0;
    }

  SetErrorMode (errm);

#if 0
  /* Not sure if there is any point in this.  */
  if (!NILP (Vwin32_generate_fake_inodes))
    fake_inode = generate_inode_val (name);
  else if (fake_inode == 0)
    {
      /* For want of something better, try to make everything unique.  */
      static DWORD gen_num = 0;
      fake_inode = ++gen_num;
    }
#endif

  /* #### MSVC defines _ino_t to be short; other libc's might not.  */
  buf->st_ino = (unsigned short) (fake_inode ^ (fake_inode >> 16));

  /* consider files to belong to current user */
  buf->st_uid = buf->st_gid = (short) nt_fake_unix_uid;

  /* volume_info is set indirectly by map_win32_filename */
  buf->st_dev = volume_info.serialnum;
  buf->st_rdev = volume_info.serialnum;

  buf->st_size = wfd.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = convert_time (wfd.ftLastWriteTime);
  buf->st_atime = convert_time (wfd.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = convert_time (wfd.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;
  else
    {
      char * p = strrchr (name, '.');
      if (p != NULL &&
	  (stricmp (p, ".exe") == 0 ||
	   stricmp (p, ".com") == 0 ||
	   stricmp (p, ".bat") == 0 ||
	   stricmp (p, ".cmd") == 0))
	permission |= _S_IEXEC;
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* From callproc.c  */
extern Lisp_Object Vbinary_process_input;
extern Lisp_Object Vbinary_process_output;

/* Unix pipe() has only one arg */
int
sys_pipe (int * phandles)
{
  int rc;
  unsigned flags;

  /* make pipe handles non-inheritable; when we spawn a child, we
     replace the relevant handle with an inheritable one.  Also put
     pipes into binary mode; we will do text mode translation ourselves
     if required.  */
  rc = _pipe (phandles, 0, _O_NOINHERIT | _O_BINARY);

  if (rc == 0)
    {
      flags = FILE_PIPE | FILE_READ;
      if (!NILP (Vbinary_process_output))
	  flags |= FILE_BINARY;
      fd_info[phandles[0]].flags = flags;

      flags = FILE_PIPE | FILE_WRITE;
      if (!NILP (Vbinary_process_input))
	  flags |= FILE_BINARY;
      fd_info[phandles[1]].flags = flags;
    }

  return rc;
}

void
term_ntproc (int unused)
{
}

void
init_ntproc (void)
{
  /* Initial preparation for subprocess support: replace our standard
     handles with non-inheritable versions. */
  {
    HANDLE parent;
    HANDLE stdin_save =  INVALID_HANDLE_VALUE;
    HANDLE stdout_save = INVALID_HANDLE_VALUE;
    HANDLE stderr_save = INVALID_HANDLE_VALUE;

    parent = GetCurrentProcess ();

    /* ignore errors when duplicating and closing; typically the
       handles will be invalid when running as a gui program. */
    DuplicateHandle (parent, 
		     GetStdHandle (STD_INPUT_HANDLE), 
		     parent,
		     &stdin_save, 
		     0, 
		     FALSE, 
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_OUTPUT_HANDLE),
		     parent,
		     &stdout_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_ERROR_HANDLE),
		     parent,
		     &stderr_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    fclose (stdin);
    fclose (stdout);
    fclose (stderr);

    if (stdin_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdin_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_RDONLY);
    _fdopen (0, "r");

    if (stdout_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdout_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (1, "w");

    if (stderr_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stderr_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (2, "w");
  }

  /* unfortunately, atexit depends on implementation of malloc */
  /* atexit (term_ntproc); */
  signal (SIGABRT, term_ntproc);

  /* determine which drives are fixed, for GetCachedVolumeInformation */
  {
    /* GetDriveType must have trailing backslash. */
    char drive[] = "A:\\";

    /* Loop over all possible drive letters */
    while ( *drive <= 'Z' )
    {
      /* Record if this drive letter refers to a fixed drive. */
      fixed_drives[ DRIVE_INDEX (*drive) ] =
	(GetDriveType (drive) == DRIVE_FIXED);

      (*drive)++;
    }
  }
}
#ifndef HAVE_TTY
Lisp_Object
tty_semi_canonicalize_console_connection (Lisp_Object connection,
					  Error_behavior errb)
{
  return Vstdio_str;
}

Lisp_Object
tty_canonicalize_console_connection (Lisp_Object connection,
				     Error_behavior errb)
{
  return Vstdio_str;
}

Lisp_Object
tty_semi_canonicalize_device_connection (Lisp_Object connection,
					 Error_behavior errb)
{
  return Vstdio_str;
}

Lisp_Object
tty_canonicalize_device_connection (Lisp_Object connection,
				    Error_behavior errb)
{
  return Vstdio_str;
}
#endif

/*--------------------------------------------------------------------*/
/* Signal support                                                     */
/*--------------------------------------------------------------------*/

/* We need MS-defined signal and raise here */
#undef signal
#undef raise

#define sigmask(nsig) (1U << nsig)

/* We can support as many signals as fit into word */
#define SIG_MAX 32

/* Signal handlers. Initial value = 0 = SIG_DFL */
static void (__cdecl *signal_handlers[SIG_MAX])(int) = {0};

/* Signal block mask: bit set to 1 means blocked */
unsigned signal_block_mask = 0;

/* Signal pending mask: bit set to 1 means sig is pending */
unsigned signal_pending_mask = 0;

mswindows_sighandler
mswindows_sigset (int nsig, mswindows_sighandler handler)
{
  /* We delegate some signals to the system function */
  if (nsig == SIGFPE || nsig == SIGABRT || nsig == SIGINT)
    return signal (nsig, handler);

  if (nsig < 0 || nsig > SIG_MAX)
    {
      errno = EINVAL;
      return NULL;
    }

  /* Store handler ptr */
  {
    mswindows_sighandler old_handler = signal_handlers[nsig];
    signal_handlers[nsig] = handler;
    return old_handler;
  }
}
  
int
mswindows_sighold (int nsig)
{
  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  signal_block_mask |= sigmask (nsig);
  return 0;
}

int
mswindows_sigrelse (int nsig)
{
  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  signal_block_mask &= ~sigmask (nsig);

  if (signal_pending_mask & sigmask (nsig))
    mswindows_raise (nsig);

  return 0;
}

int
mswindows_sigpause (int nsig)
{
  /* This is currently not called, because the only call to sigpause
     inside XEmacs is with SIGCHLD parameter. Just in case, we put an
     assert here, so anyone adds a call to sigpause will be surprised
     (or surprise someone else...) */
  assert (0);
  return 0;
}

int
mswindows_raise (int nsig)
{
  /* We delegate some raises to the system routine */
  if (nsig == SIGFPE || nsig == SIGABRT || nsig == SIGINT)
    return raise (nsig);

  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  /* If the signal is blocked, remember to issue later */
  if (signal_block_mask & sigmask (nsig))
    {
      signal_pending_mask |= sigmask (nsig);
      return 0;
    }

  if (signal_handlers[nsig] == SIG_IGN)
    return 0;

  if (signal_handlers[nsig] != SIG_DFL)
    {
      (*signal_handlers[nsig]) (nsig);
      return 0;
    }

  /* Default signal actions */
  if (nsig == SIGALRM || nsig == SIGPROF)
    exit (3);

  /* Other signals are ignored by default */
  return 0;
}


/*--------------------------------------------------------------------*/
/*                        Memory-mapped files                         */
/*--------------------------------------------------------------------*/

int
open_input_file (file_data *p_file, const char *filename)
{
  /* Synched with FSF 20.6.  We fixed some warnings. */
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  DWORD size, upper_size;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_READONLY, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0) 
    return FALSE;

  p_file->name = (char *)filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = (char *)file_base;

  return TRUE;
}

int
open_output_file (file_data *p_file, const char *filename, unsigned long size)
{
  /* Synched with FSF 20.6.  We fixed some warnings. */
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;

  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  file_mapping = CreateFileMapping (file, NULL, PAGE_READWRITE, 
				    0, size, NULL);
  if (!file_mapping) 
    return FALSE;
  
  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == NULL) 
    return FALSE;
  
  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = (char*) file_base;

  return TRUE;
}

#if 1 /* !defined(MINGW) */
/* Return pointer to section header for section containing the given
   relative virtual address. */
static IMAGE_SECTION_HEADER *
rva_to_section (DWORD rva, IMAGE_NT_HEADERS * nt_header)
{
  /* Synched with FSF 20.6.  We added MINGW stuff. */
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      /* Some linkers (eg. the NT SDK linker I believe) swapped the
	 meaning of these two values - or rather, they ignored
	 VirtualSize entirely and always set it to zero.  This affects
	 some very old exes (eg. gzip dated Dec 1993).  Since
	 mswindows_executable_type relies on this function to work reliably,
	 we need to cope with this.  */
      DWORD real_size = max (section->SizeOfRawData,
			     section->Misc.VirtualSize);
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + real_size)
	return section;
      section++;
    }
  return NULL;
}
#endif

void
mswindows_executable_type (const char * filename, int * is_dos_app,
			   int * is_cygnus_app)
{
  /* Synched with FSF 20.6.  We added MINGW stuff and casts. */
  file_data executable;
  char * p;

  /* Default values in case we can't tell for sure.  */
  *is_dos_app = FALSE;
  *is_cygnus_app = FALSE;

  if (!open_input_file (&executable, filename))
    return;

  p = strrchr (filename, '.');

  /* We can only identify DOS .com programs from the extension. */
  if (p && stricmp (p, ".com") == 0)
    *is_dos_app = TRUE;
  else if (p && (stricmp (p, ".bat") == 0 ||
		 stricmp (p, ".cmd") == 0))
    {
      /* A DOS shell script - it appears that CreateProcess is happy to
	 accept this (somewhat surprisingly); presumably it looks at
	 COMSPEC to determine what executable to actually invoke.
	 Therefore, we have to do the same here as well. */
      /* Actually, I think it uses the program association for that
	 extension, which is defined in the registry.  */
      p = egetenv ("COMSPEC");
      if (p)
	mswindows_executable_type (p, is_dos_app, is_cygnus_app);
    }
  else
    {
      /* Look for DOS .exe signature - if found, we must also check that
	 it isn't really a 16- or 32-bit Windows exe, since both formats
	 start with a DOS program stub.  Note that 16-bit Windows
	 executables use the OS/2 1.x format. */

#if 0 /* defined( MINGW ) */
      /* mingw32 doesn't have enough headers to detect cygwin
	 apps, just do what we can. */
      FILHDR * exe_header;

      exe_header = (FILHDR*) executable.file_base;
      if (exe_header->e_magic != DOSMAGIC)
	goto unwind;

      if ((char*) exe_header->e_lfanew > (char*) executable.size)
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	} 
      else if (exe_header->nt_signature != NT_SIGNATURE)
	{
	  *is_dos_app = TRUE;
	}
#else
      IMAGE_DOS_HEADER * dos_header;
      IMAGE_NT_HEADERS * nt_header;

      dos_header = (PIMAGE_DOS_HEADER) executable.file_base;
      if (dos_header->e_magic != IMAGE_DOS_SIGNATURE)
	goto unwind;
	  
      nt_header = (PIMAGE_NT_HEADERS) ((char*) dos_header + dos_header->e_lfanew);
	  
      if ((char*) nt_header > (char*) dos_header + executable.size) 
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	} 
      else if (nt_header->Signature != IMAGE_NT_SIGNATURE &&
	       LOWORD (nt_header->Signature) != IMAGE_OS2_SIGNATURE)
	{
	  *is_dos_app = TRUE;
	}
      else if (nt_header->Signature == IMAGE_NT_SIGNATURE)
	{
	  /* Look for cygwin.dll in DLL import list. */
	  IMAGE_DATA_DIRECTORY import_dir =
	    nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
	  IMAGE_IMPORT_DESCRIPTOR * imports;
	  IMAGE_SECTION_HEADER * section;

	  section = rva_to_section (import_dir.VirtualAddress, nt_header);
	  imports = (IMAGE_IMPORT_DESCRIPTOR *) RVA_TO_PTR (import_dir.VirtualAddress,
							    section, executable);
	      
	  for ( ; imports->Name; imports++)
	    {
	      char *dllname = (char*) RVA_TO_PTR (imports->Name, section, executable);

	      /* The exact name of the cygwin dll has changed with
		 various releases, but hopefully this will be reasonably
		 future proof.  */
	      if (strncmp (dllname, "cygwin", 6) == 0)
		{
		  *is_cygnus_app = TRUE;
		  break;
		}
	    }
	}
#endif
    }

 unwind:
  close_file_data (&executable);
}

static void
convert_from_time_t (time_t time, FILETIME * pft)
{
  long double tmp;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp = (long double) time * 1e7 + utc_base;
  pft->dwHighDateTime = (DWORD) (tmp / (4096.0 * 1024 * 1024));
  pft->dwLowDateTime = (DWORD) (tmp - (4096.0 * 1024 * 1024) *
                                pft->dwHighDateTime);
}

int
mswindows_utime (Lisp_Object path, struct utimbuf *times)
{
  struct utimbuf deftime;
#if 0
  HANDLE fh;
#endif
  static FILETIME mtime;
  static FILETIME atime;
  Extbyte *filename;

  if (times == NULL)
    {
      deftime.modtime = deftime.actime = time (NULL);
      times = &deftime;
    }

  LISP_STRING_TO_EXTERNAL (path, filename, Qmswindows_tstr);
  /* APA: SetFileTime fails to set mtime correctly (always 1-Jan-1970) */
#if 0
  /* Need write access to set times.  */
  fh = CreateFile (filename, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
		   0, OPEN_EXISTING, 0, NULL);
  if (fh)
    {
      convert_from_time_t (times->actime, &atime);
      convert_from_time_t (times->modtime, &mtime);
      if (!SetFileTime (fh, NULL, &atime, &mtime))
	{
	  CloseHandle (fh);
	  errno = EACCES;
	  return -1;
	}
      CloseHandle (fh);
    }
  else
    {
      errno = EINVAL;
      return -1;
    }
  return 0;
#else
  return utime (filename, times);
#endif
}

/* Close the system structures associated with the given file.  */
void
close_file_data (file_data *p_file)
{
  UnmapViewOfFile (p_file->file_base);
  CloseHandle (p_file->file_mapping);
  CloseHandle (p_file->file);
}

void
vars_of_nt (void)
{
  DEFVAR_INT ("nt-fake-unix-uid", &nt_fake_unix_uid /*
*Set uid returned by `user-uid' and `user-real-uid'.
Under NT and 9x, there is no uids, and even no almighty user called root.
By setting this variable, you can have any uid of choice. Default is 0.
Changes to this variable take effect immediately.
*/ );
  nt_fake_unix_uid = 0;
}

/* end of nt.c */
