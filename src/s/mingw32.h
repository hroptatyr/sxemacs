/* system description file for mingw32.
   Copyright (C) 1993, 1994, 1995, 1999 Free Software Foundation, Inc.

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
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* based on cygwin32.h by Andy Piper <andy@xemacs.org> */

/* Identify ourselves */
#ifndef WIN32_NATIVE
#define WIN32_NATIVE
#endif

#define MINGW

#ifndef ORDINARY_LINK
#define ORDINARY_LINK
#endif

#define C_SWITCH_SYSTEM "-mno-cygwin -Wno-sign-compare -fno-caller-saves -DWIN32_NATIVE"
#define LIBS_SYSTEM "-mno-cygwin -mwindows -lwinmm -lwsock32"
#define WIN32_LEAN_AND_MEAN

#define TEXT_START -1
#define TEXT_END -1
#define DATA_END -1
#define HEAP_IN_DATA
#define UNEXEC "unexcw.o"

#define TIME_ONESHOT 0
#define TIME_PERIODIC 1
#define LOCALE_USE_CP_ACP 0x40000000
#define NSIG 23

/* this is necessary to get the TCS_* definitions in <commctrl.h> */
#define _WIN32_IE 0x0400

/* translate NT world unexec stuff to our a.out definitions */

#define strnicmp strncasecmp
/* #ifndef HAVE_SOCKETS */
#define HAVE_SOCKETS
/* #endif */
#define OBJECTS_SYSTEM	ntplay.o nt.o ntheap.o ntproc.o dired-msw.o

#undef MAIL_USE_SYSTEM_LOCK
#define HAVE_MSW_C_DIRED

/* System calls that are encapsulated */
#define ENCAPSULATE_RENAME
#define ENCAPSULATE_OPEN
#define ENCAPSULATE_FOPEN
#define ENCAPSULATE_MKDIR
#define ENCAPSULATE_STAT
#define ENCAPSULATE_FSTAT

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no load average available. */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* Text does precede data space, but this is never a safe assumption.  */
#define VIRT_ADDR_VARIES

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here */
#define _VARARGS_ __cdecl

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal, or main) its calling
   convention must be whatever standard the libraries expect */
#define _CALLBACK_ __cdecl

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "windows-nt"

#define NO_MATHERR

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
#define SEPCHAR ';'

/* ============================================================ */

/* Here, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Define this to be the separator between devices and paths */
#define DEVICE_SEP ':'

#define DIRECTORY_SEP ((char)XCHAR(Vdirectory_sep_char))

/* The null device on Windows NT. */
#define NULL_DEVICE     "NUL:"
#define EXEC_SUFFIXES   ".exe:.com:.bat:.cmd:"
/* We'll support either convention on NT.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))
#define EXEC_SUFFIXES   ".exe:.com:.bat:.cmd:"

/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 15000

#ifndef NOT_C_CODE
#include <stdlib.h>
#include <../mingw/process.h>
#define mkdir __mkdir
#include <dir.h>
#undef mkdir

/* IO calls that are emulated or shadowed */
#define pipe    sys_pipe
int sys_pipe (int * phandles);

#ifndef HAVE_X_WINDOWS
#define sleep   sys_sleep
void sleep (int seconds);
#endif

/* subprocess calls that are emulated */
#define spawnve sys_spawnve
int spawnve (int mode, const char *cmdname, 
	     const char * const *argv, const char *const *envp);

#define wait    sys_wait
int wait (int *status);

#define kill    sys_kill
int kill (int pid, int sig);

/* map to MSVC names */
#define popen     _popen
#define pclose    _pclose

/* Encapsulation of system calls */
#ifndef DONT_ENCAPSULATE
#define getpid sys_getpid
pid_t getpid (void);
#endif

#define DONT_USE_LITOUT

/* Random global functions called everywhere. Implemented in nt.c */
/* #### Most of these are FSFisms and must be avoided */
/* #### All of these are FSFisms and must be avoided */
void dostounix_filename (char *p);
void unixtodos_filename (char *p);
int crlf_to_lf (int n, unsigned char *buf, unsigned int *lf_count);

char *getwd (char *dir);

void *sbrk (unsigned long increment);

struct passwd;
struct passwd *getpwuid (uid_t uid);
struct passwd *getpwnam (const char *name);
uid_t getuid (void);
uid_t geteuid (void);
gid_t getgid (void);
gid_t getegid (void);

/* Stuff that gets set wrongly or otherwise */
#define HAVE_SETITIMER
#define HAVE_GETTIMEOFDAY
#define HAVE_SELECT
/* systime.h includes winsock.h which defines timeval */
#define HAVE_TIMEVAL
#define HAVE_GETPAGESIZE
#define getpagesize() 4096
#ifndef HAVE_H_ERRNO
#define HAVE_H_ERRNO
#endif
#ifndef HAVE_TZNAME
#define HAVE_TZNAME
#endif

#undef GETTIMEOFDAY_ONE_ARGUMENT
#undef HAVE_SYS_WAIT_H
#undef HAVE_TERMIOS
#undef SYSV_SYSTEM_DIR
#undef CLASH_DETECTION

/* We now have emulation for some signals */
#define HAVE_SIGHOLD
#define sigset(s,h) mswindows_sigset(s,h)
#define sighold(s) mswindows_sighold(s)
#define sigrelse(s) mswindows_sigrelse(s)
#define sigpause(s) mswindows_sigpause(s)
#define signal sigset

/* Defines that we need that aren't in the standard signal.h  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGKILL 9               /* Die, die die */
#define SIGALRM 14              /* Alarm */
#define SIGPROF 29		/* Profiling timer exp */

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif
#endif /* !NOT_C_CODE */

/* Define for those source files that do not include enough NT 
   system files.  */
#ifndef NULL
#ifdef __cplusplus
#define NULL	0
#else
#define NULL	((void *)0)
#endif
#endif

/* Define process implementation */
#define HAVE_WIN32_PROCESSES

#define CORRECT_DIR_SEPS(s) \
  do { if ('/' == DIRECTORY_SEP) dostounix_filename (s); \
       else unixtodos_filename (s); \
  } while (0)
