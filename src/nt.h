/* Support routines for the NT version of XEmacs.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au> */
/* Sync'ed with Emacs 19.34.6 by Marc Paquette <marcpa@cam.org> */

/* #define FULL_DEBUG */

#ifndef INCLUDED_nt_h_
#define INCLUDED_nt_h_

#include "syswindows.h"
#include "systime.h" /* because of struct utimbuf below */

#ifdef DEBUG_XEMACS
#define DebPrint(stuff) _DebPrint stuff
#else
#define DebPrint(stuff)
#endif

/* ------------------------------------------------------------------------- */

/* child_process.status values */
enum {
  STATUS_READ_ERROR = -1,
  STATUS_READ_READY,
  STATUS_READ_IN_PROGRESS,
  STATUS_READ_FAILED,
  STATUS_READ_SUCCEEDED,
  STATUS_READ_ACKNOWLEDGED
};

/* This structure is used for both pipes and sockets; for
   a socket, the process handle in pi is NULL. */
typedef struct _child_process
{
  int                   fd;
  int                   pid;
  HANDLE                char_avail;
  HANDLE                char_consumed;
  HANDLE                thrd;
  HWND                  hwnd;
  PROCESS_INFORMATION   procinfo;
  volatile int          status;
  char                  chr;
} child_process;

#define MAX_CHILDREN  MAXDESC/2
#define CHILD_ACTIVE(cp) ((cp)->char_avail != NULL)

/* parallel array of private info on file handles */
typedef struct
{
  unsigned         flags;
  HANDLE           hnd;
  child_process *  cp;
} filedesc;

extern filedesc fd_info [];

/* fd_info flag definitions */
#define FILE_READ    0x0001
#define FILE_WRITE   0x0002
#define FILE_BINARY  0x0010
#define FILE_LAST_CR            0x0020
#define FILE_AT_EOF             0x0040
#define FILE_SEND_SIGCHLD       0x0080
#define FILE_PIPE    0x0100
#define FILE_SOCKET  0x0200

extern child_process * new_child (void);
extern void delete_child (child_process *cp);

/* ------------------------------------------------------------------------- */

/* Get long (aka "true") form of file name, if it exists.  */
extern BOOL win32_get_long_filename (char * name, char * buf, int size);

/* Prepare our standard handles for proper inheritance by child processes.  */
extern void prepare_standard_handles (int in, int out, 
				      int err, HANDLE handles[4]);

/* Reset our standard handles to their original state.  */
extern void reset_standard_handles (int in, int out, 
				    int err, HANDLE handles[4]);

/* Return the string resource associated with KEY of type TYPE.  */
extern LPBYTE nt_get_resource (char * key, LPDWORD type);

void set_process_dir (const char * dir);
time_t convert_time (FILETIME ft);
int mswindows_utime (Lisp_Object path, struct utimbuf *times);

extern void init_ntproc (void);
extern void term_ntproc (int unused);

/* ----------------------------------------------------------------- */
/* Useful routines for manipulating memory-mapped files. */

typedef struct file_data
{
  const char    *name;
  unsigned long  size;
  HANDLE         file;
  HANDLE         file_mapping;
  char *file_base;
} file_data;

#define OFFSET_TO_RVA(var,section) \
	  (section->VirtualAddress + ((DWORD)(var) - section->PointerToRawData))

#define RVA_TO_OFFSET(var,section) \
	  (section->PointerToRawData + ((DWORD)(var) - section->VirtualAddress))

#define RVA_TO_PTR(var,section,filedata) \
	  ((void *)(RVA_TO_OFFSET(var,section) + (filedata).file_base))

int open_input_file (file_data *p_file, const char *name);
int open_output_file (file_data *p_file, const char *name, unsigned long size);
void close_file_data (file_data *p_file);
void mswindows_executable_type (const char * filename, int * is_dos_app,
				int * is_cygnus_app);

/* In process-nt.c */
extern int compare_env (const void *strp1, const void *strp2);

void mswindows_set_errno (unsigned long win32_error);
void mswindows_set_last_errno (void);

void wait_for_termination (HANDLE pid);

int mswindows_fstat (int handle, struct stat *buffer);
int mswindows_stat (const char * path, struct stat * buf);

#endif /* INCLUDED_nt_h_ */
