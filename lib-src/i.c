/* I-connector utility
   Copyright (C) 2000 Kirill M. Katsnelson

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

/* When run with an argument, i treats it as a command line, and pipes
command stdin, stdout and stderr to its own respective streams. How
silly it should sound, but windowed program in Win32 cannot do output
to the console from which it has been started, and should be run using
this utility.

This utility is for running [tx]emacs as part of make process so that
its output goes to the same console as the rest of the make output
does.  It can be used also when xemacs should be run as a batch
command ina script, especially when its standart output should be
obtained programmatically. */

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <tchar.h>

typedef struct
{
  HANDLE source;
  HANDLE drain;
} I_connector;

/* 
 * Make new handle as that pointed to by PH but
 * inheritable, substitute PH with it, and close the
 * original one
 */
static void
make_inheritable (HANDLE* ph)
{
  HANDLE htmp;
  DuplicateHandle (GetCurrentProcess(), *ph, GetCurrentProcess(), &htmp,
		   0, TRUE, DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
  *ph = htmp;
}

/*
 * Worker thread proc. Reads source, pumps into drain,
 * till either clogs.
 */
static DWORD CALLBACK
pump (LPVOID pv_i)
{
  I_connector* pi = (I_connector*) pv_i;
  BYTE buffer [256];
  DWORD really_read, unused;

  while (ReadFile (pi->source, buffer, sizeof (buffer), &really_read, NULL) &&
	 WriteFile (pi->drain, buffer, really_read, &unused, NULL))
    ;

  return 0;
}

/*
 * Launch a pump for the given I-connector
 */
static void
start_pump (I_connector* pi)
{
  DWORD unused;
  HANDLE h_thread = CreateThread (NULL, 0, pump, (void*)pi, 0, &unused);
  CloseHandle (h_thread);
}

/*
 * Get command line, skip over the executable name, return the rest.
 */
static LPTSTR
get_command (void)
{
  LPTSTR cl = GetCommandLine ();
  int ix;

  while (1)
    {
      ix = _tcscspn (cl, _T(" \t\""));
      if (cl[ix] == '\"')
	{
	  cl = _tcschr (cl + ix + 1, '\"');
	  if (cl == NULL)
	    return NULL; /* Unmatched quote */
	  cl++;
	}
      else
	{
	  cl += ix;
	  cl += _tcsspn (cl, _T(" \t"));
	  return *cl ? cl : NULL;
	}
    }
}

/*
 * Brew coffee and bring snickers
 */
void
usage (void)
{
  fprintf (stderr,
   "\n"
   "usage: i command\n"
   "i executes the command and reroutes its standard handles to the calling\n"
   "console.  Good for seeing output of GUI programs that use standard output."
   "\n");
}

int
main (void)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  I_connector I_in, I_out, I_err;
  DWORD exit_code;

  LPTSTR command = get_command ();
  if (command == NULL)
    {
      usage ();
      return 1;
    }

  ZeroMemory (&si, sizeof (si));
  si.dwFlags = STARTF_USESTDHANDLES;

  I_in.source = GetStdHandle (STD_INPUT_HANDLE);
  CreatePipe (&si.hStdInput, &I_in.drain, NULL, 0);
  make_inheritable (&si.hStdInput);

  I_out.drain = GetStdHandle (STD_OUTPUT_HANDLE);
  CreatePipe (&I_out.source, &si.hStdOutput, NULL, 0);
  make_inheritable (&si.hStdOutput);

  I_err.drain = GetStdHandle (STD_ERROR_HANDLE);
  CreatePipe (&I_err.source, &si.hStdError, NULL, 0);
  make_inheritable (&si.hStdError);

  if (CreateProcess (NULL, command, NULL, NULL, TRUE, 0,
		     NULL, NULL, &si, &pi) == 0)
    {
      _ftprintf (stderr, _T("Error %d launching `%s'\n"),
		 GetLastError (), command);
      return 2;
    }

  CloseHandle (pi.hThread);

  /* Start pump in each I-connector */
  start_pump (&I_in);
  start_pump (&I_out);
  start_pump (&I_err);

  /* Wait for the process to complete */
  WaitForSingleObject (pi.hProcess, INFINITE);
  GetExitCodeProcess (pi.hProcess, &exit_code);
  CloseHandle (pi.hProcess);

  /* Make pump threads eventually die out. Looks rude, I agree */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  return exit_code;
}
