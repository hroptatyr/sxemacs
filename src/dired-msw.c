/* fast dired replacement routines for mswindows.
   Copyright (C) 1998 Darryl Okahata
   Portions Copyright (C) 1992, 1994 by Sebastian Kremer <sk@thp.uni-koeln.de>

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

/*
 * Parts of this code (& comments) were taken from ls-lisp.el
 * Author: Sebastian Kremer <sk@thp.uni-koeln.de>
 */

/*
 * insert-directory
 * - must insert _exactly_one_line_ describing FILE if WILDCARD and
 * FULL-DIRECTORY-P is nil.
 * The single line of output must display FILE's name as it was
 * given, namely, an absolute path name.
 * - must insert exactly one line for each file if WILDCARD or
 * FULL-DIRECTORY-P is t, plus one optional "total" line
 * before the file lines, plus optional text after the file lines.
 * Lines are delimited by "\n", so filenames containing "\n" are not
 * allowed.
 * File lines should display the basename.
 * - must be consistent with
 * - functions dired-move-to-filename, (these two define what a file line is)
 * dired-move-to-end-of-filename,
 * dired-between-files, (shortcut for (not (dired-move-to-filename)))
 * dired-insert-headerline
 * dired-after-subdir-garbage (defines what a "total" line is)
 * - variable dired-subdir-regexp
 */

/*
 * Insert directory listing for FILE, formatted according to SWITCHES.
 * Leaves point after the inserted text.
 * SWITCHES may be a string of options, or a list of strings.
 * Optional third arg WILDCARD means treat FILE as shell wildcard.
 * Optional fourth arg FULL-DIRECTORY-P means file is a directory and
 * switches do not contain `d', so that a full listing is expected.
 *
 * This works by running a directory listing program
 * whose name is in the variable `insert-directory-program'.
 * If WILDCARD, it also runs the shell specified by `shell-file-name'."
 */

/*
 * Set INDENT_LISTING to non-zero if the inserted text should be shifted
 * over by two spaces.
 */
#define INDENT_LISTING			0

#define ROUND_FILE_SIZES		4096


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "regex.h"

#include "sysdir.h"
#include "sysproc.h"
#include "sysfile.h"

#include <time.h>

#include <winsock.h>		/* To make nt.h happy */
#include "nt.h"		/* For prototypes */

#if ROUND_FILE_SIZES > 0
#include <math.h>		/* for floor() */
#endif


static int mswindows_ls_sort_case_insensitive;
static Fixnum mswindows_ls_round_file_size;

Lisp_Object		Qmswindows_insert_directory;

extern Lisp_Object	Vmswindows_downcase_file_names;	/* in device-msw.c */



enum mswindows_sortby {
  MSWINDOWS_SORT_BY_NAME,
  MSWINDOWS_SORT_BY_NAME_NOCASE,
  MSWINDOWS_SORT_BY_MOD_DATE,
  MSWINDOWS_SORT_BY_SIZE
};


static enum mswindows_sortby	mswindows_sort_method;
static int			mswindows_reverse_sort;


#define CMPDWORDS(t1a, t1b, t2a, t2b) \
(((t1a) == (t2a)) ? (((t1b) == (t2b)) ? 0 : (((t1b) < (t2b)) ? -1 : 1)) \
 : (((t1a) < (t2a)) ? -1 : 1))


static int
mswindows_ls_sort_fcn (const void *elem1, const void *elem2)
{
  WIN32_FIND_DATA		*e1, *e2;
  int				status;

  e1 = *(WIN32_FIND_DATA **)elem1;
  e2 = *(WIN32_FIND_DATA **)elem2;
  switch (mswindows_sort_method)
    {
    case MSWINDOWS_SORT_BY_NAME:
      status = strcmp(e1->cFileName, e2->cFileName);
      break;
    case MSWINDOWS_SORT_BY_NAME_NOCASE:
      status = _stricmp(e1->cFileName, e2->cFileName);
      break;
    case MSWINDOWS_SORT_BY_MOD_DATE:
      status = CMPDWORDS(e1->ftLastWriteTime.dwHighDateTime,
			 e1->ftLastWriteTime.dwLowDateTime,
			 e2->ftLastWriteTime.dwHighDateTime,
			 e2->ftLastWriteTime.dwLowDateTime);
      break;
    case MSWINDOWS_SORT_BY_SIZE:
      status = CMPDWORDS(e1->nFileSizeHigh, e1->nFileSizeLow,
			 e2->nFileSizeHigh, e2->nFileSizeLow);
      break;
    default:
      status = 0;
      break;
    }
  if (mswindows_reverse_sort)
    {
      status = -status;
    }
  return (status);
}


static void
mswindows_sort_files (WIN32_FIND_DATA **files, int nfiles,
		      enum mswindows_sortby sort_by, int reverse)
{
  mswindows_sort_method = sort_by;
  mswindows_reverse_sort = reverse;
  qsort(files, nfiles, sizeof(WIN32_FIND_DATA *), mswindows_ls_sort_fcn);
}


static WIN32_FIND_DATA *
mswindows_get_files (char *dirfile, int nowild, Lisp_Object pattern,
		     int hide_dot, int hide_system, int *nfiles)
{
  WIN32_FIND_DATA		*files;
  int				array_size;
  struct re_pattern_buffer	*bufp = NULL;
  int				findex, len;
  char				win32pattern[MAXNAMLEN+3];
  HANDLE			fh;
  int				errm;

  /*
   * Much of the following code and comments were taken from dired.c.
   * Yes, this is something of a waste, but we want speed, speed, SPEED.
   */
  files = NULL;
  array_size = *nfiles = 0;
  while (1)
    {
      if (!NILP(pattern))
	{
	  /* PATTERN might be a flawed regular expression.  Rather than
	     catching and signalling our own errors, we just call
	     compile_pattern to do the work for us.  */
	  bufp = compile_pattern (pattern, 0, Qnil, 0, ERROR_ME);
	}
      /* Now *bufp is the compiled form of PATTERN; don't call anything
	 which might compile a new regexp until we're done with the loop! */

      /* Initialize file info array */
      array_size = 100;		/* initial size */
      files = xmalloc(array_size * sizeof (WIN32_FIND_DATA));

      /* for Win32, we need to insure that the pathname ends with "\*". */
      strcpy (win32pattern, dirfile);
      if (!nowild)
	{
	  len = strlen (win32pattern) - 1;
	  if (!IS_DIRECTORY_SEP (win32pattern[len]))
	    strcat (win32pattern, "\\");
	  strcat (win32pattern, "*");
	}

      /*
       * Here, we use FindFirstFile()/FindNextFile() instead of opendir(),
       * xemacs_stat(), & friends, because xemacs_stat() is VERY expensive in
       * terms of time.  Hence, we take the time to write complicated
       * Win32-specific code, instead of simple Unix-style stuff.
       */
      findex = 0;
      fh = INVALID_HANDLE_VALUE;
      errm = SetErrorMode (SEM_FAILCRITICALERRORS
			   | SEM_NOOPENFILEERRORBOX);

      while (1)
	{
	  int		len;
	  char	*filename;
	  int		result;

	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      fh = FindFirstFile(win32pattern, &files[findex]);
	      if (fh == INVALID_HANDLE_VALUE)
		{
		  SetErrorMode (errm);
		  report_file_error ("Opening directory",
				     list1(build_string(dirfile)));
		}
	    }
	  else
	    {
	      if (!FindNextFile(fh, &files[findex]))
		{
		  if (GetLastError() == ERROR_NO_MORE_FILES)
		    {
		      break;
		    }
		  FindClose(fh);
		  SetErrorMode (errm);
		  report_file_error ("Reading directory",
				     list1(build_string(dirfile)));
		}
	    }

	  filename = files[findex].cFileName;
	  if (!NILP(Vmswindows_downcase_file_names))
	  {
	      strlwr(filename);
	  }
	  len = strlen(filename);
	  result = (NILP(pattern)
		    || (0 <= re_search (bufp, filename, 
					len, 0, len, 0)));
	  if (result)
	    {
	      if ( ! (filename[0] == '.' &&
		      ((hide_system && (filename[1] == '\0' ||
					(filename[1] == '.' &&
					 filename[2] == '\0'))) ||
		       hide_dot)))
		{
		  if (++findex >= array_size)
		    {
		      array_size = findex * 2;
		      files = xrealloc(files,
				       array_size * sizeof(WIN32_FIND_DATA));
		    }
		}
	    }
	}
      if (fh != INVALID_HANDLE_VALUE)
	{
	  FindClose (fh);
	}
      *nfiles = findex;
      break;
    }

  SetErrorMode (errm);
  return (files);
}


static void
mswindows_format_file (WIN32_FIND_DATA *file, char *buf, int display_size,
		       int add_newline)
{
  char			*cptr;
  int			len;
  Lisp_Object		luser;
  double		file_size;

  len = strlen(file->cFileName);
  file_size =
    file->nFileSizeHigh * (double)UINT_MAX + file->nFileSizeLow;
  cptr = buf;
#if INDENT_LISTING
  *cptr++ = ' ';
  *cptr++ = ' ';
#endif
  if (display_size)
    {
      sprintf(cptr, "%6d ", (int)((file_size + 1023.) / 1024.));
      cptr += 7;
    }
  if (file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      *cptr++ = 'd';
    } else {
      *cptr++ = '-';
    }
  cptr[0] = cptr[3] = cptr[6] = 'r';
  if (file->dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    {
      cptr[1] = cptr[4] = cptr[7] = '-';
    } else {
      cptr[1] = cptr[4] = cptr[7] = 'w';
    }
  if ((file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ||
      (len > 4 &&
       (_stricmp(&file->cFileName[len - 4], ".exe") == 0
	|| _stricmp(&file->cFileName[len - 4], ".com") == 0
	|| _stricmp(&file->cFileName[len - 4], ".bat") == 0
#if 0
	|| _stricmp(&file->cFileName[len - 4], ".pif") == 0
#endif
	)))
    {
      cptr[2] = cptr[5] = cptr[8] = 'x';
    } else {
      cptr[2] = cptr[5] = cptr[8] = '-';
    }
  cptr += 9;
  if (file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      strcpy(cptr, "   2 ");
    } else {
      strcpy(cptr, "   1 ");
    }
  cptr += 5;
  luser = Fuser_login_name(Qnil);
  if (!STRINGP(luser))
    {
      sprintf(cptr, "%-9d", 0);
    } else {
      char		*str;

      str = XSTRING_DATA(luser);
      sprintf(cptr, "%-8s ", str);
    }
  while (*cptr)
    {
      ++cptr;
    }
  sprintf(cptr, "%-8d ", getgid());
  cptr += 9;
  if (file_size > 99999999.0)
    {
      file_size = (file_size + 1023.0) / 1024.;
      if (file_size > 999999.0)
	{
	  sprintf(cptr, "%6.0fMB ", (file_size + 1023.0) / 1024.);
	} else {
	  sprintf(cptr, "%6.0fKB ", file_size);
	}
    } else {
      sprintf(cptr, "%8.0f ", file_size);
    }
  while (*cptr)
    {
      ++cptr;
    }
  {
    time_t		t, now;
    char		*ctimebuf;
    extern char		*sys_ctime(const time_t *t);	/* in nt.c */

    if (
#if 0
	/*
	 * This doesn't work.
	 * This code should be correct ...
	 */
	FileTimeToLocalFileTime(&file->ftLastWriteTime, &localtime) &&
	((t = convert_time(localtime)) != 0) &&
#else
	/*
	 * But this code "works" ...
	 */
	((t = convert_time(file->ftLastWriteTime)) != 0) &&
#endif
	((ctimebuf = sys_ctime(&t)) != NULL))
      {
	memcpy(cptr, &ctimebuf[4], 7);
	now = time(NULL);
	if (now - t > (365. / 2.0) * 86400.)
	  {
	    /* more than 6 months */
	    cptr[7] = ' ';
	    memcpy(&cptr[8], &ctimebuf[20], 4);
	  } else {
	    /* less than 6 months */
	    memcpy(&cptr[7], &ctimebuf[11], 5);
	  }
	cptr += 12;
	*cptr++ = ' ';
      }
  }
  if (add_newline)
    {
      sprintf(cptr, "%s\n", file->cFileName);
    }
  else
    {
      strcpy(cptr, file->cFileName);
    }
}


DEFUN ("mswindows-insert-directory", Fmswindows_insert_directory, 2, 4, 0, /*
Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.
*/
       (file, switches, wildcard, full_directory_p))
{
  Lisp_Object		result, handler, wildpat, fns, basename;
  char			*switchstr;
  int			nfiles, i;
  int			hide_system, hide_dot, reverse, display_size;
  WIN32_FIND_DATA	*files, **sorted_files;
  enum mswindows_sortby	sort_by;
  char			fmtbuf[MAXNAMLEN+100];	/* larger than necessary */
  struct gcpro		gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  result = Qnil;
  wildpat = Qnil;
  fns = Qnil;
  basename = Qnil;
  GCPRO5(result, file, wildpat, fns, basename);
  sorted_files = NULL;
  switchstr = NULL;
  hide_system = 1;
  hide_dot = 1;
  display_size = 0;
  reverse = 0;
  sort_by = (mswindows_ls_sort_case_insensitive
	     ? MSWINDOWS_SORT_BY_NAME_NOCASE
	     : MSWINDOWS_SORT_BY_NAME);
  nfiles = 0;
  while (1)
    {
      handler = Ffind_file_name_handler (file, Qmswindows_insert_directory);
      if (!NILP(handler))
	{
	  result = call5(handler, Qmswindows_insert_directory, file, switches,
			 wildcard, full_directory_p);
	  break;
	}
      CHECK_STRING (file);
      if (!NILP(switches))
	{
	  char	*cptr;

	  CHECK_STRING (switches);
	  switchstr = XSTRING_DATA(switches);
	  for (cptr = switchstr; *cptr; ++cptr)
	    {
	      switch (*cptr)
		{
		case 'A':
		  hide_dot = 0;
		  break;
		case 'a':
		  hide_system = 0;
		  hide_dot = 0;
		  break;
		case 'r':
		  reverse = 1;
		  break;
		case 's':
		  display_size = 1;
		  break;
		case 'S':
		  sort_by = MSWINDOWS_SORT_BY_SIZE;
		  break;
		case 't':
		  sort_by = MSWINDOWS_SORT_BY_MOD_DATE;
		  break;
		}
	    }
	}

      if (!NILP(wildcard))
	{
	  Lisp_Object	newfile;

	  file = Fdirectory_file_name (file);
	  basename = Ffile_name_nondirectory(file);
	  fns = intern("wildcard-to-regexp");
	  wildpat = call1(fns, basename);
	  newfile = Ffile_name_directory(file);
	  if (NILP(newfile))
	    {
	      /* Ffile_name_directory() can GC */
	      newfile = Ffile_name_directory(Fexpand_file_name(file, Qnil));
	    }
	  file = newfile;
	}
      if (!NILP(wildcard) || !NILP(full_directory_p))
	{
	  CHECK_STRING(file);
	  if (!NILP(wildpat))
	    {
	      CHECK_STRING(wildpat);
	    }

	  files = mswindows_get_files(XSTRING_DATA(file), FALSE, wildpat,
				      hide_dot, hide_system, &nfiles);
	  if (files == NULL || nfiles == 0)
	    {
	      break;
	    }
	}
      else
	{
	  files = mswindows_get_files(XSTRING_DATA(file), TRUE, wildpat,
				      hide_dot, hide_system, &nfiles);
	}
      if ((sorted_files = xmalloc(nfiles * sizeof(WIN32_FIND_DATA *)))
	  == NULL)
	{
	  break;
	}
      for (i = 0; i < nfiles; ++i)
	{
	  sorted_files[i] = &files[i];
	}
      if (nfiles > 1)
	{
	  mswindows_sort_files(sorted_files, nfiles, sort_by, reverse);
	}
      if (!NILP(wildcard) || !NILP(full_directory_p))
	{
	  /*
	   * By using doubles, we can handle files up to 2^53 bytes in
	   * size (IEEE doubles have 53 bits of resolution).  However,
	   * as we divide by 1024 (or 2^10), the total size is
	   * accurate up to 2^(53+10) --> 2^63 bytes.
	   *
	   * Hopefully, we won't have to handle these file sizes anytime
	   * soon.
	   */
	  double		total_size, file_size, block_size;

	  if ((block_size = mswindows_ls_round_file_size) <= 0)
	  {
	      block_size = 0;
	  }
	  total_size = 0;
	  for (i = 0; i < nfiles; ++i)
	    {
	      file_size =
		sorted_files[i]->nFileSizeHigh * (double)UINT_MAX +
		sorted_files[i]->nFileSizeLow;
	      if (block_size > 0)
	      {
		  /*
		   * Round file_size up to the next nearest block size.
		   */
		  file_size =
		      floor((file_size + block_size - 1) / block_size)
		      * block_size;
	      }
	      /* Here, we round to the nearest 1K */
	      total_size += floor((file_size + 512.) / 1024.);
	    }
	  sprintf(fmtbuf,
#if INDENT_LISTING
		  /* ANSI C compilers auto-concatenate adjacent strings */
		  "  "
#endif
		  "total %.0f\n", total_size);
	  buffer_insert1(current_buffer, build_string(fmtbuf));
	}
      for (i = 0; i < nfiles; ++i)
	{
	  mswindows_format_file(sorted_files[i], fmtbuf, display_size, TRUE);
	  buffer_insert1(current_buffer, build_string(fmtbuf));
	}
      break;
    }
  if (sorted_files)
    {
      xfree(sorted_files);
    }
  UNGCPRO;
  return (result);
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_dired_mswindows (void)
{
  defsymbol (&Qmswindows_insert_directory, "mswindows-insert-directory");

  DEFSUBR (Fmswindows_insert_directory);
}


void
vars_of_dired_mswindows (void)
{
  DEFVAR_BOOL ("mswindows-ls-sort-case-insensitive", &mswindows_ls_sort_case_insensitive /*
*Non-nil means filenames are sorted in a case-insensitive fashion.
Nil means filenames are sorted in a case-sensitive fashion, just like Unix.
*/ );
  mswindows_ls_sort_case_insensitive = 1;

  DEFVAR_INT ("mswindows-ls-round-file-size", &mswindows_ls_round_file_size /*
*If non-zero, file sizes are rounded in terms of this block size when
the file totals are being calculated.  This is useful for getting a more
accurate estimate of allocated disk space.  Note that this only affects
the total size calculation; the individual displayed file sizes are not
changed.  This block size should also be a power of 2 (but this is not
enforced), as filesystem block (cluster) sizes are typically powers-of-2.
*/ );
  /*
   * Here, we choose 4096 because it's the cluster size for both FAT32
   * and NTFS (?).  This is probably much too small for people using
   * plain FAT, but, hopefully, plain FAT will go away someday.
   *
   * We should allow something like a alist here, to make the size
   * dependent on the drive letter, etc..
   */
  mswindows_ls_round_file_size = 4096;
}
