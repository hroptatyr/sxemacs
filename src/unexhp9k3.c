/*
 * Code to do an unexec for HPUX 8.0 on an HP9000/[34]00 for a
 * dynamically linked temacs.

   Copyright (C) 1992-1993 Free Software Foundation, Inc.

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
Created 29-Oct-92 by Harlan Sexton for SunOS

Modified Jan 93 by Hamish Macdonald for HPUX
 */

/********************** Included .h Files **************************/

#include <config.h>

#include <stdarg.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#ifdef __hp9000s300
# include </usr/include/debug.h>
#endif
#include <a.out.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/dir.h>

#include "sysdep.h"

/* XEmacs: Richard Cognot <cognot@ensg.u-nancy.fr> says we need these */
extern void perror(const char*);
extern int sys_nerr;
extern char *sys_errlist[];
extern char *strerror (int);


/********************** Macros *************************************/

#define SYS_ERR \
 ((errno > 0)?((errno < sys_nerr)?(sys_errlist[errno]):\
               "unknown system error"): "unknown error")

#define MASK_UP(x,p_of_two) \
 ((((unsigned long) (x)) + ((p_of_two) - 1)) & (~((p_of_two) - 1)))

#define MASK_DOWN(x,p_of_two) (((unsigned long) (x)) & (~((p_of_two) - 1)))

/********************** Function Prototypes/Declarations ***********/

static void unexec_error (const char *fmt, int use_errno, ...);
static int unexec_open (char *filename, int flag, int mode);
static long unexec_seek (int fd, long position);
static void unexec_read (int fd, long position, char *buf, int bytes);
static void unexec_write (int fd, long position, char *buf, int bytes);
static void unexec_copy (int new_fd, int old_fd, long old_pos, long new_pos,
                         int bytes);
static void unexec_pad (int fd, int bytes);
static void unexec_fstat (int fd, struct stat *statptr);
static void unexec_fchmod (int fd, int mode);
int run_time_remap (char *dummy);

/********************** Variables **********************************/

/* for reporting error messages from system calls */
extern int sys_nerr;
extern int _DYNAMIC;
extern char **environ;             

static unsigned long sbrk_of_0_at_unexec;
             
/*******************************************************************/

static void
unexec_error (const char *fmt, int use_errno, ...)
{
  const char *err_msg = SYS_ERR;
  va_list args;

  fprintf (stderr, "unexec - ");
  va_start (args, use_errno);
  vfprintf (stderr, fmt, args);
  va_end (args);

  if (use_errno)
      fprintf (stderr, ": %s", err_msg);
  fprintf (stderr, "\n");
  exit (1);
  return;
}

static int
unexec_open (char *filename, int flag, int mode)
{
  int fd;

  errno = 0;

  fd = open (filename, flag, mode);

  if (fd < 0)
    {
      unexec_error ("Failure opening file %s", 1, (void *) filename, 0, 0);
      return -1;
    }
  else
    return fd;
}

static long
unexec_seek (int fd, long position)
{
  long seek_value;

  if (fd <= 0)
    unexec_error ("No file open in which to seek", 0, 0, 0, 0);

  errno = 0;

  if (position < 0)
    seek_value = (long) lseek (fd, 0, L_INCR);
  else
    seek_value = (long) lseek (fd, position, L_SET);

  if (seek_value < 0)
    unexec_error ("Failed to do a seek to 0x%x in %s", 1,
                  (char *) position, "unexec() output file", 0);

  return seek_value;
}

static void
unexec_read (int fd, long position, char *buf, int bytes)
{
  int n_read;
  int remains = bytes;
  position = unexec_seek (fd, position);

  if (bytes < 0)
    unexec_error ("Attempted read of %d bytes", 0, (char *) bytes, 0, 0);

  errno = 0;

  while (remains > 0)
    {
      n_read = read (fd, buf, remains);
      if (n_read <= 0)
        unexec_error ("Read failed for 0x%x bytes at offset 0x%x in %s",
                      1, (char *) bytes, (char *) position,
                      "unexec() output file");
      buf += n_read;
      remains -= n_read;
    }

  return;
}

static void
unexec_write (int fd, long position, char *buf, int bytes)
{
  int n_written;
  int remains = bytes;
  position = unexec_seek (fd, position);

  if (bytes < 0)
    unexec_error ("Attempted write of %d bytes in %s",
                  0, (char *) bytes, "unexec() output file", 0);

  errno = 0;

  while (remains > 0)
    {
      n_written = write (fd, buf, remains);
      if (n_written <= 0)
        unexec_error ("Write failed for 0x%x bytes at offset 0x%x in %s",
                      1, (char *) bytes, (char *) position,
                      "unexec() output file");
      buf += n_written;
      remains -= n_written;
    }

  return;
}

static void
unexec_copy (int new_fd, int old_fd, long old_pos, long new_pos, int bytes)
{
    int remains = bytes;        
    char buf[128];

    while (remains > 0)
      {
          int n_to_copy = remains > sizeof(buf) ? sizeof(buf) : remains;

          unexec_read (old_fd, old_pos, buf, n_to_copy);
          unexec_write (new_fd, new_pos, buf, n_to_copy);

          old_pos += n_to_copy;
          new_pos += n_to_copy;
          remains -= n_to_copy;
      }

    return;
}

static void 
unexec_pad (int fd, int bytes)
{
  if (bytes > 0)
    {
      char buf[1024];
      int remaining = bytes;

      memset (buf, 0, sizeof(buf));
  
      while (remaining > 0)
        {
          int this_write = (remaining > sizeof(buf))?sizeof(buf):remaining;
          unexec_write (fd, -1, buf, this_write);
          remaining -= this_write;
        }
    }
}

static void
unexec_fstat (int fd, struct stat *statptr)
{
  errno = 0;
  if (-1 == fstat (fd, statptr))
    unexec_error ("fstat() failed for descriptor %d", 1, (char *) fd, 0, 0);
  return;
}

static void
unexec_fchmod (int fd, int mode)
{
  errno = 0;
  if (-1 == fchmod (fd, mode))
    unexec_error ("fchmod() failed for descriptor %d", 1, (char *) fd, 0, 0);
  return;
}

/*
 * EXPORTED FUNCTIONS 
 */

/* this has to be a global variable to prevent the optimizers from
 * assuming that it can not be 0.  
*/
static void *dynamic_addr = (void *) &_DYNAMIC;

int
unexec (char *new_name, char *old_name,
        unsigned int emacs_edata, unsigned int dummy1, unsigned int dummy2)
{
  /* /dld.sl data */
  struct dynamic *ld = 0;
  /* old and new state */
  int old_fd;
  int new_fd;
  struct exec old_hdr;
  struct exec new_hdr;
  struct stat old_buf;
  /* some process specific "constants" */
  unsigned long n_pagsiz;
  caddr_t dynamic_beg;
  caddr_t current_break = (caddr_t) sbrk (0);

  /* dynamically linked image? -- if so, find dld.sl structures */
  if (dynamic_addr)
    {
      ld = (struct dynamic *) dynamic_addr;
#ifdef DEBUG
      printf ("dl_text = %#x\n", ld->text);
      printf ("dl_data = %#x\n", ld->data);
      printf ("dl_bss = %#x\n", ld->bss);
      printf ("dl_end = %#x\n", ld->end);
      printf ("dl_dmodule = %#x\n", ld->dmodule);
      printf ("dl_dlt = %#x\n", ld->dlt);
      printf ("dl_plt = %#x\n", ld->plt);
#endif
    }

  /* open the old and new files, figuring out how big the old one is
     so that we can map it in */
  old_fd = unexec_open (old_name, O_RDONLY, 0);
  new_fd = unexec_open (new_name, O_RDWR | O_CREAT | O_TRUNC, 0666);

  /* setup the header and the statbuf for old_fd */
  unexec_read (old_fd, 0, (char *) &old_hdr, sizeof (old_hdr));
  unexec_fstat (old_fd, &old_buf);

  /* set up some important constants */
  n_pagsiz = EXEC_PAGESIZE;

  /* setup beginning of data to copy from executable */
  if (ld)
      dynamic_beg = ld->dmodule;
  else
      dynamic_beg = (caddr_t)EXEC_ALIGN (old_hdr.a_text) + old_hdr.a_data;

  /* set up the new exec */
  new_hdr = old_hdr;
  new_hdr.a_text = MASK_DOWN (emacs_edata, n_pagsiz);
  new_hdr.a_data = MASK_UP (current_break, n_pagsiz)
      - EXEC_ALIGN(new_hdr.a_text);
  new_hdr.a_bss  = 0;

#ifdef DEBUG
  printf ("old text %#x\n", old_hdr.a_text);
  printf ("new text %#x\n", new_hdr.a_text);
  printf ("old data %#x\n", old_hdr.a_data);
  printf ("new data %#x\n", new_hdr.a_data);
  printf ("old bss %#x\n", old_hdr.a_bss);
  printf ("new bss %#x\n", new_hdr.a_bss);
#endif

  /* set up this variable, in case we want to reset "the break" 
     when restarting */
  sbrk_of_0_at_unexec = ((unsigned long) MASK_UP (current_break, n_pagsiz));
     
  /* Write out the first approximation to the new file. The sizes of
     each section will be correct, but there will be a number of 
     corrections that will need to be made. */
  {
    long old_datoff = DATA_OFFSET (old_hdr);
    long new_datoff = DATA_OFFSET (new_hdr);
    long old_dataddr = EXEC_ALIGN (old_hdr.a_text);
    long new_dataddr = EXEC_ALIGN (new_hdr.a_text);
    long new_mcaloff = MODCAL_OFFSET (new_hdr);
    long old_mcaloff = MODCAL_OFFSET (old_hdr);
    long newtext_size = new_hdr.a_text - old_dataddr;
    long newdata1_size = (unsigned long)dynamic_beg - new_dataddr;
    long dyn_size = (EXEC_ALIGN (old_hdr.a_text) + old_hdr.a_data)
        - (unsigned long)dynamic_beg;
    long newdata2_size = (unsigned long)current_break
        - ((unsigned long)dynamic_beg + dyn_size);
    long pad_size = 
      MASK_UP (current_break, n_pagsiz) - ((unsigned long) current_break);

#ifdef DEBUG
    printf ("current break is %#lx\n", current_break);

    printf ("old_dataddr = %#lx, dynamic_beg = %#lx\n",
            old_dataddr, dynamic_beg);
#endif

    /*
     * First, write the text segment with new header -- copy
     * everything until the start of the data segment from the old
     * file
     */
#ifdef DEBUG
    printf ("copying %#lx bytes of text from 0\n", old_datoff);
#endif
    unexec_copy (new_fd, old_fd, 0, 0, old_datoff);
    /* pad out the text segment */
#ifdef DEBUG
    printf ( "text pad size is %#x\n", old_dataddr - old_hdr.a_text);
#endif
    unexec_pad (new_fd, old_dataddr - old_hdr.a_text);

    /*
     * Update debug header spoo
     */
    if (new_hdr.a_extension > 0)
    {
	new_hdr.a_extension += LESYM_OFFSET(new_hdr) - LESYM_OFFSET(old_hdr);
    }

    /*
     * go back and write the new header.
     */
    unexec_write (new_fd, 0, (char *) &new_hdr, sizeof (new_hdr));

    
    /*
     * Copy the part of the data segment which becomes text from the
     * running image.
     */
#ifdef DEBUG
    printf ("copying %#lx bytes of new text from %#lx to position %#lx\n",
            newtext_size, old_dataddr, TEXT_OFFSET(new_hdr) + old_dataddr);
#endif
    unexec_write (new_fd, TEXT_OFFSET(new_hdr) + old_dataddr,
                  (caddr_t)old_dataddr, newtext_size);

#ifdef DEBUG
    printf ("new DATA_OFFSET is %#lx\n", new_datoff);
#endif

    /*
     * Copy the part of the old data segment which will be data
     * in the new executable (before the dynamic stuff)
     * from the running image.
     */
#ifdef DEBUG
    printf ("copying %#lx bytes of data from %#lx to position %#lx\n",
            newdata1_size, new_dataddr, new_datoff);
#endif
    unexec_write (new_fd, new_datoff, (caddr_t)new_dataddr, newdata1_size);

    /* copy the dynamic part of the data segment from the old executable */
    if (dyn_size)
      {
#ifdef DEBUG
        printf ("copying %#lx bytes of dyn data from executable"
                " at address %#lx to position %#lx\n", 
                dyn_size, dynamic_beg, new_datoff + newdata1_size);
#endif
        unexec_copy (new_fd, old_fd, old_datoff + newtext_size + newdata1_size,
                     new_datoff + newdata1_size, dyn_size);
      }

    /* copy remaining data (old bss) from the running image */
#ifdef DEBUG
    printf ("copying %#lx bytes of data from %#lx to position %#lx\n",
            newdata2_size, new_dataddr + newdata1_size + dyn_size,
            new_datoff + newdata1_size + dyn_size);
#endif
    unexec_write (new_fd, new_datoff + newdata1_size + dyn_size,
                  (caddr_t)(new_dataddr + newdata1_size + dyn_size),
                  newdata2_size);

    /* pad out the data segment */
#ifdef DEBUG
    printf ( "pad size is %#x\n", pad_size);
#endif
    unexec_pad (new_fd, pad_size);
    
    /* Finally, copy the rest of the junk from the old file. */
#ifdef DEBUG
    printf ("Copying %#lx bytes of junk from %#lx (old) to %#lx (new)\n",
            old_buf.st_size - old_mcaloff, old_mcaloff, new_mcaloff);
#endif
    unexec_copy (new_fd, old_fd, old_mcaloff, new_mcaloff,
                 old_buf.st_size - old_mcaloff);

    {
	long			curpos, offset;
	struct _debug_header	dhdr;
	int			new_header_delta;

	new_header_delta = LESYM_OFFSET(new_hdr) - LESYM_OFFSET(old_hdr);
	if ((new_header_delta > 0) &&
	    ((offset = EXT_OFFSET(old_hdr)) > 0))
	{
	    curpos = lseek(new_fd, 0, SEEK_CUR);
	    lseek(old_fd, offset, 0);
	    if (read(old_fd, &dhdr, sizeof(dhdr)) == sizeof(dhdr))
	    {
		dhdr.header_offset += new_header_delta;
		dhdr.gntt_offset += new_header_delta;
		dhdr.lntt_offset += new_header_delta;
		dhdr.slt_offset += new_header_delta;
		dhdr.vt_offset += new_header_delta;
		dhdr.xt_offset += new_header_delta;
		lseek(new_fd, EXT_OFFSET(new_hdr), SEEK_SET);
		if (write(new_fd, &dhdr, sizeof(dhdr)) != sizeof(dhdr))
		{
		    unexec_error("Unable to write debug information to \"%s\"\n",
				 1, new_name);
		}
		lseek(new_fd, curpos, SEEK_SET);
	    }
	    else
	    {
		unexec_error("Unable to read debug information from \"%s\"\n",
			     1, old_name);
	    }
	}
    }
  }
  
     
  /* make the output file executable -- then quit */
  unexec_fchmod (new_fd, 0755);
  close (old_fd);
  close (new_fd);
  return 0;
}


int
run_time_remap (char *dummy)
{
    unsigned long current_sbrk = (unsigned long) sbrk (0);

    if (sbrk_of_0_at_unexec < current_sbrk)
        fprintf (stderr, "Absurd new brk addr = 0x%x (current = 0x%x)\n", 
                 sbrk_of_0_at_unexec, current_sbrk);
    else
    {
        errno = 0;
        if (brk ((caddr_t) sbrk_of_0_at_unexec))
            fprintf (stderr, "failed to change brk addr to 0x%x: %s\n", 
                     sbrk_of_0_at_unexec, SYS_ERR);
    }
    
  return 0;
}
