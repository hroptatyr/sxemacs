/* Unexec for HP 9000 Series 800 machines.
   Bob Desinger <hpsemc!bd@hplabs.hp.com>

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

/* Synched up with: Not synched with FSF. */

/*

  Unexec creates a copy of the old a.out file, and replaces the old data
  area with the current data area.  When the new file is executed, the
  process will see the same data structures and data values that the
  original process had when unexec was called.

  Unlike other versions of unexec, this one copies symbol table and
  debug information to the new a.out file.  Thus, the new a.out file
  may be debugged with symbolic debuggers.

  If you fix any bugs in this, I'd like to incorporate your fixes.
  Send them to uunet!hpda!hpsemc!jmorris or jmorris%hpsemc@hplabs.HP.COM.

  CAVEATS:
  This routine saves the current value of all static and external
  variables.  This means that any data structure that needs to be
  initialized must be explicitly reset.  Variables will not have their
  expected default values.

  Unfortunately, the HP-UX signal handler has internal initialization
  flags which are not explicitly reset.  Thus, for signals to work in
  conjunction with this routine, the following code must executed when
  the new process starts up.

  void _sigreturn();
  ...
  sigsetreturn(_sigreturn);
*/


#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include <a.out.h>
#include "lisp.h"

/*
 * Minor modification to enable dumping with shared libraries added by
 * Dipankar Gupta (dg@hplb.hpl.hp.com). I studied Oliver Laumann's
 * more elaborate dynamic loading scheme in ELK while implementing
 * this, but don't use any of his machinery.
 *
 * Stores the BRK value at dump time, and uses the RUN_TIME_REMAP hook
 * to break back to the stored value when the dumped executable is restarted.
 *
 * CAVEATS (addenda):
 * 1. Text area of the shlibs are not stored. Thus, if a shared library is
 *    replaced between the time of dump and execution, all bets are off.
 *
 * 2. Assumes that the data and bss area are adjacent, which is true of the
 *    current VM implementation.
 *
 * 3. Any setup that defines HPUX_USE_SHLIBS *must* also define
 *    RUN_TIME_REMAP.
 */

#ifdef HPUX_USE_SHLIBS
#include <dl.h>			/* User-space dynamic loader entry points */
static void Save_Shared_Data (void);
static void Restore_Shared_Data (void);
#endif

void write_header(int file, struct header *hdr, struct som_exec_auxhdr *auxhdr);
void read_header (int file, struct header *hdr, struct som_exec_auxhdr *auxhdr);
void save_data_space (int file, struct header *hdr,
		      struct som_exec_auxhdr *auxhdr, int size);
void copy_rest (int old, int new);
void copy_file (int old, int new, int size);
void update_file_ptrs(int file, struct header *hdr,
		      struct som_exec_auxhdr *auxhdr,
		      unsigned int location, int offset);
int calculate_checksum(struct header *hdr);

/* Create a new a.out file, same as old but with current data space */
int
unexec (char *new_name,		/* name of the new a.out file to be created */
        char *old_name,		/* name of the old a.out file */
        uintptr_t new_end_of_text, /* ptr to new edata/etext; NOT USED YET */
        uintptr_t dummy1, uintptr_t dummy2) /* not used by emacs */
{
  int old, new;
  int old_size, new_size;
  struct header hdr;
  struct som_exec_auxhdr auxhdr;
  long i;

  /* For the greatest flexibility, should create a temporary file in
     the same directory as the new file.  When everything is complete,
     rename the temp file to the new name.
     This way, a program could update its own a.out file even while
     it is still executing.  If problems occur, everything is still
     intact.  NOT implemented.  */

  /* Open the input and output a.out files */
  old = open (old_name, O_RDONLY);
  if (old < 0)
    { perror(old_name); exit(1); }
  new = open (new_name, O_CREAT|O_RDWR|O_TRUNC, 0777);
  if (new < 0)
    { perror(new_name); exit(1); }

  /* Read the old headers */
  read_header(old, &hdr, &auxhdr);

#ifdef HPUX_USE_SHLIBS
  Save_Shared_Data(); /* Save break value (added: dg@hplb.hpl.hp.com) */
#endif
  /* Decide how large the new and old data areas are */
  old_size = auxhdr.exec_dsize;
  /* I suspect these two statements are separate
     to avoid a compiler bug in hpux version 8.  */
  i = (long) sbrk (0);
  new_size = i - auxhdr.exec_dmem;

  /* Copy the old file to the new, up to the data space */
  lseek(old, 0, 0);
  copy_file(old, new, auxhdr.exec_dfile);

  /* Skip the old data segment and write a new one */
  lseek(old, old_size, 1);
  save_data_space(new, &hdr, &auxhdr, new_size);

  /* Copy the rest of the file */
  copy_rest(old, new);

  /* Update file pointers since we probably changed size of data area */
  update_file_ptrs(new, &hdr, &auxhdr, auxhdr.exec_dfile, new_size-old_size);

  /* Save the modified header */
  write_header(new, &hdr, &auxhdr);

  /* Close the binary file */
  close (old);
  close (new);
  return 0;
}

/* Save current data space in the file, update header.  */

void
save_data_space (int file, struct header *hdr,
		 struct som_exec_auxhdr *auxhdr, int size)
{
  /* Write the entire data space out to the file */
  if (write(file, (void *)auxhdr->exec_dmem, size) != size)
    { perror("Can't save new data space"); exit(1); }

  /* Update the header to reflect the new data size */
  auxhdr->exec_dsize = size;
  auxhdr->exec_bsize = 0;
}

/* Update the values of file pointers when something is inserted.  */

void
update_file_ptrs(int file, struct header *hdr,
		 struct som_exec_auxhdr *auxhdr,
		 unsigned int location, int offset)
{
  struct subspace_dictionary_record subspace;
  int i;

  /* Increase the overall size of the module */
  hdr->som_length += offset;

  /* Update the various file pointers in the header */
#define update(ptr) if (ptr > location) ptr = ptr + offset
  update(hdr->aux_header_location);
  update(hdr->space_strings_location);
  update(hdr->init_array_location);
  update(hdr->compiler_location);
  update(hdr->symbol_location);
  update(hdr->fixup_request_location);
  update(hdr->symbol_strings_location);
  update(hdr->unloadable_sp_location);
  update(auxhdr->exec_tfile);
  update(auxhdr->exec_dfile);

  /* Do for each subspace dictionary entry */
  lseek(file, hdr->subspace_location, 0);
  for (i = 0; i < hdr->subspace_total; i++)
    {
      if (read(file, &subspace, sizeof(subspace)) != sizeof(subspace))
	{ perror("Can't read subspace record"); exit(1); }

      /* If subspace has a file location, update it */
      if (subspace.initialization_length > 0
	  && subspace.file_loc_init_value > location)
	{
	  subspace.file_loc_init_value += offset;
	  lseek(file, -sizeof(subspace), 1);
	  if (write(file, &subspace, sizeof(subspace)) != sizeof(subspace))
	    { perror("Can't update subspace record"); exit(1); }
	}
    }

  /* Do for each initialization pointer record */
  /* (I don't think it applies to executable files, only relocatables) */
#undef update
}

/* Read in the header records from an a.out file.  */

void
read_header(int file, struct header *hdr, struct som_exec_auxhdr *auxhdr)
{

  /* Read the header in */
  lseek(file, 0, 0);
  if (read(file, hdr, sizeof(*hdr)) != sizeof(*hdr))
    { perror("Couldn't read header from a.out file"); exit(1); }

  if (hdr->a_magic != EXEC_MAGIC && hdr->a_magic != SHARE_MAGIC
      &&  hdr->a_magic != DEMAND_MAGIC)
    {
      fprintf(stderr, "a.out file doesn't have legal magic number\n");
      exit(1);
    }

  lseek(file, hdr->aux_header_location, 0);
  if (read(file, auxhdr, sizeof(*auxhdr)) != sizeof(*auxhdr))
    {
      perror("Couldn't read auxiliary header from a.out file");
      exit(1);
    }
}

/* Write out the header records into an a.out file.  */
void
write_header(int file, struct header *hdr, struct som_exec_auxhdr *auxhdr)
{
  /* Update the checksum */
  hdr->checksum = calculate_checksum(hdr);

  /* Write the header back into the a.out file */
  lseek(file, 0, 0);
  if (write(file, hdr, sizeof(*hdr)) != sizeof(*hdr))
    { perror("Couldn't write header to a.out file"); exit(1); }
  lseek(file, hdr->aux_header_location, 0);
  if (write(file, auxhdr, sizeof(*auxhdr)) != sizeof(*auxhdr))
    { perror("Couldn't write auxiliary header to a.out file"); exit(1); }
}

/* Calculate the checksum of a SOM header record. */
int
calculate_checksum(struct header *hdr)
{
  int checksum, i, *ptr;

  checksum = 0;  ptr = (int *) hdr;

  for (i=0; i<sizeof(*hdr)/sizeof(int)-1; i++)
    checksum ^= ptr[i];

  return(checksum);
}

/* Copy size bytes from the old file to the new one.  */
void
copy_file (int old, int new, int size)
{
  int len;
  int buffer[8192];  /* word aligned will be faster */

  for (; size > 0; size -= len)
    {
      len = size < sizeof (buffer) ? size : sizeof (buffer);
      if (read (old, buffer, len) != len)
	{
	  perror ("Read failure on a.out file");
	  exit (1);
	}
      if (write (new, buffer, len) != len)
	{
	  perror ("Write failure in a.out file");
	  exit (1);
	}
    }
}

/* Copy the rest of the file, up to EOF.  */
void
copy_rest (int old, int new)
{
  int buffer[4096];
  int len;

  /* Copy bytes until end of file or error */
  while ( (len = read(old, buffer, sizeof(buffer))) > 0)
    if (write(new, buffer, len) != len) break;

  if (len != 0)
    { perror("Unable to copy the rest of the file"); exit(1); }
}

#ifdef	DEBUG
display_header(struct header *hdr, struct som_exec_auxhdr *auxhdr)
{
  /* Display the header information (debug) */
  printf("\n\nFILE HEADER\n");
  printf("magic number %d \n", hdr->a_magic);
  printf("text loc %.8x   size %d \n", auxhdr->exec_tmem, auxhdr->exec_tsize);
  printf("data loc %.8x   size %d \n", auxhdr->exec_dmem, auxhdr->exec_dsize);
  printf("entry     %x \n",   auxhdr->exec_entry);
  printf("Bss  segment size %u\n", auxhdr->exec_bsize);
  printf("\n");
  printf("data file loc %d    size %d\n",
	 auxhdr->exec_dfile, auxhdr->exec_dsize);
  printf("som_length %d\n", hdr->som_length);
  printf("unloadable sploc %d    size %d\n",
	 hdr->unloadable_sp_location, hdr->unloadable_sp_size);
}
#endif /* DEBUG */

#ifdef HPUX_USE_SHLIBS
/* Added machinery for shared libs... see comments at the beginning of this file. */

void *Brk_On_Dump = 0;		/* Brk value to restore... stored as a global */

static void
Save_Shared_Data (void)
{
  Brk_On_Dump = sbrk (0);
}

static void
Restore_Shared_Data (void)
{
  brk (Brk_On_Dump);
}

/* run_time_remap is the magic called by startup code in the dumped executable
   if RUN_TIME_REMAP is set. */
int
run_time_remap (char *dummy)
{
  Restore_Shared_Data ();
  return 0;
}
#endif /* HPUX_USE_SHLIBS */
