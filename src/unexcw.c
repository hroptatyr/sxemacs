/* unexec for GNU Emacs on Cygwin32.
   Copyright (C) 1994, 1998 Free Software Foundation, Inc.

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

*/

/* This is a complete rewrite, some code snarfed from unexnt.c and
   unexec.c, Andy Piper (andy@xemacs.org) 13-1-98 */

#include <config.h>
#include "lisp.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#define DONT_ENCAPSULATE /* filenames are external in unex*.c */
#include "sysfile.h"

#define PERROR(arg) perror(arg);exit(-1) 

#if !defined(HAVE_A_OUT_H) && !defined(WIN32_NATIVE)
unexec (char *, char *, void *, void *,	void *)
{
  PERROR("cannot unexec() a.out.h not installed");
}
#else

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

#ifdef MINGW
#include <../../include/a.out.h>
#else
#include <a.out.h>
#endif

#define STACK_SIZE 0x800000
#define ALLOC_UNIT 0xFFFF
#define ALLOC_MASK ~((unsigned long)(ALLOC_UNIT))
#define ALIGN_ALLOC(addr) \
((((unsigned long)addr) + ALLOC_UNIT) & ALLOC_MASK)
/* Note that all sections must be aligned on a 0x1000 boundary so
   this is the minimum size that our dummy bss can be. */
#ifndef NO_DEBUG
#define BSS_PAD_SIZE	0x1000
#else
#define BSS_PAD_SIZE	0
#endif

/* To prevent zero-initialized variables from being placed into the bss
   section, use non-zero values to represent an uninitialized state.  */
#define UNINIT_PTR ((void *) 0xF0A0F0A0)
#define UNINIT_LONG (0xF0A0F0A0L)

static void get_section_info (int a_out, char* a_name);
static void copy_executable_and_dump_data_section (int a_out, int a_new);
static void dup_file_area(int a_out, int a_new, long size);
#if 0
static void write_int_to_bss(int a_out, int a_new, void* va, void* newval);
#endif

/* Cached info about the .data section in the executable.  */
void* data_start_va = UNINIT_PTR;
unsigned long  data_size = UNINIT_LONG;

/* Cached info about the .bss section in the executable.  */
void* bss_start = UNINIT_PTR;
unsigned long  bss_size = UNINIT_LONG;
int sections_reversed = 0;
FILHDR f_hdr;
PEAOUTHDR f_ohdr;
SCNHDR f_data, f_bss, f_text, f_nextdata;

#define PERROR(arg) perror(arg);exit(-1) 
#define CHECK_AOUT_POS(a) \
if (lseek(a_out, 0, SEEK_CUR) != a) \
{ \
  printf("we are at %lx, should be at %lx\n", \
	 lseek(a_out, 0, SEEK_CUR), a); \
  exit(-1); \
}

/* Dump out .data and .bss sections into a new executable.  */
int
unexec (char *out_name, char *in_name, uintptr_t start_data, 
	uintptr_t d1, uintptr_t d2)
{
  /* ugly nt hack - should be in lisp */
  int a_new, a_out = -1;
  char new_name[MAX_PATH], a_name[MAX_PATH];
  char *ptr;
  
  /* Make sure that the input and output filenames have the
     ".exe" extension...patch them up if they don't.  */
  strcpy (a_name, in_name);
  ptr = a_name + strlen (a_name) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (a_name, ".exe");

  strcpy (new_name, out_name);
  ptr = new_name + strlen (new_name) - 4;
  if (strcmp (ptr, ".exe"))
    strcat (new_name, ".exe");

  /* We need to round off our heap to NT's allocation unit (64KB).  */
  /* round_heap (get_allocation_unit ()); */

  if (a_name && (a_out = open (a_name, O_RDONLY | OPEN_BINARY)) < 0)
    {
      PERROR (a_name);
    }

  if ((a_new = open (new_name, O_WRONLY | O_TRUNC | O_CREAT | OPEN_BINARY,
		     0755)) < 0)
    {
      PERROR (new_name);
    }

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (a_out, a_name);

  copy_executable_and_dump_data_section (a_out, a_new);

  close(a_out);
  close(a_new);
  return 0;
}

/* Flip through the executable and cache the info necessary for dumping.  */
static void get_section_info (int a_out, char* a_name)
{
  extern char my_ebss[];
  /* From lastfile.c  */
  extern char my_edata[];

  if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (a_name);
    }

  if (f_hdr.e_magic != DOSMAGIC) 
    {
      PERROR("unknown exe header");
    }

  /* Check the NT header signature ...  */
  if (f_hdr.nt_signature != NT_SIGNATURE) 
    {
      PERROR("invalid nt header");
    }

  /* Flip through the sections for .data and .bss ...  */
  if (f_hdr.f_opthdr > 0)
    {
      if (read (a_out, &f_ohdr, AOUTSZ) != AOUTSZ)
	{
	  PERROR (a_name);
	}
    }
  /* Loop through .data & .bss section headers, copying them in.
     With newer lds these are reversed so we have to cope with both */
  lseek (a_out, sizeof (f_hdr) + f_hdr.f_opthdr, 0);

  if (read (a_out, &f_text, sizeof (f_text)) != sizeof (f_text)
      ||
      strcmp (f_text.s_name, ".text"))
    {
      PERROR ("no .text section");
    }

  /* The .bss section.  */
  if (read (a_out, &f_bss, sizeof (f_bss)) != sizeof (f_bss)
      ||
      (strcmp (f_bss.s_name, ".bss") && strcmp (f_bss.s_name, ".data")))
    {
      PERROR ("no .bss / .data section");
    }

  /* check for reversed .bss and .data */
  if (!strcmp(f_bss.s_name, ".data"))
    {
      printf(".data and .bss reversed\n");
      sections_reversed = 1;
      memcpy(&f_data, &f_bss, sizeof(f_bss));
    }

  /* The .data section.  */
  if (!sections_reversed)
    {
      if (read (a_out, &f_data, sizeof (f_data)) != sizeof (f_data)
	  ||
	  strcmp (f_data.s_name, ".data"))
	{
	  PERROR ("no .data section");
	}
    }
  else
    {
      if (read (a_out, &f_bss, sizeof (f_bss)) != sizeof (f_bss)
	  ||
	  strcmp (f_bss.s_name, ".bss"))
	{
	  PERROR ("no .bss section");
	}
    }
  
  bss_start = (void *) ((char*)f_ohdr.ImageBase + f_bss.s_vaddr);
  bss_size = (unsigned long)((char*)&my_ebss-(char*)bss_start);
  
  /* must keep bss data that we want to be blank as blank */
  printf("found bss - keeping %lx of %lx bytes\n", bss_size, f_ohdr.bsize);

  /* The .data section.  */
  data_start_va = (void *) ((char*)f_ohdr.ImageBase + f_data.s_vaddr);

  /* We want to only write Emacs data back to the executable,
     not any of the library data (if library data is included,
     then a dumped Emacs won't run on system versions other
     than the one Emacs was dumped on).  */
  data_size = (unsigned long)my_edata - (unsigned long)data_start_va;
  printf("found data - keeping %lx of %lx bytes\n", data_size, f_ohdr.dsize);

  /* The following data section - often .idata */
  if (read (a_out, &f_nextdata, sizeof (f_nextdata)) != sizeof (f_nextdata)
      &&
      strcmp (&f_nextdata.s_name[2], "data"))
    {
      PERROR ("no other data section");
    }
}

/* The dump routines.  */

static void
copy_executable_and_dump_data_section (int a_out, int a_new)
{
  long size=0;
  unsigned long new_data_size, new_bss_size, 
    bss_padding, file_sz_change, data_padding=0,
    f_data_s_vaddr = f_data.s_vaddr,
    f_data_s_scnptr = f_data.s_scnptr,
    f_bss_s_vaddr = f_bss.s_vaddr, 
    f_nextdata_s_scnptr = f_nextdata.s_scnptr;

  int i;
  void* empty_space;
  extern int static_heap_dumped;
  SCNHDR section;
  /* calculate new sizes:

     f_ohdr.dsize is the total initialized data size on disk which is
     f_data.s_size + f_idata.s_size.

     f_ohdr.data_start is the base addres of all data and so should
     not be changed.
     
     *.s_vaddr is the virtual address of the start of the section
     *normalized from f_ohdr.ImageBase.

     *.s_paddr appears to be the number of bytes in the section
     *actually used (whereas *.s_size is aligned).

     bsize is now 0 since subsumed into .data
     dsize is dsize + (f_data.s_vaddr - f_bss.s_vaddr)
     f_data.s_vaddr is f_bss.s_vaddr
     f_data.s_size is new dsize maybe.
     what about s_paddr & s_scnptr?  */

  /* this is the amount the file increases in size */
  if (!sections_reversed)
    {
      new_bss_size = f_data.s_vaddr - f_bss.s_vaddr;
      data_padding = 0;
    }
  else
    {
      new_bss_size = f_nextdata.s_vaddr - f_bss.s_vaddr;
      data_padding = (f_bss.s_vaddr - f_data.s_vaddr) - f_data.s_size;
    }

  if ((new_bss_size - bss_size) < BSS_PAD_SIZE)
    { 
      PERROR (".bss free space too small");
    }

  file_sz_change=(new_bss_size + data_padding) - BSS_PAD_SIZE;
  new_data_size=f_ohdr.dsize + file_sz_change;

  if (!sections_reversed)
    {
      f_data.s_vaddr = f_bss.s_vaddr;
    }
  f_data.s_paddr += file_sz_change;
#if 0 
  if (f_data.s_size + f_nextdata.s_size != f_ohdr.dsize)
    {
      printf("section size doesn't tally with dsize %lx != %lx\n", 
	     f_data.s_size + f_nextdata.s_size, f_ohdr.dsize);
    }
#endif
  f_data.s_size += file_sz_change;
  lseek (a_new, 0, SEEK_SET);
  /* write file header */
  f_hdr.f_symptr += file_sz_change;
#ifdef NO_DEBUG
  f_hdr.f_nscns--;
#endif

  printf("writing file header\n");
  if (write(a_new, &f_hdr, sizeof(f_hdr)) != sizeof(f_hdr))
    {
      PERROR("failed to write file header");
    }
  /* write optional header fixing dsize & bsize*/
  printf("writing optional header\n");
  printf("new data size is %lx, >= %lx\n", new_data_size,
	 f_ohdr.dsize + f_ohdr.bsize);
  if (new_data_size < f_ohdr.dsize + f_ohdr.bsize )
    {
      printf("warning: new data size is < approx\n");
    }
  f_ohdr.dsize=new_data_size;
  f_ohdr.bsize=BSS_PAD_SIZE;
  /* Prevent stack overflow with regexp usage. */
  f_ohdr.SizeOfStackReserve = STACK_SIZE;

  if (write(a_new, &f_ohdr, sizeof(f_ohdr)) != sizeof(f_ohdr))
    {
      PERROR("failed to write optional header");
    }
  /* write text as is */
  printf("writing text header (unchanged)\n");

  if (write(a_new, &f_text, sizeof(f_text)) != sizeof(f_text))
    {
      PERROR("failed to write text header");
    }
#ifndef NO_DEBUG
  /* Write small bss section. */
  if (!sections_reversed)
    {
      f_bss.s_size = BSS_PAD_SIZE;
      f_bss.s_paddr = BSS_PAD_SIZE;
      f_bss.s_vaddr = f_data.s_vaddr - BSS_PAD_SIZE;
      if (write(a_new, &f_bss, sizeof(f_bss)) != sizeof(f_bss))
	{
	  PERROR("failed to write bss header");
	}
    }
#endif
  /* write new data header */
  printf("writing .data header\n");

  if (write(a_new, &f_data, sizeof(f_data)) != sizeof(f_data))
    {
      PERROR("failed to write data header");
    }
#ifndef NO_DEBUG
  /* Write small bss section. */
  if (sections_reversed)
    {
      f_bss.s_size = BSS_PAD_SIZE;
      f_bss.s_paddr = BSS_PAD_SIZE;
      f_bss.s_vaddr = f_nextdata.s_vaddr - BSS_PAD_SIZE;
      if (write(a_new, &f_bss, sizeof(f_bss)) != sizeof(f_bss))
	{
	  PERROR("failed to write bss header");
	}
    }
#endif
  printf("writing following data header\n");
  f_nextdata.s_scnptr += file_sz_change;
  if (f_nextdata.s_lnnoptr != 0) f_nextdata.s_lnnoptr += file_sz_change;
  if (f_nextdata.s_relptr != 0) f_nextdata.s_relptr += file_sz_change;
  if (write(a_new, &f_nextdata, sizeof(f_nextdata)) != sizeof(f_nextdata))
    {
      PERROR("failed to write nextdata header");
    }

  /* copy other section headers adjusting the file offset */
  for (i=0; i<(f_hdr.f_nscns-3); i++)
    {
      if (read (a_out, &section, sizeof (section)) != sizeof (section))
	{
	  PERROR ("no .data section");
	}
      
      section.s_scnptr += file_sz_change;
      if (section.s_lnnoptr != 0) section.s_lnnoptr += file_sz_change;
      if (section.s_relptr != 0) section.s_relptr += file_sz_change;

      if (write(a_new, &section, sizeof(section)) != sizeof(section))
	{
	  PERROR("failed to write data header");
	}
    }
#ifdef NO_DEBUG
  /* dump bss to maintain offsets */
  memset(&f_bss, 0, sizeof(f_bss));
  if (write(a_new, &f_bss, sizeof(f_bss)) != sizeof(f_bss))
    {
      PERROR("failed to write bss header");
    }
#endif
  size=lseek(a_new, 0, SEEK_CUR);
  CHECK_AOUT_POS(size);

  /* copy eveything else until start of data */
  size = f_data_s_scnptr - lseek (a_out, 0, SEEK_CUR);

  printf ("copying executable up to data section ... %lx bytes\n", 
	  size);
  dup_file_area(a_out, a_new, size);

  CHECK_AOUT_POS(f_data_s_scnptr);

  if (!sections_reversed)
    {
      /* dump bss + padding between sections, sans small bss pad */
      printf ("dumping .bss into executable... %lx bytes\n", bss_size);
      if (write(a_new, bss_start, bss_size) != (int)bss_size)
	{
	  PERROR("failed to write bss section");
	}
      
      /* pad, needs to be zero */
      bss_padding = (new_bss_size - bss_size) - BSS_PAD_SIZE;
      if (bss_padding < 0)
	{
	  PERROR("padded .bss too small");
	}
      printf ("padding .bss ... %lx bytes\n", bss_padding);
      empty_space = malloc(bss_padding);
      memset(empty_space, 0, bss_padding);
      if (write(a_new, empty_space, bss_padding) != (int)bss_padding)
	{
	  PERROR("failed to write bss section");
	}
      free(empty_space);
    }

  /* tell dumped version not to free pure heap */
  static_heap_dumped = 1;
  /* Get a pointer to the raw data in our address space.  */
  printf ("dumping .data section... %lx bytes\n", data_size);
  if (write(a_new, data_start_va, data_size) != (int)data_size)
    {
      PERROR("failed to write data section");
    }
  /* were going to use free again ... */
  static_heap_dumped = 0;
  
  size = lseek(a_out, f_data_s_scnptr + data_size, SEEK_SET);

  if (!sections_reversed)
    {
      size = f_nextdata_s_scnptr - size;
      dup_file_area(a_out, a_new, size);
    }
  else
    {
      /* need to pad to bss with data in file */
      printf ("padding .data ... %lx bytes\n", data_padding);
      size = (f_bss_s_vaddr - f_data_s_vaddr) - data_size;
      dup_file_area(a_out, a_new, size);

      /* dump bss + padding between sections */
      printf ("dumping .bss into executable... %lx bytes\n", bss_size);
      if (write(a_new, bss_start, bss_size) != (int)bss_size)
	{
	  PERROR("failed to write bss section");
	}
      
      /* pad, needs to be zero */
      bss_padding = (new_bss_size - bss_size) - BSS_PAD_SIZE;
      if (bss_padding < 0)
	{
	  PERROR("padded .bss too small");
	}
      printf ("padding .bss ... %lx bytes\n", bss_padding);
      empty_space = malloc(bss_padding);
      memset(empty_space, 0, bss_padding);
      if (write(a_new, empty_space, bss_padding) != (int)bss_padding)
	{
	  PERROR("failed to write bss section");
	}
      free(empty_space);
      if (lseek(a_new, 0, SEEK_CUR) != f_nextdata.s_scnptr)
	{
	  printf("at %lx should be at %lx\n", 
		 lseek(a_new, 0, SEEK_CUR),
		 f_nextdata.s_scnptr);
	  PERROR("file positioning error\n");
	}
      lseek(a_out, f_nextdata_s_scnptr, SEEK_SET);
    }

  CHECK_AOUT_POS(f_nextdata_s_scnptr);

  /* now dump - nextdata don't need to do this cygwin ds is in .data! */
  printf ("dumping following data section... %lx bytes\n", f_nextdata.s_size);

  dup_file_area(a_out,a_new,f_nextdata.s_size);

  /* write rest of file */
  printf ("writing rest of file\n");
  size = lseek(a_out, 0, SEEK_END);
  size = size - (f_nextdata_s_scnptr + f_nextdata.s_size); /* length remaining in a_out */
  lseek(a_out, f_nextdata_s_scnptr + f_nextdata.s_size, SEEK_SET);

  dup_file_area(a_out, a_new, size);
}

/*
 * copy from aout to anew
 */
static void dup_file_area(int a_out, int a_new, long size)
{
  char page[BUFSIZ];
  long n;
  for (; size > 0; size -= sizeof (page))
    {
      n = size > sizeof (page) ? sizeof (page) : size;
      if (read (a_out, page, n) != n || write (a_new, page, n) != n)
	{
	  PERROR ("dump_out()");
	}
    }
}

#if 0
static void write_int_to_bss(int a_out, int a_new, void* va, void* newval)
{
  int cpos;

  cpos = lseek(a_new, 0, SEEK_CUR);
  if (va < bss_start || va > bss_start + f_data.s_size)
    {
      PERROR("address not in data space\n");
    }
  lseek(a_new, f_data.s_scnptr + ((unsigned long)va - 
				  (unsigned long)bss_start), SEEK_SET);
  if (write(a_new, newval, sizeof(int)) != (int)sizeof(int))
    {
      PERROR("failed to write int value");
    }
  lseek(a_new, cpos, SEEK_SET);
}
#endif

#endif /* HAVE_A_OUT_H */
