/* Unexec for DEC alpha.  schoepf@sc.ZIB-Berlin.DE (Rainer Schoepf).

   Copyright (C) 1994 Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */


#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>
#include <varargs.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>

static void fatal_unexec (char *, char *);
static void mark_x (char *);

#define READ(_fd, _buffer, _size, _error_message, _error_arg) \
	errno = EEOF; \
	if (read (_fd, _buffer, _size) != _size) \
	  fatal_unexec (_error_message, _error_arg);

#define WRITE(_fd, _buffer, _size, _error_message, _error_arg) \
	if (write (_fd, _buffer, _size) != _size) \
	  fatal_unexec (_error_message, _error_arg);

#define SEEK(_fd, _position, _error_message, _error_arg) \
	errno = EEOF; \
	if (lseek (_fd, _position, L_SET) != _position) \
	  fatal_unexec (_error_message, _error_arg);

#define EEOF -1

static struct scnhdr *text_section;
static struct scnhdr *init_section;
static struct scnhdr *finit_section;
static struct scnhdr *rdata_section;
static struct scnhdr *rconst_section;
static struct scnhdr *data_section;
static struct scnhdr *pdata_section;
static struct scnhdr *xdata_section;
static struct scnhdr *got_section;
static struct scnhdr *lit8_section;
static struct scnhdr *lit4_section;
static struct scnhdr *sdata_section;
static struct scnhdr *sbss_section;
static struct scnhdr *bss_section;

static unsigned long Brk;

struct headers {
    struct filehdr fhdr;
    struct aouthdr aout;
    struct scnhdr section[_MIPS_NSCNS_MAX];
};


/* Define name of label for entry point for the dumped executable.  */

#ifndef DEFAULT_ENTRY_ADDRESS
#define DEFAULT_ENTRY_ADDRESS __start
#endif
EXTERN_C int DEFAULT_ENTRY_ADDRESS (void);


int
unexec (char *new_name, char *a_name,
	unsigned long data_start,
	unsigned long bss_start,
	unsigned long entry_address)
{
  int new, old;
  char * oldptr;
  struct headers ohdr, nhdr;
  struct stat stat;
  long pagesize, brk;
  long newsyms, symrel;
  int i;
  long vaddr, scnptr;
#define BUFSIZE 8192
  char buffer[BUFSIZE];

  if ((old = open (a_name, O_RDONLY)) < 0)
    fatal_unexec ("opening %s", a_name);

  new = creat (new_name, 0666);
  if (new < 0) fatal_unexec ("creating %s", new_name);

  if ((fstat (old, &stat) == -1))
    fatal_unexec ("fstat %s", a_name);

  oldptr = (char *)mmap (0, stat.st_size, PROT_READ, MAP_FILE|MAP_SHARED, old, 0);

  if (oldptr == (char *)-1)
    fatal_unexec ("mmap %s", a_name);

  close (old);

  /* This is a copy of the a.out header of the original executable */

  ohdr = (*(struct headers *)oldptr);

  /* This is where we build the new header from the in-memory copy */

  nhdr = *((struct headers *)TEXT_START);

  /* First do some consistency checks */

  if (nhdr.fhdr.f_magic != ALPHAMAGIC
      && nhdr.fhdr.f_magic != ALPHAUMAGIC)
    {
      fprintf (stderr, "unexec: input file magic number is %x, not %x or %x.\n",
	       nhdr.fhdr.f_magic, ALPHAMAGIC, ALPHAUMAGIC);
      exit (1);
    }

  if (nhdr.fhdr.f_opthdr != sizeof (nhdr.aout))
    {
      fprintf (stderr, "unexec: input a.out header is %d bytes, not %ld.\n",
	       nhdr.fhdr.f_opthdr, (long) (sizeof (nhdr.aout)));
      exit (1);
    }
  if (nhdr.aout.magic != ZMAGIC)
    {
      fprintf (stderr, "unexec: input file a.out magic number is %o, not %o.\n",
	       nhdr.aout.magic, ZMAGIC);
      exit (1);
    }


  /* Now check the existence of certain header section and grab
     their addresses. */

#define CHECK_SCNHDR(ptr, name, flags)					\
  ptr = NULL;								\
  for (i = 0; i < nhdr.fhdr.f_nscns && !ptr; i++)			\
    if (strcmp (nhdr.section[i].s_name, name) == 0)			\
      {									\
	if (nhdr.section[i].s_flags != flags)				\
	  fprintf (stderr, "unexec: %x flags (%x expected) in %s section.\n", \
		   nhdr.section[i].s_flags, flags, name);		\
	ptr = nhdr.section + i;						\
      }									\

  CHECK_SCNHDR (text_section,  _TEXT,  STYP_TEXT);
  CHECK_SCNHDR (init_section,  _INIT,  STYP_INIT);
#ifdef _FINI
  CHECK_SCNHDR (finit_section, _FINI,  STYP_FINI);
#endif /* _FINI */
  CHECK_SCNHDR (rdata_section, _RDATA, STYP_RDATA);
#ifdef _RCONST
  CHECK_SCNHDR (rconst_section, _RCONST, STYP_RCONST);
#endif
#ifdef _PDATA
  CHECK_SCNHDR (pdata_section, _PDATA, STYP_PDATA);
#endif /* _PDATA */
#ifdef _GOT
  CHECK_SCNHDR (got_section,   _GOT,   STYP_GOT);
#endif /* _GOT */
  CHECK_SCNHDR (data_section,  _DATA,  STYP_DATA);
#ifdef _XDATA
  CHECK_SCNHDR (xdata_section, _XDATA, STYP_XDATA);
#endif /* _XDATA */
#ifdef _LIT8
  CHECK_SCNHDR (lit8_section,  _LIT8,  STYP_LIT8);
  CHECK_SCNHDR (lit4_section,  _LIT4,  STYP_LIT4);
#endif /* _LIT8 */
  CHECK_SCNHDR (sdata_section, _SDATA, STYP_SDATA);
  CHECK_SCNHDR (sbss_section,  _SBSS,  STYP_SBSS);
  CHECK_SCNHDR (bss_section,   _BSS,   STYP_BSS);


  pagesize = getpagesize ();
  brk = (((long) (sbrk (0))) + pagesize - 1) & (-pagesize);

  /* Remember the current break */

  Brk = brk;

  nhdr.aout.dsize = brk - DATA_START;
  nhdr.aout.bsize = 0;
  if (entry_address == 0)
    {
      nhdr.aout.entry = (unsigned long)DEFAULT_ENTRY_ADDRESS;
    }
  else
    nhdr.aout.entry = entry_address;

  nhdr.aout.bss_start = nhdr.aout.data_start + nhdr.aout.dsize;

  if (rdata_section != NULL)
    {
      rdata_section->s_size = data_start - DATA_START;

      /* Adjust start and virtual addresses of rdata_section, too.  */
      rdata_section->s_vaddr = DATA_START;
      rdata_section->s_paddr = DATA_START;
      rdata_section->s_scnptr = text_section->s_scnptr + nhdr.aout.tsize;
    }

  data_section->s_vaddr = data_start;
  data_section->s_paddr = data_start;
  data_section->s_size = brk - data_start;

  if (rdata_section != NULL)
    {
      data_section->s_scnptr = rdata_section->s_scnptr + rdata_section->s_size;
    }

  vaddr = data_section->s_vaddr + data_section->s_size;
  scnptr = data_section->s_scnptr + data_section->s_size;
  if (lit8_section != NULL)
    {
      lit8_section->s_vaddr = vaddr;
      lit8_section->s_paddr = vaddr;
      lit8_section->s_size = 0;
      lit8_section->s_scnptr = scnptr;
    }
  if (lit4_section != NULL)
    {
      lit4_section->s_vaddr = vaddr;
      lit4_section->s_paddr = vaddr;
      lit4_section->s_size = 0;
      lit4_section->s_scnptr = scnptr;
    }
  if (sdata_section != NULL)
    {
      sdata_section->s_vaddr = vaddr;
      sdata_section->s_paddr = vaddr;
      sdata_section->s_size = 0;
      sdata_section->s_scnptr = scnptr;
    }
#ifdef _XDATA
  if (xdata_section != NULL)
    {
      xdata_section->s_vaddr = vaddr;
      xdata_section->s_paddr = vaddr;
      xdata_section->s_size = 0;
      xdata_section->s_scnptr = scnptr;
    }
#endif
#ifdef _GOT
  if (got_section != NULL)
    {
      got_section->s_vaddr = vaddr;
      got_section->s_paddr = vaddr;
      got_section->s_size = 0;
      got_section->s_scnptr = scnptr;
    }
#endif /*_GOT */
  if (sbss_section != NULL)
    {
      sbss_section->s_vaddr = vaddr;
      sbss_section->s_paddr = vaddr;
      sbss_section->s_size = 0;
      sbss_section->s_scnptr = scnptr;
    }
  if (bss_section != NULL)
    {
      bss_section->s_vaddr = vaddr;
      bss_section->s_paddr = vaddr;
      bss_section->s_size = 0;
      bss_section->s_scnptr = scnptr;
    }

  WRITE (new, (char *)TEXT_START, nhdr.aout.tsize,
	 "writing text section to %s", new_name);
  WRITE (new, (char *)DATA_START, nhdr.aout.dsize,
	 "writing data section to %s", new_name);


  /*
   * Construct new symbol table header
   */

  memcpy (buffer, oldptr + nhdr.fhdr.f_symptr, cbHDRR);

#define symhdr ((pHDRR)buffer)
  newsyms = nhdr.aout.tsize + nhdr.aout.dsize;
  symrel = newsyms - nhdr.fhdr.f_symptr;
  nhdr.fhdr.f_symptr = newsyms;
  symhdr->cbLineOffset += symrel;
  symhdr->cbDnOffset += symrel;
  symhdr->cbPdOffset += symrel;
  symhdr->cbSymOffset += symrel;
  symhdr->cbOptOffset += symrel;
  symhdr->cbAuxOffset += symrel;
  symhdr->cbSsOffset += symrel;
  symhdr->cbSsExtOffset += symrel;
  symhdr->cbFdOffset += symrel;
  symhdr->cbRfdOffset += symrel;
  symhdr->cbExtOffset += symrel;

  WRITE (new, buffer, cbHDRR, "writing symbol table header of %s", new_name);

  /*
   * Copy the symbol table and line numbers
   */
  WRITE (new, oldptr + ohdr.fhdr.f_symptr + cbHDRR,
	 stat.st_size - ohdr.fhdr.f_symptr - cbHDRR,
	 "writing symbol table of %s", new_name);

#if 0

/* Not needed for now */

  update_dynamic_symbols (oldptr, new_name, new, newsyms,
			  ((pHDRR) (oldptr + ohdr.fhdr.f_symptr))->issExtMax,
                          ((pHDRR) (oldptr + ohdr.fhdr.f_symptr))->cbExtOffset,
                          ((pHDRR) (oldptr + ohdr.fhdr.f_symptr))->cbSsExtOffset);

#endif

#undef symhdr

  SEEK (new, 0, "seeking to start of header in %s", new_name);
  WRITE (new, &nhdr, sizeof (nhdr),
	 "writing header of %s", new_name);

  close (old);
  close (new);
  mark_x (new_name);
  return 0;
}


#if 0

/* Not needed for now */

/* The following function updates the values of some symbols
   that are used by the dynamic loader:

   _edata
   _end

*/

int
update_dynamic_symbols (
     char *old,			/* Pointer to old executable */
     char *new_name,            /* Name of new executable */
     int new,			/* File descriptor for new executable */
     long newsyms,		/* Offset of Symbol table in new executable */
     int nsyms,			/* Number of symbol table entries */
     long symoff,		/* Offset of External Symbols in old file */
     long stroff)		/* Offset of string table in old file */
{
  long i;
  int found = 0;
  EXTR n_end, n_edata;

  /* We go through the symbol table entries until we have found the two
     symbols. */

  /* cbEXTR is the size of an external symbol table entry */

  for (i = 0; i < nsyms && found < 2; i += cbEXTR)
    {
      REGISTER pEXTR x = (pEXTR) (old + symoff + i);
      char *s;
  
      s = old + stroff + x->asym.iss; /* name of the symbol */

      if (!strcmp(s,"_edata"))
	{
	  found++;
          memcpy (&n_edata, x, cbEXTR);
	  n_edata.asym.value = Brk;
	  SEEK (new, newsyms + cbHDRR + i,
		"seeking to symbol _edata in %s", new_name);
	  WRITE (new, &n_edata, cbEXTR,
		 "writing symbol table entry for _edata into %s", new_name);
	}
      else if (!strcmp(s,"_end"))
	{
	  found++;
          memcpy (&n_end, x, cbEXTR);
	  n_end.asym.value = Brk;
	  SEEK (new, newsyms + cbHDRR + i,
		"seeking to symbol _end in %s", new_name);
	  WRITE (new, &n_end, cbEXTR,
		 "writing symbol table entry for _end into %s", new_name);
	}
    }

}

#endif


/*
 * mark_x
 *
 * After successfully building the new a.out, mark it executable
 */

static void
mark_x (char *name)
{
  struct stat sbuf;
  int um = umask (777);
  umask (um);
  if (stat (name, &sbuf) < 0)
    fatal_unexec ("getting protection on %s", name);
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) < 0)
    fatal_unexec ("setting protection on %s", name);
}

static void
fatal_unexec (char *s, char *arg)
{
  if (errno == EEOF)
    fputs ("unexec: unexpected end of file, ", stderr);
  else
    fprintf (stderr, "unexec: %s, ", strerror (errno));
  fprintf (stderr, s, arg);
  fputs (".\n", stderr);
  exit (1);
}
