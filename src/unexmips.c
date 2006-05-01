/* Unexec for MIPS (including IRIS4D).
   Copyright (C) 1988, 1992, 1993, 1994
   Free Software Foundation, Inc.

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

#include <config.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <varargs.h>

#ifdef MACH

#include <a.out.h>

/* I don't know why this isn't defined.  */
#ifndef STYP_INIT
#define STYP_INIT  0x80000000
#endif

/* I don't know why this isn't defined.  */
#ifndef _RDATA
#define	_RDATA	".rdata"
#define STYP_RDATA 0x00000100
#endif

/* Small ("near") data section.  */
#ifndef _SDATA
#define	_SDATA	".sdata"
#define STYP_SDATA 0x00000200
#endif

/* Small ("near") bss section.  */
#ifndef _SBSS
#define _SBSS ".sbss"
#define STYP_SBSS 0x00000400
#endif

/* We don't seem to have a sym.h or syms.h anywhere, so we'll do it the
   hard way.  This stinks.  */
typedef struct {
	short magic;
	short vstamp;
	long ilineMax;
	struct {
		long foo, offset;
	} offsets[11];
} HDRR, *pHDRR;

#else				/* not MACH */

#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <sym.h>

#endif				/* not MACH */

#if defined (IRIS_4D) || defined (sony)
#include "getpagesize.h"
#include <fcntl.h>
#endif

static void fatal_unexec();
static void mark_x();

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

extern char *strerror();
#define EEOF -1

static struct scnhdr *text_section;
static struct scnhdr *init_section;
static struct scnhdr *finit_section;
static struct scnhdr *rdata_section;
static struct scnhdr *data_section;
static struct scnhdr *lit8_section;
static struct scnhdr *lit4_section;
static struct scnhdr *sdata_section;
static struct scnhdr *sbss_section;
static struct scnhdr *bss_section;

struct headers {
	struct filehdr fhdr;
	struct aouthdr aout;
	struct scnhdr section[10];
};

/* Define name of label for entry point for the dumped executable.  */

#ifndef DEFAULT_ENTRY_ADDRESS
#define DEFAULT_ENTRY_ADDRESS __start
#endif

unexec(new_name, a_name, data_start, bss_start, entry_address)
char *new_name, *a_name;
unsigned data_start, bss_start, entry_address;
{
	int new, old;
	int pagesize, brk;
	int newsyms, symrel;
	int nread;
	struct headers hdr;
	int i;
	int vaddr, scnptr;
#define BUFSIZE 8192
	char buffer[BUFSIZE];

	old = open(a_name, O_RDONLY, 0);
	if (old < 0)
		fatal_unexec("opening %s", a_name);

	new = creat(new_name, 0666);
	if (new < 0)
		fatal_unexec("creating %s", new_name);

	hdr = *((struct headers *)TEXT_START);
#ifdef MIPS2
	if (hdr.fhdr.f_magic != MIPSELMAGIC
	    && hdr.fhdr.f_magic != MIPSEBMAGIC
	    && hdr.fhdr.f_magic != (MIPSELMAGIC | 1)
	    && hdr.fhdr.f_magic != (MIPSEBMAGIC | 1)) {
		fprintf(stderr,
			"unexec: input file magic number is %x, not %x, %x, %x or %x.\n",
			hdr.fhdr.f_magic,
			MIPSELMAGIC, MIPSEBMAGIC,
			MIPSELMAGIC | 1, MIPSEBMAGIC | 1);
		exit(1);
	}
#else				/* not MIPS2 */
	if (hdr.fhdr.f_magic != MIPSELMAGIC && hdr.fhdr.f_magic != MIPSEBMAGIC) {
		fprintf(stderr,
			"unexec: input file magic number is %x, not %x or %x.\n",
			hdr.fhdr.f_magic, MIPSELMAGIC, MIPSEBMAGIC);
		exit(1);
	}
#endif				/* not MIPS2 */
	if (hdr.fhdr.f_opthdr != sizeof(hdr.aout)) {
		fprintf(stderr,
			"unexec: input a.out header is %d bytes, not %d.\n",
			hdr.fhdr.f_opthdr, sizeof(hdr.aout));
		exit(1);
	}
	if (hdr.aout.magic != ZMAGIC) {
		fprintf(stderr,
			"unexec: input file a.out magic number is %o, not %o.\n",
			hdr.aout.magic, ZMAGIC);
		exit(1);
	}
#define CHECK_SCNHDR(ptr, name, flags)					\
  ptr = NULL;								\
  for (i = 0; i < hdr.fhdr.f_nscns && !ptr; i++)			\
    if (strcmp (hdr.section[i].s_name, name) == 0)			\
      {									\
        if (hdr.section[i].s_flags != flags)				\
        fprintf (stderr, "unexec: %x flags (%x expected) in %s section.\n", \
                 hdr.section[i].s_flags, flags, name);			\
        ptr = hdr.section + i;						\
  }

	CHECK_SCNHDR(text_section, _TEXT, STYP_TEXT);
	CHECK_SCNHDR(init_section, _INIT, STYP_INIT);
	CHECK_SCNHDR(rdata_section, _RDATA, STYP_RDATA);
	CHECK_SCNHDR(data_section, _DATA, STYP_DATA);
#ifdef _LIT8
	CHECK_SCNHDR(lit8_section, _LIT8, STYP_LIT8);
	CHECK_SCNHDR(lit4_section, _LIT4, STYP_LIT4);
#endif				/* _LIT8 */
	CHECK_SCNHDR(sdata_section, _SDATA, STYP_SDATA);
	CHECK_SCNHDR(sbss_section, _SBSS, STYP_SBSS);
	CHECK_SCNHDR(bss_section, _BSS, STYP_BSS);
#if 0				/* Apparently this error check goes off on irix 3.3,
				   but it doesn't indicate a real problem.  */
	if (i != hdr.fhdr.f_nscns)
		fprintf(stderr, "unexec: %d sections found instead of %d.\n",
			i, hdr.fhdr.f_nscns);
#endif

	text_section->s_scnptr = 0;

	pagesize = getpagesize();
	/* Casting to int avoids compiler error on NEWS-OS 5.0.2.  */
	brk = (((int)(sbrk(0))) + pagesize - 1) & (-pagesize);
	hdr.aout.dsize = brk - DATA_START;
	hdr.aout.bsize = 0;
	if (entry_address == 0) {
		extern DEFAULT_ENTRY_ADDRESS();
		hdr.aout.entry = (unsigned)DEFAULT_ENTRY_ADDRESS;
	} else
		hdr.aout.entry = entry_address;

	hdr.aout.bss_start = hdr.aout.data_start + hdr.aout.dsize;
	rdata_section->s_size = data_start - DATA_START;

	/* Adjust start and virtual addresses of rdata_section, too. */
	rdata_section->s_vaddr = DATA_START;
	rdata_section->s_paddr = DATA_START;
	rdata_section->s_scnptr = text_section->s_scnptr + hdr.aout.tsize;

	data_section->s_vaddr = data_start;
	data_section->s_paddr = data_start;
	data_section->s_size = brk - data_start;
	data_section->s_scnptr =
	    rdata_section->s_scnptr + rdata_section->s_size;
	vaddr = data_section->s_vaddr + data_section->s_size;
	scnptr = data_section->s_scnptr + data_section->s_size;
	if (lit8_section != NULL) {
		lit8_section->s_vaddr = vaddr;
		lit8_section->s_paddr = vaddr;
		lit8_section->s_size = 0;
		lit8_section->s_scnptr = scnptr;
	}
	if (lit4_section != NULL) {
		lit4_section->s_vaddr = vaddr;
		lit4_section->s_paddr = vaddr;
		lit4_section->s_size = 0;
		lit4_section->s_scnptr = scnptr;
	}
	if (sdata_section != NULL) {
		sdata_section->s_vaddr = vaddr;
		sdata_section->s_paddr = vaddr;
		sdata_section->s_size = 0;
		sdata_section->s_scnptr = scnptr;
	}
	if (sbss_section != NULL) {
		sbss_section->s_vaddr = vaddr;
		sbss_section->s_paddr = vaddr;
		sbss_section->s_size = 0;
		sbss_section->s_scnptr = scnptr;
	}
	if (bss_section != NULL) {
		bss_section->s_vaddr = vaddr;
		bss_section->s_paddr = vaddr;
		bss_section->s_size = 0;
		bss_section->s_scnptr = scnptr;
	}

	WRITE(new, (void *)TEXT_START, hdr.aout.tsize,
	      "writing text section to %s", new_name);
	WRITE(new, (void *)DATA_START, hdr.aout.dsize,
	      "writing data section to %s", new_name);

	SEEK(old, hdr.fhdr.f_symptr, "seeking to start of symbols in %s",
	     a_name);
	errno = EEOF;
	nread = read(old, buffer, BUFSIZE);
	if (nread < sizeof(HDRR))
		fatal_unexec("reading symbols from %s", a_name);
	newsyms = hdr.aout.tsize + hdr.aout.dsize;
	symrel = newsyms - hdr.fhdr.f_symptr;
	hdr.fhdr.f_symptr = newsyms;
#define symhdr ((pHDRR)buffer)
#ifdef MACH
	for (i = 0; i < 11; i++)
		symhdr->offsets[i].offset += symrel;
#else
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
#endif
#undef symhdr
	do {
		if (write(new, buffer, nread) != nread)
			fatal_unexec("writing symbols to %s", new_name);
		nread = read(old, buffer, BUFSIZE);
		if (nread < 0)
			fatal_unexec("reading symbols from %s", a_name);
#undef BUFSIZE
	} while (nread != 0);

	SEEK(new, 0, "seeking to start of header in %s", new_name);
	WRITE(new, &hdr, sizeof(hdr), "writing header of %s", new_name);

	close(old);
	close(new);
	mark_x(new_name);
}

/*
 * mark_x
 *
 * After successfully building the new a.out, mark it executable
 */

static void mark_x(name)
char *name;
{
	struct stat sbuf;
	int um = umask(777);
	umask(um);
	if (stat(name, &sbuf) < 0)
		fatal_unexec("getting protection on %s", name);
	sbuf.st_mode |= 0111 & ~um;
	if (chmod(name, sbuf.st_mode) < 0)
		fatal_unexec("setting protection on %s", name);
}

static void fatal_unexec(s, va_alist)
va_dcl
{
	va_list ap;
	if (errno == EEOF)
		fputs("unexec: unexpected end of file, ", stderr);
	else
		fprintf(stderr, "unexec: %s, ", strerror(errno));
	va_start(ap);
	_doprnt(s, ap, stderr);
	fputs(".\n", stderr);
	exit(1);
}
