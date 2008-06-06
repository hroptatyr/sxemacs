/* Code to do an unexec for Sun O/S 4.1 for a temacs linked -Bdynamic.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not synched with FSF. */

/*
Created 29-Oct-92 by Harlan Sexton
Tweaked 06-Aug-93 by Dean Michaels to work with sun3.
 */

/********************** Included .h Files **************************/

#include <config.h>

/* I don't understand this, but it's necessary to get some slots in struct exec
   from /usr/include/sys/exec.h when running LCC in strict ANSI mode.  We don't
   need this in K&R mode...
 */
#if defined(__lucid) && defined(__sparc) && !defined(sun)
# define sun 1
#endif

#include <stdarg.h>
#include <sys/param.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <a.out.h>
#include <unistd.h>
#include <ctype.h>
#include <stab.h>
#include <sys/dir.h>
#include <link.h>

/********************** Macros *************************************/

#define SYS_ERR \
 ((errno > 0)?((errno < sys_nerr)?(sys_errlist[errno]):\
               "unknown system error"): "unknown error")

#define MASK_UP(x,p_of_two) \
 ((((unsigned long) (x)) + ((p_of_two) - 1)) & (~((p_of_two) - 1)))

#define MASK_DOWN(x,p_of_two) (((unsigned long) (x)) & (~((p_of_two) - 1)))

#ifndef mc68020
#define relocation_info reloc_info_sparc
#endif

/********************** Typedefs and Structs ***********************/

struct translation_struct {
	long txtaddr;
	long txtoff;
	long dataddr;
	long datoff;
	long bssaddr;
};

/********************** Function Prototypes/Declarations ***********/

static void unexec_error(const char *m, int use_errno, ...);
static int unexec_open(char *filename, int flag, int mode);
static caddr_t unexec_mmap(int fd, size_t len, int prot, int flags);
static long unexec_seek(int fd, long position);
static void unexec_read(int fd, long position, char *buf, int bytes);
static void unexec_write(int fd, long position, char *buf, int bytes);
static void unexec_pad(int fd, int bytes);
static void unexec_fstat(int fd, struct stat *statptr);
static void unexec_fchmod(int fd, int mode);
static long unexec_addr_to_offset(long addr, struct translation_struct *ts);
static void copy_relocation_site(struct relocation_info *ri,
				 caddr_t from_base_addr,
				 caddr_t to_base_addr,
				 struct translation_struct *ts);
static void reset_symtab(struct nlist *start, struct nlist *end,
			 char *strtab, long edata_value, long end_value,
			 int ld_so_table, int shlib_image);
int run_time_remap(char *dummy);

/********************** Variables **********************************/

/* for reporting error messages from system calls */
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;
extern int _DYNAMIC;
extern char **environ;

static unsigned long mprotect_bottom_addr;
static unsigned long mprotect_top_addr;

static unsigned long sbrk_of_0_at_unexec;

/*******************************************************************/

static void unexec_error(const char *fmt, int use_errno, ...)
{
	const char *err_msg = SYS_ERR;
	va_list args;

	fprintf(stderr, "unexec - ");
	va_start(args, use_errno);
	vfprintf(stderr, fmt, args);
	va_end(args);

	if (use_errno)
		fprintf(stderr, ": %s", err_msg);
	fprintf(stderr, "\n");
	exit(1);
	return;
}

static int unexec_open(char *filename, int flag, int mode)
{
	int fd;

	errno = 0;

	fd = open(filename, flag, mode);

	if (fd < 0)
		unexec_error("Failure opening file %s", 1, (void *)filename, 0,
			     0);
	return fd;
}

static caddr_t unexec_mmap(int fd, size_t len, int prot, int flags)
{
	caddr_t return_val;

	unexec_seek(fd, 0);
	errno = 0;
	return_val = mmap(0, len, prot, flags, fd, 0);

	if (return_val == (caddr_t) - 1)
		unexec_error("Failure mmap'ing file", 1, 0, 0, 0);
	return return_val;
}

static long unexec_seek(int fd, long position)
{
	long seek_value;

	if (fd <= 0)
		unexec_error("No file open in which to seek", 0, 0, 0, 0);

	errno = 0;

	if (position < 0)
		seek_value = (long)lseek(fd, 0, L_INCR);
	else
		seek_value = (long)lseek(fd, position, L_SET);

	if (seek_value < 0)
		unexec_error("Failed to do a seek to 0x%x in %s", 1,
			     (char *)position, "unexec() output file", 0);

	return seek_value;
}

static void unexec_read(int fd, long position, char *buf, int bytes)
{
	int n_read;
	int remains = bytes;
	position = unexec_seek(fd, position);

	if (bytes < 0)
		unexec_error("Attempted read of %d bytes", 0, (char *)bytes, 0,
			     0);

	errno = 0;

	while (remains > 0) {
		n_read = read(fd, buf, remains);
		if (n_read <= 0)
			unexec_error
			    ("Read failed for 0x%x bytes at offset 0x%x in %s",
			     1, (char *)bytes, (char *)position,
			     "unexec() output file");
		buf += n_read;
		remains -= n_read;
	}

	return;
}

static void unexec_write(int fd, long position, char *buf, int bytes)
{
	int n_written;
	int remains = bytes;
	position = unexec_seek(fd, position);

	if (bytes < 0)
		unexec_error("Attempted write of %d bytes in %s",
			     0, (char *)bytes, "unexec() output file", 0);

	errno = 0;

	while (remains > 0) {
		n_written = write(fd, buf, remains);
		if (n_written <= 0)
			unexec_error
			    ("Write failed for 0x%x bytes at offset 0x%x in %s",
			     1, (char *)bytes, (char *)position,
			     "unexec() output file");
		buf += n_written;
		remains -= n_written;
	}

	return;
}

static void unexec_pad(int fd, int bytes)
{
	if (bytes > 0) {
		char buf[1024];
		int remaining = bytes;

		memset(buf, 0, sizeof(buf));

		while (remaining > 0) {
			int this_write =
			    (remaining > sizeof(buf)) ? sizeof(buf) : remaining;
			unexec_write(fd, -1, buf, this_write);
			remaining -= this_write;
		}
	}
}

static void unexec_fstat(int fd, struct stat *statptr)
{
	errno = 0;
	if (-1 == fstat(fd, statptr))
		unexec_error("fstat() failed for descriptor %d", 1, (char *)fd,
			     0, 0);
	return;
}

static void unexec_fchmod(int fd, int mode)
{
	errno = 0;
	if (-1 == fchmod(fd, mode))
		unexec_error("fchmod() failed for descriptor %d", 1, (char *)fd,
			     0, 0);
	return;
}

static long unexec_addr_to_offset(long addr, struct translation_struct *ts)
{
	if ((addr < ts->txtaddr) || (addr >= ts->bssaddr))
		unexec_error("bad address 0x%x to addr_to_offset()",
			     0, (char *)addr, 0, 0);
	if (addr >= ts->dataddr)
		return ((long)((addr - ts->dataddr) + ts->datoff));
	else
		return ((long)((addr - ts->txtaddr) + ts->txtoff));
}

/*
 * "LD.SO" DATA AND SYMBOL TABLE OPERATIONS 
 */

static void
copy_relocation_site(struct relocation_info *ri,
		     caddr_t from_base_addr,
		     caddr_t to_base_addr, struct translation_struct *ts)
{
	long offset = unexec_addr_to_offset(ri->r_address, ts);
	caddr_t from = from_base_addr + offset;
	caddr_t to = to_base_addr + offset;

#ifdef mc68020
#define r_type r_length
#endif				/* mc68020 */
	switch (ri->r_type) {
#ifdef mc68020
	case 0:
		*((char *)to) = *((char *)from);
		break;
	case 1:
		*((short *)to) = *((short *)from);
		break;
	case 2:
		*((long *)to) = *((long *)from);
		break;
#else				/* !mc68020 */
	case RELOC_8:
	case RELOC_DISP8:
		*((char *)to) = *((char *)from);
		break;
	case RELOC_16:
	case RELOC_DISP16:
		*((short *)to) = *((short *)from);
		break;
	case RELOC_LO10:
	case RELOC_13:
	case RELOC_22:
	case RELOC_HI22:
	case RELOC_WDISP22:
	case RELOC_WDISP30:
	case RELOC_32:
	case RELOC_DISP32:
	case RELOC_GLOB_DAT:
		*((long *)to) = *((long *)from);
		break;
	case RELOC_JMP_SLOT:
		{
			long *target = (long *)to;
			long *source = (long *)from;
			*target = *source;
			target++;
			source++;
			*target = *source;
			target++;
			source++;
			*target = *source;
		}
		break;
#endif				/* !mc68020 */
	default:
		unexec_error("unknown reloc type %d seen during unexec()",
			     0, (char *)ri->r_type, 0, 0);
		break;
	}
	return;
}

static void
reset_symtab(struct nlist *start, struct nlist *end, char *strtab,
	     long edata_value, long end_value, int ld_so_table, int shlib_image)
{
	struct nlist *tmp = start;
	int found_edata = 0;
	int found_end = 0;

	while (tmp < end) {
		int type = tmp->n_type;
		int named = (ld_so_table) ? 1 : (tmp->n_un.n_strx);

		if ((type == (N_UNDF | N_EXT)) && (tmp->n_value != 0))
			unexec_error
			    ("unexec'ing image has COMMON symbols in it -- we quit!",
			     0, 0, 0, 0);

		if (!(type & N_STAB)) {
			if (!found_edata &&
			    (type == (N_EXT | N_DATA)) &&
			    named &&
			    !strcmp("_edata", strtab + tmp->n_un.n_strx)) {
				tmp->n_value = edata_value;
				found_edata = 1;
			}

			if ((type & N_TYPE) == N_BSS) {
				if (!found_end &&
				    (type == (N_EXT | N_BSS)) &&
				    named &&
				    !strcmp("_end", strtab + tmp->n_un.n_strx))
				{
					tmp->n_value = end_value;
					found_end = 1;
				} else if (type & N_EXT)
					tmp->n_type = N_DATA | N_EXT;
				else
					tmp->n_type = N_DATA;
			}

			/* the way things are being handled here, having sbrk() in the
			   image is fatal for an image linked with shared lib's (although 
			   the code could be modified to support it), but this should 
			   never happen anyway */
			if (shlib_image &&
			    (type == (N_EXT | N_TEXT)) &&
			    named &&
			    !strcmp("_sbrk", strtab + tmp->n_un.n_strx))
				unexec_error
				    ("unexec'd shlib image has sbrk() in it -- we quit!",
				     0, 0, 0, 0);
		}

		tmp++;
	}
}

extern int getpagesize(void);

/*
 * EXPORTED FUNCTIONS 
 */

/* this has to be a global variable to prevent the optimizers from
 * assuming that it can not be 0.  
*/
static void *dynamic_addr = (void *)&_DYNAMIC;

int
unexec(char *new_name, char *old_name,
       unsigned int emacs_edata, unsigned int dummy1, unsigned int dummy2)
{
	/* ld.so data */
	struct link_dynamic *ld = 0;
	struct link_dynamic_2 *ld2 = 0;
	/* old and new state */
	int old_fd;
	int new_fd;
	caddr_t old_base_addr;
	caddr_t new_base_addr;
	struct exec old_hdr;
	struct exec new_hdr;
	struct stat old_buf;
	struct stat new_buf;
	/* some process specific "constants" */
	unsigned long n_pagsiz;
	long page_size = getpagesize();
	caddr_t plt_end;
	caddr_t current_break = (caddr_t) sbrk(0);

	if (!page_size)
		unexec_error
		    ("unexec() failed because we can't get the size of a page!",
		     0, 0, 0, 0);

	/* see if this is a -Bdynamic image -- if so, find ld.so structures */
	if (dynamic_addr) {
		ld = (struct link_dynamic *)dynamic_addr;
		ld2 = ld->ld_un.ld_2;
		if (ld->ld_version < 2)
			unexec_error
			    ("%s linked with obsolete version of ld -- we quit!",
			     0, old_name, 0, 0);
	}

	/* open the old and new files, figuring out how big the old one is
	   so that we can map it in */
	old_fd = unexec_open(old_name, O_RDONLY, 0);
	new_fd = unexec_open(new_name, O_RDWR | O_CREAT | O_TRUNC, 0666);

	/* setup the header and the statbuf for old_fd */
	unexec_read(old_fd, 0, (char *)&old_hdr, sizeof(old_hdr));
	unexec_fstat(old_fd, &old_buf);

	/* set up some important constants */
	n_pagsiz = N_PAGSIZ(old_hdr);
	if (dynamic_addr)
		plt_end =
		    (caddr_t) MASK_UP(ld2->ld_plt + ld2->ld_plt_sz,
				      sizeof(double));
	else
		plt_end = (caddr_t) N_DATADDR(old_hdr);

	/* never write protect the variable "environ", defined in /lib/crt0.o, and
	   set in process.c and callproc.c */
	mprotect_bottom_addr = ((unsigned long)&environ) + sizeof(char **);
	/* never protect ABOVE the end of data emacs_edata specified */
	mprotect_top_addr =
	    MIN(emacs_edata, N_DATADDR(old_hdr) + old_hdr.a_data);

	/* Set up the image of the old file */
	old_base_addr =
	    unexec_mmap(old_fd, old_buf.st_size, PROT_READ, MAP_PRIVATE);
	close(old_fd);

	/* set up the new exec */
	new_hdr = old_hdr;
	new_hdr.a_data = (((unsigned long)MASK_UP(current_break, n_pagsiz)) -
			  ((unsigned long)N_DATADDR(old_hdr)));
	new_hdr.a_bss = 0;

	/* set up this variable, in case we want to reset "the break" 
	   when restarting */
	sbrk_of_0_at_unexec = ((unsigned long)MASK_UP(current_break, n_pagsiz));

	/* Write out the first approximation to the new file. The sizes of
	   each section will be correct, but there will be a number of 
	   corrections that will need to be made. */
	{
		long old_datoff = N_DATOFF(old_hdr);
		long old_dataddr = N_DATADDR(old_hdr);
		long new_treloff = N_TRELOFF(new_hdr);
		long old_treloff = N_TRELOFF(old_hdr);
		long ld_so_size = ((unsigned long)plt_end) - old_dataddr;
		long real_data_size = current_break - plt_end;
		long pad_size =
		    MASK_UP(current_break,
			    n_pagsiz) - ((unsigned long)current_break);

		/* First, write the text segment with new header -- copy everything until
		   the start of the data segment from the old file, and then go back and 
		   write the new header. */
		unexec_write(new_fd, 0, old_base_addr, old_datoff + ld_so_size);
		unexec_write(new_fd, 0, (char *)&new_hdr, sizeof(new_hdr));

		/* Copy the rest of the data segment from the running image. */
		unexec_write(new_fd, old_datoff + ld_so_size,
			     plt_end, real_data_size);

		/* pad out the data segment */
		unexec_pad(new_fd, pad_size);

		/* Finally, copy the symbol table information from the old file. */
		unexec_write(new_fd, new_treloff,
			     old_base_addr + old_treloff,
			     old_buf.st_size - old_treloff);
	}

	/* Next, map in the output file so that we can jump around fixing it
	   up. We retain the old file so that we can refer to it. */
	unexec_fstat(new_fd, &new_buf);
	new_base_addr = unexec_mmap(new_fd,
				    MASK_UP(new_buf.st_size, page_size),
				    PROT_READ | PROT_WRITE, MAP_SHARED);

	/* We need to do 2 things. First, make sure that _edata and _end (and
	   hence, curbrk) are set to the correct values. At the same time, for
	   neatness and to help with debugging, mark all the types of all ld.so
	   and nm BSS symbols in the new file to be DATA, and make sure that
	   there are no COMMON symbols in the output file, as any references to
	   these can lose really big. Second, reset all of the ld.so "relocation
	   sites" in the new file to have the values that appear in the old file
	   -- the failure to do this was the biggest loser in the old version of
	   this code. */

	/* STEP 1 */
	{
		/* Reset the regular symbol table first. */
		reset_symtab((struct nlist *)(new_base_addr +
					      N_SYMOFF(new_hdr)),
			     (struct nlist *)(new_base_addr +
					      N_SYMOFF(new_hdr) +
					      new_hdr.a_syms),
			     (char *)(new_base_addr + N_STROFF(new_hdr)),
			     N_DATADDR(new_hdr) + new_hdr.a_data,
			     N_BSSADDR(new_hdr) + new_hdr.a_bss, 0,
			     !!dynamic_addr);
		/* Now reset the ld.so symbol table. */
		if (dynamic_addr)
			reset_symtab((struct nlist *)(new_base_addr +
						      ld2->ld_stab),
				     (struct nlist *)(new_base_addr +
						      ld2->ld_symbols),
				     (char *)(new_base_addr + ld2->ld_symbols),
				     N_DATADDR(new_hdr) + new_hdr.a_data,
				     N_BSSADDR(new_hdr) + new_hdr.a_bss, 1,
				     !!dynamic_addr);
	}

	/* STEP 2 */
	if (dynamic_addr) {
		struct translation_struct ts;
		struct relocation_info *tmp =
		    (struct relocation_info *)(old_base_addr + ld2->ld_rel);
		struct relocation_info *end =
		    (struct relocation_info *)(old_base_addr + ld2->ld_hash);

		/* set up the structure that we use to translate addresses in the
		   old file into file offsets */
		ts.txtaddr = N_TXTADDR(old_hdr);
		ts.txtoff = N_TXTOFF(old_hdr);
		ts.dataddr = N_DATADDR(old_hdr);
		ts.datoff = N_DATOFF(old_hdr);
		ts.bssaddr = N_BSSADDR(old_hdr);

		while (tmp < end) {
			copy_relocation_site(tmp, old_base_addr, new_base_addr,
					     &ts);
			tmp++;
		}
	}

	/* get rid of the mmap-ed file space and make the output file 
	   executable -- then quit */
	munmap(new_base_addr, MASK_UP(new_buf.st_size, page_size));
	munmap(old_base_addr, MASK_UP(old_buf.st_size, page_size));
	unexec_fchmod(new_fd, 0755);
	close(new_fd);
	return 0;
}

int run_time_remap(char *dummy)
{
	long page_size = getpagesize();
	unsigned long base_addr = MASK_UP(mprotect_bottom_addr, page_size);
	unsigned long top_addr = MASK_DOWN(mprotect_top_addr, page_size);
	long len = top_addr - base_addr;

	if (!dynamic_addr) {
		unsigned long current_sbrk = (unsigned long)sbrk(0);

		if (sbrk_of_0_at_unexec < current_sbrk) {
			if (sbrk_of_0_at_unexec != 0)
				/* GCC -Wall even catches incorrect printf type errors.
				   How utterly cool. */
				fprintf(stderr,
					"Absurd new brk addr = 0x%lx (current = 0x%lx)\n",
					sbrk_of_0_at_unexec, current_sbrk);
		} else {
			errno = 0;
			if (brk((caddr_t) sbrk_of_0_at_unexec))
				fprintf(stderr,
					"failed to change brk addr to 0x%lx: %s\n",
					sbrk_of_0_at_unexec, SYS_ERR);
		}
	}

	if (len > 0) {
		errno = 0;
		if (mprotect((caddr_t) base_addr, len, PROT_READ | PROT_EXEC))
			fprintf(stderr,
				"failed to change protection on data pages: %s\n",
				SYS_ERR);
	}

	return 0;
}
