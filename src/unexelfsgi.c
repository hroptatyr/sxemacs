/* Copyright (C) 1985, 1986, 1987, 1988, 1990, 1992, 1999, 2000
   Free Software Foundation, Inc.

   This file is part of XEmacs.

   XEmacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU Emacs is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   In other words, you are welcome to use, share and improve this
   program.  You are forbidden to forbid anyone else to use, share and
   improve what you give them.  Help stamp out software-hoarding!  */


/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 * void
 * unexec (char *new_name,
 *	   char *old_name,
 *	   uintptr_t data_start,
 *	   uintptr_t bss_start,
 *	   uintptr_t entry_address)
 *
 * The basic idea is that we start with an ELF file which contains
 * .bss (uninitialized global data) section which is normally not in
 * the file. As we load lisp the variables, which were first set to 0,
 * will change their values. We want to save those changed values into
 * another ELF file, which will become a new xemacs image. To do this,
 * we need to change several structures in the ELF file.
 *
 *   First of all, we need to change the programm header which tells
 *   the linker how to load stuff into memory so that data will come
 *   from the file and not from the /dev/zero. To do this, we find the
 *   segment, which is marked as loadable (type PT_LOAD) and which
 *   covers the old .bss section. We will next change the filesz and
 *   memsz for that segment to extend over the new data section.
 *
 *   Next we have to make sure that section header for the stuff which
 *   used to be uninitialized is changed to be initialized and to come
 *   from the file. To do this, we change the size and the type of the old
 *   .bss section (and all other section of the type SHT_NOBITS) to cover the
 *   new section and to be of type SHT_PROCBITS.
 *
 *   We also insert a new SHT_NOBITS section to keep some tools, which expect
 *   .bss happy.
 *
 *   Finally we need to patch up some references to the section
 *   indexes since we change the order and undo the relocation info to
 *   be the same as it was "before" because we actually used the data
 *   from the memory which were changed by the run-time linker.
 */

#ifndef emacs
#define fatal(a, b, c) fprintf (stderr, a, b, c), exit (1)
#include <string.h>
#else
#include <config.h>
extern void fatal (const char *, ...);
#endif

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <memory.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#ifdef HAVE_ELF_H
#include <elf.h>
#endif
#include <sys/mman.h>
#if defined (__sony_news) && defined (_SYSTYPE_SYSV)
#include <sys/elf_mips.h>
#include <sym.h>
#endif /* __sony_news && _SYSTYPE_SYSV */
#if __sgi
#include <syms.h> /* for HDRR declaration */
#endif /* __sgi */

#if __GNU_LIBRARY__ - 0 >= 6
# include <link.h>	/* get ElfW etc */
#endif

#ifndef ElfW
# ifdef __STDC__
#  define ElfBitsW(bits, type) Elf##bits##_##type
# else
#  define ElfBitsW(bits, type) Elf/**/bits/**/_/**/type
# endif
# ifdef _LP64
#  define ELFSIZE 64
# else
#  define ELFSIZE 32
# endif
  /* This macro expands `bits' before invoking ElfBitsW.  */
# define ElfExpandBitsW(bits, type) ElfBitsW (bits, type)
# define ElfW(type) ElfExpandBitsW (ELFSIZE, type)
#endif

#ifndef ELF_BSS_SECTION_NAME
#define ELF_BSS_SECTION_NAME ".bss"
#endif

/* Get the address of a particular section or program header entry,
 * accounting for the size of the entries. */

#define OLD_SECTION_H(n) \
     (*(ElfW(Shdr) *) ((byte *) old_section_h + old_file_h->e_shentsize * (n)))
#define NEW_SECTION_H(n) \
     (*(ElfW(Shdr) *) ((byte *) new_section_h + new_file_h->e_shentsize * (n)))
#define OLD_PROGRAM_H(n) \
     (*(ElfW(Phdr) *) ((byte *) old_program_h + old_file_h->e_phentsize * (n)))
#define NEW_PROGRAM_H(n) \
     (*(ElfW(Phdr) *) ((byte *) new_program_h + new_file_h->e_phentsize * (n)))

#define PATCH_INDEX(n) \
  do { \
	 if ((int) (n) >= growme_index) \
	   (n)++; } while (0)

typedef unsigned char byte;

/* Round X up to a multiple of Y.  */

static ElfW(Addr)
round_up (ElfW(Addr) x, ElfW(Addr) y)
{
  int rem = x % y;
  if (rem == 0)
    return x;
  return x - rem + y;
}

/* Return the index of the section named NAME.
   SECTION_NAMES, FILE_NAME and FILE_H give information
   about the file we are looking in.

   If we don't find the section NAME, that is a fatal error
   if NOERROR is 0; we return -1 if NOERROR is nonzero.  */

static int
find_section (char *name,
	      const char *section_names,
	      char *file_name,
	      ElfW(Ehdr) *old_file_h,
	      ElfW(Shdr) *old_section_h,
	      int noerror)
{
  int idx;

  for (idx = 1; idx < old_file_h->e_shnum; idx++)
    {
#ifdef DEBUG
      fprintf (stderr, "Looking for %s - found %s\n", name,
	       section_names + OLD_SECTION_H (idx).sh_name);
#endif
      if (!strcmp (section_names + OLD_SECTION_H (idx).sh_name,
		   name))
	  return idx;
    }

  /* If we're here, we found nothing or return did not work */
  if ( ! noerror)
      fatal ("Can't find %s in %s.\n", name, file_name);

  return -1;
}

/* ****************************************************************
 * unexec
 *
 * driving logic.
 *
 * In ELF, this works by replacing the old .bss section with a new
 * .data section, and inserting an empty .bss immediately afterwards.
 *
 */
void
unexec (char *new_name,
	char *old_name,
	uintptr_t data_start,
	uintptr_t bss_start,
	uintptr_t entry_address)
{
  int old_file;

  struct stat stat_buf;
  caddr_t old_base, new_base;

  ElfW(Ehdr) *old_file_h, * new_file_h;
  ElfW(Phdr) *old_program_h, * new_program_h;
  ElfW(Shdr) *old_section_h, * new_section_h;
  ElfW(Shdr) * growme = NULL, * grown = NULL;
  ElfW(Addr) old_bss_addr = 0,  new_data2_addr = 0;

  int growme_index = -1;
  int n, nn;
  const char *old_section_names;
  int old_mdebug_index, old_data_index;
  int new_bss_addr, new_data2_size, new_data2_offset, new_file, new_file_size;

  /* Open the old file */
  if ( (old_file = open (old_name, O_RDONLY)) < 0 )
      fatal ("Can't open %s for reading: errno %d\n", old_name, errno);

  if (fstat (old_file, &stat_buf) == -1)
      fatal ("Can't fstat (%s): errno %d\n", old_name, errno);

  /* map old file into the address space. */
  old_base = (caddr_t) mmap ((caddr_t) 0, stat_buf.st_size,
			     PROT_READ, MAP_SHARED, old_file, 0);
  if (old_base == (caddr_t) MAP_FAILED)
    fatal ("Can't mmap (%s): errno %d\n", old_name, errno);

  old_file_h    = (ElfW(Ehdr) *) old_base;
  old_program_h = (ElfW(Phdr) *) ((byte *) old_base + old_file_h->e_phoff);
  old_section_h = (ElfW(Shdr) *) ((byte *) old_base + old_file_h->e_shoff);
  old_section_names = (const char *) old_base
      + OLD_SECTION_H (old_file_h->e_shstrndx).sh_offset;

  /* Find a section which we will grow by looking for the SHT_NOBITS
   * section with ALLOCATE flag and with the biggest address. */
  for (n = 1; n < old_file_h->e_shnum; n++) {
      ElfW(Shdr) * sh = & OLD_SECTION_H(n);

      if ((sh->sh_type == SHT_NOBITS) && (sh->sh_flags & SHF_ALLOC)) {
	  if ( old_bss_addr < sh->sh_addr ) {
	      growme = sh;
	      growme_index = n;
	      new_data2_addr = old_bss_addr =  sh->sh_addr;
	  }
      }
  }

  if (growme == NULL )
      fatal ("Can't find a section to grow\n", 0, 0);

  old_data_index = find_section (".data", old_section_names,
				 old_name, old_file_h, old_section_h, 0);

  new_bss_addr = (ElfW(Addr)) sbrk (0);
  new_data2_size = new_bss_addr - old_bss_addr;
  new_data2_offset  = OLD_SECTION_H (old_data_index).sh_offset +
      (new_data2_addr - OLD_SECTION_H (old_data_index).sh_addr);

  if ( new_bss_addr < old_bss_addr + growme->sh_size )
      fatal (".bss shrank when undumping???\n", 0, 0);

  /* Set the output file to the right size and mmap it. */
  if ( (new_file = open (new_name, O_RDWR | O_CREAT, 0666)) < 0 )
      fatal ("Can't create (%s): errno %d\n", new_name, errno);

  new_file_size = stat_buf.st_size +  old_file_h->e_shentsize + new_data2_size;

  if (ftruncate (new_file, new_file_size))
      fatal ("Can't ftruncate (%s): errno %d\n", new_name, errno);

  new_base = (caddr_t) mmap ((caddr_t) 0, new_file_size,
			     PROT_READ | PROT_WRITE,
#ifdef UNEXEC_USE_MAP_PRIVATE
			     MAP_PRIVATE,
#else
			     MAP_SHARED,
#endif
			     new_file, 0);

  if (new_base == (caddr_t) -1)
      fatal ("Can't mmap (%s): errno %d\n", new_name, errno);

  new_file_h = (ElfW(Ehdr) *) new_base;
  new_program_h = (ElfW(Phdr) *) ((byte *) new_base + old_file_h->e_phoff);
  new_section_h = (ElfW(Shdr) *) ((byte *) new_base + old_file_h->e_shoff +
				  new_data2_size);

  /* Make our new file, program and section headers as copies of the
   * originals.  */
  memcpy (new_file_h, old_file_h, old_file_h->e_ehsize);
  memcpy (new_program_h, old_program_h,
	  old_file_h->e_phnum * old_file_h->e_phentsize);

  /* Modify the e_shstrndx if necessary. */
  PATCH_INDEX (new_file_h->e_shstrndx);

  /* Fix up file header.  We'll add one section.  Section header is
   * further away now.  */
  new_file_h->e_shoff += new_data2_size;
  new_file_h->e_shnum += 1;

  /* Fix up a new program header by extending the writable data
   * segment so that the bss area is covered too. Find that segment by
   * looking for one that starts before and ends after the .bss and is
   * PT_LOADable. */
  for (n = new_file_h->e_phnum - 1; n >= 0; n--) {
      ElfW(Phdr) * ph = & NEW_PROGRAM_H(n);
#ifdef DEBUG
      printf ("%d @ %0x + %0x against %0x + %0x",
	      n, ph->p_vaddr, ph->p_memsz,growme->sh_addr, growme->sh_size);
#endif
      if ((ph->p_type == PT_LOAD) &&
	  (ph->p_vaddr <= growme->sh_addr) &&
	  ((ph->p_vaddr+ph->p_memsz) >= (growme->sh_addr + growme->sh_size))) {
	  /* Make sure that the size includes any padding before the
	   * old .bss section.  */
	  ph->p_memsz = ph->p_filesz = new_bss_addr - ph->p_vaddr;
#ifdef DEBUG
	  puts (" That's the one!");
#endif
	  break;
      }
#ifdef DEBUG
      putchar ('\n');
#endif
  }

  if (n < 0)
      fatal ("Couldn't find segment which covers %s",
	     old_section_names + growme->sh_name);

  /* Walk through all section headers, insert the new data2 section
   * right before the new bss section. */
  for (n = 1, nn = 1; n < (int) old_file_h->e_shnum;  n++, nn++) {
      ElfW(Shdr) * nsec = & NEW_SECTION_H(nn);
      ElfW(Shdr) * osec = & OLD_SECTION_H(n);

      /* If this is the section we want to grow, insert the new data
       * section before it. */
      if ( osec == growme ) {
	  /* Steal the data section header for this data2 section but
	   * use the * 'grow' section's alignment. This * will assure
	   * that the new section * always be placed in the same spot
	   * * as the old section by any other * application. */
	  ElfW(Shdr) * od = &OLD_SECTION_H(old_data_index);

	  memcpy (nsec, od, new_file_h->e_shentsize);

	  nsec->sh_addr = new_data2_addr;
	  nsec->sh_offset =  new_data2_offset;
	  nsec->sh_size = new_data2_size;
	  nsec->sh_addralign = osec->sh_addralign;

	  /* Copy over what we have in memory now. */
	  memcpy (nsec->sh_offset + new_base, (caddr_t) osec->sh_addr,
		  new_data2_size);
	  nn++;
	  grown = nsec++;
      }

      memcpy (nsec, osec, old_file_h->e_shentsize);

      if ( osec == growme ) {
	  /* The new bss section's size is zero, and its file offset
	   * and virtual address should be off by NEW_DATA2_SIZE.  */
	  nsec->sh_offset = grown->sh_offset + new_data2_size;
	  nsec->sh_addr = grown->sh_addr + new_data2_size;

	  /* Let the new bss section address alignment be the same as
	   * the section address alignment followed the old bss
	   * section, so this section will be placed in exactly the
	   * same place. */
	  nsec->sh_addralign = osec->sh_addralign;
	  nsec->sh_size = 0;
      } else {
	  /* Any section that was originally placed AFTER the bss
	   * section should now be off by NEW_DATA2_SIZE. */
	  if ( round_up (nsec->sh_offset, growme->sh_addralign) >=
	       new_data2_offset)
	      nsec->sh_offset += new_data2_size;
      }

      /* Any section that was originally placed after the section *
       * header table should now be off by the size of one section
       * header table entry.  */
      if (nsec->sh_offset > new_file_h->e_shoff)
	  nsec->sh_offset += new_file_h->e_shentsize;


      /* If any section hdr refers to the section after the new .data
       * section, make it refer to next one because we have inserted a
       * new section in between.  */
      PATCH_INDEX (nsec->sh_link);

      /* For symbol tables, info is a symbol table index, so don't
       * change it.  */
      if (nsec->sh_type != SHT_SYMTAB && nsec->sh_type != SHT_DYNSYM)
	  PATCH_INDEX (nsec->sh_info);

      /* Any section which used to be NOBITS will now becomes PROGBITS
       * if it's ALLOC-atable, unless, of cause, it's not the one we
       * decided to grow */
      if ( (osec->sh_type == SHT_NOBITS) && (osec->sh_flags & SHF_ALLOC) &&
	   (osec != growme ) ) {
	  nsec->sh_type = SHT_PROGBITS;
      }

      /* Now, start to copy the content of sections */
      if ( nsec->sh_type != SHT_NULL || nsec->sh_type != SHT_NOBITS ) {

	  /* Write out the sections. .data and .data1 (and data2,
	   * called ".data" in the strings table) get copied from the
	   * current process instead of the old file.  */
	  caddr_t src =  old_base + osec->sh_offset;
	  const char * secname = old_section_names + nsec->sh_name;
	  const char * names[] = {
	      ".data",".sdata", ".lit4", ".lit8", ".sdata1", ".data1",
	      ".sbss", NULL};
	  int i;

	  for ( i=0; names[i] != NULL; i++ ) {
	      if ( ! strcmp (secname, names[i]) ) {
		  src = (caddr_t) osec->sh_addr;
		  break;
	      }
	  }

	  memcpy (nsec->sh_offset + new_base, src, nsec->sh_size);
      }

      old_mdebug_index = find_section (".mdebug", old_section_names,
				       old_name, old_file_h, old_section_h, 1);

#if defined (__sony_news) && defined (_SYSTYPE_SYSV)
      if (nsec->sh_type == SHT_MIPS_DEBUG && old_mdebug_index != -1) {
	  int diff = nsec->sh_offset-OLD_SECTION_H(old_mdebug_index).sh_offset;
	  HDRR *phdr = (HDRR *)(nsec->sh_offset + new_base);

	  if (diff) {
	      phdr->cbLineOffset += diff;
	      phdr->cbDnOffset   += diff;
	      phdr->cbPdOffset   += diff;
	      phdr->cbSymOffset  += diff;
	      phdr->cbOptOffset  += diff;
	      phdr->cbAuxOffset  += diff;
	      phdr->cbSsOffset   += diff;
	      phdr->cbSsExtOffset += diff;
	      phdr->cbFdOffset   += diff;
	      phdr->cbRfdOffset  += diff;
	      phdr->cbExtOffset  += diff;
	  }
      }
#endif /* __sony_news && _SYSTYPE_SYSV */

#if __sgi
      /* Adjust the HDRR offsets in .mdebug and copy the line data if
       * it's in its usual 'hole' in the object.  Makes the new file
       * debuggable with dbx.  patches up two problems: the absolute
       * file offsets in the HDRR record of .mdebug (see
       * /usr/include/syms.h), and the ld bug that gets the line table
       * in a hole in the elf file rather than in the .mdebug section
       * proper.
       *
       * David Anderson. davea@sgi.com Jan 16,1994 */
#define MDEBUGADJUST(__ct,__fileaddr)		\
  if (n_phdrr->__ct > 0)			\
    {						\
      n_phdrr->__fileaddr += movement;		\
    }

      if (n == old_mdebug_index) {
	  HDRR * o_phdrr = (HDRR *)((byte *)old_base + osec->sh_offset);
	  HDRR * n_phdrr = (HDRR *)((byte *)new_base + nsec->sh_offset);
	  unsigned movement = new_data2_size;

	  MDEBUGADJUST (idnMax, cbDnOffset);
	  MDEBUGADJUST (ipdMax, cbPdOffset);
	  MDEBUGADJUST (isymMax, cbSymOffset);
	  MDEBUGADJUST (ioptMax, cbOptOffset);
	  MDEBUGADJUST (iauxMax, cbAuxOffset);
	  MDEBUGADJUST (issMax, cbSsOffset);
	  MDEBUGADJUST (issExtMax, cbSsExtOffset);
	  MDEBUGADJUST (ifdMax, cbFdOffset);
	  MDEBUGADJUST (crfd, cbRfdOffset);
	  MDEBUGADJUST (iextMax, cbExtOffset);

	  /* The Line Section, being possible off in a hole of the
	   * object, requires special handling.  */
	  if (n_phdrr->cbLine > 0) {
	      if (o_phdrr->cbLineOffset >
		  osec->sh_offset+ osec->sh_size){
		  /* line data is in a hole in elf. do special copy
		   * and adjust for this ld mistake.  */
		  n_phdrr->cbLineOffset += movement;

		  memcpy (n_phdrr->cbLineOffset + new_base,
			  o_phdrr->cbLineOffset + old_base, n_phdrr->cbLine);
	      } else {
		  /* somehow line data is in .mdebug as it is supposed
		   * to be.  */
		  MDEBUGADJUST (cbLine, cbLineOffset);
	      }
	  }
      }
#endif /* __sgi */
      /* If it is the symbol table, its st_shndx field needs to be
       * patched.  */
      if (nsec->sh_type == SHT_SYMTAB || nsec->sh_type == SHT_DYNSYM) {
	  unsigned int num = nsec->sh_size / nsec->sh_entsize;
	  ElfW(Sym) * sym = (ElfW(Sym) *)(nsec->sh_offset + new_base);
	  byte *symnames = ((byte *) new_base +
			    NEW_SECTION_H (nsec->sh_link).sh_offset);

	  for (; num--; sym++) {
	      const char * symnam = (char *) (symnames + sym->st_name);

	      /* Update the symbol values of _edata and _end. */
	      if (strcmp (symnam, "_end") == 0
		  || strcmp (symnam, "end") == 0
		  || strcmp (symnam, "_edata") == 0
		  || strcmp (symnam, "edata") == 0)
		  memcpy (&sym->st_value, &new_bss_addr,sizeof (new_bss_addr));


	      if ((sym->st_shndx == SHN_UNDEF) || (sym->st_shndx == SHN_ABS)
		  || (sym->st_shndx == SHN_COMMON)
		  || (sym->st_shndx >= SHN_LOPROC &&
		      sym->st_shndx <= SHN_HIPROC))
		  continue;

	      PATCH_INDEX (sym->st_shndx);
	  }
      }
  }

  /* This loop seeks out relocation sections for the data section, so
   * that it can undo relocations performed by the runtime linker.  */
  for (n = new_file_h->e_shnum - 1; n; n--) {
      ElfW(Shdr) section = NEW_SECTION_H (n);

      if ( section.sh_type == SHT_REL || section.sh_type == SHT_RELA ) {
	  /* This code handles two different size structs, but there
	   * should be no harm in that provided that r_offset is
	   * always the first member.  */
	  ElfW(Shdr) * info = & NEW_SECTION_H(section.sh_info);
	  const char * nm = old_section_names + info->sh_name;

	  if (!strcmp (nm, ".data") || !strcmp (nm, ".sdata")
	      || !strcmp (nm, ".lit4") || !strcmp (nm, ".lit8")
	      || !strcmp (nm, ".sdata1") || !strcmp (nm, ".data1")) {
	      ElfW(Addr) offset =  info->sh_addr - info->sh_offset;
	      caddr_t end, reloc = old_base + section.sh_offset;

	      for (end = reloc + section.sh_size; reloc < end;
		   reloc += section.sh_entsize) {
		  ElfW(Addr) addr = ((ElfW(Rel) *) reloc)->r_offset - offset;
#ifdef __alpha__
		  /* The Alpha ELF binutils currently have a bug that
		   * sometimes results in relocs that contain all
		   * zeroes.  Work around this for now...  */
		  if (((ElfW(Rel) *) reloc)->r_offset == 0)
		      continue;
#endif
		  memcpy (new_base + addr, old_base + addr,
			  sizeof(ElfW(Addr)));
	      }
	  }
      }
  }

#ifdef UNEXEC_USE_MAP_PRIVATE
  if (lseek (new_file, 0, SEEK_SET) == -1)
      fatal ("Can't rewind (%s): errno %d\n", new_name, errno);

  if (write (new_file, new_base, new_file_size) != new_file_size)
      fatal ("Can't write (%s): errno %d\n", new_name, errno);
#endif

  /* Close the files and make the new file executable.  */
  if (close (old_file))
      fatal ("Can't close (%s): errno %d\n", old_name, errno);

  if (close (new_file))
      fatal ("Can't close (%s): errno %d\n", new_name, errno);

  if (stat (new_name, &stat_buf) == -1)
      fatal ("Can't stat (%s): errno %d\n", new_name, errno);

  n = umask (777);
  umask (n);
  stat_buf.st_mode |= 0111 & ~n;
  if (chmod (new_name, stat_buf.st_mode) == -1)
      fatal ("Can't chmod (%s): errno %d\n", new_name, errno);
}
