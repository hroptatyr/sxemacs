/*
 *	Copyright (c) 1995 by Sun Microsystems, Inc.
 *	All rights reserved.
 *
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 *
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/*
 * dynodump(3x) dumps a running executable into a specified ELF file.  The new
 * file consists of the memory contents of the original file together with any
 * heap.  This heap is assigned to a new `.heap' section within the new file.
 *
 * The new file may be re-executed, and will contain any data modifications
 * made to the original image up until the time dynodump(3x) was called.
 *
 * The original image may have undergone relocations (performed by ld.so.1)
 * prior to control being transferred to the image.  These relocations will
 * reside as the data copied from the image.  To prevent subsequent executions
 * of the new image from undergoing the same relocations, any relocation entries
 * (besides copy or jump slot relocations) are nulled out.  Note that copy
 * relocations such as required for __iob must be reinitialized each time the
 * process starts, so it is not sufficient to simply null out the .dynamic
 * sections relocation information.  The effect of this is that if the new
 * image was bound to definitions in any shared object dependencies, then these
 * dependencies *must* reside in the same location as when dynodump(3x) was
 * called.  Any changes to the shared object dependencies of the new image, or
 * uses of such things as LD_PRELOAD, may result in the bindings encoded in the
 * image becoming invalid.
 *
 * The following flags modify the data of the image created:
 *
 *  RTLD_SAVREL	save the original relocation data.  Under this option any
 *		relocation offset is reset to contain the same data as was
 *		found in the images original file.
 *
 *		This option allows relocation information to be retained in the
 *		new image so that it may be re-executed when the new image is
 *		run.  This allows far greater flexibility as the new image can
 *		now take advantage of new shared objects.
 *
 *		Note. under this mechanism, any data item that undergoes
 *		relocation and is then further modified during the execution of
 *		the image before dynodump(3x) is called will lose the
 *		modification that occurred during the applications execution.
 *
 * N.B. The above commentary is not quite correct in the flags have been hardwired
 *      to RTLD_SAVREL.
 */
#pragma ident	"@(#) $Id: dynodump.c,v 1.8 2001/04/12 18:20:43 michaels Exp $ - SMI"

#define __EXTENSIONS__ 1

#include	<sys/param.h>
#include	<sys/procfs.h>
#include	<fcntl.h>
#include	<stdio.h>
#include	<libelf.h>
#include	<link.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	<errno.h>
#include	<malloc.h>
#include	"machdep.h"
#include	"_dynodump.h"

/*
 * Generic elf error message generator
 */
static int
elferr(const char * str)
{
    fprintf(stderr, "%s: %s\n", str, elf_errmsg(elf_errno()));
    return (1);
}

int dynodump (const char * file);
int
dynodump(const char * file)
{
    Elf		*ielf, *oelf;
    Ehdr	*iehdr, *oehdr;
    Phdr	*iphdr, *ophdr, *data_phdr = 0;
    Cache	*icache, *ocache, *_icache, *_ocache;
    Cache	*data_cache = 0, *shstr_cache = 0;
    Cache	*heap_cache = 0;
    Word	heap_sz = 0;
    Elf_Scn	*scn;
    Shdr	*shdr;
    Elf_Data	*data, rundata;
    Half	ndx, _ndx;
    int		fd, _fd;
    Addr	edata, _addr;
    char	*istrs, *ostrs, *_ostrs, proc[16];
    const char 	heap[] = ".heap";
    prstatus_t	pstat;

    /* make a call to the processor specific un-init stuff */
    dynodump_uninit();

    /*
     * Obtain a file descriptor for this process,
     * for the executable and get a prstatus_t
     * structure.
     */
    sprintf(proc, "/proc/%ld", getpid());
    if (((_fd = open(proc, O_RDONLY, 0)) == -1) ||
	((fd = ioctl(_fd, PIOCOPENM, (void *)0)) == -1) ||
	(ioctl(_fd, PIOCSTATUS, &pstat) == -1)) {
	fprintf(stderr, "/proc: initialization error: %s\n",
		strerror(errno));
	close(_fd);
	return (1);
    }
    close(_fd);

    /*
     * Initialize with the ELF library and make sure this is an executable
     * ELF file we're dealing with.
     */
    elf_version(EV_CURRENT);
    if ((ielf = elf_begin(fd, ELF_C_READ, NULL)) == NULL) {
	close(fd);
	return (elferr("elf_begin"));
    }
    close(fd);

    if ((elf_kind(ielf) != ELF_K_ELF) ||
	((iehdr = elf_getehdr(ielf)) == NULL) ||
	(iehdr->e_type != ET_EXEC)) {
	fprintf(stderr, "image is not an ELF executable\n");
	elf_end(ielf);
	return (1);
    }
    /*
     * Elf_elf_header(iehdr);
     */

    /*
     * Create the new output file.
     */
    if ((fd = open(file, O_RDWR | O_CREAT | O_TRUNC, 0777)) == -1) {
	fprintf(stderr, "%s: open failed: %s\n", file,
		       strerror(errno));
	elf_end(ielf);
	return (1);
    }
    if ((oelf = elf_begin(fd, ELF_C_WRITE, NULL)) == NULL) {
	elf_end(ielf);
	close(fd);
	return (elferr("elf_begin"));
    }

    /*
     * Obtain the input program headers.  Remember the data segments
     * program header entry as this will be updated later to reflect the
     * new .heap sections size.
     */
    if ((iphdr = elf_getphdr(ielf)) == NULL)
	return (elferr("elf_getphdr"));

    for (ndx = 0, ophdr = iphdr; ndx != iehdr->e_phnum; ndx++, ophdr++) {
	/*
	 * Save the program header that contains the NOBITS section, or
	 * the last loadable program header if no NOBITS exists.
	 * A NOBITS section translates to a memory size requirement that
	 * is greater than the file data it is mapped from.
	 */
	if (ophdr->p_type == PT_LOAD) {
	    if (ophdr->p_filesz != ophdr->p_memsz)
		data_phdr = ophdr;
	    else if (data_phdr) {
		if (data_phdr->p_vaddr < ophdr->p_vaddr)
		    data_phdr = ophdr;
	    } else
		data_phdr = ophdr;
	}
    }
    if (data_phdr == 0) {
	fprintf(stderr, "no data segment found!\n");
	return (0);
    }

    /*
     * Obtain the input files section header string table.
     */
    if ((scn = elf_getscn(ielf, iehdr->e_shstrndx)) == NULL)
	return (elferr("elf_getscn"));
    if ((data = elf_getdata(scn, NULL)) == NULL)
	return (elferr("elf_getdata"));
    istrs = (char *) data->d_buf;

    /*
     * Construct a cache to maintain the input files section information.
     */
    if ((icache = (Cache *) malloc(iehdr->e_shnum * sizeof (Cache))) == 0) {
	fprintf(stderr, "malloc failed: %s\n", strerror(errno));
	return (1);
    }
    _icache = icache;
    _icache++;

    /*
     * Traverse each section from the input file.
     */
    for (ndx = 1, scn = 0;
	 (_icache->c_scn = elf_nextscn(ielf, scn));
	 ndx++, scn = _icache->c_scn, _icache++) {

	if ((_icache->c_shdr = shdr = elf_getshdr(_icache->c_scn)) == NULL)
	    return (elferr("elf_getshdr"));

	if ((_icache->c_data = elf_getdata(_icache->c_scn, NULL)) == NULL)
	    return (elferr("elf_getdata"));

	_icache->c_name = istrs + (size_t)(shdr->sh_name);

	/*
	 * For each section that has a virtual address reestablish the
	 * data buffer to point to the memory image.
	 *
	 * if (shdr->sh_addr)
	 *     _icache->c_data->d_buf = (void *)shdr->sh_addr;
	 */

	/*
	 * Remember the last section of the data segment, the new .heap
	 * section will be added after this section.
	 * If we already have one, then set data_cache to the previous
	 * section and set heap_cache to this one.
	 */
	if ((shdr->sh_addr + shdr->sh_size)
	    == (data_phdr->p_vaddr + data_phdr->p_memsz)) {
	    if (strcmp(_icache->c_name, heap) == 0) {
#ifdef DEBUG
		printf("Found a previous .heap section\n");
#endif
		data_cache = _icache - 1;
		heap_cache = _icache;
		heap_sz = shdr->sh_size;
	    } else {
		data_cache = _icache;
	    }
	}

	/*
	 * Remember the section header string table as this will be
	 * rewritten with the new .heap name.
	 */
	if ((shdr->sh_type == SHT_STRTAB) &&
	    ((strcmp(_icache->c_name, ".shstrtab")) == 0))
	    shstr_cache = _icache;
    }
    if (data_cache == 0) {
	fprintf(stderr, "final data section not found!\n");
	return (0);
    }

    /*
     * Determine the new .heap section to create.
     */
    rundata.d_buf = (void *)(data_cache->c_shdr->sh_addr +
			     data_cache->c_shdr->sh_size);
    rundata.d_size = (int)sbrk(0) - (int)rundata.d_buf;
    rundata.d_type = ELF_T_BYTE;
    rundata.d_off = 0;
    rundata.d_align = 1;
    rundata.d_version = EV_CURRENT;

    /*
     * From the new data buffer determine the new value for _end and _edata.
     * This will also be used to update the data segment program header.
     *
     * If we had a .heap section, then its size is part of the program
     * headers notion of data size.  Because we're only going to output one
     * heap section (ignoring the one in the running binary) we need to
     * subtract the size of that which we're ignoring.
     */
    if (heap_cache) {
	edata = S_ROUND((data_phdr->p_vaddr
			 + data_phdr->p_memsz
			 - heap_sz), rundata.d_align) + rundata.d_size;
    } else {
	edata = S_ROUND((data_phdr->p_vaddr + data_phdr->p_memsz),
			rundata.d_align) + rundata.d_size;
    }

    /*
     * We're now ready to construct the new elf image.
     *
     * Obtain a new elf header and initialize it with any basic information
     * that isn't calculated as part of elf_update().  Bump the section
     * header string table index to account for the .heap section we'll be
     * adding.
     */
    if ((oehdr = elf_newehdr(oelf)) == NULL)
	return (elferr("elf_newehdr"));

    oehdr->e_entry = iehdr->e_entry;
    oehdr->e_machine = iehdr->e_machine;
    oehdr->e_type = iehdr->e_type;
    oehdr->e_flags = iehdr->e_flags;
    /*
     * If we already have a heap section, we don't need any adjustment
     */
    if (heap_cache)
	oehdr->e_shstrndx = iehdr->e_shstrndx;
    else
	oehdr->e_shstrndx = iehdr->e_shstrndx + 1;

#ifdef DEBUG
    printf("iehdr->e_flags   = %x\n", iehdr->e_flags);
    printf("iehdr->e_entry   = %x\n", iehdr->e_entry);
    printf("iehdr->e_shstrndx= %d\n", iehdr->e_shstrndx);
    printf("iehdr->e_machine = %d\n", iehdr->e_machine);
    printf("iehdr->e_type    = 0x%x\n", iehdr->e_type);
    printf("oehdr->e_machine = %d\n", oehdr->e_machine);
    printf("oehdr->e_type    = 0x%x\n", oehdr->e_type);
#endif

    /*
     * Obtain a new set of program headers.  Initialize these with the same
     * information as the input program headers and update the data segment
     * to reflect the new .heap section.
     */
    if ((ophdr = elf_newphdr(oelf, iehdr->e_phnum)) == NULL)
	return (elferr("elf_newphdr"));

    for (ndx = 0; ndx != iehdr->e_phnum; ndx++, iphdr++, ophdr++) {
	*ophdr = *iphdr;
	if (data_phdr == iphdr)
	    ophdr->p_filesz = ophdr->p_memsz = edata - ophdr->p_vaddr;
    }

    /*
     * Obtain a new set of sections.
     */
    _icache = icache;
    _icache++;
    for (ndx = 1; ndx != iehdr->e_shnum; ndx++, _icache++) {
	/*
	 * Skip the heap section of the running executable
	 */
	if (_icache == heap_cache)
	    continue;
	/*
	 * Create a matching section header in the output file.
	 */
	if ((scn = elf_newscn(oelf)) == NULL)
	    return (elferr("elf_newscn"));
	if ((shdr = elf_getshdr(scn)) == NULL)
	    return (elferr("elf_getshdr"));
	*shdr = *_icache->c_shdr;

	/*
	 * Create a matching data buffer for this section.
	 */
	if ((data = elf_newdata(scn)) == NULL)
	    return (elferr("elf_newdata"));
	*data = *_icache->c_data;

	/*
	 * For each section that has a virtual address reestablish the
	 * data buffer to point to the memory image.  Note, we skip
	 * the plt section.
	 */
	if ((shdr->sh_addr) && (!((shdr->sh_type == SHT_PROGBITS)
				  && (strcmp(_icache->c_name, ".plt") == 0))))
	    data->d_buf = (void *)shdr->sh_addr;

	/*
	 * Update any NOBITS section to indicate that it now contains
	 * data.
	 */
	if (shdr->sh_type == SHT_NOBITS)
	    shdr->sh_type = SHT_PROGBITS;

	/*
	 * Add the new .heap section after the last section of the
	 * present data segment.  If we had a heap section, then
	 * this is the section preceding it.
	 */
	if (data_cache == _icache) {
	    if ((scn = elf_newscn(oelf)) == NULL)
		return (elferr("elf_newscn"));
	    if ((shdr = elf_getshdr(scn)) == NULL)
		return (elferr("elf_getshdr"));
	    shdr->sh_type = SHT_PROGBITS;
	    shdr->sh_flags = SHF_ALLOC | SHF_WRITE;

	    if ((data = elf_newdata(scn)) == NULL)
		return (elferr("elf_newdata"));
	    *data = rundata;
	}

	/*
	 * Update the section header string table size to reflect the
	 * new section name (only if we didn't already have a heap).
	 */
	if (!heap_cache) {
	    if (shstr_cache && (shstr_cache == _icache)) {
		data->d_size += sizeof (heap);
	    }
	}
    }

    /*
     * Write out the new image, and obtain a new elf descriptor that will
     * allow us to write to the new image.
     */
    if (elf_update(oelf, ELF_C_WRITE) == -1)
	return (elferr("elf_update"));
    elf_end(oelf);
    if ((oelf = elf_begin(fd, ELF_C_RDWR, NULL)) == NULL)
	return (elferr("elf_begin"));
    if ((oehdr = elf_getehdr(oelf)) == NULL)
	return (elferr("elf_getehdr"));

    /*
     * Obtain the output files section header string table.
     */
    if ((scn = elf_getscn(oelf, oehdr->e_shstrndx)) == NULL)
	return (elferr("elf_getscn"));
    if ((data = elf_getdata(scn, NULL)) == NULL)
	return (elferr("elf_getdata"));
    ostrs = _ostrs = (char *) data->d_buf;
    *_ostrs++ = '\0';

    /*
     * Construct a cache to maintain the output files section information.
     */
    if ((ocache = (Cache *)malloc(oehdr->e_shnum * sizeof (Cache))) == 0) {
	fprintf(stderr, "malloc failed: %s\n", strerror(errno));
	return (1);
    }
    _ocache = ocache;
    _ocache++;
    _icache = icache;
    _icache++;

    /*
     * Traverse each section from the input file rebuilding the section
     * header string table as we go.
     */
    _ndx = _addr = 0;
    for (ndx = 1, scn = 0;
	 (_ocache->c_scn = elf_nextscn(oelf, scn));
	 ndx++, scn = _ocache->c_scn, _ocache++, _icache++) {

	const char *strs;

	if (_icache == heap_cache) {
#ifdef DEBUG
	    printf("ignoring .heap section in input\n");
#endif
	    _icache++;
	}

	if ((_ocache->c_shdr = shdr =
	     elf_getshdr(_ocache->c_scn)) == NULL)
	    return (elferr("elf_getshdr"));
	if ((_ocache->c_data =
	     elf_getdata(_ocache->c_scn, NULL)) == NULL)
	    return (elferr("elf_getdata"));

	/*
	 * If were inserting the new .heap section, insert the new
	 * section name and initialize its virtual address.
	 */
	if (_addr) {
	    strs = heap;
	    shdr->sh_addr = S_ROUND(_addr, shdr->sh_addralign);
	    _addr = 0;
	} else {
	    strs = istrs + (size_t)(_icache->c_shdr->sh_name);
	}

	strcpy(_ostrs, strs);
	shdr->sh_name = _ostrs - ostrs;
	_ocache->c_name = _ostrs;
	_ostrs += strlen(strs) + 1;

	/*
	 * If we've inserted a new section any later section may need
	 * their sh_link fields updated.
	 * If we already had a heap section, then this is not required.
	 */
	if (!heap_cache) {
	    if (_ndx) {
		if (_ocache->c_shdr->sh_link >= _ndx)
		    _ocache->c_shdr->sh_link++;
	    }
	}

	/*
	 * If this is the last section of the original data segment
	 * determine sufficient information to initialize the new .heap
	 * section which will be obtained next.
	 */
	if (data_cache == _icache) {
	    _ndx = ndx + 1;
	    _addr = shdr->sh_addr + shdr->sh_size;
	    _icache--;
	    data_cache = 0;
	}
    }

    /*
     * Now that we have a complete description of the new image update any
     * sections that are required.
     *
     *  o	update the value of _edata and _end.
     *
     *  o	reset any relocation entries if necessary.
     */
    _ocache = &ocache[1];
    _icache = &icache[1];
    for (ndx = 1; ndx < oehdr->e_shnum; ndx++, _ocache++, _icache++) {
	if ((_ocache->c_shdr->sh_type == SHT_SYMTAB) ||
	    (_ocache->c_shdr->sh_type == SHT_DYNSYM))
	    update_sym(ocache, _ocache, edata);

	if (_ocache->c_shdr->sh_type == M_REL_SHT_TYPE)
	    update_reloc(ocache, _ocache, icache, _icache, oehdr->e_shnum);
    }

    if (elf_update(oelf, ELF_C_WRITE) == -1)
	return (elferr("elf_update"));

    elf_end(oelf);
    elf_end(ielf);
    return (0);
}
