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

#pragma ident	"@(#) $Id: _relocate.c,v 1.3 1997/05/29 04:23:02 steve Exp $ - SMI"

/* LINTLIBRARY */

#include	<libelf.h>
#include	<string.h>
#include	<machdep.h>
#include	"_dynodump.h"

void
update_reloc(Cache *ocache, Cache *_ocache,
	     Cache *icache, Cache *_icache,
	     Half shnum)
{
    Shdr *shdr;
    Rel *rels;
    int reln, cnt;
    Cache *orcache, *ircache;

    /*
     * Set up to readh the output relocation table.
     */
    shdr = _ocache->c_shdr;
    rels = (Rel *) _ocache->c_data->d_buf;
    reln = shdr->sh_size / shdr->sh_entsize;

    /*
     * Determine the section that is being relocated.
     */
    orcache = &ocache[shdr->sh_info];
    shdr = _icache->c_shdr;
    ircache = &icache[shdr->sh_info];

    /*
     * Determine the section that is being relocated.  Note that for this
     * stupid architecture the .rel.plt actually contains offsets into the
     * .got.
     */
    if (strcmp(_ocache->c_name, ".rel.plt")) {
	orcache = &ocache[shdr->sh_info];
	shdr = _icache->c_shdr;
	ircache = &icache[shdr->sh_info];
    } else {
	Half	ndx;
	Cache *	__ocache = ocache;

	for (__ocache++, ndx = 1; ndx != shnum; ndx++, __ocache++) {
	    if (strcmp(__ocache->c_name, ".got") == 0) {
		orcache = __ocache;
		ircache = &icache[ndx];
		break;
	    }
	}
    }

    /*
     * Loop through the relocation table.
     */
    for (cnt = 0; cnt < reln; cnt++, rels++) {
	unsigned char *iaddr, *oaddr;
	Addr off;
	unsigned char type = ELF_R_TYPE(rels->r_info);

	/*
	 * Ignore some relocations as these can be safely carried out
	 * twice (they simply override any existing data).  In fact,
	 * some relocations like __iob's copy relocation must be carried
	 * out each time the process restarts, otherwise stdio blows up.
	 */
	if ((type == R_386_COPY) || (type == R_386_NONE))
	    continue;

	/*
	 * If we are required to restore the relocation location
	 * to its value prior to relocation, then read the
	 * location's original contents from the input image and
	 * copy it to the output image.
	 */
	off = rels->r_offset - ircache->c_shdr->sh_addr;
	iaddr = (unsigned char *) ircache->c_data->d_buf + off;
	oaddr = (unsigned char *) orcache->c_data->d_buf + off;
	*(unsigned long *) oaddr = *(unsigned long *) iaddr;
    }
}
