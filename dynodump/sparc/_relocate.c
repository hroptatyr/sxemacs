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
 * Update the value of the `_edata' and `_end' symbols.
 */
#pragma ident	"@(#) $Id: _relocate.c,v 1.4 1998/03/31 20:10:55 steve Exp $ - SMI"

#include	<libelf.h>
#include	<string.h>
#include	"machdep.h"
#include	"_dynodump.h"


#define	GETbyte(P)	((long)((unsigned long)(P)[0]))
#define	GEThalf(P)	((long)(((unsigned long)(P)[0] << 8) | \
			    ((unsigned long)(P)[1])))
#define	GETword(P)	((long)(((unsigned long)(P)[0] << 24) | \
			    ((unsigned long)(P)[1] << 16) | \
			    ((unsigned long)(P)[2] << 8) | \
			    (unsigned long)(P)[3]))
#define	GETdisp30(P)	(GETword(P) & 0x3fffffff)
#define	GETdisp22(P)	(GETword(P) & 0x3fffff)
#define	GETdisp16(P)	(((GETword(P) & 0x300000) >> 6) | \
			    (GETword(P) & 0x3fff))
#define	GETdisp19(P)	(GETword(P) & 0x7ffff)
#define	GETimm22(P)	(GETword(P) & 0x3fffff)
#define	GETimm5(P)	(GEThalf((P)+2) & 0x1f)
#define	GETimm6(P)	(GEThalf((P)+2) & 0x2f)
#define	GETimm7(P)	(GEThalf((P)+2) & 0x3f)
#define	GETsimm13(P)	(GEThalf((P)+2) & 0x1fff)
#define	GETsimm10(P)	(GEThalf((P)+2) & 0x3ff)
#define	GETsimm11(P)	(GEThalf((P)+2) & 0x7ff)
#define	GETplt22(P)	(GETword((P)+8) & 0x3fffff)

#define	PUTbyte(V, P)	(P)[0] = (V)
#define	PUThalf(V, P)	(P)[0] = ((V) >> 8); \
			(P)[1] = ((V))
#define	PUTword(V, P)	(P)[0] = (unsigned char)((V) >> 24); \
			(P)[1] = (unsigned char)((V) >> 16); \
			(P)[2] = (unsigned char)((V) >> 8); \
			(P)[3] = (unsigned char)(V)
#define	PUTdisp30(V, P)	{ \
			unsigned long int temp; \
			temp = GETword(P) & ~0x3fffffff; \
			temp |= ((V) & 0x3fffffff); \
			PUTword(temp, P); \
			}
#define	PUTdisp22(V, P)	{ \
			unsigned long int temp; \
			temp = GETword(P) & ~0x3fffff; \
			temp |= ((V) & 0x3fffff); \
			PUTword(temp, P); \
			}
#define	PUTimm22(V, P)	{ \
			unsigned long int temp; \
			temp = GETword(P) & ~0x3fffff; \
			temp |= ((V) & 0x3fffff); \
			PUTword(temp, P); \
			}
#define	PUTimm5(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x1f; \
			temp |= ((V) & 0x1f); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTimm6(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x2f; \
			temp |= ((V) & 0x2f); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTimm7(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x3f; \
			temp |= ((V) & 0x3f); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTsimm13(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x1fff; \
			temp |= ((V) & 0x1fff); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTplt22(V, P)	{ \
			unsigned long int temp; \
			temp = GETword((P)+8) & ~0x3fffff; \
			temp |= ((V) & 0x3fffff); \
			PUTword(temp, ((P)+8)); \
			}
#define	PUTsimm10(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x3ff; \
			temp |= ((V) & 0x3ff); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTsimm11(V, P)	{ \
			unsigned long int temp; \
			temp = GEThalf(P+2) & ~0x7ff; \
			temp |= ((V) & 0x7ff); \
			PUThalf(temp, (P+2)); \
			}
#define	PUTdisp16(V, P) { \
			unsigned long int temp; \
			temp = GETword(P) & ~0x303fff; \
			temp |= ((V) & 0xc000) << 6; \
			temp |= ((V) & 0x3fff); \
			PUTword(temp, P); \
			}
#define	PUTdisp19(V, P)	{ \
			unsigned long int temp; \
			temp = GETword(P) & ~0x7ffff; \
			temp |= ((V) & 0x7ffff); \
			PUTword(temp, P); \
			}

static void
move_reloc(unsigned char * iaddr, unsigned char * oaddr, unsigned char type)
{
    switch (type) {
    case R_SPARC_8:
    case R_SPARC_DISP8:
	PUTbyte(GETbyte(iaddr), oaddr);
	break;

    case R_SPARC_16:
    case R_SPARC_DISP16:
	PUThalf(GEThalf(iaddr), oaddr);
	break;

    case R_SPARC_32:
    case R_SPARC_DISP32:
    case R_SPARC_GLOB_DAT:
    case R_SPARC_RELATIVE:
    case R_SPARC_UA32:
	PUTword(GETword(iaddr), oaddr);
	break;

    case R_SPARC_WDISP30:
    case R_SPARC_WPLT30:
	PUTdisp30(GETdisp30(iaddr), oaddr);
	break;

    case R_SPARC_WDISP22:
    case R_SPARC_PC22:
	PUTdisp22(GETdisp22(iaddr), oaddr);
	break;

    case R_SPARC_HI22:
    case R_SPARC_GOT22:
    case R_SPARC_22:
	PUTimm22(GETimm22(iaddr), oaddr);
	break;

    case R_SPARC_13:
    case R_SPARC_GOT13:
	PUTsimm13(GETsimm13(iaddr), oaddr);
	break;

    case R_SPARC_LO10:
    case R_SPARC_GOT10:
    case R_SPARC_PC10:
#ifdef R_SPARC_10
    case R_SPARC_10:
#endif
	PUTsimm10(GETsimm10(iaddr), oaddr);
	break;

#ifdef R_SPARC_11
    case R_SPARC_11:
	PUTsimm11(GETsimm11(iaddr), oaddr);
	break;
#endif

#ifdef R_SPARC_WDISP16
    case R_SPARC_WDISP16:
	PUTdisp16(GETdisp16(iaddr), oaddr);
	break;
#endif

#ifdef R_SPARC_WDISP19
    case R_SPARC_WDISP19:
	PUTdisp19(GETdisp19(iaddr), oaddr);
	break;
#endif

#ifdef R_SPARC_5
    case R_SPARC_5:
	PUTimm5(GETimm5(iaddr), oaddr);
	break;
#endif

#ifdef R_SPARC_6
    case R_SPARC_6:
	PUTimm6(GETimm6(iaddr), oaddr);
	break;
#endif

#ifdef R_SPARC_7
    case R_SPARC_7:
	PUTimm7(GETimm7(iaddr), oaddr);
	break;
#endif

    default:
	break;
    }
}

void
update_reloc(Cache *ocache, Cache *_ocache,
	     Cache *icache, Cache *_icache,
	     Half shnum)
{
    Shdr *shdr;
    Rel *rels;
    int	reln, cnt;
    Cache *orcache, *ircache;

    /*
     * Set up to read the output relocation table.
     */
    shdr = _ocache->c_shdr;
    rels = (Rel *)_ocache->c_data->d_buf;
    reln = shdr->sh_size / shdr->sh_entsize;

    /*
     * Determine the section that is being relocated.
     */
    orcache = &ocache[shdr->sh_info];
    shdr = _icache->c_shdr;
    ircache = &icache[shdr->sh_info];

    /*
     * Loop through the relocation table.
     */
    for (cnt = 0; cnt < reln; cnt++, rels++) {
	unsigned char *iaddr, *oaddr;
	Addr off;
	unsigned char type = ELF_R_TYPE(rels->r_info);

	/*
	 * Ignore some relocations as these can safely be carried out
	 * twice (they simply override any existing data).  In fact,
	 * some relocations like __iob's copy relocation must be carried
	 * out each time the process restarts otherwise stdio blows up.
	 */
	if ((type == R_SPARC_COPY) || (type == R_SPARC_JMP_SLOT) ||
	    (type == R_SPARC_NONE))
	    continue;

	/*
	 * If we are required to restore the relocation location
	 * to its value prior to relocation, then read the
	 * locations original contents from the input image and
	 * copy it to the output image.
	 */
	off = rels->r_offset - ircache->c_shdr->sh_addr;
	iaddr = (unsigned char *)ircache->c_data->d_buf + off;
	oaddr = (unsigned char *)orcache->c_data->d_buf + off;
	move_reloc(iaddr, oaddr, type);
    }
}
