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

#pragma ident "@(#) $Id: _relocate.c,v 1.4 1998/03/31 20:10:55 steve Exp $ - SMI"

/* LINTLIBRARY */

#include	<string.h>
#include	<sys/elf_ppc.h>
#include	"_dynodump.h"


/*
 *	NOTE: These macros will work reliably only on 32-bit 2's
 *	complement machines. The type of P in all cases should
 *	by unsigned char *
 */
#if defined(_BIG_ENDIAN)

#define	GET4(P)	((long)(((unsigned long)(P)[0] << 24) | \
			((unsigned long)(P)[1] << 16) | \
			((unsigned long)(P)[2] << 8) | \
			(unsigned long)(P)[3]))
#define	PUT4(V, P)	{ \
				(P)[0] = (unsigned char)((V) >> 24); \
				(P)[1] = (unsigned char)((V) >> 16); \
				(P)[2] = (unsigned char)((V) >> 8); \
				(P)[3] = (unsigned char)(V); \
			}
#define	GEThalf(P)	((long)	(((unsigned long) (P)[0] << 8) | \
				((unsigned long) (P)[1])))
#define	GETword(P)	GET4(P)
#define	PUThalf(V, P)	{ \
				(P)[0] = ((V) >> 8); \
				(P)[1] = ((V)); \
			}
#define	PUTword(V, P)	PUT4(V, P)

#elif defined(_LITTLE_ENDIAN)

#define	GET4(P)	((long)(((unsigned long)(P)[0]) | \
			((unsigned long)(P)[1] << 8) | \
			((unsigned long)(P)[2] << 16) | \
			((unsigned long)(P)[3]) << 24))
#define	PUT4(V, P)	{ \
				(P)[0] = (unsigned char)(V); \
				(P)[1] = (unsigned char)((V) >> 8); \
				(P)[2] = (unsigned char)((V) >> 16); \
				(P)[3] = (unsigned char)((V) >> 24); \
			}
#define	GEThalf(P)	((long)	(((unsigned long) (P)[0]) | \
				((unsigned long) (P)[1] << 8)))
#define	GETword(P)	GET4(P)
#define	PUThalf(V, P)	{ \
				(P)[0] = (V); \
				(P)[1] = ((V) >> 8); \
			}
#define	PUTword(V, P)	PUT4(V, P)

#endif /* defined(_LITTLE_ENDIAN) */

/*
 * NAME			VALUE	FIELD		CALCULATION
 *
 * R_PPC_NONE			0	none		none
 * R_PPC_ADDR32			1	word32		S + A
 * R_PPC_ADDR24			2	low24		(S + A) >> 2
 * R_PPC_ADDR16			3	half16		S + A
 * R_PPC_ADDR16_LO		4	half16		#lo(S + A)
 * R_PPC_ADDR16_HI		5	half16		#hi(S + A)
 * R_PPC_ADDR16_HA		6	half16		#ha(S + A)
 * R_PPC_ADDR14			7	low14		(S + A) >> 2
 * R_PPC_ADDR14_BRTAKEN		8	low14		(S + A) >> 2
 * R_PPC_ADDR14_BRNTAKEN	9	low14		(S + A) >> 2
 * R_PPC_REL24			10	low24		(S + A - P) >> 2
 * R_PPC_REL14			11	low14		(S + A - P) >> 2
 * R_PPC_REL14_BRTAKEN		12	low14		(S + A - P) >> 2
 * R_PPC_REL14_BRNTAKEN		13	low14		(S + A - P) >> 2
 * R_PPC_GOT16			14	half16		G + A
 * R_PPC_GOT16_LO		15	half16		#lo(G + A)
 * R_PPC_GOT16_HI		16	half16		#hi(G + A)
 * R_PPC_GOT16_HA		17	half16		#ha(G + A)
 * R_PPC_PLT24			18	low24		(L + A - P) >> 2
 * R_PPC_COPY			19	none		none
 * R_PPC_GLOB_DAT		20	word32		S + A
 * R_PPC_JMP_SLOT		21	none		see below
 * R_PPC_RELATIVE		22	word32		B + A
 * R_PPC_LOCAL24PC		23	low24		see below
 * R_PPC_UADDR32		24	word32		S + A
 * R_PPC_UADDR16		25	half16		S + A
 *
 *	This is Figure 4-3: Relocation Types from the Draft Copy of
 * the ABI, Printed on 7/25/94.
 *
 *	The field column specifies how much of the data
 * at the reference address is to be used. The data are assumed to be
 * right-justified with the least significant bit at the right.
 *	In the case of plt24 addresses, the reference address is
 * assumed to be that of a 6-word PLT entry. The address is the right-
 * most 24 bits of the third word.
 */
static void
move_reloc(unsigned char *iaddr, unsigned char *oaddr, unsigned char type)
{
    switch(type) {
    case R_PPC_NONE:
	break;

    case R_PPC_ADDR32:
    case R_PPC_UADDR32:
	PUTword(GETword(iaddr), oaddr);
	break;

    case R_PPC_ADDR24:
    case R_PPC_REL24:
    case R_PPC_PLT24:
    case R_PPC_LOCAL24PC:
	/* XXX - big assumption here that the original contents were masked
	 * properly.  If this assumption proves correct, then these 24bit
	 * cases can be folded into the above 32bit cases.
	 */
	PUTword(GETword(iaddr), oaddr);
	break;

    case R_PPC_ADDR16:
    case R_PPC_UADDR16:
    case R_PPC_GOT16:
	PUThalf(GEThalf(iaddr), oaddr);
	break;

    case R_PPC_ADDR16_LO:
    case R_PPC_GOT16_LO:
	/* XXX - more assumptions which if proved correct, we can
	 * do some folding with above cases
	 */
	PUThalf(GEThalf(iaddr), oaddr);
	break;

    case R_PPC_ADDR16_HI:
    case R_PPC_GOT16_HI:
	/* XXX - more assumptions which if proved correct, we can
	 * do some folding with above cases
	 */
	PUThalf(GEThalf(iaddr), oaddr);
	break;

    case R_PPC_ADDR16_HA:
    case R_PPC_GOT16_HA:
	/* XXX - more assumptions which if proved correct, we can
	 * do some folding with above cases
	 */
	PUThalf(GEThalf(iaddr), oaddr);
	break;

    case R_PPC_ADDR14:
    case R_PPC_ADDR14_BRTAKEN:
    case R_PPC_ADDR14_BRNTAKEN:
    case R_PPC_REL14:
    case R_PPC_REL14_BRTAKEN:
    case R_PPC_REL14_BRNTAKEN:
	/* XXX - big assumption here that the original contents were masked
	 * properly.  If this assumption proves correct, then these 14bit
	 * cases can be folded into the above 32bit cases.
	 */
	PUTword(GETword(iaddr), oaddr);
	break;

    case R_PPC_COPY:
	break;

    case R_PPC_GLOB_DAT:
    case R_PPC_RELATIVE:
	PUTword(GETword(iaddr), oaddr);
	break;

    case R_PPC_JMP_SLOT:
	break;

    default:
	break;
    }
}

void
update_reloc(Cache *ocache, Cache *_ocache, Cache *icache, Cache *_icache, Half shnum)
{
    Shdr *shdr;
    Rel *rels;
    int	reln, cnt;
    Cache *orcache, * ircache;

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
	unsigned char	type = ELF_R_TYPE(rels->r_info);

	/*
	 * Ignore some relocations as these can safely be carried out
	 * twice (they simply override any existing data).  In fact,
	 * some relocations like __iob's copy relocation must be carried
	 * out each time the process restarts otherwise stdio blows up.
	 */
	if ((type == R_PPC_COPY) || (type == R_PPC_JMP_SLOT) ||
	    (type == R_PPC_NONE))
	    continue;

	{
	    unsigned char *iaddr, *oaddr;
	    Addr off;

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
}
