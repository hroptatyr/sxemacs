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

#pragma ident	"@(#) $Id: _dynodump.h,v 1.3 1997/05/29 04:22:29 steve Exp $ - SMI"

#ifndef	_DYNODUMP_DOT_H
#define	_DYNODUMP_DOT_H

#include	<libelf.h>
#include	"machdep.h"

/* General rounding macro */
#define	S_ROUND(x, a)   (((int)(x) + (((int)(a) ? (int)(a) : 1) - 1)) & \
			 ~(((int)(a) ? (int)(a) : 1) - 1))

/*
 * Define a cache structure that is used to retain all elf section information.
 */
typedef struct cache {
    Elf_Scn *c_scn;
    Shdr *c_shdr;
    Elf_Data *c_data;
    char *c_name;
} Cache;

/*
 * Define any local prototypes.
 */
extern void update_dynamic(Cache *);
extern void update_reloc(Cache *, Cache *, Cache *, Cache *, Half shnum);
extern void update_sym(Cache *, Cache *, Addr);
extern void dynodump_uninit(void);

#endif
