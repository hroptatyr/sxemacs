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
#pragma ident	"@(#) $Id: syms.c,v 1.3 1997/05/29 04:22:30 steve Exp $ - SMI"

#include	<libelf.h>
#include	<string.h>
#include	"machdep.h"
#include	"_dynodump.h"

void
update_sym(Cache * cache, Cache * _cache, Addr edata)
{
    char *strs;
    Sym *syms;
    Shdr *shdr;
    int	symn, cnt;

    /*
     * Set up to read the symbol table and its associated string table.
     */
    shdr = _cache->c_shdr;
    syms = (Sym *) _cache->c_data->d_buf;
    symn = shdr->sh_size / shdr->sh_entsize;

    strs = (char *) cache[shdr->sh_link].c_data->d_buf;

    /*
     * Loop through the symbol table looking for `_end' and `_edata'.
     */
    for (cnt = 0; cnt < symn; cnt++, syms++) {
	char *name = strs + syms->st_name;

	if (strcmp(name, "_end") && strcmp(name, "_edata"))
	    continue;

	syms->st_value = edata;
    }
}
