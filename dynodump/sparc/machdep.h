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

#pragma ident	"@(#) $Id: machdep.h,v 1.3 1997/05/29 04:23:26 steve Exp $ - SMI"

/*
 * Global include file for all sgs SPARC machine dependent macros, constants
 * and declarations.
 */
#ifndef	MACHDEP_DOT_H
#define	MACHDEP_DOT_H

#include	<link.h>
#include	<sys/elf_SPARC.h>

/*
 * Make machine class dependent data types transparent to the common code
 */
#define	Word		Elf32_Word
#define	Sword		Elf32_Sword
#define	Half		Elf32_Half
#define	Addr		Elf32_Addr
#define	Off		Elf32_Off
#define	Byte		unsigned char

#define	Ehdr		Elf32_Ehdr
#define	Shdr		Elf32_Shdr
#define	Sym		Elf32_Sym
#define	Rel		Elf32_Rela
#define	Phdr		Elf32_Phdr
#define	Dyn		Elf32_Dyn
#define	Boot		Elf32_Boot
#define	Verdef		Elf32_Verdef
#define	Verdaux		Elf32_Verdaux
#define	Verneed		Elf32_Verneed
#define	Vernaux		Elf32_Vernaux
#define	Versym		Elf32_Versym

/*
 * Make machine class dependent functions transparent to the common code
 */
#define	ELF_R_TYPE	ELF32_R_TYPE
#define	ELF_R_INFO	ELF32_R_INFO
#define	ELF_R_SYM	ELF32_R_SYM
#define	ELF_ST_BIND	ELF32_ST_BIND
#define	ELF_ST_TYPE	ELF32_ST_TYPE
#define	ELF_ST_INFO	ELF32_ST_INFO
#define	elf_fsize	elf32_fsize
#define	elf_getehdr	elf32_getehdr
#define	elf_getphdr	elf32_getphdr
#define	elf_newehdr	elf32_newehdr
#define	elf_newphdr	elf32_newphdr
#define	elf_getshdr	elf32_getshdr

/*
 * Make relocation types transparent to the common code
 */
#define	M_REL_SHT_TYPE	SHT_RELA	/* section header type */

#endif
