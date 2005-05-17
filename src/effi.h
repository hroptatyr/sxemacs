/*
 * effi.h --- Header for Foreign Function Interface.
 *
 * Copyright (C) 2004 by XWEM Org.
 *
 * This file is part of SXEmacs.
 * 
 * SXEmacs is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with SXEmacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#ifndef _SXEMACS_EFFI_H_
#define _SXEMACS_EFFI_H_

#ifndef EFFI_STACK_SIZE
#define EFFI_STACK_SIZE 4096
#endif  /* FFI_STACK_SIZE */

/** Errors **/
#define EFFI_OK  0
#define EFFI_ARG 1
#define EFFI_ERR 2

struct Lisp_EffiObject {
        struct lcrecord_header header;
        Lisp_Object type;               /* type of ffi object */
        Lisp_Object size;               /* size of ffi object type */

	Lisp_Object plist;              /* properties list */

        /* Foreign stuff */
        int fotype;
#define EFFI_FOT_NONE  0                 /* fop is not used */
#define EFFI_FOT_ALLOC 1                 /* fop is allocated using `ffi-alloc' */
#define EFFI_FOT_BIND  2                 /* fop is reference to foreign data */
#define EFFI_FOT_FUNC  3                 /* fop is pointer to function */

        /*
         * Declared as union just for the style, there no problem to
         * always use fop.generic to access pointer.
         */
        union {
                void *ptr;              /* pointer to foreign data */
		void *fun;              /* pointer to foreign function */
                void *generic;          /* generic storer */
        } fop;
        char fostorage[16];             /* tiny storage */
};

#define EFFIO_HAS_FOP(fo) ((fo)->fotype != EFFI_FOT_NONE)

typedef struct Lisp_EffiObject Lisp_EffiObject;

DECLARE_LRECORD(ffiobject, Lisp_EffiObject);
#define XEFFIO(x) XRECORD (x, ffiobject, Lisp_EffiObject)
#define XSETEFFIO(x, p) XSETRECORD (x, p, ffiobject)
#define EFFIOP(x) RECORDP (x, ffiobject)
#define CHECK_EFFIO(x) CHECK_RECORD (x, ffiobject)
#define CONCHECK_EFFIO(x) CONCHECK_RECORD (x, ffiobject)

EXFUN(Fffi_slot_offset, 2);
EXFUN(Fffi_size_of_type, 1);
EXFUN(Fffi_plist, 1);

#endif /* _SXEMACS_FFI_H_ */
