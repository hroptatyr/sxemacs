/* Opaque Lisp objects.
   Copyright (C) 1993 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing, October 1993. */

#ifndef INCLUDED_opaque_h_
#define INCLUDED_opaque_h_

typedef struct Lisp_Opaque
{
  struct lcrecord_header header;
  size_t size;
  max_align_t data[1];
} Lisp_Opaque;

DECLARE_LRECORD (opaque, Lisp_Opaque);
#define XOPAQUE(x) XRECORD (x, opaque, Lisp_Opaque)
#define XSETOPAQUE(x, p) XSETRECORD (x, p, opaque)
#define OPAQUEP(x) RECORDP (x, opaque)
/* #define CHECK_OPAQUE(x) CHECK_RECORD (x, opaque)
   Opaque pointers should never escape to the Lisp level, so
   functions should not be doing this. */

/* Alternative DATA arguments to make_opaque() */
#define OPAQUE_CLEAR  ((const void *)  0)
#define OPAQUE_UNINIT ((const void *) -1)

#define OPAQUE_SIZE(op) ((op)->size)
#define OPAQUE_DATA(op) ((void *) ((op)->data))
#define XOPAQUE_SIZE(op) OPAQUE_SIZE (XOPAQUE (op))
#define XOPAQUE_DATA(op) OPAQUE_DATA (XOPAQUE (op))

Lisp_Object make_opaque (const void *data, size_t size);

typedef struct Lisp_Opaque_Ptr
{
  struct lcrecord_header header;
  void *ptr;
} Lisp_Opaque_Ptr;

DECLARE_LRECORD (opaque_ptr, Lisp_Opaque_Ptr);
#define XOPAQUE_PTR(x) XRECORD (x, opaque_ptr, Lisp_Opaque_Ptr)
#define XSETOPAQUE_PTR(x, p) XSETRECORD (x, p, opaque_ptr)
#define OPAQUE_PTRP(x) RECORDP (x, opaque_ptr)

Lisp_Object make_opaque_ptr (void *val);
void free_opaque_ptr (Lisp_Object ptr);

#define get_opaque_ptr(op) (XOPAQUE_PTR (op)->ptr)
#define set_opaque_ptr(op, ptr_) (XOPAQUE_PTR (op)->ptr = (ptr_))

#endif /* INCLUDED_opaque_h_ */
