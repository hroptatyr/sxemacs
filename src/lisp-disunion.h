/* Fundamental definitions for XEmacs Lisp interpreter -- non-union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: FSF 19.30.  Split out from lisp.h. */
/* This file has diverged greatly from FSF Emacs.  Syncing is no
   longer desirable or possible */

/*
 Format of a non-union-type Lisp Object

	     3         2         1         0
       bit  10987654321098765432109876543210
	    --------------------------------
	    VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVTT

   Integers are treated specially, and look like this:

	     3         2         1         0
       bit  10987654321098765432109876543210
	    --------------------------------
	    VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVT

 For integral Lisp types, i.e. integers and characters, the value
 bits are the Lisp object.  Some people call such Lisp_Objects "immediate".

 The object is obtained by masking off the type bits.
     Bit 1 is used as a value bit by splitting the Lisp integer type
 into two subtypes, Lisp_Type_Int_Even and Lisp_Type_Int_Odd.
 By this trickery we get 31 bits for integers instead of 30.

 For non-integral types, the value bits of a Lisp_Object contain
 a pointer to a structure containing the object.  The pointer is
 obtained by masking off the type and mark bits.

     All pointer-based types are coalesced under a single type called
 Lisp_Type_Record.  The type bits for this type are required by the
 implementation to be 00, just like the least significant bits of
 word-aligned struct pointers on 32-bit hardware.  This requires that
 all structs implementing Lisp_Objects have an alignment of at least 4
 bytes.  Because of this, Lisp_Object pointers don't have to be masked
 and are full-sized.

 There are no mark bits in the Lisp_Object itself (there used to be).

 Integers and characters don't need to be marked.  All other types are
 lrecord-based, which means they get marked by setting the mark bit in
 the struct lrecord_header.

 Here is a brief description of the following macros:

 XTYPE     The type bits of a Lisp_Object
 XPNTRVAL  The value bits of a Lisp_Object storing a pointer
 XCHARVAL  The value bits of a Lisp_Object storing a Emchar
 XREALINT  The value bits of a Lisp_Object storing an integer, signed
 XUINT     The value bits of a Lisp_Object storing an integer, unsigned
 INTP      Non-zero if this Lisp_Object is an integer
 Qzero     Lisp Integer 0
 EQ        Non-zero if two Lisp_Objects are identical, not merely equal. */

typedef EMACS_INT Lisp_Object;

#define Lisp_Type_Int_Bit (Lisp_Type_Int_Even & Lisp_Type_Int_Odd)
extern_inline Lisp_Object
wrap_object(void *ptr)
	__attribute__((always_inline));
extern_inline Lisp_Object
wrap_object(void *ptr)
{
	return (Lisp_Object)ptr;
}
#define make_int(x) ((Lisp_Object) (((x) << INT_GCBITS) | Lisp_Type_Int_Bit))
#define make_char(x) ((Lisp_Object) (((x) << GCBITS) | Lisp_Type_Char))
#define VALMASK (((1UL << VALBITS) - 1UL) << GCTYPEBITS)
#define XTYPE(x) ((enum Lisp_Type) (((EMACS_UINT)(x)) & ~VALMASK))
#define XPNTRVAL(x) (x)		/* This depends on Lisp_Type_Record == 0 */
#define XCHARVAL(x) ((x) >> GCBITS)
#define XREALINT(x) ((x) >> INT_GCBITS)
#define XUINT(x) ((EMACS_UINT)(x) >> INT_GCBITS)
#define INTP(x) ((EMACS_UINT)(x) & Lisp_Type_Int_Bit)
#define INT_PLUS(x,y)  ((x)+(y)-Lisp_Type_Int_Bit)
#define INT_MINUS(x,y) ((x)-(y)+Lisp_Type_Int_Bit)
#define INT_PLUS1(x)   INT_PLUS  (x, make_int (1))
#define INT_MINUS1(x)  INT_MINUS (x, make_int (1))

#define Qnull_pointer ((Lisp_Object) 0)
#define EQ(x,y) ((x) == (y))
#define XSETINT(var, value) ((void) ((var) = make_int (value)))
#define XSETCHAR(var, value) ((void) ((var) = make_char (value)))
#define XSETOBJ(var, value) ((void) ((var) = wrap_object (value)))

/* Convert between a (void *) and a Lisp_Object, as when the
   Lisp_Object is passed to a toolkit callback function */
#define VOID_TO_LISP(larg,varg) ((void) ((larg) = ((Lisp_Object) (varg))))
#define CVOID_TO_LISP VOID_TO_LISP
#define LISP_TO_VOID(larg) ((void *) (larg))
#define LISP_TO_CVOID(larg) ((const void *) (larg))

/* Convert a Lisp_Object into something that can't be used as an
   lvalue.  Useful for type-checking. */
#define NON_LVALUE(larg) ((larg) + 0)
