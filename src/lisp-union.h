/* Fundamental definitions for XEmacs Lisp interpreter -- union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.

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

/* Divergent from FSF.  */

/* Definition of Lisp_Object type as a union.
   The declaration order of the objects within the struct members
   of the union is dependent on ENDIAN-ness.
   See lisp-disunion.h for more details.  */

typedef
union Lisp_Object
{
  /* if non-valbits are at lower addresses */
#ifdef WORDS_BIGENDIAN
  struct
  {
    EMACS_UINT val : VALBITS;
    enum_field (Lisp_Type) type : GCTYPEBITS;
  } gu;

  struct
  {
    signed EMACS_INT val : INT_VALBITS;
    unsigned int bits : INT_GCBITS;
  } s;

  struct
  {
    EMACS_UINT val : INT_VALBITS;
    unsigned int bits : INT_GCBITS;
  } u;
#else /* non-valbits are at higher addresses */
  struct
  {
    enum_field (Lisp_Type) type : GCTYPEBITS;
    EMACS_UINT val : VALBITS;
  } gu;

  struct
  {
    unsigned int bits : INT_GCBITS;
    signed EMACS_INT val : INT_VALBITS;
  } s;

  struct
  {
    unsigned int bits : INT_GCBITS;
    EMACS_UINT val : INT_VALBITS;
  } u;

#endif /* non-valbits are at higher addresses */

  EMACS_UINT ui;
  signed EMACS_INT i;

  /* This was formerly declared 'void *v' etc. but that causes
     GCC to accept any (yes, any) pointer as the argument of
     a function declared to accept a Lisp_Object. */
  struct nosuchstruct *v;
  const struct nosuchstruct *cv;
}
Lisp_Object;

#define XCHARVAL(x) ((x).gu.val)

# define XSETINT(var, value) do {	\
  EMACS_INT xset_value = (value);	\
  Lisp_Object *xset_var = &(var);	\
  xset_var->s.bits = 1;			\
  xset_var->s.val = xset_value;		\
} while (0)
# define XSETCHAR(var, value) do {	\
  Emchar xset_value = (value);		\
  Lisp_Object *xset_var = &(var);	\
  xset_var->gu.type = Lisp_Type_Char;	\
  xset_var->gu.val = xset_value;	\
} while (0)
# define XSETOBJ(var, value) do {		\
  EMACS_UINT xset_value = (EMACS_UINT) (value);	\
  (var).ui = xset_value;			\
} while (0)
# define XPNTRVAL(x) ((x).ui)

INLINE_HEADER Lisp_Object make_int (EMACS_INT val);
INLINE_HEADER Lisp_Object
make_int (EMACS_INT val)
{
  Lisp_Object obj;
  XSETINT (obj, val);
  return obj;
}

INLINE_HEADER Lisp_Object make_char (Emchar val);
INLINE_HEADER Lisp_Object
make_char (Emchar val)
{
  Lisp_Object obj;
  XSETCHAR (obj, val);
  return obj;
}

INLINE_HEADER Lisp_Object wrap_object (void *ptr);
INLINE_HEADER Lisp_Object
wrap_object (void *ptr)
{
  Lisp_Object obj;
  XSETOBJ (obj, ptr);
  return obj;
}

extern Lisp_Object Qnull_pointer, Qzero;

#define XREALINT(x) ((x).s.val)
#define XUINT(x) ((x).u.val)
#define XTYPE(x) ((x).gu.type)
#define EQ(x,y) ((x).v == (y).v)

#define INTP(x) ((x).s.bits)
#define INT_PLUS(x,y)  make_int (XINT (x) + XINT (y))
#define INT_MINUS(x,y) make_int (XINT (x) - XINT (y))
#define INT_PLUS1(x)   make_int (XINT (x) + 1)
#define INT_MINUS1(x)  make_int (XINT (x) - 1)

/* Convert between a (void *) and a Lisp_Object, as when the
   Lisp_Object is passed to a toolkit callback function */
#define VOID_TO_LISP(larg,varg) \
     ((void) ((larg).v = (struct nosuchstruct *) (varg)))
#define CVOID_TO_LISP(larg,varg) \
     ((void) ((larg).cv = (const struct nosuchstruct *) (varg)))
#define LISP_TO_VOID(larg) ((void *) ((larg).v))
#define LISP_TO_CVOID(larg) ((const void *) ((larg).cv))

/* Convert a Lisp_Object into something that can't be used as an
   lvalue.  Useful for type-checking. */
#if (__GNUC__ > 1)
#define NON_LVALUE(larg) ({ (larg); })
#else
/* Well, you can't really do it without using a function call, and
   there's no real point in that; no-union-type is the rule, and that
   will catch errors. */
#define NON_LVALUE(larg) (larg)
#endif
