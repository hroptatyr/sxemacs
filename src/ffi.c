/*
 * ffi.c --- Foreign Function Interface for SXEmacs.
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

#include <config.h>
#include "lisp.h"

#include "ffi.h"

/* Foreign types */
Lisp_Object Q_byte, Q_unsigned_byte;
Lisp_Object Q_char, Q_unsigned_char;
Lisp_Object Q_short, Q_unsigned_short;
Lisp_Object Q_int, Q_unsigned_int;
Lisp_Object Q_long, Q_unsigned_long;
Lisp_Object Q_pointer_void;
Lisp_Object Q_float, Q_double;
Lisp_Object Q_object;

DEFUN("ffi-size-of-type", Fffi_size_of_type, 1, 1, 0,	/*
Return size of foreign type TAG.

Valid foreign types are: `byte', `unsigned-byte', `char',
`unsigned-char', `short', `unsigned-short', `int', `unsigned-int',
`long', `unsigned-long', `pointer-void', `float', `double', `object'.
                                                        */
      (tag))
{
        int tsize;

        if (EQ(tag, Q_byte))
                tsize = sizeof(int8_t);
        else if (EQ(tag, Q_unsigned_byte))
                tsize = sizeof(uint8_t);
        else if (EQ(tag, Q_char))
                tsize = sizeof(char);
        else if (EQ(tag, Q_unsigned_char))
                tsize = sizeof(unsigned char);
        else if (EQ(tag, Q_short))
                tsize = sizeof(short);
        else if (EQ(tag, Q_unsigned_short))
                tsize = sizeof(unsigned short);
        else if (EQ(tag, Q_int))
                tsize = sizeof(int);
        else if (EQ(tag, Q_unsigned_int))
                tsize = sizeof(unsigned int);
        else if (EQ(tag, Q_long))
                tsize = sizeof(long);
        else if (EQ(tag, Q_unsigned_long))
                tsize = sizeof(unsigned long);
        else if (EQ(tag, Q_pointer_void))
                tsize = sizeof(void *);
        else if (EQ(tag, Q_object))
                tsize = sizeof(Lisp_Object);
        else if (EQ(tag, Q_float))
                tsize = sizeof(float);
        else if (EQ(tag, Q_double))
                tsize = sizeof(double);
        else
                signal_simple_error("Unrecognized foreign type", tag);

        return make_int(tsize);
}

void
syms_of_ffi(void)
{
	defsymbol(&Q_byte, "byte");
	defsymbol(&Q_unsigned_byte, "unsigned-byte");
	defsymbol(&Q_char, "char");
	defsymbol(&Q_unsigned_char, "unsigned-char");
	defsymbol(&Q_short, "short");
	defsymbol(&Q_unsigned_short, "unsigned-short");
	defsymbol(&Q_int, "int");
	defsymbol(&Q_unsigned_int, "unsigned-int");
	defsymbol(&Q_long, "long");
	defsymbol(&Q_unsigned_long, "unsigned-long");
	defsymbol(&Q_pointer_void, "pointer-void");
	defsymbol(&Q_object, "object");
	defsymbol(&Q_float, "float");
	defsymbol(&Q_double, "double");

	DEFSUBR(Fffi_size_of_type);
}

void
vars_of_ffi(void)
{
}
