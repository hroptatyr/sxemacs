/* SXEmacs routines to deal with char tables.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995, 1997, 1999 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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


/* Synched up with: Mule 2.3.  Not synched with FSF.

   This file was written independently of the FSF implementation,
   and is not compatible. */

/* Authorship:

   Ben Wing: wrote, for 19.13 (Mule).  Some category table stuff
	     loosely based on the original Mule.
   Jareth Hein: fixed a couple of bugs in the implementation, and
	     added regex support for categories with check_category_at
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "chartab.h"
#include "syntax.h"

Lisp_Object Qchar_tablep, Qchar_table;

Lisp_Object Vall_syntax_tables;

#ifdef MULE
Lisp_Object Qcategory_table_p;
Lisp_Object Qcategory_designator_p;
Lisp_Object Qcategory_table_value_p;

Lisp_Object Vstandard_category_table;

/* Variables to determine word boundary.  */
Lisp_Object Vword_combining_categories, Vword_separating_categories;
#endif				/* MULE */

/* A char table maps from ranges of characters to values.

   Implementing a general data structure that maps from arbitrary
   ranges of numbers to values is tricky to do efficiently.  As it
   happens, it should suffice (and is usually more convenient, anyway)
   when dealing with characters to restrict the sorts of ranges that
   can be assigned values, as follows:

   1) All characters.
   2) All characters in a charset.
   3) All characters in a particular row of a charset, where a "row"
      means all characters with the same first byte.
   4) A particular character in a charset.

   We use char tables to generalize the 256-element vectors now
   littering the Emacs code.

   Possible uses (all should be converted at some point):

   1) category tables
   2) syntax tables
   3) display tables
   4) case tables
   5) keyboard-translate-table?

   We provide an
   abstract type to generalize the Emacs vectors and Mule
   vectors-of-vectors goo.
   */

/************************************************************************/
/*                         Char Table object                            */
/************************************************************************/

#ifdef MULE

static Lisp_Object mark_char_table_entry(Lisp_Object obj)
{
	Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY(obj);
	int i;

	for (i = 0; i < 96; i++) {
		mark_object(cte->level2[i]);
	}
	return Qnil;
}

static int char_table_entry_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Char_Table_Entry *cte1 = XCHAR_TABLE_ENTRY(obj1);
	Lisp_Char_Table_Entry *cte2 = XCHAR_TABLE_ENTRY(obj2);
	int i;

	for (i = 0; i < 96; i++)
		if (!internal_equal
		    (cte1->level2[i], cte2->level2[i], depth + 1))
			return 0;

	return 1;
}

static unsigned long char_table_entry_hash(Lisp_Object obj, int depth)
{
	Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY(obj);

	return internal_array_hash(cte->level2, 96, depth);
}

static const struct lrecord_description char_table_entry_description[] = {
	{XD_LISP_OBJECT_ARRAY, offsetof(Lisp_Char_Table_Entry, level2), 96},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("char-table-entry", char_table_entry,
			      mark_char_table_entry, internal_object_printer,
			      0, char_table_entry_equal,
			      char_table_entry_hash,
			      char_table_entry_description,
			      Lisp_Char_Table_Entry);
#endif				/* MULE */

static Lisp_Object mark_char_table(Lisp_Object obj)
{
	Lisp_Char_Table *ct = XCHAR_TABLE(obj);
	int i;

	for (i = 0; i < NUM_ASCII_CHARS; i++)
		mark_object(ct->ascii[i]);
#ifdef MULE
	for (i = 0; i < NUM_LEADING_BYTES; i++)
		mark_object(ct->level1[i]);
#endif
	return ct->mirror_table;
}

/* WARNING: All functions of this nature need to be written extremely
   carefully to avoid crashes during GC.  Cf. prune_specifiers()
   and prune_weak_hash_tables(). */

void prune_syntax_tables(void)
{
	Lisp_Object rest, prev = Qnil;

	for (rest = Vall_syntax_tables;
	     !NILP(rest); rest = XCHAR_TABLE(rest)->next_table) {
		if (!marked_p(rest)) {
			/* This table is garbage.  Remove it from the list. */
			if (NILP(prev))
				Vall_syntax_tables =
				    XCHAR_TABLE(rest)->next_table;
			else
				XCHAR_TABLE(prev)->next_table =
				    XCHAR_TABLE(rest)->next_table;
		}
	}
}

static Lisp_Object char_table_type_to_symbol(enum char_table_type type)
{
	switch (type) {
	default:
		abort();
		return Qnil;
	case CHAR_TABLE_TYPE_GENERIC:
		return Qgeneric;
	case CHAR_TABLE_TYPE_SYNTAX:
		return Qsyntax;
	case CHAR_TABLE_TYPE_DISPLAY:
		return Qdisplay;
	case CHAR_TABLE_TYPE_CHAR:
		return Qchar;
#ifdef MULE
	case CHAR_TABLE_TYPE_CATEGORY:
		return Qcategory;
#endif
	}
}

static enum char_table_type symbol_to_char_table_type(Lisp_Object symbol)
{
	CHECK_SYMBOL(symbol);

	if (EQ(symbol, Qgeneric))
		return CHAR_TABLE_TYPE_GENERIC;
	if (EQ(symbol, Qsyntax))
		return CHAR_TABLE_TYPE_SYNTAX;
	if (EQ(symbol, Qdisplay))
		return CHAR_TABLE_TYPE_DISPLAY;
	if (EQ(symbol, Qchar))
		return CHAR_TABLE_TYPE_CHAR;
#ifdef MULE
	if (EQ(symbol, Qcategory))
		return CHAR_TABLE_TYPE_CATEGORY;
#endif

	signal_simple_error("Unrecognized char table type", symbol);
	return CHAR_TABLE_TYPE_GENERIC;	/* not reached */
}

static void
print_chartab_range(Emchar first, Emchar last, Lisp_Object val,
		    Lisp_Object printcharfun)
{
	if (first != last) {
		write_c_string(" (", printcharfun);
		print_internal(make_char(first), printcharfun, 0);
		write_c_string(" ", printcharfun);
		print_internal(make_char(last), printcharfun, 0);
		write_c_string(") ", printcharfun);
	} else {
		write_c_string(" ", printcharfun);
		print_internal(make_char(first), printcharfun, 0);
		write_c_string(" ", printcharfun);
	}
	print_internal(val, printcharfun, 1);
}

#ifdef MULE

static void
print_chartab_charset_row(Lisp_Object charset,
			  int row,
			  Lisp_Char_Table_Entry * cte, Lisp_Object printcharfun)
{
	int i;
	Lisp_Object cat = Qunbound;
	int first = -1;

	for (i = 32; i < 128; i++) {
		Lisp_Object pam = cte->level2[i - 32];

		if (first == -1) {
			first = i;
			cat = pam;
			continue;
		}

		if (!EQ(cat, pam)) {
			if (row == -1)
				print_chartab_range(MAKE_CHAR
						    (charset, first, 0),
						    MAKE_CHAR(charset, i - 1,
							      0), cat,
						    printcharfun);
			else
				print_chartab_range(MAKE_CHAR
						    (charset, row, first),
						    MAKE_CHAR(charset, row,
							      i - 1), cat,
						    printcharfun);
			first = -1;
			i--;
		}
	}

	if (first != -1) {
		if (row == -1)
			print_chartab_range(MAKE_CHAR(charset, first, 0),
					    MAKE_CHAR(charset, i - 1, 0),
					    cat, printcharfun);
		else
			print_chartab_range(MAKE_CHAR(charset, row, first),
					    MAKE_CHAR(charset, row, i - 1),
					    cat, printcharfun);
	}
}

static void
print_chartab_two_byte_charset(Lisp_Object charset,
			       Lisp_Char_Table_Entry * cte,
			       Lisp_Object printcharfun)
{
	int i;

	for (i = 32; i < 128; i++) {
		Lisp_Object jen = cte->level2[i - 32];

		if (!CHAR_TABLE_ENTRYP(jen)) {
			write_c_string(" [", printcharfun);
			print_internal(XCHARSET_NAME(charset), printcharfun, 0);
			write_fmt_str(printcharfun, " %d] ", i);
			print_internal(jen, printcharfun, 0);
		} else
			print_chartab_charset_row(charset, i,
						  XCHAR_TABLE_ENTRY(jen),
						  printcharfun);
	}
}

#endif				/* MULE */

static void
print_char_table(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Char_Table *ct = XCHAR_TABLE(obj);
	Lisp_Object tmp_sym = char_table_type_to_symbol(ct->type);

	write_fmt_string(printcharfun, "#s(char-table type %s data (",
			 string_data(symbol_name(XSYMBOL(tmp_sym))));

	/* Now write out the ASCII/Control-1 stuff. */
	{
		int i;
		int first = -1;
		Lisp_Object val = Qunbound;

		for (i = 0; i < NUM_ASCII_CHARS; i++) {
			if (first == -1) {
				first = i;
				val = ct->ascii[i];
				continue;
			}

			if (!EQ(ct->ascii[i], val)) {
				print_chartab_range(first, i - 1, val,
						    printcharfun);
				first = -1;
				i--;
			}
		}

		if (first != -1)
			print_chartab_range(first, i - 1, val, printcharfun);
	}

#ifdef MULE
	{
		int i;

		for (i = MIN_LEADING_BYTE;
		     i < MIN_LEADING_BYTE + NUM_LEADING_BYTES; i++) {
			Lisp_Object ann = ct->level1[i - MIN_LEADING_BYTE];
			Lisp_Object charset = CHARSET_BY_LEADING_BYTE(i);

			if (!CHARSETP(charset) || i == LEADING_BYTE_ASCII
			    || i == LEADING_BYTE_CONTROL_1)
				continue;
			if (!CHAR_TABLE_ENTRYP(ann)) {
				write_c_string(" ", printcharfun);
				print_internal(XCHARSET_NAME(charset),
					       printcharfun, 0);
				write_c_string(" ", printcharfun);
				print_internal(ann, printcharfun, 0);
			} else {
				Lisp_Char_Table_Entry *cte =
				    XCHAR_TABLE_ENTRY(ann);
				if (XCHARSET_DIMENSION(charset) == 1)
					print_chartab_charset_row(charset, -1,
								  cte,
								  printcharfun);
				else
					print_chartab_two_byte_charset(charset,
								       cte,
								       printcharfun);
			}
		}
	}
#endif				/* MULE */

	write_c_string("))", printcharfun);
}

static int char_table_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	Lisp_Char_Table *ct1 = XCHAR_TABLE(obj1);
	Lisp_Char_Table *ct2 = XCHAR_TABLE(obj2);
	int i;

	if (CHAR_TABLE_TYPE(ct1) != CHAR_TABLE_TYPE(ct2))
		return 0;

	for (i = 0; i < NUM_ASCII_CHARS; i++)
		if (!internal_equal(ct1->ascii[i], ct2->ascii[i], depth + 1))
			return 0;

#ifdef MULE
	for (i = 0; i < NUM_LEADING_BYTES; i++)
		if (!internal_equal(ct1->level1[i], ct2->level1[i], depth + 1))
			return 0;
#endif				/* MULE */

	return 1;
}

static unsigned long char_table_hash(Lisp_Object obj, int depth)
{
	Lisp_Char_Table *ct = XCHAR_TABLE(obj);
	unsigned long hashval = internal_array_hash(ct->ascii, NUM_ASCII_CHARS,
						    depth);
#ifdef MULE
	hashval = HASH2(hashval,
			internal_array_hash(ct->level1, NUM_LEADING_BYTES,
					    depth));
#endif				/* MULE */
	return hashval;
}

static const struct lrecord_description char_table_description[] = {
	{XD_LISP_OBJECT_ARRAY, offsetof(Lisp_Char_Table, ascii),
	 NUM_ASCII_CHARS},
#ifdef MULE
	{XD_LISP_OBJECT_ARRAY, offsetof(Lisp_Char_Table, level1),
	 NUM_LEADING_BYTES},
#endif
	{XD_LISP_OBJECT, offsetof(Lisp_Char_Table, mirror_table)},
	{XD_LO_LINK, offsetof(Lisp_Char_Table, next_table)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("char-table", char_table,
			      mark_char_table, print_char_table, 0,
			      char_table_equal, char_table_hash,
			      char_table_description, Lisp_Char_Table);

DEFUN("char-table-p", Fchar_table_p, 1, 1, 0,	/*
Return non-nil if OBJECT is a char table.

A char table is a table that maps characters (or ranges of characters)
to values.  Char tables are specialized for characters, only allowing
particular sorts of ranges to be assigned values.  Although this
loses in generality, it makes for extremely fast (constant-time)
lookups, and thus is feasible for applications that do an extremely
large number of lookups (e.g. scanning a buffer for a character in
a particular syntax, where a lookup in the syntax table must occur
once per character).

When Mule support exists, the types of ranges that can be assigned
values are

-- all characters
-- an entire charset
-- a single row in a two-octet charset
-- a single character

When Mule support is not present, the types of ranges that can be
assigned values are

-- all characters
-- a single character

To create a char table, use `make-char-table'.
To modify a char table, use `put-char-table' or `remove-char-table'.
To retrieve the value for a particular character, use `get-char-table'.
See also `map-char-table', `clear-char-table', `copy-char-table',
`valid-char-table-type-p', `char-table-type-list',
`valid-char-table-value-p', and `check-char-table-value'.
*/
      (object))
{
	return CHAR_TABLEP(object) ? Qt : Qnil;
}

DEFUN("char-table-type-list", Fchar_table_type_list, 0, 0, 0,	/*
Return a list of the recognized char table types.
See `valid-char-table-type-p'.
*/
      ())
{
#ifdef MULE
	return list5(Qchar, Qcategory, Qdisplay, Qgeneric, Qsyntax);
#else
	return list4(Qchar, Qdisplay, Qgeneric, Qsyntax);
#endif
}

DEFUN("valid-char-table-type-p", Fvalid_char_table_type_p, 1, 1, 0,	/*
Return t if TYPE if a recognized char table type.

Each char table type is used for a different purpose and allows different
sorts of values.  The different char table types are

`category'
Used for category tables, which specify the regexp categories
that a character is in.  The valid values are nil or a
bit vector of 95 elements.  Higher-level Lisp functions are
provided for working with category tables.  Currently categories
and category tables only exist when Mule support is present.
`char'
A generalized char table, for mapping from one character to
another.  Used for case tables, syntax matching tables,
`keyboard-translate-table', etc.  The valid values are characters.
`generic'
An even more generalized char table, for mapping from a
character to anything.
`display'
Used for display tables, which specify how a particular character
is to appear when displayed.  #### Not yet implemented.
`syntax'
Used for syntax tables, which specify the syntax of a particular
character.  Higher-level Lisp functions are provided for
working with syntax tables.  The valid values are integers.

*/
      (type))
{
	return (EQ(type, Qchar) ||
#ifdef MULE
		EQ(type, Qcategory) ||
#endif
		EQ(type, Qdisplay) ||
		EQ(type, Qgeneric) || EQ(type, Qsyntax)) ? Qt : Qnil;
}

DEFUN("char-table-type", Fchar_table_type, 1, 1, 0,	/*
Return the type of CHAR-TABLE.
See `valid-char-table-type-p'.
*/
      (char_table))
{
	CHECK_CHAR_TABLE(char_table);
	return char_table_type_to_symbol(XCHAR_TABLE(char_table)->type);
}

void fill_char_table(Lisp_Char_Table * ct, Lisp_Object value)
{
	int i;

	for (i = 0; i < NUM_ASCII_CHARS; i++)
		ct->ascii[i] = value;
#ifdef MULE
	for (i = 0; i < NUM_LEADING_BYTES; i++)
		ct->level1[i] = value;
#endif				/* MULE */

	if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
		update_syntax_table(ct);
}

DEFUN("reset-char-table", Freset_char_table, 1, 1, 0,	/*
Reset CHAR-TABLE to its default state.
*/
      (char_table))
{
	Lisp_Char_Table *ct;

	CHECK_CHAR_TABLE(char_table);
	ct = XCHAR_TABLE(char_table);

	switch (ct->type) {
	case CHAR_TABLE_TYPE_CHAR:
		fill_char_table(ct, make_char(0));
		break;
	case CHAR_TABLE_TYPE_DISPLAY:
	case CHAR_TABLE_TYPE_GENERIC:
#ifdef MULE
	case CHAR_TABLE_TYPE_CATEGORY:
#endif				/* MULE */
		fill_char_table(ct, Qnil);
		break;

	case CHAR_TABLE_TYPE_SYNTAX:
		fill_char_table(ct, make_int(Sinherit));
		break;

	default:
		abort();
	}

	return Qnil;
}

DEFUN("make-char-table", Fmake_char_table, 1, 1, 0,	/*
Return a new, empty char table of type TYPE.
Currently recognized types are 'char, 'category, 'display, 'generic,
and 'syntax.  See `valid-char-table-type-p'.
*/
      (type))
{
	Lisp_Char_Table *ct;
	Lisp_Object obj;
	enum char_table_type ty = symbol_to_char_table_type(type);

	ct = alloc_lcrecord_type(Lisp_Char_Table, &lrecord_char_table);
	ct->type = ty;
	if (ty == CHAR_TABLE_TYPE_SYNTAX) {
		ct->mirror_table = Fmake_char_table(Qgeneric);
		fill_char_table(XCHAR_TABLE(ct->mirror_table),
				make_int(Spunct));
	} else
		ct->mirror_table = Qnil;
	ct->next_table = Qnil;
	XSETCHAR_TABLE(obj, ct);
	if (ty == CHAR_TABLE_TYPE_SYNTAX) {
		ct->next_table = Vall_syntax_tables;
		Vall_syntax_tables = obj;
	}
	Freset_char_table(obj);
	return obj;
}

#ifdef MULE

static Lisp_Object make_char_table_entry(Lisp_Object initval)
{
	Lisp_Object obj;
	int i;
	Lisp_Char_Table_Entry *cte =
	    alloc_lcrecord_type(Lisp_Char_Table_Entry,
				&lrecord_char_table_entry);

	for (i = 0; i < 96; i++)
		cte->level2[i] = initval;

	XSETCHAR_TABLE_ENTRY(obj, cte);
	return obj;
}

static Lisp_Object copy_char_table_entry(Lisp_Object entry)
{
	Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY(entry);
	Lisp_Object obj;
	int i;
	Lisp_Char_Table_Entry *ctenew =
	    alloc_lcrecord_type(Lisp_Char_Table_Entry,
				&lrecord_char_table_entry);

	for (i = 0; i < 96; i++) {
		Lisp_Object new = cte->level2[i];
		if (CHAR_TABLE_ENTRYP(new))
			ctenew->level2[i] = copy_char_table_entry(new);
		else
			ctenew->level2[i] = new;
	}

	XSETCHAR_TABLE_ENTRY(obj, ctenew);
	return obj;
}

#endif				/* MULE */

DEFUN("copy-char-table", Fcopy_char_table, 1, 1, 0,	/*
Return a new char table which is a copy of CHAR-TABLE.
It will contain the same values for the same characters and ranges
as CHAR-TABLE.  The values will not themselves be copied.
*/
      (char_table))
{
	Lisp_Char_Table *ct, *ctnew;
	Lisp_Object obj;
	int i;

	CHECK_CHAR_TABLE(char_table);
	ct = XCHAR_TABLE(char_table);
	ctnew = alloc_lcrecord_type(Lisp_Char_Table, &lrecord_char_table);
	ctnew->type = ct->type;

	for (i = 0; i < NUM_ASCII_CHARS; i++) {
		Lisp_Object new = ct->ascii[i];
#ifdef MULE
		assert(!(CHAR_TABLE_ENTRYP(new)));
#endif				/* MULE */
		ctnew->ascii[i] = new;
	}

#ifdef MULE

	for (i = 0; i < NUM_LEADING_BYTES; i++) {
		Lisp_Object new = ct->level1[i];
		if (CHAR_TABLE_ENTRYP(new))
			ctnew->level1[i] = copy_char_table_entry(new);
		else
			ctnew->level1[i] = new;
	}

#endif				/* MULE */

	if (CHAR_TABLEP(ct->mirror_table))
		ctnew->mirror_table = Fcopy_char_table(ct->mirror_table);
	else
		ctnew->mirror_table = ct->mirror_table;
	ctnew->next_table = Qnil;
	XSETCHAR_TABLE(obj, ctnew);
	if (ctnew->type == CHAR_TABLE_TYPE_SYNTAX) {
		ctnew->next_table = Vall_syntax_tables;
		Vall_syntax_tables = obj;
	}
	return obj;
}

static void
decode_char_table_range(Lisp_Object range, struct chartab_range *outrange)
{
	if (EQ(range, Qt))
		outrange->type = CHARTAB_RANGE_ALL;
	else if (CHAR_OR_CHAR_INTP(range)) {
		outrange->type = CHARTAB_RANGE_CHAR;
		outrange->ch = XCHAR_OR_CHAR_INT(range);
	}
#ifndef MULE
	else
		signal_simple_error("Range must be t or a character", range);
#else				/* MULE */
	else if (VECTORP(range)) {
		Lisp_Vector *vec = XVECTOR(range);
		Lisp_Object *elts = vector_data(vec);
		if (vector_length(vec) != 2)
			signal_simple_error
			    ("Length of charset row vector must be 2", range);
		outrange->type = CHARTAB_RANGE_ROW;
		outrange->charset = Fget_charset(elts[0]);
		CHECK_INT(elts[1]);
		outrange->row = XINT(elts[1]);
		switch (XCHARSET_TYPE(outrange->charset)) {
		case CHARSET_TYPE_94:
		case CHARSET_TYPE_96:
			signal_simple_error
			    ("Charset in row vector must be multi-byte",
			     outrange->charset);
		case CHARSET_TYPE_94X94:
			check_int_range(outrange->row, 33, 126);
			break;
		case CHARSET_TYPE_96X96:
			check_int_range(outrange->row, 32, 127);
			break;
		default:
			abort();
		}
	} else {
		if (!CHARSETP(range) && !SYMBOLP(range))
			signal_simple_error
			    ("Char table range must be t, charset, char, or vector",
			     range);
		outrange->type = CHARTAB_RANGE_CHARSET;
		outrange->charset = Fget_charset(range);
	}
#endif				/* MULE */
}

#ifdef MULE

/* called from CHAR_TABLE_VALUE(). */
Lisp_Object
get_non_ascii_char_table_value(Lisp_Char_Table * ct, int leading_byte, Emchar c)
{
	Lisp_Object val;
	Lisp_Object charset = CHARSET_BY_LEADING_BYTE(leading_byte);
	int byte1, byte2;

	BREAKUP_CHAR_1_UNSAFE(c, charset, byte1, byte2);
	val = ct->level1[leading_byte - MIN_LEADING_BYTE];
	if (CHAR_TABLE_ENTRYP(val)) {
		Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY(val);
		val = cte->level2[byte1 - 32];
		if (CHAR_TABLE_ENTRYP(val)) {
			cte = XCHAR_TABLE_ENTRY(val);
			assert(byte2 >= 32);
			val = cte->level2[byte2 - 32];
			assert(!CHAR_TABLE_ENTRYP(val));
		}
	}

	return val;
}

#endif				/* MULE */

Lisp_Object get_char_table(Emchar ch, Lisp_Char_Table * ct)
{
#ifdef MULE
	{
		Lisp_Object charset;
		int byte1, byte2;
		Lisp_Object val;

		BREAKUP_CHAR(ch, charset, byte1, byte2);

		if (EQ(charset, Vcharset_ascii))
			val = ct->ascii[byte1];
		else if (EQ(charset, Vcharset_control_1))
			val = ct->ascii[byte1 + 128];
		else {
			int lb =
			    XCHARSET_LEADING_BYTE(charset) - MIN_LEADING_BYTE;
			val = ct->level1[lb];
			if (CHAR_TABLE_ENTRYP(val)) {
				Lisp_Char_Table_Entry *cte =
				    XCHAR_TABLE_ENTRY(val);
				val = cte->level2[byte1 - 32];
				if (CHAR_TABLE_ENTRYP(val)) {
					cte = XCHAR_TABLE_ENTRY(val);
					assert(byte2 >= 32);
					val = cte->level2[byte2 - 32];
					assert(!CHAR_TABLE_ENTRYP(val));
				}
			}
		}

		return val;
	}
#else				/* not MULE */
	return ct->ascii[(unsigned char)ch];
#endif				/* not MULE */
}

DEFUN("get-char-table", Fget_char_table, 2, 2, 0,	/*
Find value for CHARACTER in CHAR-TABLE.
*/
      (character, char_table))
{
	CHECK_CHAR_TABLE(char_table);
	CHECK_CHAR_COERCE_INT(character);

	return get_char_table(XCHAR(character), XCHAR_TABLE(char_table));
}

DEFUN("get-range-char-table", Fget_range_char_table, 2, 3, 0,	/*
Find value for a range in CHAR-TABLE.
If there is more than one value, return MULTI (defaults to nil).
*/
      (range, char_table, multi))
{
	Lisp_Char_Table *ct;
	struct chartab_range rainj;

	if (CHAR_OR_CHAR_INTP(range))
		return Fget_char_table(range, char_table);
	CHECK_CHAR_TABLE(char_table);
	ct = XCHAR_TABLE(char_table);

	decode_char_table_range(range, &rainj);
	switch (rainj.type) {
	case CHARTAB_RANGE_ALL:
		{
			int i;
			Lisp_Object first = ct->ascii[0];

			for (i = 1; i < NUM_ASCII_CHARS; i++)
				if (!EQ(first, ct->ascii[i]))
					return multi;

#ifdef MULE
			for (i = MIN_LEADING_BYTE;
			     i < MIN_LEADING_BYTE + NUM_LEADING_BYTES; i++) {
				Lisp_Object foo = CHARSET_BY_LEADING_BYTE(i);
				if (!CHARSETP(foo)
				    || i == LEADING_BYTE_ASCII
				    || i == LEADING_BYTE_CONTROL_1) {
					continue;
				}
				if (!EQ
				    (first, ct->level1[i - MIN_LEADING_BYTE])) {
					return multi;
				}
			}
#endif				/* MULE */

			return first;
		}

#ifdef MULE
	case CHARTAB_RANGE_CHARSET:
		if (EQ(rainj.charset, Vcharset_ascii)) {
			int i;
			Lisp_Object first = ct->ascii[0];

			for (i = 1; i < 128; i++)
				if (!EQ(first, ct->ascii[i]))
					return multi;
			return first;
		}

		if (EQ(rainj.charset, Vcharset_control_1)) {
			int i;
			Lisp_Object first = ct->ascii[128];

			for (i = 129; i < 160; i++)
				if (!EQ(first, ct->ascii[i]))
					return multi;
			return first;
		}

		{
			Lisp_Object val =
			    ct->level1[XCHARSET_LEADING_BYTE(rainj.charset) -
				       MIN_LEADING_BYTE];
			if (CHAR_TABLE_ENTRYP(val))
				return multi;
			return val;
		}

	case CHARTAB_RANGE_ROW:
		{
			Lisp_Object val =
			    ct->level1[XCHARSET_LEADING_BYTE(rainj.charset) -
				       MIN_LEADING_BYTE];
			if (!CHAR_TABLE_ENTRYP(val))
				return val;
			val = XCHAR_TABLE_ENTRY(val)->level2[rainj.row - 32];
			if (CHAR_TABLE_ENTRYP(val))
				return multi;
			return val;
		}

		/* list all cases */
	case CHARTAB_RANGE_CHAR:
#endif				/* not MULE */

	default:
		abort();
	}

	return Qnil;		/* not reached */
}

static int
check_valid_char_table_value(Lisp_Object value, enum char_table_type type,
			     Error_behavior errb)
{
	switch (type) {
	case CHAR_TABLE_TYPE_SYNTAX:
		if (!ERRB_EQ(errb, ERROR_ME))
			return INTP(value) || (CONSP(value) && INTP(XCAR(value))
					       &&
					       CHAR_OR_CHAR_INTP(XCDR(value)));
		if (CONSP(value)) {
			Lisp_Object cdr = XCDR(value);
			CHECK_INT(XCAR(value));
			CHECK_CHAR_COERCE_INT(cdr);
		} else
			CHECK_INT(value);
		break;

#ifdef MULE
	case CHAR_TABLE_TYPE_CATEGORY:
		if (!ERRB_EQ(errb, ERROR_ME))
			return CATEGORY_TABLE_VALUEP(value);
		CHECK_CATEGORY_TABLE_VALUE(value);
		break;
#endif				/* MULE */

	case CHAR_TABLE_TYPE_GENERIC:
		return 1;

	case CHAR_TABLE_TYPE_DISPLAY:
		/* #### fix this */
		maybe_signal_simple_error
		    ("Display char tables not yet implemented", value,
		     Qchar_table, errb);
		return 0;

	case CHAR_TABLE_TYPE_CHAR:
		if (!ERRB_EQ(errb, ERROR_ME))
			return CHAR_OR_CHAR_INTP(value);
		CHECK_CHAR_COERCE_INT(value);
		break;

	default:
		abort();
	}

	return 0;		/* not reached */
}

static Lisp_Object
canonicalize_char_table_value(Lisp_Object value, enum char_table_type type)
{
	switch (type) {
	case CHAR_TABLE_TYPE_SYNTAX:
		if (CONSP(value)) {
			Lisp_Object car = XCAR(value);
			Lisp_Object cdr = XCDR(value);
			CHECK_CHAR_COERCE_INT(cdr);
			return Fcons(car, cdr);
		}
		break;
	case CHAR_TABLE_TYPE_CHAR:
		CHECK_CHAR_COERCE_INT(value);
		break;

	case CHAR_TABLE_TYPE_GENERIC:
#ifdef MULE
	case CHAR_TABLE_TYPE_CATEGORY:
#endif
	case CHAR_TABLE_TYPE_DISPLAY:
	default:
		break;
	}
	return value;
}

DEFUN("valid-char-table-value-p", Fvalid_char_table_value_p, 2, 2, 0,	/*
Return non-nil if VALUE is a valid value for CHAR-TABLE-TYPE.
*/
      (value, char_table_type))
{
	enum char_table_type type = symbol_to_char_table_type(char_table_type);

	return check_valid_char_table_value(value, type,
					    ERROR_ME_NOT) ? Qt : Qnil;
}

DEFUN("check-valid-char-table-value", Fcheck_valid_char_table_value, 2, 2, 0,	/*
Signal an error if VALUE is not a valid value for CHAR-TABLE-TYPE.
*/
      (value, char_table_type))
{
	enum char_table_type type = symbol_to_char_table_type(char_table_type);

	check_valid_char_table_value(value, type, ERROR_ME);
	return Qnil;
}

/* Assign VAL to all characters in RANGE in char table CT. */

void
put_char_table(Lisp_Char_Table * ct, struct chartab_range *range,
	       Lisp_Object val)
{
	switch (range->type) {
	case CHARTAB_RANGE_ALL:
		fill_char_table(ct, val);
		/* avoid the duplicate call to update_syntax_table() below,
		   since fill_char_table() also did that. */
		return;

#ifdef MULE
	case CHARTAB_RANGE_CHARSET:
		if (EQ(range->charset, Vcharset_ascii)) {
			int i;
			for (i = 0; i < 128; i++)
				ct->ascii[i] = val;
		} else if (EQ(range->charset, Vcharset_control_1)) {
			int i;
			for (i = 128; i < 160; i++)
				ct->ascii[i] = val;
		} else {
			int lb =
			    XCHARSET_LEADING_BYTE(range->charset) -
			    MIN_LEADING_BYTE;
			ct->level1[lb] = val;
		}
		break;

	case CHARTAB_RANGE_ROW:
		{
			Lisp_Char_Table_Entry *cte;
			int lb =
			    XCHARSET_LEADING_BYTE(range->charset) -
			    MIN_LEADING_BYTE;
			/* make sure that there is a separate entry for the
			   row. */
			if (!CHAR_TABLE_ENTRYP(ct->level1[lb]))
				ct->level1[lb] =
				    make_char_table_entry(ct->level1[lb]);
			cte = XCHAR_TABLE_ENTRY(ct->level1[lb]);
			cte->level2[range->row - 32] = val;
		}
		break;
#endif				/* MULE */

	case CHARTAB_RANGE_CHAR: {
#ifdef MULE
		Lisp_Object charset;
		int byte1, byte2;

		BREAKUP_CHAR(range->ch, charset, byte1, byte2);
		if (EQ(charset, Vcharset_ascii))
			ct->ascii[byte1] = val;
		else if (EQ(charset, Vcharset_control_1))
			ct->ascii[byte1 + 128] = val;
		else {
			Lisp_Char_Table_Entry *cte;
			int lb =
				XCHARSET_LEADING_BYTE(charset) -
				MIN_LEADING_BYTE;
			/* make sure that there is a separate entry for the
			   row. */
			if (!CHAR_TABLE_ENTRYP(ct->level1[lb]))
				ct->level1[lb] =
					make_char_table_entry(ct->
							      level1[lb]);
			cte = XCHAR_TABLE_ENTRY(ct->level1[lb]);
			/* now CTE is a char table entry for the charset;
			   each entry is for a single row (or character of
			   a one-octet charset). */
			if (XCHARSET_DIMENSION(charset) == 1)
				cte->level2[byte1 - 32] = val;
			else {
				/* assigning to one character in a two-octet
				   charset. */
				/* make sure that the charset row contains a
				   separate entry for each character. */
				if (!CHAR_TABLE_ENTRYP
				    (cte->level2[byte1 - 32]))
					cte->level2[byte1 - 32] =
						make_char_table_entry(cte->
								      level2
								      [byte1
								       -
								       32]);
				cte = XCHAR_TABLE_ENTRY(cte->
							level2[byte1 - 32]);
				cte->level2[byte2 - 32] = val;
			}
		}
#else				/* not MULE */
		ct->ascii[(unsigned char)(range->ch)] = val;
		break;
#endif				/* not MULE */
	}
	default:
		/* shouldnt happen should it? */
		break;
	}

	if (ct->type == CHAR_TABLE_TYPE_SYNTAX) {
		update_syntax_table(ct);
	}
	return;
}

DEFUN("put-char-table", Fput_char_table, 3, 3, 0,	/*
Set the value for chars in RANGE to be VALUE in CHAR-TABLE.

RANGE specifies one or more characters to be affected and should be
one of the following:

-- t (all characters are affected)
-- A charset (only allowed when Mule support is present)
-- A vector of two elements: a two-octet charset and a row number
(only allowed when Mule support is present)
-- A single character

VALUE must be a value appropriate for the type of CHAR-TABLE.
See `valid-char-table-type-p'.
*/
      (range, value, char_table))
{
	Lisp_Char_Table *ct;
	struct chartab_range rainj;

	CHECK_CHAR_TABLE(char_table);
	ct = XCHAR_TABLE(char_table);
	check_valid_char_table_value(value, ct->type, ERROR_ME);
	decode_char_table_range(range, &rainj);
	value = canonicalize_char_table_value(value, ct->type);
	put_char_table(ct, &rainj, value);
	return Qnil;
}

/* Map FN over the ASCII chars in CT. */

static int
map_over_charset_ascii(Lisp_Char_Table * ct,
		       int (*fn) (struct chartab_range * range,
				  Lisp_Object val, void *arg), void *arg)
{
	struct chartab_range rainj;
	int i, retval;
	int start = 0;
#ifdef MULE
	int stop = 128;
#else
	int stop = 256;
#endif

	rainj.type = CHARTAB_RANGE_CHAR;

	for (i = start, retval = 0; i < stop && retval == 0; i++) {
		rainj.ch = (Emchar) i;
		retval = (fn) (&rainj, ct->ascii[i], arg);
	}

	return retval;
}

#ifdef MULE

/* Map FN over the Control-1 chars in CT. */

static int
map_over_charset_control_1(Lisp_Char_Table * ct,
			   int (*fn) (struct chartab_range * range,
				      Lisp_Object val, void *arg), void *arg)
{
	struct chartab_range rainj;
	int i, retval;
	int start = 128;
	int stop = start + 32;

	rainj.type = CHARTAB_RANGE_CHAR;

	for (i = start, retval = 0; i < stop && retval == 0; i++) {
		rainj.ch = (Emchar) (i);
		retval = (fn) (&rainj, ct->ascii[i], arg);
	}

	return retval;
}

/* Map FN over the row ROW of two-byte charset CHARSET.
   There must be a separate value for that row in the char table.
   CTE specifies the char table entry for CHARSET. */

static int
map_over_charset_row(Lisp_Char_Table_Entry * cte,
		     Lisp_Object charset, int row,
		     int (*fn) (struct chartab_range * range,
				Lisp_Object val, void *arg), void *arg)
{
	Lisp_Object val = cte->level2[row - 32];

	if (!CHAR_TABLE_ENTRYP(val)) {
		struct chartab_range rainj;

		rainj.type = CHARTAB_RANGE_ROW;
		rainj.charset = charset;
		rainj.row = row;
		return (fn) (&rainj, val, arg);
	} else {
		struct chartab_range rainj;
		int i, retval;
		int charset94_p = (XCHARSET_CHARS(charset) == 94);
		int start = charset94_p ? 33 : 32;
		int stop = charset94_p ? 127 : 128;

		cte = XCHAR_TABLE_ENTRY(val);

		rainj.type = CHARTAB_RANGE_CHAR;

		for (i = start, retval = 0; i < stop && retval == 0; i++) {
			rainj.ch = MAKE_CHAR(charset, row, i);
			retval = (fn) (&rainj, cte->level2[i - 32], arg);
		}
		return retval;
	}
}

static int
map_over_other_charset(Lisp_Char_Table * ct, int lb,
		       int (*fn) (struct chartab_range * range,
				  Lisp_Object val, void *arg), void *arg)
{
	Lisp_Object val = ct->level1[lb - MIN_LEADING_BYTE];
	Lisp_Object charset = CHARSET_BY_LEADING_BYTE(lb);

	if (!CHARSETP(charset)
	    || lb == LEADING_BYTE_ASCII || lb == LEADING_BYTE_CONTROL_1)
		return 0;

	if (!CHAR_TABLE_ENTRYP(val)) {
		struct chartab_range rainj;

		rainj.type = CHARTAB_RANGE_CHARSET;
		rainj.charset = charset;
		return (fn) (&rainj, val, arg);
	}

	{
		Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY(val);
		int charset94_p = (XCHARSET_CHARS(charset) == 94);
		int start = charset94_p ? 33 : 32;
		int stop = charset94_p ? 127 : 128;
		int i, retval;

		if (XCHARSET_DIMENSION(charset) == 1) {
			struct chartab_range rainj;
			rainj.type = CHARTAB_RANGE_CHAR;

			for (i = start, retval = 0; i < stop && retval == 0;
			     i++) {
				rainj.ch = MAKE_CHAR(charset, i, 0);
				retval =
				    (fn) (&rainj, cte->level2[i - 32], arg);
			}
		} else {
			for (i = start, retval = 0; i < stop && retval == 0;
			     i++)
				retval =
				    map_over_charset_row(cte, charset, i, fn,
							 arg);
		}

		return retval;
	}
}

#endif				/* MULE */

/* Map FN (with client data ARG) over range RANGE in char table CT.
   Mapping stops the first time FN returns non-zero, and that value
   becomes the return value of map_char_table(). */

int
map_char_table(Lisp_Char_Table * ct,
	       struct chartab_range *range,
	       int (*fn) (struct chartab_range * range,
			  Lisp_Object val, void *arg), void *arg)
{
	switch (range->type) {
	case CHARTAB_RANGE_ALL:
		{
			int retval;

			retval = map_over_charset_ascii(ct, fn, arg);
			if (retval)
				return retval;
#ifdef MULE
			retval = map_over_charset_control_1(ct, fn, arg);
			if (retval)
				return retval;
			{
				int i;
				int start = MIN_LEADING_BYTE;
				int stop = start + NUM_LEADING_BYTES;

				for (i = start, retval = 0;
				     i < stop && retval == 0; i++) {
					retval =
					    map_over_other_charset(ct, i, fn,
								   arg);
				}
			}
#endif				/* MULE */
			return retval;
		}

#ifdef MULE
	case CHARTAB_RANGE_CHARSET:
		return map_over_other_charset(ct,
					      XCHARSET_LEADING_BYTE(range->
								    charset),
					      fn, arg);

	case CHARTAB_RANGE_ROW:
		{
			Lisp_Object val =
			    ct->level1[XCHARSET_LEADING_BYTE(range->charset) -
				       MIN_LEADING_BYTE];
			if (!CHAR_TABLE_ENTRYP(val)) {
				struct chartab_range rainj;

				rainj.type = CHARTAB_RANGE_ROW;
				rainj.charset = range->charset;
				rainj.row = range->row;
				return (fn) (&rainj, val, arg);
			} else
				return
				    map_over_charset_row(XCHAR_TABLE_ENTRY(val),
							 range->charset,
							 range->row, fn, arg);
		}
#endif				/* MULE */

	case CHARTAB_RANGE_CHAR:
		{
			Emchar ch = range->ch;
			Lisp_Object val = CHAR_TABLE_VALUE_UNSAFE(ct, ch);
			struct chartab_range rainj;

			rainj.type = CHARTAB_RANGE_CHAR;
			rainj.ch = ch;
			return (fn) (&rainj, val, arg);
		}

	default:
		abort();
	}

	return 0;
}

struct slow_map_char_table_arg {
	Lisp_Object function;
	Lisp_Object retval;
};

static int
slow_map_char_table_fun(struct chartab_range *range, Lisp_Object val, void *arg)
{
	Lisp_Object ranjarg = Qnil;
	struct slow_map_char_table_arg *closure =
	    (struct slow_map_char_table_arg *)arg;

	switch (range->type) {
	case CHARTAB_RANGE_ALL:
		ranjarg = Qt;
		break;

#ifdef MULE
	case CHARTAB_RANGE_CHARSET:
		ranjarg = XCHARSET_NAME(range->charset);
		break;

	case CHARTAB_RANGE_ROW:
		ranjarg = vector2(XCHARSET_NAME(range->charset),
				  make_int(range->row));
		break;
#endif				/* MULE */
	case CHARTAB_RANGE_CHAR:
		ranjarg = make_char(range->ch);
		break;
	default:
		abort();
	}

	closure->retval = call2(closure->function, ranjarg, val);
	return !NILP(closure->retval);
}

DEFUN("map-char-table", Fmap_char_table, 2, 3, 0,	/*
Map FUNCTION over entries in CHAR-TABLE, calling it with two args,
each key and value in the table.

RANGE specifies a subrange to map over and is in the same format as
the RANGE argument to `put-range-table'.  If omitted or t, it defaults to
the entire table.
*/
      (function, char_table, range))
{
	Lisp_Char_Table *ct;
	struct slow_map_char_table_arg slarg;
	struct gcpro gcpro1, gcpro2;
	struct chartab_range rainj;

	CHECK_CHAR_TABLE(char_table);
	ct = XCHAR_TABLE(char_table);
	if (NILP(range))
		range = Qt;
	decode_char_table_range(range, &rainj);
	slarg.function = function;
	slarg.retval = Qnil;
	GCPRO2(slarg.function, slarg.retval);
	map_char_table(ct, &rainj, slow_map_char_table_fun, &slarg);
	UNGCPRO;

	return slarg.retval;
}

/************************************************************************/
/*                         Char table read syntax                       */
/************************************************************************/

static int
chartab_type_validate(Lisp_Object keyword, Lisp_Object value,
		      Error_behavior errb)
{
	/* #### should deal with ERRB */
	symbol_to_char_table_type(value);
	return 1;
}

static int
chartab_data_validate(Lisp_Object keyword, Lisp_Object value,
		      Error_behavior errb)
{
	Lisp_Object rest;

	/* #### should deal with ERRB */
	EXTERNAL_LIST_LOOP(rest, value) {
		Lisp_Object range = XCAR(rest);
		struct chartab_range dummy;

		rest = XCDR(rest);
		if (!CONSP(rest))
			signal_simple_error("Invalid list format", value);
		if (CONSP(range)) {
			if (!CONSP(XCDR(range))
			    || !NILP(XCDR(XCDR(range))))
				signal_simple_error("Invalid range format",
						    range);
			decode_char_table_range(XCAR(range), &dummy);
			decode_char_table_range(XCAR(XCDR(range)), &dummy);
		} else
			decode_char_table_range(range, &dummy);
	}

	return 1;
}

static Lisp_Object chartab_instantiate(Lisp_Object data)
{
	Lisp_Object chartab;
	Lisp_Object type = Qgeneric;
	Lisp_Object dataval = Qnil;

	while (!NILP(data)) {
		Lisp_Object keyw = Fcar(data);
		Lisp_Object valw;

		data = Fcdr(data);
		valw = Fcar(data);
		data = Fcdr(data);
		if (EQ(keyw, Qtype))
			type = valw;
		else if (EQ(keyw, Qdata))
			dataval = valw;
	}

	chartab = Fmake_char_table(type);

	data = dataval;
	while (!NILP(data)) {
		Lisp_Object range = Fcar(data);
		Lisp_Object val = Fcar(Fcdr(data));

		data = Fcdr(Fcdr(data));
		if (CONSP(range)) {
			if (CHAR_OR_CHAR_INTP(XCAR(range))) {
				Emchar first = XCHAR_OR_CHAR_INT(Fcar(range));
				Emchar last =
				    XCHAR_OR_CHAR_INT(Fcar(Fcdr(range)));
				Emchar i;

				for (i = first; i <= last; i++)
					Fput_char_table(make_char(i), val,
							chartab);
			} else
				abort();
		} else
			Fput_char_table(range, val, chartab);
	}

	return chartab;
}

#ifdef MULE

/************************************************************************/
/*                     Category Tables, specifically                    */
/************************************************************************/

DEFUN("category-table-p", Fcategory_table_p, 1, 1, 0,	/*
Return t if OBJECT is a category table.
A category table is a type of char table used for keeping track of
categories.  Categories are used for classifying characters for use
in regexps -- you can refer to a category rather than having to use
a complicated [] expression (and category lookups are significantly
faster).

There are 95 different categories available, one for each printable
character (including space) in the ASCII charset.  Each category
is designated by one such character, called a "category designator".
They are specified in a regexp using the syntax "\\cX", where X is
a category designator.

A category table specifies, for each character, the categories that
the character is in.  Note that a character can be in more than one
category.  More specifically, a category table maps from a character
to either the value nil (meaning the character is in no categories)
or a 95-element bit vector, specifying for each of the 95 categories
whether the character is in that category.

Special Lisp functions are provided that abstract this, so you do not
have to directly manipulate bit vectors.
*/
      (object))
{
	return (CHAR_TABLEP(object) &&
		XCHAR_TABLE_TYPE(object) == CHAR_TABLE_TYPE_CATEGORY) ?
	    Qt : Qnil;
}

static Lisp_Object
check_category_table(Lisp_Object object, Lisp_Object default_)
{
	if (NILP(object))
		object = default_;
	while (NILP(Fcategory_table_p(object)))
		object = wrong_type_argument(Qcategory_table_p, object);
	return object;
}

int
check_category_char(Emchar ch, Lisp_Object table,
		    unsigned int designator, unsigned int not_p)
{
	REGISTER Lisp_Object temp;
	Lisp_Char_Table *ctbl;
#ifdef ERROR_CHECK_TYPECHECK
	if (NILP(Fcategory_table_p(table)))
		signal_simple_error("Expected category table", table);
#endif
	ctbl = XCHAR_TABLE(table);
	temp = get_char_table(ch, ctbl);
	if (NILP(temp))
		return not_p;

	designator -= ' ';
	return bit_vector_bit(XBIT_VECTOR(temp), designator) ? !not_p : not_p;
}

DEFUN("check-category-at", Fcheck_category_at, 2, 4, 0,	/*
Return t if category of the character at POSITION includes DESIGNATOR.
Optional third arg BUFFER specifies which buffer to use, and defaults
to the current buffer.
Optional fourth arg CATEGORY-TABLE specifies the category table to
use, and defaults to BUFFER's category table.
*/
      (position, designator, buffer, category_table))
{
	Lisp_Object ctbl;
	Emchar ch;
	unsigned int des;
	struct buffer *buf = decode_buffer(buffer, 0);

	CHECK_INT(position);
	CHECK_CATEGORY_DESIGNATOR(designator);
	des = XCHAR(designator);
	ctbl = check_category_table(category_table, Vstandard_category_table);
	ch = BUF_FETCH_CHAR(buf, XINT(position));
	return check_category_char(ch, ctbl, des, 0) ? Qt : Qnil;
}

DEFUN("char-in-category-p", Fchar_in_category_p, 2, 3, 0,	/*
Return t if category of CHARACTER includes DESIGNATOR, else nil.
Optional third arg CATEGORY-TABLE specifies the category table to use,
and defaults to the standard category table.
*/
      (character, designator, category_table))
{
	Lisp_Object ctbl;
	Emchar ch;
	unsigned int des;

	CHECK_CATEGORY_DESIGNATOR(designator);
	des = XCHAR(designator);
	CHECK_CHAR(character);
	ch = XCHAR(character);
	ctbl = check_category_table(category_table, Vstandard_category_table);
	return check_category_char(ch, ctbl, des, 0) ? Qt : Qnil;
}

DEFUN("category-table", Fcategory_table, 0, 1, 0,	/*
Return BUFFER's current category table.
BUFFER defaults to the current buffer.
*/
      (buffer))
{
	return decode_buffer(buffer, 0)->category_table;
}

DEFUN("standard-category-table", Fstandard_category_table, 0, 0, 0,	/*
Return the standard category table.
This is the one used for new buffers.
*/
      ())
{
	return Vstandard_category_table;
}

DEFUN("copy-category-table", Fcopy_category_table, 0, 1, 0,	/*
Return a new category table which is a copy of CATEGORY-TABLE.
CATEGORY-TABLE defaults to the standard category table.
*/
      (category_table))
{
	if (NILP(Vstandard_category_table))
		return Fmake_char_table(Qcategory);

	category_table =
	    check_category_table(category_table, Vstandard_category_table);
	return Fcopy_char_table(category_table);
}

DEFUN("set-category-table", Fset_category_table, 1, 2, 0,	/*
Select CATEGORY-TABLE as the new category table for BUFFER.
BUFFER defaults to the current buffer if omitted.
*/
      (category_table, buffer))
{
	struct buffer *buf = decode_buffer(buffer, 0);
	category_table = check_category_table(category_table, Qnil);
	buf->category_table = category_table;
	/* Indicate that this buffer now has a specified category table.  */
	buf->local_var_flags |= XINT(buffer_local_flags.category_table);
	return category_table;
}

DEFUN("category-designator-p", Fcategory_designator_p, 1, 1, 0,	/*
Return t if OBJECT is a category designator (a char in the range ' ' to '~').
*/
      (object))
{
	return CATEGORY_DESIGNATORP(object) ? Qt : Qnil;
}

DEFUN("category-table-value-p", Fcategory_table_value_p, 1, 1, 0,	/*
Return t if OBJECT is a category table value.
Valid values are nil or a bit vector of size 95.
*/
      (object))
{
	return CATEGORY_TABLE_VALUEP(object) ? Qt : Qnil;
}

#define CATEGORYP(x) \
  (CHARP (x) && XCHAR (x) >= 0x20 && XCHAR (x) <= 0x7E)

#define CATEGORY_SET(c)						\
  (get_char_table(c, XCHAR_TABLE(current_buffer->category_table)))

/* Return 1 if CATEGORY_SET contains CATEGORY, else return 0.
   The faster version of `!NILP (Faref (category_set, category))'.  */
#define CATEGORY_MEMBER(category, category_set)			\
  (bit_vector_bit(XBIT_VECTOR (category_set), category - 32))

/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   Use the macro WORD_BOUNDARY_P instead of calling this function
   directly.  */

int word_boundary_p(Emchar c1, Emchar c2);
int word_boundary_p(Emchar c1, Emchar c2)
{
	Lisp_Object category_set1, category_set2;
	Lisp_Object tail;
	int default_result;

#if 0
	if (COMPOSITE_CHAR_P(c1))
		c1 = cmpchar_component(c1, 0, 1);
	if (COMPOSITE_CHAR_P(c2))
		c2 = cmpchar_component(c2, 0, 1);
#endif

	if (EQ(CHAR_CHARSET(c1), CHAR_CHARSET(c2))) {
		tail = Vword_separating_categories;
		default_result = 0;
	} else {
		tail = Vword_combining_categories;
		default_result = 1;
	}

	category_set1 = CATEGORY_SET(c1);
	if (NILP(category_set1))
		return default_result;
	category_set2 = CATEGORY_SET(c2);
	if (NILP(category_set2))
		return default_result;

	for (; CONSP(tail); tail = XCONS(tail)->cdr) {
		Lisp_Object elt = XCONS(tail)->car;

		if (CONSP(elt)
		    && CATEGORYP(XCONS(elt)->car)
		    && CATEGORYP(XCONS(elt)->cdr)
		    && CATEGORY_MEMBER(XCHAR(XCONS(elt)->car), category_set1)
		    && CATEGORY_MEMBER(XCHAR(XCONS(elt)->cdr), category_set2))
			return !default_result;
	}
	return default_result;
}
#endif				/* MULE */

void syms_of_chartab(void)
{
	INIT_LRECORD_IMPLEMENTATION(char_table);

#ifdef MULE
	INIT_LRECORD_IMPLEMENTATION(char_table_entry);

	defsymbol(&Qcategory_table_p, "category-table-p");
	defsymbol(&Qcategory_designator_p, "category-designator-p");
	defsymbol(&Qcategory_table_value_p, "category-table-value-p");
#endif				/* MULE */

	defsymbol(&Qchar_table, "char-table");
	defsymbol(&Qchar_tablep, "char-table-p");

	DEFSUBR(Fchar_table_p);
	DEFSUBR(Fchar_table_type_list);
	DEFSUBR(Fvalid_char_table_type_p);
	DEFSUBR(Fchar_table_type);
	DEFSUBR(Freset_char_table);
	DEFSUBR(Fmake_char_table);
	DEFSUBR(Fcopy_char_table);
	DEFSUBR(Fget_char_table);
	DEFSUBR(Fget_range_char_table);
	DEFSUBR(Fvalid_char_table_value_p);
	DEFSUBR(Fcheck_valid_char_table_value);
	DEFSUBR(Fput_char_table);
	DEFSUBR(Fmap_char_table);

#ifdef MULE
	DEFSUBR(Fcategory_table_p);
	DEFSUBR(Fcategory_table);
	DEFSUBR(Fstandard_category_table);
	DEFSUBR(Fcopy_category_table);
	DEFSUBR(Fset_category_table);
	DEFSUBR(Fcheck_category_at);
	DEFSUBR(Fchar_in_category_p);
	DEFSUBR(Fcategory_designator_p);
	DEFSUBR(Fcategory_table_value_p);
#endif				/* MULE */

}

void vars_of_chartab(void)
{
	/* DO NOT staticpro this.  It works just like Vweak_hash_tables. */
	Vall_syntax_tables = Qnil;
	dump_add_weak_object_chain(&Vall_syntax_tables);
}

void structure_type_create_chartab(void)
{
	struct structure_type *st;

	st = define_structure_type(Qchar_table, 0, chartab_instantiate);

	define_structure_type_keyword(st, Qtype, chartab_type_validate);
	define_structure_type_keyword(st, Qdata, chartab_data_validate);
}

void complex_vars_of_chartab(void)
{
#ifdef MULE
	/* Set this now, so first buffer creation can refer to it. */
	/* Make it nil before calling copy-category-table
	   so that copy-category-table will know not to try to copy from garbage */
	Vstandard_category_table = Qnil;
	Vstandard_category_table = Fcopy_category_table(Qnil);
	staticpro(&Vstandard_category_table);

	DEFVAR_LISP("word-combining-categories", &Vword_combining_categories	/*
List of pair (cons) of categories to determine word boundary.

Emacs treats a sequence of word constituent characters as a single
word (i.e. finds no word boundary between them) iff they belongs to
the same charset.  But, exceptions are allowed in the following cases.

\(1) The case that characters are in different charsets is controlled
by the variable `word-combining-categories'.

Emacs finds no word boundary between characters of different charsets
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's no word boundary between C1 and C2.

For instance, to tell that ASCII characters and Latin-1 characters can
form a single word, the element `(?l . ?l)' should be in this list
because both characters have the category `l' (Latin characters).

\(2) The case that character are in the same charset is controlled by
the variable `word-separating-categories'.

Emacs find a word boundary between characters of the same charset
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's a word boundary between C1 and C2.

For instance, to tell that there's a word boundary between Japanese
Hiragana and Japanese Kanji (both are in the same charset), the
element `(?H . ?C) should be in this list.
										 */ );

	Vword_combining_categories = Qnil;

	DEFVAR_LISP("word-separating-categories", &Vword_separating_categories	/*
List of pair (cons) of categories to determine word boundary.
See the documentation of the variable `word-combining-categories'.
										 */ );

	Vword_separating_categories = Qnil;
#endif				/* MULE */
}
