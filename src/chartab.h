/* Declarations having to do with Mule char tables.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

#ifndef INCLUDED_chartab_h_
#define INCLUDED_chartab_h_

/************************************************************************/
/*                               Char Tables                            */
/************************************************************************/

/* Under Mule, we use a complex representation (see below).
   When not under Mule, there are only 256 possible characters
   so we just represent them directly. */

#ifdef MULE

struct Lisp_Char_Table_Entry {
	struct lcrecord_header header;

	/* In the interests of simplicity, we just use a fixed 96-entry
	   table.  If we felt like being smarter, we could make this
	   variable-size and add an offset value into this structure. */
	Lisp_Object level2[96];
};
typedef struct Lisp_Char_Table_Entry Lisp_Char_Table_Entry;

DECLARE_LRECORD(char_table_entry, Lisp_Char_Table_Entry);
#define XCHAR_TABLE_ENTRY(x) \
  XRECORD (x, char_table_entry, Lisp_Char_Table_Entry)
#define XSETCHAR_TABLE_ENTRY(x, p) XSETRECORD (x, p, char_table_entry)
#define CHAR_TABLE_ENTRYP(x) RECORDP (x, char_table_entry)
/* #define CHECK_CHAR_TABLE_ENTRY(x) CHECK_RECORD (x, char_table_entry)
   char table entries should never escape to Lisp */

#endif				/* MULE */

enum char_table_type {
	CHAR_TABLE_TYPE_GENERIC,
#ifdef MULE
	CHAR_TABLE_TYPE_CATEGORY,
#endif
	CHAR_TABLE_TYPE_SYNTAX,
	CHAR_TABLE_TYPE_DISPLAY,
	CHAR_TABLE_TYPE_CHAR
};

#ifdef MULE
#define NUM_ASCII_CHARS 160
#else
#define NUM_ASCII_CHARS 256
#endif

struct Lisp_Char_Table {
	struct lcrecord_header header;

	Lisp_Object ascii[NUM_ASCII_CHARS];

#ifdef MULE
	/* We basically duplicate the Mule vectors-of-vectors implementation.
	   We can do this because we know a great deal about the sorts of
	   things we are going to be indexing.

	   The current implementation is as follows:

	   ascii[0-159] is used for ASCII and Control-1 characters.

	   level1[0 .. (NUM_LEADING_BYTES-1)] indexes charsets by leading
	   byte (subtract MIN_LEADING_BYTE from the leading byte).  If the
	   value of this is not an opaque, then it specifies a value for all
	   characters in the charset.  Otherwise, it will be a
	   96-Lisp-Object opaque that we created, specifying a value for
	   each row.  If the value of this is not an opaque, then it
	   specifies a value for all characters in the row.  Otherwise, it
	   will be a 96-Lisp-Object opaque that we created, specifying a
	   value for each character.

	   NOTE: 1) This will fail if some C routine passes an opaque to
	   Fput_char_table().  Currently this is not a problem
	   since all char tables that are created are Lisp-visible
	   and thus no one should ever be putting an opaque in
	   a char table.  Another possibility is to consider
	   adding a type to */

	Lisp_Object level1[NUM_LEADING_BYTES];

#endif				/* MULE */

	enum char_table_type type;

	/* stuff used for syntax tables */
	Lisp_Object mirror_table;
	Lisp_Object next_table;	/* DO NOT mark through this. */
};
typedef struct Lisp_Char_Table Lisp_Char_Table;

DECLARE_LRECORD(char_table, Lisp_Char_Table);
#define XCHAR_TABLE(x) XRECORD (x, char_table, Lisp_Char_Table)
#define XSETCHAR_TABLE(x, p) XSETRECORD (x, p, char_table)
#define CHAR_TABLEP(x) RECORDP (x, char_table)
#define CHECK_CHAR_TABLE(x) CHECK_RECORD (x, char_table)
#define CONCHECK_CHAR_TABLE(x) CONCHECK_RECORD (x, char_table)

#define CHAR_TABLE_TYPE(ct) ((ct)->type)
#define XCHAR_TABLE_TYPE(ct) CHAR_TABLE_TYPE (XCHAR_TABLE (ct))

#ifdef MULE

Lisp_Object get_non_ascii_char_table_value(Lisp_Char_Table * ct,
					   int leading_byte, Emchar c);

extern_inline Lisp_Object
CHAR_TABLE_NON_ASCII_VALUE_UNSAFE(Lisp_Char_Table * ct, Emchar ch);
extern_inline Lisp_Object
CHAR_TABLE_NON_ASCII_VALUE_UNSAFE(Lisp_Char_Table * ct, Emchar ch)
{
	unsigned char lb = CHAR_LEADING_BYTE(ch);
	if (!CHAR_TABLE_ENTRYP((ct)->level1[lb - MIN_LEADING_BYTE]))
		return (ct)->level1[lb - MIN_LEADING_BYTE];
	else
		return get_non_ascii_char_table_value(ct, lb, ch);
}

#define CHAR_TABLE_VALUE_UNSAFE(ct, ch)		\
  ((ch) < NUM_ASCII_CHARS			\
   ? (ct)->ascii[ch]				\
   : CHAR_TABLE_NON_ASCII_VALUE_UNSAFE (ct, ch))

#else				/* not MULE */

#define CHAR_TABLE_VALUE_UNSAFE(ct, ch)	((ct)->ascii[(unsigned char) (ch)])

#endif				/* not MULE */

#define XCHAR_TABLE_VALUE_UNSAFE(ct, ch) \
  CHAR_TABLE_VALUE_UNSAFE (XCHAR_TABLE (ct), ch)

enum chartab_range_type {
	CHARTAB_RANGE_ALL,
#ifdef MULE
	CHARTAB_RANGE_CHARSET,
	CHARTAB_RANGE_ROW,
#endif
	CHARTAB_RANGE_CHAR
};

struct chartab_range {
	enum chartab_range_type type;
	Emchar ch;
	Lisp_Object charset;
	int row;
};

void fill_char_table(Lisp_Char_Table * ct, Lisp_Object value);
void put_char_table(Lisp_Char_Table * ct, struct chartab_range *range,
		    Lisp_Object val);
Lisp_Object get_char_table(Emchar, Lisp_Char_Table *);
int map_char_table(Lisp_Char_Table * ct,
		   struct chartab_range *range,
		   int (*fn) (struct chartab_range * range,
			      Lisp_Object val, void *arg), void *arg);
void prune_syntax_tables(void);

EXFUN(Fcopy_char_table, 1);
EXFUN(Fmake_char_table, 1);
EXFUN(Fput_char_table, 3);
EXFUN(Fget_char_table, 2);

extern Lisp_Object Vall_syntax_tables;

#ifdef MULE
int check_category_char(Emchar ch, Lisp_Object ctbl,
			unsigned int designator, unsigned int not_p);

extern Lisp_Object Vstandard_category_table;

#define CATEGORY_DESIGNATORP(x) \
 (CHARP (x) && XCHAR (x) >= 32 && XCHAR (x) <= 126)

#define CHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    dead_wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)

#define CONCHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    x = wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)

#define CATEGORY_TABLE_VALUEP(x) \
 (NILP (x) || (BIT_VECTORP (x) && (bit_vector_length (XBIT_VECTOR (x)) == 95)))

#define CHECK_CATEGORY_TABLE_VALUE(x) do {			\
  if (!CATEGORY_TABLE_VALUEP (x))				\
    dead_wrong_type_argument (Qcategory_table_value_p, x);	\
} while (0)

#define CONCHECK_CATEGORY_TABLE_VALUE(x) do {			\
  if (!CATEGORY_TABLE_VALUEP (x))				\
    x = wrong_type_argument (Qcategory_table_value_p, x);	\
} while (0)

#endif				/* MULE */

#endif				/* INCLUDED_chartab_h_ */
