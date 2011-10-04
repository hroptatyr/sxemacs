/* SXEmacs routines to deal with case tables.
   Copyright (C) 1987, 1992, 1993, 1994 Free Software Foundation, Inc.
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


 /* Synched up with: FSF 19.28.  Between FSF 19.28 and 19.30, casetab.c
    was rewritten to use junky FSF char tables.  Meanwhile I rewrote it
    to use more logical char tables.  RMS also discards the "list of four
    tables" format and instead stuffs the other tables as "extra slots"
    in the downcase table.  I've kept the four-lists format for now. */

/* Written by Howard Gayle.  See some mythical and not-in-the-Emacs-
   distribution file chartab.c for details. */

/* Modified for Mule by Ben Wing. */

/* Case table consists of four char-table.  Those are for downcase,
   upcase, canonical and equivalent respectively.

   It's entry is like this:

   downcase:	a -> a, A -> a.
   upcase:	a -> A, A -> a.  (The latter is for NOCASEP.)
   canon:	a -> a, A -> a.
   eqv:		a -> A, A -> a.
*/

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "opaque.h"
#include "chartab.h"
#include "casetab.h"

Lisp_Object Qcase_tablep, Qdowncase, Qupcase;
Lisp_Object Vstandard_case_table;

static void compute_trt_inverse(Lisp_Object trt, Lisp_Object inverse);
Lisp_Object case_table_char(Lisp_Object ch, Lisp_Object table);

#define STRING256_P(obj) ((STRINGP (obj) && XSTRING_CHAR_LENGTH (obj) == 256))

static Lisp_Object mark_case_table(Lisp_Object obj)
{
	Lisp_Case_Table *ct = XCASE_TABLE(obj);

	mark_object(CASE_TABLE_DOWNCASE(ct));
	mark_object(CASE_TABLE_UPCASE(ct));
	mark_object(CASE_TABLE_CANON(ct));
	mark_object(CASE_TABLE_EQV(ct));
	return Qnil;
}

static void
print_case_table(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Case_Table *ct = XCASE_TABLE(obj);
	if (print_readably)
		error("printing unreadable object #<case-table 0x%x",
		      ct->header.uid);
	write_fmt_str(printcharfun, "#<case-table 0x%x>", ct->header.uid);
}

static const struct lrecord_description case_table_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_Case_Table, downcase_table)},
	{XD_LISP_OBJECT, offsetof(Lisp_Case_Table, upcase_table)},
	{XD_LISP_OBJECT, offsetof(Lisp_Case_Table, case_canon_table)},
	{XD_LISP_OBJECT, offsetof(Lisp_Case_Table, case_eqv_table)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("case-table", case_table,
			      mark_case_table, print_case_table, 0,
			      0, 0, case_table_description, Lisp_Case_Table);

static Lisp_Object allocate_case_table(void)
{
	Lisp_Object val;
	Lisp_Case_Table *ct =
	    alloc_lcrecord_type(Lisp_Case_Table, &lrecord_case_table);

	SET_CASE_TABLE_DOWNCASE(ct, Qnil);
	SET_CASE_TABLE_UPCASE(ct, Qnil);
	SET_CASE_TABLE_CANON(ct, Qnil);
	SET_CASE_TABLE_EQV(ct, Qnil);

	XSETCASE_TABLE(val, ct);
	return val;
}

DEFUN("case-table-p", Fcase_table_p, 1, 1, 0,	/*
Return t if OBJECT is a case table.
See `set-case-table' for more information on these data structures.
*/
      (object))
{
	if (CASE_TABLEP(object))
		return Qt;
	else {
		Lisp_Object down, up, canon, eqv;
		if (!CONSP(object))
			return Qnil;
		down = XCAR(object);
		object = XCDR(object);
		if (!CONSP(object))
			return Qnil;
		up = XCAR(object);
		object = XCDR(object);
		if (!CONSP(object))
			return Qnil;
		canon = XCAR(object);
		object = XCDR(object);
		if (!CONSP(object))
			return Qnil;
		eqv = XCAR(object);

		return ((STRING256_P(down)
			 && (NILP(up) || STRING256_P(up))
			 && ((NILP(canon) && NILP(eqv))
			     || STRING256_P(canon))
			 && (NILP(eqv) || STRING256_P(eqv)))
			? Qt : Qnil);

	}
}

static Lisp_Object check_case_table(Lisp_Object object)
{
	/* This function can GC */
	while (NILP(Fcase_table_p(object)))
		object = wrong_type_argument(Qcase_tablep, object);
	return object;
}

Lisp_Object case_table_char(Lisp_Object ch, Lisp_Object table)
{
	Lisp_Object ct_char;
	ct_char = get_char_table(XCHAR(ch), XCHAR_TABLE(table));
	if (NILP(ct_char))
		return ch;
	else
		return ct_char;
}

DEFUN("get-case-table", Fget_case_table, 3, 3, 0,	/*
Return CHAR-CASE version of CHARACTER in CASE-TABLE.

CHAR-CASE is either downcase or upcase.
*/
      (char_case, character, case_table))
{
	CHECK_CHAR(character);
	CHECK_CASE_TABLE(case_table);
	if (EQ(char_case, Qdowncase))
		return case_table_char(character,
				       XCASE_TABLE_DOWNCASE(case_table));
	else if (EQ(char_case, Qupcase))
		return case_table_char(character,
				       XCASE_TABLE_UPCASE(case_table));
	else
		signal_simple_error("Char case must be downcase or upcase",
				    char_case);

	return Qnil;		/* Not reached. */
}

DEFUN("put-case-table", Fput_case_table, 4, 4, 0,	/*
Set CHAR-CASE version of CHARACTER to be VALUE in CASE-TABLE.

CHAR-CASE is either downcase or upcase.
See also `put-case-table-pair'.
*/
      (char_case, character, value, case_table))
{
	CHECK_CHAR(character);
	CHECK_CHAR(value);

	if (EQ(char_case, Qdowncase)) {
		Fput_char_table(character, value,
				XCASE_TABLE_DOWNCASE(case_table));
		/* This one is not at all intuitive.  */
		Fput_char_table(character, value,
				XCASE_TABLE_UPCASE(case_table));
		Fput_char_table(character, value,
				XCASE_TABLE_CANON(case_table));
		Fput_char_table(value, value, XCASE_TABLE_CANON(case_table));
		Fput_char_table(value, character, XCASE_TABLE_EQV(case_table));
		Fput_char_table(character, value, XCASE_TABLE_EQV(case_table));
	} else if (EQ(char_case, Qupcase)) {
		Fput_char_table(character, value,
				XCASE_TABLE_UPCASE(case_table));
		Fput_char_table(character, character,
				XCASE_TABLE_DOWNCASE(case_table));
		Fput_char_table(character, character,
				XCASE_TABLE_CANON(case_table));
		Fput_char_table(value, character,
				XCASE_TABLE_CANON(case_table));
		Fput_char_table(value, character, XCASE_TABLE_EQV(case_table));
		Fput_char_table(character, value, XCASE_TABLE_EQV(case_table));
	} else
		signal_simple_error("Char case must be downcase or upcase",
				    char_case);

	return Qnil;
}

DEFUN("put-case-table-pair", Fput_case_table_pair, 3, 3, 0,	/*
Make UC and LC a pair of inter-case-converting letters in CASE-TABLE.
UC is an uppercase character and LC is a downcase character.
*/
      (uc, lc, case_table))
{
	CHECK_CHAR(uc);
	CHECK_CHAR(lc);
	CHECK_CASE_TABLE(case_table);

	Fput_char_table(lc, lc, XCASE_TABLE_DOWNCASE(case_table));
	Fput_char_table(uc, lc, XCASE_TABLE_UPCASE(case_table));
	Fput_char_table(uc, lc, XCASE_TABLE_DOWNCASE(case_table));
	Fput_char_table(lc, uc, XCASE_TABLE_UPCASE(case_table));

	Fput_char_table(lc, lc, XCASE_TABLE_CANON(case_table));
	Fput_char_table(uc, lc, XCASE_TABLE_CANON(case_table));
	Fput_char_table(uc, lc, XCASE_TABLE_EQV(case_table));
	Fput_char_table(lc, uc, XCASE_TABLE_EQV(case_table));
	return Qnil;
}

DEFUN("copy-case-table", Fcopy_case_table, 1, 1, 0,	/*
Return a new case table which is a copy of CASE-TABLE
*/
      (case_table))
{
	Lisp_Object new_obj;
	CHECK_CASE_TABLE(case_table);

	new_obj = allocate_case_table();
	XSET_CASE_TABLE_DOWNCASE
	    (new_obj, Fcopy_char_table(XCASE_TABLE_DOWNCASE(case_table)));
	XSET_CASE_TABLE_UPCASE
	    (new_obj, Fcopy_char_table(XCASE_TABLE_UPCASE(case_table)));
	XSET_CASE_TABLE_CANON
	    (new_obj, Fcopy_char_table(XCASE_TABLE_CANON(case_table)));
	XSET_CASE_TABLE_EQV
	    (new_obj, Fcopy_char_table(XCASE_TABLE_EQV(case_table)));
	return new_obj;
}

DEFUN("current-case-table", Fcurrent_case_table, 0, 1, 0,	/*
Return the case table of BUFFER, which defaults to the current buffer.
*/
      (buffer))
{
	struct buffer *buf = decode_buffer(buffer, 0);

	return buf->case_table;
}

DEFUN("standard-case-table", Fstandard_case_table, 0, 0, 0,	/*
Return the standard case table.
This is the one used for new buffers.
*/
      ())
{
	return Vstandard_case_table;
}

static Lisp_Object set_case_table(Lisp_Object table, int standard);

DEFUN("set-case-table", Fset_case_table, 1, 1, 0, /*
Select CASE-TABLE as the new case table for the current buffer.
A case table is a case-table object or list
DOWNCASE UPCASE CANONICALIZE EQUIVALENCES
where each element is either nil or a string of length 256.
The latter is provided for backward-compatibility.
DOWNCASE maps each character to its lower-case equivalent.
UPCASE maps each character to its upper-case equivalent,
if lower and upper case characters are in 1-1 correspondence,
you may use nil and the upcase table will be deduced from DOWNCASE.
CANONICALIZE maps each character to a canonical equivalent,
any two characters that are related by case-conversion have the same
canonical equivalent character, it may be nil, in which case it is
deduced from DOWNCASE and UPCASE.
EQUIVALENCES is a map that cyclicly permutes each equivalence class
of characters with the same canonical equivalent it may be nil,
in which case it is deduced from CANONICALIZE.

See also `get-case-table', `put-case-table' and `put-case-table-pair'.
*/
      (case_table))
{
	/* This function can GC */
	return set_case_table(case_table, 0);
}

DEFUN("set-standard-case-table", Fset_standard_case_table, 1, 1, 0,	/*
Select CASE-TABLE as the new standard case table for new buffers.
See `set-case-table' for more info on case tables.
*/
      (case_table))
{
	/* This function can GC */
	return set_case_table(case_table, 1);
}

static Lisp_Object set_case_table(Lisp_Object table, int standard)
{
	/* This function can GC */
	struct buffer *buf =
	    standard ? XBUFFER(Vbuffer_defaults) : current_buffer;

	check_case_table(table);

	if (CASE_TABLEP(table)) {
		if (standard)
			Vstandard_case_table = table;

		buf->case_table = table;
	} else {
		/* For backward compatibility. */
		Lisp_Object down, up, canon, eqv, tail = table;
		Lisp_Object temp;
		int i;

		down = XCAR(tail);
		tail = XCDR(tail);
		up = XCAR(tail);
		tail = XCDR(tail);
		canon = XCAR(tail);
		tail = XCDR(tail);
		eqv = XCAR(tail);

		temp = down;
		down = MAKE_TRT_TABLE();
		for (i = 0; i < 256; i++)
			SET_TRT_TABLE_CHAR_1(down, i,
					     string_char(XSTRING(temp), i));

		if (NILP(up)) {
			up = MAKE_TRT_TABLE();
			compute_trt_inverse(down, up);
		} else {
			temp = up;
			up = MAKE_TRT_TABLE();
			for (i = 0; i < 256; i++)
				SET_TRT_TABLE_CHAR_1(up, i,
						     string_char(XSTRING(temp),
								 i));
		}
		if (NILP(canon)) {
			canon = MAKE_TRT_TABLE();

			/* Set up the CANON table; for each character,
			   this sequence of upcasing and downcasing ought to
			   get the "preferred" lowercase equivalent.  */
			for (i = 0; i < 256; i++)
				SET_TRT_TABLE_CHAR_1(canon, i,
						     TRT_TABLE_CHAR_1
						     (down,
						      TRT_TABLE_CHAR_1
						      (up,
						       TRT_TABLE_CHAR_1(down,
									i))));
		} else {
			temp = canon;
			canon = MAKE_TRT_TABLE();
			for (i = 0; i < 256; i++)
				SET_TRT_TABLE_CHAR_1(canon, i,
						     string_char(XSTRING(temp),
								 i));
		}

		if (NILP(eqv)) {
			eqv = MAKE_TRT_TABLE();
			compute_trt_inverse(canon, eqv);
		} else {
			temp = eqv;
			eqv = MAKE_TRT_TABLE();
			for (i = 0; i < 256; i++)
				SET_TRT_TABLE_CHAR_1(eqv, i,
						     string_char(XSTRING(temp),
								 i));
		}

		if (standard) {
			XSET_CASE_TABLE_DOWNCASE(Vstandard_case_table, down);
			XSET_CASE_TABLE_UPCASE(Vstandard_case_table, up);
			XSET_CASE_TABLE_CANON(Vstandard_case_table, canon);
			XSET_CASE_TABLE_EQV(Vstandard_case_table, eqv);
		}

		buf->case_table = allocate_case_table();
		XSET_CASE_TABLE_DOWNCASE(buf->case_table, down);
		XSET_CASE_TABLE_UPCASE(buf->case_table, up);
		XSET_CASE_TABLE_CANON(buf->case_table, canon);
		XSET_CASE_TABLE_EQV(buf->case_table, eqv);
	}

	return buf->case_table;
}

/* Given a translate table TRT, store the inverse mapping into INVERSE.
   Since TRT is not one-to-one, INVERSE is not a simple mapping.
   Instead, it divides the space of characters into equivalence classes.
   All characters in a given class form one circular list, chained through
   the elements of INVERSE.  */

static void compute_trt_inverse(Lisp_Object trt, Lisp_Object inverse)
{
	Charcount i = 0400;
	Emchar c, q;

	while (--i)
		SET_TRT_TABLE_CHAR_1(inverse, i, (Emchar) i);
	i = 0400;
	while (--i) {
		if ((q = TRT_TABLE_CHAR_1(trt, i)) != (Emchar) i) {
			c = TRT_TABLE_CHAR_1(inverse, q);
			SET_TRT_TABLE_CHAR_1(inverse, q, (Emchar) i);
			SET_TRT_TABLE_CHAR_1(inverse, i, c);
		}
	}
}

void syms_of_casetab(void)
{
	INIT_LRECORD_IMPLEMENTATION(case_table);

	defsymbol(&Qcase_tablep, "case-table-p");
	defsymbol(&Qdowncase, "downcase");
	defsymbol(&Qupcase, "upcase");

	DEFSUBR(Fcase_table_p);
	DEFSUBR(Fget_case_table);
	DEFSUBR(Fput_case_table);
	DEFSUBR(Fput_case_table_pair);
	DEFSUBR(Fcurrent_case_table);
	DEFSUBR(Fstandard_case_table);
	DEFSUBR(Fcopy_case_table);
	DEFSUBR(Fset_case_table);
	DEFSUBR(Fset_standard_case_table);
}

void complex_vars_of_casetab(void)
{
	REGISTER Emchar i;
	Lisp_Object tem;

	staticpro(&Vstandard_case_table);

	Vstandard_case_table = allocate_case_table();

	tem = MAKE_TRT_TABLE();
	XSET_CASE_TABLE_DOWNCASE(Vstandard_case_table, tem);
	XSET_CASE_TABLE_CANON(Vstandard_case_table, tem);

	/* Under Mule, can't do set_string_char() until Vcharset_control_1
	   and Vcharset_ascii are initialized. */
	for (i = 0; i < 256; i++) {
		unsigned char lowered = tolower(i);

		SET_TRT_TABLE_CHAR_1(tem, i, lowered);
	}

	tem = MAKE_TRT_TABLE();
	XSET_CASE_TABLE_UPCASE(Vstandard_case_table, tem);
	XSET_CASE_TABLE_EQV(Vstandard_case_table, tem);

	for (i = 0; i < 256; i++) {
		unsigned char flipped = (isupper(i) ? tolower(i)
					 : (islower(i) ? toupper(i) : i));

		SET_TRT_TABLE_CHAR_1(tem, i, flipped);
	}
}
