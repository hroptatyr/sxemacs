/* SXEmacs case conversion functions.
   Copyright (C) 1985, 1992, 1993, 1994, 1997, 1998 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.34, but substantially rewritten by Martin. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "ui/insdel.h"
#include "syntax.h"

enum case_action { CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP };

static Lisp_Object
casify_object(enum case_action flag, Lisp_Object string_or_char,
	      Lisp_Object buffer)
{
	struct buffer *buf = decode_buffer(buffer, 0);

      retry:

	if (CHAR_OR_CHAR_INTP(string_or_char)) {
		Emchar c;
		CHECK_CHAR_COERCE_INT(string_or_char);
		c = XCHAR(string_or_char);
		c = (flag == CASE_DOWN) ? DOWNCASE(buf, c) : UPCASE(buf, c);
		return make_char(c);
	}

	if (STRINGP(string_or_char)) {
		Lisp_Char_Table *syntax_table =
		    XCHAR_TABLE(buf->mirror_syntax_table);
		Bufbyte *storage =
		    alloca_array(Bufbyte,
				 XSTRING_LENGTH(string_or_char) *
				 MAX_EMCHAR_LEN);
		Bufbyte *newp = storage;
		Bufbyte *oldp = XSTRING_DATA(string_or_char);
		int wordp = 0, wordp_prev;

		while (*oldp) {
			Emchar c = charptr_emchar(oldp);
			switch (flag) {
			case CASE_UP:
				c = UPCASE(buf, c);
				break;
			case CASE_DOWN:
				c = DOWNCASE(buf, c);
				break;
			case CASE_CAPITALIZE:
			case CASE_CAPITALIZE_UP:
				wordp_prev = wordp;
				wordp = WORD_SYNTAX_P(syntax_table, c);
				if (!wordp)
					break;
				if (wordp_prev) {
					if (flag == CASE_CAPITALIZE)
						c = DOWNCASE(buf, c);
				} else
					c = UPCASE(buf, c);
				break;

				/* can't happen */
			default:
				/* abort()? */
				break;
			}

			newp += set_charptr_emchar(newp, c);
			INC_CHARPTR(oldp);
		}

		return make_string(storage, newp - storage);
	}

	string_or_char = wrong_type_argument(Qchar_or_string_p, string_or_char);
	goto retry;
}

DEFUN("upcase", Fupcase, 1, 2, 0,	/*
Convert STRING-OR-CHAR to upper case and return that.
STRING-OR-CHAR may be a character or string.  The result has the same type.
STRING-OR-CHAR is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'.
Optional second arg BUFFER specifies which buffer's case tables to use,
and defaults to the current buffer.
*/
      (string_or_char, buffer))
{
	return casify_object(CASE_UP, string_or_char, buffer);
}

DEFUN("downcase", Fdowncase, 1, 2, 0,	/*
Convert STRING-OR-CHAR to lower case and return that.
STRING-OR-CHAR may be a character or string.  The result has the same type.
STRING-OR-CHAR is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
and defaults to the current buffer.
*/
      (string_or_char, buffer))
{
	return casify_object(CASE_DOWN, string_or_char, buffer);
}

DEFUN("capitalize", Fcapitalize, 1, 2, 0,	/*
Convert STRING-OR-CHAR to capitalized form and return that.
This means that each word's first character is upper case
and the rest is lower case.
STRING-OR-CHAR may be a character or string.  The result has the same type.
STRING-OR-CHAR is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
and defaults to the current buffer.
*/
      (string_or_char, buffer))
{
	return casify_object(CASE_CAPITALIZE, string_or_char, buffer);
}

/* Like Fcapitalize but change only the initial characters.  */

DEFUN("upcase-initials", Fupcase_initials, 1, 2, 0,	/*
Convert the initial of each word in STRING-OR-CHAR to upper case.
Do not change the other letters of each word.
STRING-OR-CHAR may be a character or string.  The result has the same type.
STRING-OR-CHAR is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
and defaults to the current buffer.
*/
      (string_or_char, buffer))
{
	return casify_object(CASE_CAPITALIZE_UP, string_or_char, buffer);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   START and END specify range of buffer to operate on. */

static void
casify_region_internal(enum case_action flag, Lisp_Object start,
		       Lisp_Object end, struct buffer *buf)
{
	/* This function can GC */
	Bufpos pos, s, e;
	Lisp_Char_Table *syntax_table = XCHAR_TABLE(buf->mirror_syntax_table);
	int mccount;
	int wordp = 0, wordp_prev;

	if (EQ(start, end))
		/* Not modifying because nothing marked */
		return;

	get_buffer_range_char(buf, start, end, &s, &e, 0);

	mccount = begin_multiple_change(buf, s, e);
	record_change(buf, s, e - s);

	for (pos = s; pos < e; pos++) {
		Emchar oldc = BUF_FETCH_CHAR(buf, pos);
		Emchar c = oldc;

		switch (flag) {
		case CASE_UP:
			c = UPCASE(buf, oldc);
			break;
		case CASE_DOWN:
			c = DOWNCASE(buf, oldc);
			break;
		case CASE_CAPITALIZE:
		case CASE_CAPITALIZE_UP:
			/* !!#### need to revalidate the start and end pointers in case
			   the buffer was changed */
			wordp_prev = wordp;
			wordp = WORD_SYNTAX_P(syntax_table, c);
			if (!wordp)
				continue;
			if (wordp_prev) {
				if (flag == CASE_CAPITALIZE)
					c = DOWNCASE(buf, c);
			} else
				c = UPCASE(buf, c);
			break;

			/* can't happen */
		default:
			/* abort()? */
			break;
		}

		if (oldc == c)
			continue;
		buffer_replace_char(buf, pos, c, 1, (pos == s));
		BUF_MODIFF(buf)++;
	}

	end_multiple_change(buf, mccount);
}

static Lisp_Object
casify_region(enum case_action flag, Lisp_Object start, Lisp_Object end,
	      Lisp_Object buffer)
{
	casify_region_internal(flag, start, end, decode_buffer(buffer, 1));
	return Qnil;
}

DEFUN("upcase-region", Fupcase_region, 2, 3, "r",	/*
Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also `capitalize-region'.
Optional third arg BUFFER defaults to the current buffer.
*/
      (start, end, buffer))
{
	/* This function can GC */
	return casify_region(CASE_UP, start, end, buffer);
}

DEFUN("downcase-region", Fdowncase_region, 2, 3, "r",	/*
Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
Optional third arg BUFFER defaults to the current buffer.
*/
      (start, end, buffer))
{
	/* This function can GC */
	return casify_region(CASE_DOWN, start, end, buffer);
}

DEFUN("capitalize-region", Fcapitalize_region, 2, 3, "r",	/*
Convert the region to capitalized form.
Capitalized form means each word's first character is upper case
and the rest of it is lower case.
In programs, give two arguments, the starting and ending
character positions to operate on.
Optional third arg BUFFER defaults to the current buffer.
*/
      (start, end, buffer))
{
	/* This function can GC */
	return casify_region(CASE_CAPITALIZE, start, end, buffer);
}

/* Like Fcapitalize_region but change only the initials.  */

DEFUN("upcase-initials-region", Fupcase_initials_region, 2, 3, "r",	/*
Upcase the initial of each word in the region.
Subsequent letters of each word are not changed.
In programs, give two arguments, the starting and ending
character positions to operate on.
Optional third arg BUFFER defaults to the current buffer.
*/
      (start, end, buffer))
{
	return casify_region(CASE_CAPITALIZE_UP, start, end, buffer);
}

static Lisp_Object
casify_word(enum case_action flag, Lisp_Object arg, Lisp_Object buffer)
{
	Bufpos farend;
	struct buffer *buf = decode_buffer(buffer, 1);

	CHECK_INT(arg);

	farend = scan_words(buf, BUF_PT(buf), XINT(arg));
	if (!farend)
		farend = XINT(arg) > 0 ? BUF_ZV(buf) : BUF_BEGV(buf);

	casify_region_internal(flag, make_int(BUF_PT(buf)), make_int(farend),
			       buf);
	BUF_SET_PT(buf, max(BUF_PT(buf), farend));
	return Qnil;
}

DEFUN("upcase-word", Fupcase_word, 1, 2, "p",	/*
Convert following word (or COUNT words) to upper case, moving over.
With negative argument, convert previous words but do not move.
See also `capitalize-word'.
Optional second arg BUFFER defaults to the current buffer.
*/
      (count, buffer))
{
	/* This function can GC */
	return casify_word(CASE_UP, count, buffer);
}

DEFUN("downcase-word", Fdowncase_word, 1, 2, "p",	/*
Convert following word (or COUNT words) to lower case, moving over.
With negative argument, convert previous words but do not move.
Optional second arg BUFFER defaults to the current buffer.
*/
      (count, buffer))
{
	/* This function can GC */
	return casify_word(CASE_DOWN, count, buffer);
}

DEFUN("capitalize-word", Fcapitalize_word, 1, 2, "p",	/*
Capitalize the following word (or COUNT words), moving over.
This gives the word(s) a first character in upper case
and the rest lower case.
With negative argument, capitalize previous words but do not move.
Optional second arg BUFFER defaults to the current buffer.
*/
      (count, buffer))
{
	/* This function can GC */
	return casify_word(CASE_CAPITALIZE, count, buffer);
}

void syms_of_casefiddle(void)
{
	DEFSUBR(Fupcase);
	DEFSUBR(Fdowncase);
	DEFSUBR(Fcapitalize);
	DEFSUBR(Fupcase_initials);
	DEFSUBR(Fupcase_region);
	DEFSUBR(Fdowncase_region);
	DEFSUBR(Fcapitalize_region);
	DEFSUBR(Fupcase_initials_region);
	DEFSUBR(Fupcase_word);
	DEFSUBR(Fdowncase_word);
	DEFSUBR(Fcapitalize_word);
}
