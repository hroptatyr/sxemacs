/* Simple built-in editing commands.
   Copyright (C) 1985, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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


/* Synched up with: Mule 2.0, FSF 19.30. */

#include <config.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"
#include "ui/insdel.h"

Lisp_Object Qkill_forward_chars;
Lisp_Object Qself_insert_command;
Lisp_Object Qno_self_insert;

Lisp_Object Vblink_paren_function;

/* A possible value for a buffer's overwrite-mode variable.  */
Lisp_Object Qoverwrite_mode_binary;

/* Non-nil means put this face on the next self-inserting character.  */
Lisp_Object Vself_insert_face;

/* This is the command that set up Vself_insert_face.  */
Lisp_Object Vself_insert_face_command;

/* A char-table for characters which may invoke auto-filling.  */
Lisp_Object Vauto_fill_chars;

DEFUN("forward-char", Fforward_char, 0, 2, "_p",	/*
Move point right COUNT characters (left if COUNT is negative).
On attempt to pass end of buffer, stop and signal `end-of-buffer'.
On attempt to pass beginning of buffer, stop and signal `beginning-of-buffer'.
On reaching end of buffer, stop and signal error.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.
*/
      (count, buffer))
{
	struct buffer *buf = decode_buffer(buffer, 1);
	EMACS_INT n;

	if (NILP(count))
		n = 1;
	else {
		CHECK_INT(count);
		n = XINT(count);
	}

	/* This used to just set point to point + XINT (count), and then check
	   to see if it was within boundaries.  But now that SET_PT can
	   potentially do a lot of stuff (calling entering and exiting
	   hooks, etcetera), that's not a good approach.  So we validate the
	   proposed position, then set point.  */
	{
		Bufpos new_point = BUF_PT(buf) + n;

		if (new_point < BUF_BEGV(buf)) {
			BUF_SET_PT(buf, BUF_BEGV(buf));
			Fsignal(Qbeginning_of_buffer, Qnil);
			return Qnil;
		}
		if (new_point > BUF_ZV(buf)) {
			BUF_SET_PT(buf, BUF_ZV(buf));
			Fsignal(Qend_of_buffer, Qnil);
			return Qnil;
		}

		BUF_SET_PT(buf, new_point);
	}

	return Qnil;
}

DEFUN("backward-char", Fbackward_char, 0, 2, "_p",	/*
Move point left COUNT characters (right if COUNT is negative).
On attempt to pass end of buffer, stop and signal `end-of-buffer'.
On attempt to pass beginning of buffer, stop and signal `beginning-of-buffer'.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.
*/
      (count, buffer))
{
	if (NILP(count))
		count = make_int(-1);
	else {
		CHECK_INT(count);
		count = make_int(-XINT(count));
	}
	return Fforward_char(count, buffer);
}

DEFUN("forward-line", Fforward_line, 0, 2, "_p",	/*
Move COUNT lines forward (backward if COUNT is negative).
Precisely, if point is on line I, move to the start of line I + COUNT.
If there isn't room, go as far as possible (no error).
Returns the count of lines left to move.  If moving forward,
that is COUNT - number of lines moved; if backward, COUNT + number moved.
With positive COUNT, a non-empty line at the end counts as one line
successfully moved (for the return value).
If BUFFER is nil, the current buffer is assumed.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.
*/
      (count, buffer))
{
	struct buffer *buf = decode_buffer(buffer, 1);
	Bufpos pos2 = BUF_PT(buf);
	Bufpos pos;
	EMACS_INT n, shortage, negp;

	if (NILP(count))
		n = 1;
	else {
		CHECK_INT(count);
		n = XINT(count);
	}

	negp = n <= 0;
	pos = scan_buffer(buf, '\n', pos2, 0, n - negp, &shortage, 1);
	if (shortage > 0 && (negp || (BUF_ZV(buf) > BUF_BEGV(buf)
				      && pos != pos2
				      && BUF_FETCH_CHAR(buf, pos - 1) != '\n')))
		shortage--;
	BUF_SET_PT(buf, pos);
	return make_int(negp ? -shortage : shortage);
}

DEFUN("point-at-bol", Fpoint_at_bol, 0, 2, 0,	/*
Return the character position of the first character on the current line.
With argument COUNT not nil or 1, move forward COUNT - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point.
*/
      (count, buffer))
{
	struct buffer *b = decode_buffer(buffer, 1);
	REGISTER int orig, end;

	XSETBUFFER(buffer, b);
	if (NILP(count))
		count = make_int(0);
	else {
		CHECK_INT(count);
		count = make_int(XINT(count) - 1);
	}

	orig = BUF_PT(b);
	Fforward_line(count, buffer);
	end = BUF_PT(b);
	BUF_SET_PT(b, orig);

	return make_int(end);
}

DEFUN("beginning-of-line", Fbeginning_of_line, 0, 2, "_p",	/*
Move point to beginning of current line.
With argument COUNT not nil or 1, move forward COUNT - 1 lines first.
If scan reaches end of buffer, stop there without error.
If BUFFER is nil, the current buffer is assumed.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.
*/
      (count, buffer))
{
	struct buffer *b = decode_buffer(buffer, 1);

	BUF_SET_PT(b, XINT(Fpoint_at_bol(count, buffer)));
	return Qnil;
}

DEFUN("point-at-eol", Fpoint_at_eol, 0, 2, 0,	/*
Return the character position of the last character on the current line.
With argument COUNT not nil or 1, move forward COUNT - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point.
*/
      (count, buffer))
{
	struct buffer *buf = decode_buffer(buffer, 1);
	EMACS_INT n;

	if (NILP(count))
		n = 1;
	else {
		CHECK_INT(count);
		n = XINT(count);
	}

	return make_int(find_before_next_newline(buf, BUF_PT(buf), 0,
						 n - (n <= 0)));
}

DEFUN("end-of-line", Fend_of_line, 0, 2, "_p",	/*
Move point to end of current line.
With argument COUNT not nil or 1, move forward COUNT - 1 lines first.
If scan reaches end of buffer, stop there without error.
If BUFFER is nil, the current buffer is assumed.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.
*/
      (count, buffer))
{
	struct buffer *b = decode_buffer(buffer, 1);

	BUF_SET_PT(b, XINT(Fpoint_at_eol(count, buffer)));
	return Qnil;
}

DEFUN("delete-char", Fdelete_char, 0, 2, "*p\nP",	/*
Delete the following COUNT characters (previous, with negative COUNT).
Optional second arg KILLP non-nil means kill instead (save in kill ring).
Interactively, COUNT is the prefix arg, and KILLP is set if
COUNT was explicitly specified.
*/
      (count, killp))
{
	/* This function can GC */
	Bufpos pos;
	struct buffer *buf = current_buffer;
	EMACS_INT n;

	if (NILP(count))
		n = 1;
	else {
		CHECK_INT(count);
		n = XINT(count);
	}

	pos = BUF_PT(buf) + n;
	if (NILP(killp)) {
		if (n < 0) {
			if (pos < BUF_BEGV(buf))
				signal_error(Qbeginning_of_buffer, Qnil);
			else
				buffer_delete_range(buf, pos, BUF_PT(buf), 0);
		} else {
			if (pos > BUF_ZV(buf))
				signal_error(Qend_of_buffer, Qnil);
			else
				buffer_delete_range(buf, BUF_PT(buf), pos, 0);
		}
	} else {
		call1(Qkill_forward_chars, count);
	}
	return Qnil;
}

DEFUN("delete-backward-char", Fdelete_backward_char, 0, 2, "*p\nP",	/*
Delete the previous COUNT characters (following, with negative COUNT).
Optional second arg KILLP non-nil means kill instead (save in kill ring).
Interactively, COUNT is the prefix arg, and KILLP is set if
COUNT was explicitly specified.
*/
      (count, killp))
{
	/* This function can GC */
	EMACS_INT n;

	if (NILP(count))
		n = 1;
	else {
		CHECK_INT(count);
		n = XINT(count);
	}

	return Fdelete_char(make_int(-n), killp);
}

static void internal_self_insert(Emchar ch, int noautofill);

DEFUN("self-insert-command", Fself_insert_command, 1, 1, "*p",	/*
Insert the character you type.
Whichever character you type to run this command is inserted.
If a prefix arg COUNT is specified, the character is inserted COUNT times.
*/
      (count))
{
	/* This function can GC */
	Emchar ch;
	Lisp_Object c;
	EMACS_INT n;

	CHECK_NATNUM(count);
	n = XINT(count);

	if (CHAR_OR_CHAR_INTP(Vlast_command_char))
		c = Vlast_command_char;
	else
		c = Fevent_to_character(Vlast_command_event, Qnil, Qnil, Qt);

	if (NILP(c))
		signal_simple_error
		    ("Last typed character has no ASCII equivalent",
		     Fcopy_event(Vlast_command_event, Qnil));

	CHECK_CHAR_COERCE_INT(c);

	ch = XCHAR(c);

	while (n--)
		internal_self_insert(ch, (n != 0));

	return Qnil;
}

/* Insert character C1.  If NOAUTOFILL is nonzero, don't do autofill
   even if it is enabled.

   FSF:

   If this insertion is suitable for direct output (completely simple),
   return 0.  A value of 1 indicates this *might* not have been simple.
   A value of 2 means this did things that call for an undo boundary.  */

static void internal_self_insert(Emchar c1, int noautofill)
{
	/* This function can GC */
	/* int hairy = 0; -- unused */
	REGISTER enum syntaxcode synt;
	REGISTER Emchar c2;
	Lisp_Object overwrite;
	Lisp_Char_Table *syntax_table;
	struct buffer *buf = current_buffer;
	int tab_width;

	overwrite = buf->overwrite_mode;
	syntax_table = XCHAR_TABLE(buf->mirror_syntax_table);

#if 0
	/* No, this is very bad, it makes undo *always* undo a character at a time
	   instead of grouping consecutive self-inserts together.  Nasty nasty.
	 */
	if (!NILP(Vbefore_change_functions) || !NILP(Vafter_change_functions)
	    || !NILP(Vbefore_change_function) || !NILP(Vafter_change_function))
		hairy = 1;
#endif

	if (!NILP(overwrite)
	    && BUF_PT(buf) < BUF_ZV(buf)
	    && (EQ(overwrite, Qoverwrite_mode_binary)
		|| (c1 != '\n' && BUF_FETCH_CHAR(buf, BUF_PT(buf)) != '\n'))
	    && (EQ(overwrite, Qoverwrite_mode_binary)
		|| BUF_FETCH_CHAR(buf, BUF_PT(buf)) != '\t'
		|| ((tab_width = XINT(buf->tab_width), tab_width <= 0)
		    || tab_width > 20
		    || !((current_column(buf) + 1) % tab_width)))) {
		buffer_delete_range(buf, BUF_PT(buf), BUF_PT(buf) + 1, 0);
		/* hairy = 2; */
	}

	if (!NILP(buf->abbrev_mode)
	    && !WORD_SYNTAX_P(syntax_table, c1)
	    && NILP(buf->read_only)
	    && BUF_PT(buf) > BUF_BEGV(buf)) {
		c2 = BUF_FETCH_CHAR(buf, BUF_PT(buf) - 1);

		if (WORD_SYNTAX_P(syntax_table, c2)) {
#if 1
			Fexpand_abbrev();
#else				/* FSFmacs */
			Lisp_Object sym = Fexpand_abbrev();

			/* I think this is too bogus to add.  The function should
			   have a way of examining the character to be inserted, so
			   it can decide whether to insert it or not.  We should
			   design it better than that.  */

			/* Here FSFmacs remembers MODIFF, compares it after
			   Fexpand_abbrev() finishes, and updates HAIRY.  */

			/* NOTE: we cannot simply check for Vlast_abbrev, because
			   Fexpand_abbrev() can bail out before setting it to
			   anything meaningful, leaving us stuck with an old value.
			   Thus Fexpand_abbrev() was extended to return the actual
			   abbrev symbol.  */
			if (!NILP(sym)
			    && !NILP(symbol_function(XSYMBOL(sym)))
			    && SYMBOLP(symbol_function(XSYMBOL(sym)))) {
				Lisp_Object prop =
				    Fget(symbol_function(XSYMBOL(sym)),
					 Qno_self_insert, Qnil);
				if (!NILP(prop))
					return;
			}
#endif				/* FSFmacs */
		}
	}
	if ((CHAR_TABLEP(Vauto_fill_chars)
	     ? !NILP(XCHAR_TABLE_VALUE_UNSAFE(Vauto_fill_chars, c1))
	     : (c1 == ' ' || c1 == '\n'))
	    && !noautofill && !NILP(buf->auto_fill_function)) {
		buffer_insert_emacs_char(buf, c1);
		if (c1 == '\n')
			/* After inserting a newline, move to previous line and fill */
			/* that.  Must have the newline in place already so filling and */
			/* justification, if any, know where the end is going to be. */
			BUF_SET_PT(buf, BUF_PT(buf) - 1);
		call0(buf->auto_fill_function);
		if (c1 == '\n')
			BUF_SET_PT(buf, BUF_PT(buf) + 1);
		/* hairy = 2; */
	} else
		buffer_insert_emacs_char(buf, c1);

	/* If previous command specified a face to use, use it.  */
	if (!NILP(Vself_insert_face)
	    && EQ(Vlast_command, Vself_insert_face_command)) {
		Lisp_Object before = make_int(BUF_PT(buf) - 1);
		Lisp_Object after = make_int(BUF_PT(buf));
		Fput_text_property(before, after, Qface, Vself_insert_face,
				   Qnil);
		Fput_text_property(before, after, Qstart_open, Qt, Qnil);
		Fput_text_property(before, after, Qend_open, Qnil, Qnil);
		/* #### FSFmacs properties are normally closed ("sticky") on the
		   end but not the beginning.  It's the opposite for us. */
		Vself_insert_face = Qnil;
	}
	synt = SYNTAX(syntax_table, c1);
	if ((synt == Sclose || synt == Smath)
	    && !NILP(Vblink_paren_function) && INTERACTIVE && !noautofill) {
		call0(Vblink_paren_function);
		/* hairy = 2; */
	}

	/* return hairy; */
}

/* (this comes from Mule but is a generally good idea) */

DEFUN("self-insert-internal", Fself_insert_internal, 1, 1, 0,	/*
Invoke `self-insert-command' as if CHARACTER is entered from keyboard.
*/
      (character))
{
	/* This function can GC */
	CHECK_CHAR_COERCE_INT(character);
	internal_self_insert(XCHAR(character), 0);
	return Qnil;
}

/* module initialization */

void syms_of_cmds(void)
{
	defsymbol(&Qkill_forward_chars, "kill-forward-chars");
	defsymbol(&Qself_insert_command, "self-insert-command");
	defsymbol(&Qoverwrite_mode_binary, "overwrite-mode-binary");
	defsymbol(&Qno_self_insert, "no-self-insert");

	DEFSUBR(Fforward_char);
	DEFSUBR(Fbackward_char);
	DEFSUBR(Fforward_line);
	DEFSUBR(Fbeginning_of_line);
	DEFSUBR(Fend_of_line);

	DEFSUBR(Fpoint_at_bol);
	DEFSUBR(Fpoint_at_eol);

	DEFSUBR(Fdelete_char);
	DEFSUBR(Fdelete_backward_char);

	DEFSUBR(Fself_insert_command);
	DEFSUBR(Fself_insert_internal);
}

void vars_of_cmds(void)
{
	DEFVAR_LISP("self-insert-face", &Vself_insert_face	/*
If non-nil, set the face of the next self-inserting character to this.
See also `self-insert-face-command'.
								 */ );
	Vself_insert_face = Qnil;

	DEFVAR_LISP("self-insert-face-command", &Vself_insert_face_command	/*
This is the command that set up `self-insert-face'.
If `last-command' does not equal this value, we ignore `self-insert-face'.
										 */ );
	Vself_insert_face_command = Qnil;

	DEFVAR_LISP("blink-paren-function", &Vblink_paren_function	/*
Function called, if non-nil, whenever a close parenthesis is inserted.
More precisely, a char with closeparen syntax is self-inserted.
									 */ );
	Vblink_paren_function = Qnil;

	DEFVAR_LISP("auto-fill-chars", &Vauto_fill_chars	/*
A char-table for characters which invoke auto-filling.
Such characters have value t in this table.
								 */ );
	Vauto_fill_chars = Fmake_char_table(Qgeneric);
	XCHAR_TABLE(Vauto_fill_chars)->ascii[' '] = Qt;
	XCHAR_TABLE(Vauto_fill_chars)->ascii['\n'] = Qt;
}
