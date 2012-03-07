/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1992-1995 Free Software Foundation, Inc.
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


/* Synched up with: Mule 2.0, FSF 19.28.  Mule-ized except as noted.
   Substantially different from FSF. */

/* #### dmoore - All sorts of things in here can call lisp, like message.
   Track all this stuff. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "console-stream.h"
#include "events/events.h"
#include "frame.h"
#include "insdel.h"
#include "redisplay.h"
#include "window.h"

/* Depth in minibuffer invocations.  */
int minibuf_level;

Lisp_Object Qcompletion_ignore_case;

/* Nonzero means completion ignores case.  */
int completion_ignore_case;

/* List of regexps that should restrict possible completions.  */
Lisp_Object Vcompletion_regexp_list;

/* The echo area buffer. */
Lisp_Object Vecho_area_buffer;

/* Prompt to display in front of the minibuffer contents */
Lisp_Object Vminibuf_prompt;

/* Added on 97/3/14 by Jareth Hein (jhod@po.iijnet.or.jp) for input system support */
/* String to be displayed in front of prompt of the minibuffer contents */
Lisp_Object Vminibuf_preprompt;

/* Hook to run just after entry to minibuffer. */
Lisp_Object Qminibuffer_setup_hook, Vminibuffer_setup_hook;

Lisp_Object Qappend_message, Qcurrent_message_label,
    Qclear_message, Qdisplay_message;

DEFUN("minibuffer-depth", Fminibuffer_depth, 0, 0, 0,	/*
Return current depth of activations of minibuffer, a nonnegative integer.
*/
      ())
{
	return make_int(minibuf_level);
}

/* The default buffer to use as the window-buffer of minibuffer windows */
/*  Note there is special code in kill-buffer to make this unkillable */
Lisp_Object Vminibuffer_zero;

/* Actual minibuffer invocation. */

static Lisp_Object read_minibuffer_internal_unwind(Lisp_Object unwind_data)
{
	Lisp_Object frame;
	XWINDOW(minibuf_window)->last_modified[CURRENT_DISP] = Qzero;
	XWINDOW(minibuf_window)->last_modified[DESIRED_DISP] = Qzero;
	XWINDOW(minibuf_window)->last_modified[CMOTION_DISP] = Qzero;
	XWINDOW(minibuf_window)->last_facechange[CURRENT_DISP] = Qzero;
	XWINDOW(minibuf_window)->last_facechange[DESIRED_DISP] = Qzero;
	XWINDOW(minibuf_window)->last_facechange[CMOTION_DISP] = Qzero;
	Vminibuf_prompt = Felt(unwind_data, Qzero);
	minibuf_level = XINT(Felt(unwind_data, make_int(1)));
	while (CONSP(unwind_data)) {
		Lisp_Object victim = unwind_data;
		unwind_data = XCDR(unwind_data);
		free_cons(XCONS(victim));
	}

	/* If cursor is on the minibuffer line,
	   show the user we have exited by putting it in column 0.  */
	frame = Fselected_frame(Qnil);
	if (!noninteractive && !NILP(frame)
	    && !NILP(XFRAME(frame)->minibuffer_window)) {
		struct window *w = XWINDOW(XFRAME(frame)->minibuffer_window);
		redisplay_move_cursor(w, 0, 0);
	}

	return Qnil;
}

/* 97/4/13 jhod: Added for input methods */
DEFUN("set-minibuffer-preprompt", Fset_minibuffer_preprompt, 1, 1, 0,	/*
Set the minibuffer preprompt string to PREPROMPT. This is used by language
input methods to relay state information to the user.
*/
      (preprompt))
{
	if (NILP(preprompt)) {
		Vminibuf_preprompt = Qnil;
	} else {
		CHECK_STRING(preprompt);

		Vminibuf_preprompt = LISP_GETTEXT(preprompt);
	}
	return Qnil;
}

DEFUN("read-minibuffer-internal", Fread_minibuffer_internal, 1, 1, 0,	/*
Lowest-level interface to minibuffers.  Don't call this.
*/
      (prompt))
{
	/* This function can GC */
	int speccount = specpdl_depth();
	Lisp_Object val;

	CHECK_STRING(prompt);

	single_console_state();

	record_unwind_protect(read_minibuffer_internal_unwind,
			      noseeum_cons
			      (Vminibuf_prompt,
			       noseeum_cons(make_int(minibuf_level), Qnil)));
	Vminibuf_prompt = LISP_GETTEXT(prompt);

	/* NOTE: Here (or somewhere around here), in FSFmacs 19.30,
	   choose_minibuf_frame() is called.  This is the only
	   place in FSFmacs that it's called any more -- there's
	   also a call in xterm.c, but commented out, and 19.28
	   had the calls in different places.

	   choose_minibuf_frame() does the following:

	   if (!EQ (minibuf_window, selected_frame()->minibuffer_window))
	   {
	   Fset_window_buffer (selected_frame()->minibuffer_window,
	   XWINDOW (minibuf_window)->buffer);
	   minibuf_window = selected_frame()->minibuffer_window;
	   }

	   #### Note that we don't do the set-window-buffer.  This call is
	   similar, but not identical, to a set-window-buffer call made
	   in `read-from-minibuffer' in minibuf.el.  I hope it's close
	   enough, because minibuf_window isn't really exported to Lisp.

	   The comment above choose_minibuf_frame() reads:

	   Put minibuf on currently selected frame's minibuffer.
	   We do this whenever the user starts a new minibuffer
	   or when a minibuffer exits.  */

	minibuf_window = FRAME_MINIBUF_WINDOW(selected_frame());

	run_hook(Qminibuffer_setup_hook);

	minibuf_level++;
	clear_echo_area(selected_frame(), Qnil, 0);

	val = call_command_loop(Qt);

	return unbind_to(speccount, val);
}

/* Completion hair */

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

/* Note that this function works in Charcounts, unlike most functions.
   This is necessary for many reasons, one of which is that two
   strings may match even if they have different numbers of bytes,
   if IGNORE_CASE is true. */

Charcount
scmp_1(const Bufbyte * s1, const Bufbyte * s2, Charcount len, int ignore_case)
{
	Charcount l = len;

	if (ignore_case) {
		while (l) {
			Emchar c1 =
			    DOWNCASE(current_buffer, charptr_emchar(s1));
			Emchar c2 =
			    DOWNCASE(current_buffer, charptr_emchar(s2));

			if (c1 == c2) {
				l--;
				INC_CHARPTR(s1);
				INC_CHARPTR(s2);
			} else
				break;
		}
	} else {
		while (l && charptr_emchar(s1) == charptr_emchar(s2)) {
			l--;
			INC_CHARPTR(s1);
			INC_CHARPTR(s2);
		}
	}

	if (l == 0)
		return -1;
	else
		return len - l;
}

int
regexp_ignore_completion_p(const Bufbyte * nonreloc,
			   Lisp_Object reloc, Bytecount offset,
			   Bytecount length)
{
	/* Ignore this element if it fails to match all the regexps.  */
	if (!NILP(Vcompletion_regexp_list)) {
		Lisp_Object regexps;
		EXTERNAL_LIST_LOOP(regexps, Vcompletion_regexp_list) {
			Lisp_Object re = XCAR(regexps);
			CHECK_STRING(re);
			if (fast_string_match(re, nonreloc, reloc, offset,
					      length, 0, ERROR_ME, 0) < 0)
				return 1;
		}
	}
	return 0;
}

/* Callers should GCPRO, since this may call eval */
static int
ignore_completion_p(Lisp_Object completion_string,
		    Lisp_Object pred, Lisp_Object completion)
{
	if (regexp_ignore_completion_p(0, completion_string, 0, -1))
		return 1;

	/* Ignore this element if there is a predicate
	   and the predicate doesn't like it. */
	if (!NILP(pred)) {
		Lisp_Object tem;
		if (EQ(pred, Qcommandp))
			tem = Fcommandp(completion);
		else
			tem = call1(pred, completion);
		if (NILP(tem))
			return 1;
	}
	return 0;
}

/* #### Maybe we should allow COLLECTION to be a hash table.
   It is wrong for the use of obarrays to be better-rewarded than the
   use of hash tables.  By better-rewarded I mean that you can pass an
   obarray to all of the completion functions, whereas you can't do
   anything like that with a hash table.

   To do so, there should probably be a
   map_obarray_or_alist_or_hash_table function which would be used by
   both Ftry_completion and Fall_completions.  But would the
   additional funcalls slow things down?  */

DEFUN("try-completion", Ftry_completion, 2, 3, 0,	/*
Return common substring of all completions of STRING in COLLECTION.
COLLECTION must be an alist, an obarray, or a function.
Each string in COLLECTION is tested to see if it begins with STRING.
All that match are compared together; the longest initial sequence
common to all matches is returned as a string.  If there is no match
at all, nil is returned.  For an exact match, t is returned.

If COLLECTION is an alist, the cars of the elements of the alist
\(which must be strings) form the set of possible completions.

If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

If COLLECTION is a function, it is called with three arguments: the
values STRING, PREDICATE and nil.  Whatever it returns becomes the
value of `try-completion'.

If optional third argument PREDICATE is non-nil, it is used to test
each possible match.  The match is a candidate only if PREDICATE
returns non-nil.  The argument given to PREDICATE is the alist element
or the symbol from the obarray.
*/
      (string, collection, predicate))
{
	/* This function can GC */
	Lisp_Object bestmatch, tail;
	Charcount bestmatchsize = 0;
	int list;
	int indice = 0;
	int matchcount = 0;
	int obsize;
	Lisp_Object bucket;
	Charcount slength, blength;

	CHECK_STRING(string);

	if (CONSP(collection)) {
		Lisp_Object tem = XCAR(collection);
		if (SYMBOLP(tem))	/* lambda, autoload, etc.  Emacs-lisp sucks */
			return call3(collection, string, predicate, Qnil);
		else
			list = 1;
	} else if (VECTORP(collection))
		list = 0;
	else if (NILP(collection))
		list = 1;
	else
		return call3(collection, string, predicate, Qnil);

	bestmatch = Qnil;
	blength = 0;
	slength = XSTRING_CHAR_LENGTH(string);

	/* If COLLECTION is not a list, set TAIL just for gc pro.  */
	tail = collection;
	if (!list) {
		obsize = XVECTOR_LENGTH(collection);
		bucket = XVECTOR_DATA(collection)[indice];
	} else {		/* warning suppression */

		obsize = 0;
		bucket = Qnil;
	}

	while (1) {
		/* Get the next element of the alist or obarray. */
		/* Exit the loop if the elements are all used up. */
		/* elt gets the alist element or symbol.
		   eltstring gets the name to check as a completion. */
		Lisp_Object elt;
		Lisp_Object eltstring;

		if (list) {
			if (NILP(tail))
				break;
			elt = Fcar(tail);
			eltstring = Fcar(elt);
			tail = Fcdr(tail);
		} else {
			if (!ZEROP(bucket)) {
				Lisp_Symbol *next;
				if (!SYMBOLP(bucket)) {
					signal_simple_error
					    ("Bad obarray passed to try-completions",
					     bucket);
				}
				next = symbol_next(XSYMBOL(bucket));
				elt = bucket;
				eltstring = Fsymbol_name(elt);
				if (next)
					XSETSYMBOL(bucket, next);
				else
					bucket = Qzero;
			} else if (++indice >= obsize)
				break;
			else {
				bucket = XVECTOR_DATA(collection)[indice];
				continue;
			}
		}

		/* Is this element a possible completion? */

		if (STRINGP(eltstring)) {
			Charcount eltlength = XSTRING_CHAR_LENGTH(eltstring);
			if (slength <= eltlength
			    && (0 > scmp(XSTRING_DATA(eltstring),
					 XSTRING_DATA(string), slength))) {
				{
					struct gcpro gcpro1, gcpro2, gcpro3,
					    gcpro4;
					int loser;
					GCPRO4(tail, string, eltstring,
					       bestmatch);
					loser =
					    ignore_completion_p(eltstring,
								predicate, elt);
					UNGCPRO;
					if (loser)	/* reject this one */
						continue;
				}

				/* Update computation of how much all possible
				   completions match */

				matchcount++;
				if (NILP(bestmatch)) {
					bestmatch = eltstring;
					blength = eltlength;
					bestmatchsize = eltlength;
				} else {
					Charcount compare =
					    min(bestmatchsize, eltlength);
					Charcount matchsize =
					    scmp(XSTRING_DATA(bestmatch),
						 XSTRING_DATA(eltstring),
						 compare);
					if (matchsize < 0)
						matchsize = compare;
					if (completion_ignore_case) {
						/* If this is an exact match except for case,
						   use it as the best match rather than one that is not
						   an exact match.  This way, we get the case pattern
						   of the actual match.  */
						if ((matchsize == eltlength
						     && matchsize < blength)
						    ||
						    /* If there is more than one exact match ignoring
						       case, and one of them is exact including case,
						       prefer that one.  */
						    /* If there is no exact match ignoring case,
						       prefer a match that does not change the case
						       of the input.  */
						    ((matchsize == eltlength)
						     == (matchsize == blength)
						     && 0 >
						     scmp_1(XSTRING_DATA
							    (eltstring),
							    XSTRING_DATA
							    (string), slength,
							    0)
						     && 0 <=
						     scmp_1(XSTRING_DATA
							    (bestmatch),
							    XSTRING_DATA
							    (string), slength,
							    0))) {
							bestmatch = eltstring;
							blength = eltlength;
						}
					}
					bestmatchsize = matchsize;
				}
			}
		}
	}

	if (NILP(bestmatch))
		return Qnil;	/* No completions found */
	/* If we are ignoring case, and there is no exact match,
	   and no additional text was supplied,
	   don't change the case of what the user typed.  */
	if (completion_ignore_case
	    && bestmatchsize == slength && blength > bestmatchsize)
		return string;

	/* Return t if the supplied string is an exact match (counting case);
	   it does not require any change to be made.  */
	if (matchcount == 1
	    && bestmatchsize == slength
	    && 0 > scmp_1(XSTRING_DATA(bestmatch),
			  XSTRING_DATA(string), bestmatchsize, 0))
		return Qt;

	/* Else extract the part in which all completions agree */
	return Fsubstring(bestmatch, Qzero, make_int(bestmatchsize));
}

DEFUN("all-completions", Fall_completions, 2, 3, 0,	/*
Search for partial matches to STRING in COLLECTION.
COLLECTION must be an alist, an obarray, or a function.
Each string in COLLECTION is tested to see if it begins with STRING.
The value is a list of all the strings from COLLECTION that match.

If COLLECTION is an alist, the cars of the elements of the alist
\(which must be strings) form the set of possible completions.

If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

If COLLECTION is a function, it is called with three arguments: the
values STRING, PREDICATE and t.  Whatever it returns becomes the
value of `all-completions'.

If optional third argument PREDICATE is non-nil, it is used to test
each possible match.  The match is a candidate only if PREDICATE
returns non-nil.  The argument given to PREDICATE is the alist element
or the symbol from the obarray.
*/
      (string, collection, predicate))
{
	/* This function can GC */
	Lisp_Object tail;
	Lisp_Object allmatches;
	int list;
	int indice = 0;
	int obsize;
	Lisp_Object bucket;
	Charcount slength;

	CHECK_STRING(string);

	if (CONSP(collection)) {
		Lisp_Object tem = XCAR(collection);
		if (SYMBOLP(tem))	/* lambda, autoload, etc.  Emacs-lisp sucks */
			return call3(collection, string, predicate, Qt);
		else
			list = 1;
	} else if (VECTORP(collection))
		list = 0;
	else if (NILP(collection))
		list = 1;
	else
		return call3(collection, string, predicate, Qt);

	allmatches = Qnil;
	slength = XSTRING_CHAR_LENGTH(string);

	/* If COLLECTION is not a list, set TAIL just for gc pro.  */
	tail = collection;
	if (!list) {
		obsize = XVECTOR_LENGTH(collection);
		bucket = XVECTOR_DATA(collection)[indice];
	} else {		/* warning suppression */

		obsize = 0;
		bucket = Qnil;
	}

	while (1) {
		/* Get the next element of the alist or obarray. */
		/* Exit the loop if the elements are all used up. */
		/* elt gets the alist element or symbol.
		   eltstring gets the name to check as a completion. */
		Lisp_Object elt;
		Lisp_Object eltstring;

		if (list) {
			if (NILP(tail))
				break;
			elt = Fcar(tail);
			eltstring = Fcar(elt);
			tail = Fcdr(tail);
		} else {
			if (!ZEROP(bucket)) {
				Lisp_Symbol *next =
				    symbol_next(XSYMBOL(bucket));
				elt = bucket;
				eltstring = Fsymbol_name(elt);
				if (next)
					XSETSYMBOL(bucket, next);
				else
					bucket = Qzero;
			} else if (++indice >= obsize)
				break;
			else {
				bucket = XVECTOR_DATA(collection)[indice];
				continue;
			}
		}

		/* Is this element a possible completion? */

		if (STRINGP(eltstring)
		    && (slength <= XSTRING_CHAR_LENGTH(eltstring))
		    /* Reject alternatives that start with space
		       unless the input starts with space.  */
		    && ((XSTRING_CHAR_LENGTH(string) > 0 &&
			 string_char(XSTRING(string), 0) == ' ')
			|| string_char(XSTRING(eltstring), 0) != ' ')
		    && (0 > scmp(XSTRING_DATA(eltstring),
				 XSTRING_DATA(string), slength))) {
			/* Yes.  Now check whether predicate likes it. */
			struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
			int loser;
			GCPRO4(tail, eltstring, allmatches, string);
			loser = ignore_completion_p(eltstring, predicate, elt);
			UNGCPRO;
			if (!loser)
				/* Ok => put it on the list. */
				allmatches = Fcons(eltstring, allmatches);
		}
	}

	return Fnreverse(allmatches);
}

/* Useless FSFmacs functions */
/* More than useless.  I've nuked minibuf_prompt_width so they won't
   function at all in XEmacs at the moment.  They are used to
   implement some braindamage in FSF which we aren't including. --cet */

#if 0
xxDEFUN("minibuffer-prompt", Fminibuffer_prompt, 0, 0, 0,	/*
Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.
								 */
	())
{
	return Fcopy_sequence(Vminibuf_prompt);
}

xxDEFUN("minibuffer-prompt-width", Fminibuffer_prompt_width, 0, 0, 0,	/*
Return the display width of the minibuffer prompt.
									 */
	())
{
	return make_int(minibuf_prompt_width);
}
#endif				/* 0 */

/************************************************************************/
/*                              echo area                               */
/************************************************************************/

extern int stdout_needs_newline;

static Lisp_Object
clear_echo_area_internal(struct frame *f, Lisp_Object label, int from_print,
			 int no_restore)
{
	/* This function can call lisp */
	if (!NILP(Ffboundp(Qclear_message))) {
		Lisp_Object frame;

		XSETFRAME(frame, f);
		return call4(Qclear_message, label, frame,
			     from_print ? Qt : Qnil, no_restore ? Qt : Qnil);
	} else {
		write_string_to_stdio_stream(stderr, 0, (const Bufbyte *)"\n",
					     0, 1, Qterminal, 0);
		return Qnil;
	}
}

Lisp_Object clear_echo_area(struct frame * f, Lisp_Object label, int no_restore)
{
	/* This function can call lisp */
	return clear_echo_area_internal(f, label, 0, no_restore);
}

Lisp_Object
clear_echo_area_from_print(struct frame * f, Lisp_Object label, int no_restore)
{
	/* This function can call lisp */
	return clear_echo_area_internal(f, label, 1, no_restore);
}

void
echo_area_append(struct frame *f, const Bufbyte * nonreloc, Lisp_Object reloc,
		 Bytecount offset, Bytecount length, Lisp_Object label)
{
	/* This function can call lisp */
	Lisp_Object obj = Qnil;
	struct gcpro gcpro1;
	Lisp_Object frame;

	/* There is an inlining bug in egcs-20000131 c++ that can be worked
	   around as follows:  */
#if defined (__GNUC__) && defined (__cplusplus)
	alloca(4);
#endif

	/* some callers pass in a null string as a way of clearing the echo area.
	   check for length == 0 now; if this case, neither nonreloc nor reloc
	   may be valid.  */
	if (length == 0)
		return;

	fixup_internal_substring(nonreloc, reloc, offset, &length);

	/* also check it here, in case the string was really blank. */
	if (length <= 0)
		return;

	if (!NILP(Ffboundp(Qappend_message))) {
		GCPRO1(obj);
		if (STRINGP(reloc) && offset == 0
		    && length == XSTRING_LENGTH(reloc))
			obj = reloc;
		else {
			if (STRINGP(reloc))
				nonreloc = XSTRING_DATA(reloc);
			obj = make_string(nonreloc + offset, length);
		}

		XSETFRAME(frame, f);
		call4(Qappend_message, label, obj, frame,
		      EQ(label, Qprint) ? Qt : Qnil);
		UNGCPRO;
	} else {
		if (STRINGP(reloc))
			nonreloc = XSTRING_DATA(reloc);
		write_string_to_stdio_stream(stderr, 0, nonreloc, offset,
					     length, Qterminal, 0);
	}
}

void
echo_area_message(struct frame *f, const Bufbyte * nonreloc,
		  Lisp_Object reloc, Bytecount offset, Bytecount length,
		  Lisp_Object label)
{
	/* This function can call lisp */
	clear_echo_area(f, label, 1);
	echo_area_append(f, nonreloc, reloc, offset, length, label);
}

int echo_area_active(struct frame *f)
{
	/* By definition, the echo area is active if the echo-area buffer
	   is not empty.  No need to call Lisp code. (Anyway, this function
	   is called from redisplay.) */
	struct buffer *echo_buffer = XBUFFER(Vecho_area_buffer);
	return BUF_BEGV(echo_buffer) != BUF_ZV(echo_buffer);
}

Lisp_Object echo_area_status(struct frame * f)
{
	/* This function can call lisp */
	if (!NILP(Ffboundp(Qcurrent_message_label))) {
		Lisp_Object frame;

		XSETFRAME(frame, f);
		return call1(Qcurrent_message_label, frame);
	} else
		return stdout_needs_newline ? Qmessage : Qnil;
}

Lisp_Object echo_area_contents(struct frame * f)
{
	/* See above.  By definition, the contents of the echo-area buffer
	   are the contents of the echo area. */
	return Fbuffer_substring(Qnil, Qnil, Vecho_area_buffer);
}

/* Dump an informative message to the echo area.  This function takes a
   string in internal format. */
void
message_internal(const Bufbyte * nonreloc, Lisp_Object reloc,
		 Bytecount offset, Bytecount length)
{
	/* This function can call lisp  */
	if (NILP(Vexecuting_macro))
		echo_area_message(selected_frame(), nonreloc, reloc, offset,
				  length, Qmessage);
}

void
message_append_internal(const Bufbyte * nonreloc, Lisp_Object reloc,
			Bytecount offset, Bytecount length)
{
	/* This function can call lisp  */
	if (NILP(Vexecuting_macro))
		echo_area_append(selected_frame(), nonreloc, reloc, offset,
				 length, Qmessage);
}

/* The next three functions are interfaces to message_internal() that
   take strings in external format.  message() does I18N3 translating
   on the format string; message_no_translate() does not. */

static void message_1(const char *fmt, va_list args)
{
/* MT-safe */
	/* This function can call lisp */
	if (fmt) {
		struct gcpro gcpro1;
		/* message_internal() might GC, e.g. if there are
		   after-change-hooks on the echo area buffer */
		Lisp_Object obj = Qnil;

		GCPRO1(obj);
		obj = emacs_doprnt_string_va(
			(const Bufbyte *)fmt, Qnil, -1, args);
		message_internal(0, obj, 0, -1);
		UNGCPRO;
	} else
		message_internal(0, Qnil, 0, 0);
}

static void message_append_1(const char *fmt, va_list args)
{
	/* This function can call lisp */
	if (fmt) {
		struct gcpro gcpro1;
		/* message_internal() might GC, e.g. if there are after-change-hooks
		   on the echo area buffer */
		Lisp_Object obj =
		    emacs_doprnt_string_va((const Bufbyte *)fmt, Qnil,
					   -1, args);
		GCPRO1(obj);
		message_append_internal(0, obj, 0, -1);
		UNGCPRO;
	} else
		message_append_internal(0, Qnil, 0, 0);
}

void clear_message(void)
{
	/* This function can call lisp */
	message_internal(0, Qnil, 0, 0);
}

void message(const char *fmt, ...)
{
	/* This function can call lisp */
	/* I think it's OK to pass the data of Lisp strings as arguments to
	   this function.  No GC'ing will occur until the data has already
	   been copied. */
	va_list args;

	va_start(args, fmt);
	if (fmt)
		fmt = GETTEXT(fmt);
	message_1(fmt, args);
	va_end(args);
}

void message_append(const char *fmt, ...)
{
	/* This function can call lisp */
	va_list args;

	va_start(args, fmt);
	if (fmt)
		fmt = GETTEXT(fmt);
	message_append_1(fmt, args);
	va_end(args);
}

void message_no_translate(const char *fmt, ...)
{
	/* This function can call lisp */
	/* I think it's OK to pass the data of Lisp strings as arguments to
	   this function.  No GC'ing will occur until the data has already
	   been copied. */
	va_list args;

	va_start(args, fmt);
	message_1(fmt, args);
	va_end(args);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_minibuf(void)
{
	defsymbol(&Qminibuffer_setup_hook, "minibuffer-setup-hook");

	defsymbol(&Qcompletion_ignore_case, "completion-ignore-case");

	DEFSUBR(Fminibuffer_depth);
#if 0
	DEFSUBR(Fminibuffer_prompt);
	DEFSUBR(Fminibuffer_prompt_width);
#endif
	DEFSUBR(Fset_minibuffer_preprompt);
	DEFSUBR(Fread_minibuffer_internal);

	DEFSUBR(Ftry_completion);
	DEFSUBR(Fall_completions);

	defsymbol(&Qappend_message, "append-message");
	defsymbol(&Qclear_message, "clear-message");
	defsymbol(&Qdisplay_message, "display-message");
	defsymbol(&Qcurrent_message_label, "current-message-label");
}

void reinit_vars_of_minibuf(void)
{
	minibuf_level = 0;
}

void vars_of_minibuf(void)
{
	reinit_vars_of_minibuf();

	staticpro(&Vminibuf_prompt);
	Vminibuf_prompt = Qnil;

	/* Added by Jareth Hein (jhod@po.iijnet.or.jp) for input system support */
	staticpro(&Vminibuf_preprompt);
	Vminibuf_preprompt = Qnil;

	DEFVAR_LISP("minibuffer-setup-hook", &Vminibuffer_setup_hook	/*
Normal hook run just after entry to minibuffer.
									 */ );
	Vminibuffer_setup_hook = Qnil;

	DEFVAR_BOOL("completion-ignore-case", &completion_ignore_case	/*
Non-nil means don't consider case significant in completion.
									 */ );
	completion_ignore_case = 0;

	DEFVAR_LISP("completion-regexp-list", &Vcompletion_regexp_list	/*
List of regexps that should restrict possible completions.
Each completion has to match all regexps in this list.
									 */ );
	Vcompletion_regexp_list = Qnil;
}

void reinit_complex_vars_of_minibuf(void)
{
	/* This function can GC */
#ifdef I18N3
	/* #### This needs to be fixed up so that the gettext() gets called
	   at runtime instead of at load time. */
#endif
	Vminibuffer_zero
	    = Fget_buffer_create(build_string(DEFER_GETTEXT(" *Minibuf-0*")));
	Vecho_area_buffer
	    = Fget_buffer_create(build_string(DEFER_GETTEXT(" *Echo Area*")));
}

void complex_vars_of_minibuf(void)
{
	reinit_complex_vars_of_minibuf();
}
