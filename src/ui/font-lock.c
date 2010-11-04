/* Routines to compute the current syntactic context, for font-lock mode.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
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


/* Synched up with: Not in FSF. */

/* This code computes the syntactic context of the current point, that is,
   whether point is within a comment, a string, what have you.  It does
   this by picking a point "known" to be outside of any syntactic constructs
   and moving forward, examining the syntax of each character.

   Two caches are used: one caches the last point computed, and the other
   caches the last point at the beginning of a line.  This makes there
   be little penalty for moving left-to-right on a line a character at a
   time; makes starting over on a line be cheap; and makes random-accessing
   within a line relatively cheap.

   When we move to a different line farther down in the file (but within the
   current top-level form) we simply continue computing forward.  If we move
   backward more than a line, or move beyond the end of the current tlf, or
   switch buffers, then we call `beginning-of-defun' and start over from
   there.

   #### We should really rewrite this to keep extents over the buffer
   that hold the current syntactic information.  This would be a big win.
   This way there would be no guessing or incorrect results.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "insdel.h"
#include "syntax.h"

Lisp_Object Qcomment;
Lisp_Object Qblock_comment;
Lisp_Object Qbeginning_of_defun;

enum syntactic_context {
	context_none,
	context_string,
	context_comment,
	context_block_comment,
	context_generic_comment,
	context_generic_string
};

enum block_comment_context {
	ccontext_none,
	ccontext_start1,
	ccontext_start2,
	ccontext_end1
};

enum comment_style {
	comment_style_none,
	comment_style_a,
	comment_style_b
};

struct context_cache {
	Bufpos start_point;	/* beginning of defun */
	Bufpos cur_point;	/* cache location */
	Bufpos end_point;	/* end of defun */
	struct buffer *buffer;	/* does this need to be staticpro'd? */
	enum syntactic_context context;	/* single-char-syntax state */
	enum block_comment_context ccontext;	/* block-comment state */
	enum comment_style style;	/* which comment group */
	Emchar scontext;	/* active string delimiter */
	int depth;		/* depth in parens */
	int backslash_p;	/* just read a backslash */
	int needs_its_head_reexamined;	/* we're apparently outside of
					   a top level form, and far away
					   from it.  This is a bad situation
					   because it will lead to constant
					   slowness as we keep going way
					   back to that form and moving
					   forward again.  In this case,
					   we try to compute a "pseudo-
					   top-level-form" where the
					   depth is 0 and the context
					   is none at both ends. */
};

/* We have two caches; one for the current point and one for
   the beginning of line.  We used to rely on the caller to
   tell us when to invalidate them, but now we do it ourselves;
   it lets us be smarter. */

static struct context_cache context_cache;

static struct context_cache bol_context_cache;

int font_lock_debug;

#define reset_context_cache(cc) memset (cc, 0, sizeof (struct context_cache))

/* This function is called from signal_after_change() to tell us when
   textual changes are made so we can flush our caches when necessary.

   We make the following somewhat heuristic assumptions:

     (remember that current_point is always >= start_point, but may be
     less than or greater than end_point (we might not be inside any
     top-level form)).

   1) Textual changes before the beginning of the current top-level form
      don't affect anything; all we need to do is offset the caches
      appropriately.
   2) Textual changes right at the beginning of the current
      top-level form messes things up and requires that we flush
      the caches.
   3) Textual changes after the beginning of the current top-level form
      and before one or both or the caches invalidates the corresponding
      cache(s).
   4) Textual changes after the caches and before the end of the
      current top-level form don't affect anything; all we need to do is
      offset the caches appropriately.
   5) Textual changes right at the end of the current top-level form
      necessitate recomputing that end value.
   6) Textual changes after the end of the current top-level form
      are ignored. */

void
font_lock_maybe_update_syntactic_caches(struct buffer *buf, Bufpos start,
					Bufpos orig_end, Bufpos new_end)
{
	/* Note: either both context_cache and bol_context_cache are valid and
	   point to the same buffer, or both are invalid.  If we have to
	   invalidate just context_cache, we recopy it from bol_context_cache.
	 */
	if (context_cache.buffer != buf)
		/* caches don't apply */
		return;
	/* NOTE: The order of the if statements below is important.  If you
	   change them around unthinkingly, you will probably break something. */
	if (orig_end <= context_cache.start_point - 1) {
		/* case 1: before the beginning of the current top-level form */
		Charcount diff = new_end - orig_end;
		if (font_lock_debug)
			stderr_out("font-lock; Case 1\n");
		context_cache.start_point += diff;
		context_cache.cur_point += diff;
		context_cache.end_point += diff;
		bol_context_cache.start_point += diff;
		bol_context_cache.cur_point += diff;
		bol_context_cache.end_point += diff;
	} else if (start <= context_cache.start_point) {
		if (font_lock_debug)
			stderr_out("font-lock; Case 2\n");
		/* case 2: right at the current top-level form (paren that starts
		   top level form got deleted or moved away from the newline it
		   was touching) */
		reset_context_cache(&context_cache);
		reset_context_cache(&bol_context_cache);
	}
	/* OK, now we know that the start is after the beginning of the
	   current top-level form. */
	else if (start < bol_context_cache.cur_point) {
		if (font_lock_debug)
			stderr_out("font-lock; Case 3 (1)\n");
		/* case 3: after the beginning of the current top-level form
		   and before both of the caches */
		reset_context_cache(&context_cache);
		reset_context_cache(&bol_context_cache);
	} else if (start < context_cache.cur_point) {
		if (font_lock_debug)
			stderr_out("font-lock; Case 3 (2)\n");
		/* case 3: but only need to invalidate one cache */
		context_cache = bol_context_cache;
	}
	/* OK, now we know that the start is after the caches. */
	else if (start >= context_cache.end_point) {
		if (font_lock_debug)
			stderr_out("font-lock; Case 6\n");
		/* case 6: after the end of the current top-level form
		   and after the caches. */
	} else if (orig_end <= context_cache.end_point - 2) {
		/* case 4: after the caches and before the end of the
		   current top-level form */
		Charcount diff = new_end - orig_end;
		if (font_lock_debug)
			stderr_out("font-lock; Case 4\n");
		context_cache.end_point += diff;
		bol_context_cache.end_point += diff;
	} else {
		if (font_lock_debug)
			stderr_out("font-lock; Case 5\n");
		/* case 5: right at the end of the current top-level form */
		context_cache.end_point = context_cache.start_point - 1;
		bol_context_cache.end_point = context_cache.start_point - 1;
	}
}

/* This function is called from Fkill_buffer(). */

void font_lock_buffer_was_killed(struct buffer *buf)
{
	if (context_cache.buffer == buf) {
		reset_context_cache(&context_cache);
		reset_context_cache(&bol_context_cache);
	}
}

static Bufpos beginning_of_defun(struct buffer *buf, Bufpos pt)
{
	/* This function can GC */
	Bufpos opt = BUF_PT(buf);
	if (pt == BUF_BEGV(buf))
		return pt;
	BUF_SET_PT(buf, pt);
	/* There used to be some kludginess to call c++-beginning-of-defun
	   if we're in C++ mode.  There's no point in this any more;
	   we're using cc-mode.  If you really want to get the old c++
	   mode working, fix it rather than the C code. */
	call0_in_buffer(buf, Qbeginning_of_defun);
	pt = BUF_PT(buf);
	BUF_SET_PT(buf, opt);
	return pt;
}

static Bufpos end_of_defun(struct buffer *buf, Bufpos pt)
{
	Lisp_Object retval = scan_lists(buf, pt, 1, 0, 0, 1);
	if (NILP(retval))
		return BUF_ZV(buf);
	else
		return XINT(retval);
}

/* Set up context_cache for attempting to determine the syntactic context
   in buffer BUF at point PT. */

static void setup_context_cache(struct buffer *buf, Bufpos pt)
{
	int recomputed_start_point = 0;
	/* This function can GC */
	if (context_cache.buffer != buf || pt < context_cache.start_point) {
	      start_over:
		if (font_lock_debug)
			stderr_out("reset context cache\n");
		/* OK, completely invalid. */
		reset_context_cache(&context_cache);
		reset_context_cache(&bol_context_cache);
	}
	if (!context_cache.buffer) {
		/* Need to recompute the start point. */
		if (font_lock_debug)
			stderr_out("recompute start\n");
		context_cache.start_point = beginning_of_defun(buf, pt);
		recomputed_start_point = 1;
		bol_context_cache.start_point = context_cache.start_point;
		bol_context_cache.buffer = context_cache.buffer = buf;
	}
	if (context_cache.end_point < context_cache.start_point) {
		/* Need to recompute the end point. */
		if (font_lock_debug)
			stderr_out("recompute end\n");
		context_cache.end_point =
		    end_of_defun(buf, context_cache.start_point);
		bol_context_cache.end_point = context_cache.end_point;
	}
	if (bol_context_cache.cur_point == 0 ||
	    pt < bol_context_cache.cur_point) {
		if (font_lock_debug)
			stderr_out("reset to start\n");
		if (pt > context_cache.end_point
		    /* 3000 is some arbitrary delta but seems reasonable;
		       about the size of a reasonable function */
		    && pt - context_cache.end_point > 3000)
			/* If we're far past the end of the top level form,
			   don't trust it; recompute it. */
		{
			/* But don't get in an infinite loop doing this.
			   If we're really far past the end of the top level
			   form, try to compute a pseudo-top-level form. */
			if (recomputed_start_point)
				context_cache.needs_its_head_reexamined = 1;
			else
				/* force recomputation */
				goto start_over;
		}
		/* Go to the nearest end of the top-level form that's before
		   us. */
		if (pt > context_cache.end_point)
			pt = context_cache.end_point;
		else
			pt = context_cache.start_point;
		/* Reset current point to start of buffer. */
		context_cache.cur_point = pt;
		context_cache.context = context_none;
		context_cache.ccontext = ccontext_none;
		context_cache.style = comment_style_none;
		context_cache.scontext = '\000';
		context_cache.depth = 0;
		/* #### shouldn't this be checking the character's syntax instead of
		   explicitly testing for backslash characters? */
		context_cache.backslash_p = ((pt > 1) &&
					     (BUF_FETCH_CHAR(buf, pt - 1) ==
					      '\\'));
		/* Note that the BOL context cache may not be at the beginning
		   of the line, but that should be OK, nobody's checking. */
		bol_context_cache = context_cache;
		return;
	} else if (pt < context_cache.cur_point) {
		if (font_lock_debug)
			stderr_out("reset to bol\n");
		/* bol cache is OK but current_cache is not. */
		context_cache = bol_context_cache;
		return;
	} else if (pt <= context_cache.end_point) {
		if (font_lock_debug)
			stderr_out("everything is OK\n");
		/* in same top-level form. */
		return;
	}
	{
		/* OK, we're past the end of the top-level form. */
		Bufpos maxpt =
		    max(context_cache.end_point, context_cache.cur_point);
#if 0
		int shortage;
#endif

		if (font_lock_debug)
			stderr_out("past end\n");
		if (pt <= maxpt)
			/* OK, fine. */
			return;
#if 0
		/* This appears to cause huge slowdowns in files which have no
		   top-level forms.

		   In any case, it's not really necessary that we know for
		   sure the top-level form we're in; if we're in a form
		   but the form we have recorded is the previous one,
		   it will be OK. */

		scan_buffer(buf, '\n', maxpt, pt, 1, &shortage, 1);
		if (!shortage)
			/* If there was a newline in the region past the known universe,
			   we might be inside another top-level form, so start over.
			   Otherwise, we're outside of any top-level forms and we know
			   the one directly before us, so it's OK. */
			goto start_over;
#endif
	}
}

/* GCC 2.95.4 seems to need the casts */
#define SYNTAX_START_STYLE(c1, c2)					\
  ((enum comment_style)                                                 \
   (SYNTAX_CODES_MATCH_START_P (c1, c2, SYNTAX_COMMENT_STYLE_A) ?	\
   comment_style_a :							\
   SYNTAX_CODES_MATCH_START_P (c1, c2, SYNTAX_COMMENT_STYLE_B) ?	\
   comment_style_b :							\
   comment_style_none))

#define SYNTAX_END_STYLE(c1, c2)				\
  ((enum comment_style)                                         \
   (SYNTAX_CODES_MATCH_END_P (c1, c2, SYNTAX_COMMENT_STYLE_A) ?	\
   comment_style_a :						\
   SYNTAX_CODES_MATCH_END_P (c1, c2, SYNTAX_COMMENT_STYLE_B) ?	\
   comment_style_b :						\
   comment_style_none))

#define SINGLE_SYNTAX_STYLE(c)					\
  ((enum comment_style)                                         \
   (SYNTAX_CODE_MATCHES_1CHAR_P (c, SYNTAX_COMMENT_STYLE_A) ?	\
   comment_style_a :						\
   SYNTAX_CODE_MATCHES_1CHAR_P (c, SYNTAX_COMMENT_STYLE_B) ?	\
   comment_style_b :						\
   comment_style_none))

/* Set up context_cache for position PT in BUF. */

static void find_context(struct buffer *buf, Bufpos pt)
{
	/* This function can GC */
#ifndef emacs
	Lisp_Char_Table *mirrortab = XCHAR_TABLE(buf->mirror_syntax_table);
	Lisp_Object syntaxtab = buf->syntax_table;
#endif
	Emchar prev_c, c;
	int prev_syncode, syncode;
	Bufpos target = pt;
	setup_context_cache(buf, pt);
	pt = context_cache.cur_point;

	SCS_STATISTICS_SET_FUNCTION(scs_find_context);
	SETUP_SYNTAX_CACHE(pt - 1, 1);
	if (pt > BUF_BEGV(buf)) {
		c = BUF_FETCH_CHAR(buf, pt - 1);
		syncode = SYNTAX_CODE_FROM_CACHE(mirrortab, c);
	} else {
		c = '\n';	/* to get bol_context_cache at point-min */
		syncode = Swhitespace;
	}

	for (; pt < target; pt++, context_cache.cur_point = pt) {
		if (context_cache.needs_its_head_reexamined) {
			if (context_cache.depth == 0
			    && context_cache.context == context_none) {
				/* We've found an anchor spot.
				   Try to put the start of defun within 6000 chars of
				   the target, and the end of defun as close as possible.
				   6000 is also arbitrary but tries to strike a balance
				   between two conflicting pulls when dealing with a
				   file that has lots of stuff sitting outside of a top-
				   level form:

				   a) If you move past the start of defun, you will
				   have to recompute defun, which in this case
				   means that start of defun goes all the way back
				   to the beginning of the file; so you want
				   to set start of defun a ways back from the
				   current point.
				   b) If you move a line backwards but within start of
				   defun, you have to move back to start of defun;
				   so you don't want start of defun too far from
				   the current point.
				 */
				if (target - context_cache.start_point > 6000)
					context_cache.start_point = pt;
				context_cache.end_point = pt;
				bol_context_cache = context_cache;
			}
		}

		UPDATE_SYNTAX_CACHE_FORWARD(pt);
		prev_c = c;
		prev_syncode = syncode;
		c = BUF_FETCH_CHAR(buf, pt);
		syncode = SYNTAX_CODE_FROM_CACHE(mirrortab, c);

		if (prev_c == '\n')
			bol_context_cache = context_cache;

		if (context_cache.backslash_p) {
			context_cache.backslash_p = 0;
			continue;
		}

		switch (SYNTAX_FROM_CACHE(mirrortab, c)) {
		case Sescape:
			context_cache.backslash_p = 1;
			break;

		case Sopen:
			if (context_cache.context == context_none)
				context_cache.depth++;
			break;

		case Sclose:
			if (context_cache.context == context_none)
				context_cache.depth--;
			break;

		case Scomment:
			if (context_cache.context == context_none) {
				context_cache.context = context_comment;
				context_cache.ccontext = ccontext_none;
				context_cache.style =
				    SINGLE_SYNTAX_STYLE(syncode);
				if (context_cache.style == comment_style_none)
					abort();
			}
			break;

		case Sendcomment:
			if (context_cache.style !=
			    SINGLE_SYNTAX_STYLE(syncode)) ;
			else if (context_cache.context == context_comment) {
				context_cache.context = context_none;
				context_cache.style = comment_style_none;
			} else if (context_cache.context ==
				   context_block_comment
				   && (context_cache.ccontext == ccontext_start2
				       || context_cache.ccontext ==
				       ccontext_end1)) {
				context_cache.context = context_none;
				context_cache.ccontext = ccontext_none;
				context_cache.style = comment_style_none;
			}
			break;

		case Sstring:
			{
				if (context_cache.context == context_string &&
				    context_cache.scontext == c) {
					context_cache.context = context_none;
					context_cache.scontext = '\000';
				} else if (context_cache.context ==
					   context_none) {
					Lisp_Object stringtermobj =
					    syntax_match(syntax_cache.
							 current_syntax_table,
							 c);
					Emchar stringterm;

					if (CHARP(stringtermobj))
						stringterm =
						    XCHAR(stringtermobj);
					else
						stringterm = c;
					context_cache.context = context_string;
					context_cache.scontext = stringterm;
					context_cache.ccontext = ccontext_none;
				}
				break;
			}

		case Scomment_fence:
			{
				if (context_cache.context ==
				    context_generic_comment) {
					context_cache.context = context_none;
				} else if (context_cache.context ==
					   context_none) {
					context_cache.context =
					    context_generic_comment;
					context_cache.ccontext = ccontext_none;
				}
				break;
			}

		case Sstring_fence:
			{
				if (context_cache.context ==
				    context_generic_string) {
					context_cache.context = context_none;
				} else if (context_cache.context ==
					   context_none) {
					context_cache.context =
					    context_generic_string;
					context_cache.ccontext = ccontext_none;
				}
				break;
			}

		case Swhitespace:
		case Spunct:
		case Sword:
		case Ssymbol:
		case Squote:
		case Smath:
		case Scharquote:
		case Sinherit:
		case Smax:
		default:
			;
		}

		/* That takes care of the characters with manifest syntax.
		   Now we've got to hack multi-char sequences that start
		   and end block comments.
		 */
		if ((SYNTAX_CODE_COMMENT_BITS(syncode) & SYNTAX_SECOND_CHAR_START) && context_cache.context == context_none && context_cache.ccontext == ccontext_start1 && SYNTAX_CODES_START_P(prev_syncode, syncode)	/* the two chars match */
		    ) {
			context_cache.ccontext = ccontext_start2;
			context_cache.style =
			    SYNTAX_START_STYLE(prev_syncode, syncode);
			if (context_cache.style == comment_style_none)
				abort();
		} else if ((SYNTAX_CODE_COMMENT_BITS(syncode) &
			    SYNTAX_FIRST_CHAR_START) &&
			   context_cache.context == context_none &&
			   (context_cache.ccontext == ccontext_none ||
			    context_cache.ccontext == ccontext_start1)) {
			context_cache.ccontext = ccontext_start1;
			context_cache.style = comment_style_none;	/* should be this already */
		} else if ((SYNTAX_CODE_COMMENT_BITS(syncode) &
			    SYNTAX_SECOND_CHAR_END) &&
			   context_cache.context == context_block_comment &&
			   context_cache.ccontext == ccontext_end1 &&
			   SYNTAX_CODES_END_P(prev_syncode, syncode) &&
			   /* the two chars match */
			   context_cache.style ==
			   SYNTAX_END_STYLE(prev_syncode, syncode)
		    ) {
			context_cache.context = context_none;
			context_cache.ccontext = ccontext_none;
			context_cache.style = comment_style_none;
		} else if ((SYNTAX_CODE_COMMENT_BITS(syncode) &
			    SYNTAX_FIRST_CHAR_END) &&
			   context_cache.context == context_block_comment &&
#if 0
			   /* #### pre-Matt code had: */
			   (context_cache.style ==
			    SYNTAX_END_STYLE(c, BUF_FETCH_CHAR(buf, pt + 1))) &&
			   /* why do these differ here?! */
#endif
			   context_cache.style == SINGLE_SYNTAX_STYLE(syncode)
			   && (context_cache.ccontext == ccontext_start2
			       || context_cache.ccontext == ccontext_end1))
			/* check end1, to detect a repetition of the first char of a
			   comment-end sequence. ie, '/xxx foo xxx/' or '/xxx foo x/',
			   where 'x' = '*' -- mct */
		{
			if (context_cache.style == comment_style_none)
				abort();
			context_cache.ccontext = ccontext_end1;
		}

		else if (context_cache.ccontext == ccontext_start1) {
			if (context_cache.context != context_none)
				abort();
			context_cache.ccontext = ccontext_none;
		} else if (context_cache.ccontext == ccontext_end1) {
			if (context_cache.context != context_block_comment)
				abort();
			context_cache.context = context_none;
			context_cache.ccontext = ccontext_start2;
		}

		if (context_cache.ccontext == ccontext_start2 &&
		    context_cache.context == context_none) {
			context_cache.context = context_block_comment;
			if (context_cache.style == comment_style_none)
				abort();
		} else if (context_cache.ccontext == ccontext_none &&
			   context_cache.context == context_block_comment) {
			context_cache.context = context_none;
		}
	}

	context_cache.needs_its_head_reexamined = 0;
}

static Lisp_Object context_to_symbol(enum syntactic_context context)
{
	switch (context) {
	case context_none:
		return Qnil;
	case context_string:
		return Qstring;
	case context_comment:
		return Qcomment;
	case context_block_comment:
		return Qblock_comment;
	case context_generic_comment:
		return Qblock_comment;
	case context_generic_string:
		return Qstring;
	default:
		abort();
		return Qnil;	/* suppress compiler warning */
	}
}

DEFUN("buffer-syntactic-context", Fbuffer_syntactic_context, 0, 1, 0,	/*
Return the syntactic context of BUFFER at point.
If BUFFER is nil or omitted, the current buffer is assumed.
The returned value is one of the following symbols:

nil           ; meaning no special interpretation
string                ; meaning point is within a string
comment               ; meaning point is within a line comment
block-comment ; meaning point is within a block comment

See also the function `buffer-syntactic-context-depth', which returns
the current nesting-depth within all parenthesis-syntax delimiters
and the function `syntactically-sectionize', which will map a function
over each syntactic context in a region.

WARNING: this may alter match-data.
*/
      (buffer))
{
	/* This function can GC */
	struct buffer *buf = decode_buffer(buffer, 0);
	find_context(buf, BUF_PT(buf));
	return context_to_symbol(context_cache.context);
}

DEFUN("buffer-syntactic-context-depth", Fbuffer_syntactic_context_depth, 0, 1, 0,	/*
Return the depth within all parenthesis-syntax delimiters at point.
If BUFFER is nil or omitted, the current buffer is assumed.
WARNING: this may alter match-data.
*/
      (buffer))
{
	/* This function can GC */
	struct buffer *buf = decode_buffer(buffer, 0);
	find_context(buf, BUF_PT(buf));
	return make_int(context_cache.depth);
}

DEFUN("syntactically-sectionize", Fsyntactically_sectionize, 3, 4, 0,	/*
Call FUNCTION for each contiguous syntactic context in the region.
Call the given function with four arguments: the start and end of the
region, a symbol representing the syntactic context, and the current
depth (as returned by the functions `buffer-syntactic-context' and
`buffer-syntactic-context-depth').  When this function is called, the
current buffer will be set to BUFFER.

WARNING: this may alter match-data.
*/
      (function, start, end, buffer))
{
	/* This function can GC */
	Bufpos s, pt, e;
	int edepth;
	enum syntactic_context this_context;
	struct buffer *buf = decode_buffer(buffer, 0);

	get_buffer_range_char(buf, start, end, &s, &e, 0);

	pt = s;
	find_context(buf, pt);

	while (pt < e) {
		Bufpos estart, eend;
		/* skip over "blank" areas, and bug out at end-of-buffer. */
		while (context_cache.context == context_none) {
			pt++;
			if (pt >= e)
				goto DONE_LABEL;
			find_context(buf, pt);
		}
		/* We've found a non-blank area; keep going until we reach its end */
		this_context = context_cache.context;
		estart = pt;

		/* Minor kludge: consider the comment-start character(s) a part of
		   the comment.
		 */
		if (this_context == context_block_comment &&
		    context_cache.ccontext == ccontext_start2)
			estart -= 2;
		else if (this_context == context_comment
			 || this_context == context_generic_comment)
			estart -= 1;

		edepth = context_cache.depth;
		while (context_cache.context == this_context && pt < e) {
			pt++;
			find_context(buf, pt);
		}

		eend = pt;

		/* Minor kludge: consider the character which terminated the comment
		   a part of the comment.
		 */
		if ((this_context == context_block_comment ||
		     this_context == context_comment
		     || this_context == context_generic_comment)
		    && pt < e)
			eend++;

		if (estart == eend)
			continue;
		/* Make sure not to pass in values that are outside the
		   actual bounds of this function. */
		call4_in_buffer(buf, function, make_int(max(s, estart)),
				make_int(eend == e ? e : eend - 1),
				context_to_symbol(this_context),
				make_int(edepth));
	}
DONE_LABEL:
	return Qnil;
}

void syms_of_font_lock(void)
{
	defsymbol(&Qcomment, "comment");
	defsymbol(&Qblock_comment, "block-comment");
	defsymbol(&Qbeginning_of_defun, "beginning-of-defun");

	DEFSUBR(Fbuffer_syntactic_context);
	DEFSUBR(Fbuffer_syntactic_context_depth);
	DEFSUBR(Fsyntactically_sectionize);
}

void reinit_vars_of_font_lock(void)
{
	xzero(context_cache);
	xzero(bol_context_cache);
}

void vars_of_font_lock(void)
{
	reinit_vars_of_font_lock();
}
