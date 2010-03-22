/* undo handling for SXEmacs.
   Copyright (C) 1990, 1992, 1993, 1994 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.28. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "extents.h"

/* Maintained in event-stream.c */
extern Bufpos last_point_position;
extern Lisp_Object last_point_position_buffer;

/* Extent code needs to know about undo because the behavior of insert()
   with regard to extents varies depending on whether we are inside
   an undo or not. */
int inside_undo;

/* Last buffer for which undo information was recorded.  */
static Lisp_Object last_undo_buffer;

Lisp_Object Qinhibit_read_only;

/* The first time a command records something for undo.
   it also allocates the undo-boundary object
   which will be added to the list at the end of the command.
   This ensures we can't run out of space while trying to make
   an undo-boundary.  */
static Lisp_Object pending_boundary;

static void undo_boundary(struct buffer *b)
{
	Lisp_Object tem = Fcar(b->undo_list);
	if (!NILP(tem)) {
		/* One way or another, cons nil onto the front of the undo list.  */
		if (CONSP(pending_boundary)) {
			/* If we have preallocated the cons cell to use here,
			   use that one.  */
			XCDR(pending_boundary) = b->undo_list;
			b->undo_list = pending_boundary;
			pending_boundary = Qnil;
		} else
			b->undo_list = Fcons(Qnil, b->undo_list);
	}
}

static int undo_prelude(struct buffer *b, int hack_pending_boundary)
{
	if (EQ(b->undo_list, Qt))
		return (0);

	if (NILP(last_undo_buffer)
	    || (BUFFER_BASE_BUFFER(b)
		!= BUFFER_BASE_BUFFER(XBUFFER(last_undo_buffer)))) {
		undo_boundary(b);
		XSETBUFFER(last_undo_buffer, b);
	}

	/* Allocate a cons cell to be the undo boundary after this command.  */
	if (hack_pending_boundary && NILP(pending_boundary))
		pending_boundary = Fcons(Qnil, Qnil);

	if (BUF_MODIFF(b) <= BUF_SAVE_MODIFF(b)) {
		/* Record that an unmodified buffer is about to be changed.
		   Record the file modification date so that when undoing this
		   entry we can tell whether it is obsolete because the file was
		   saved again.  */
		b->undo_list
		    = Fcons(Fcons(Qt,
				  Fcons(make_int((b->modtime >> 16) & 0xffff),
					make_int(b->modtime & 0xffff))),
			    b->undo_list);
	}
	return 1;
}

static Lisp_Object restore_inside_undo(Lisp_Object val)
{
	inside_undo = XINT(val);
	return val;
}

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

void record_insert(struct buffer *b, Bufpos beg, Charcount length)
{
	if (!undo_prelude(b, 1))
		return;

	/* If this is following another insertion and consecutive with it
	   in the buffer, combine the two.  */
	if (CONSP(b->undo_list)) {
		Lisp_Object elt;
		elt = XCAR(b->undo_list);
		if (CONSP(elt)
		    && INTP(XCAR(elt))
		    && INTP(XCDR(elt))
		    && XINT(XCDR(elt)) == beg) {
			XCDR(elt) = make_int(beg + length);
			return;
		}
	}

	b->undo_list = Fcons(Fcons(make_int(beg),
				   make_int(beg + length)), b->undo_list);
}

/* Record that a deletion is about to take place,
   for LENGTH characters at location BEG.  */

void record_delete(struct buffer *b, Bufpos beg, Charcount length)
{
	/* This function can GC */
	Lisp_Object sbeg;
	int at_boundary;

	if (!undo_prelude(b, 1))
		return;

	at_boundary = (CONSP(b->undo_list)
		       && NILP(XCAR(b->undo_list)));

	if (BUF_PT(b) == beg + length)
		sbeg = make_int(-beg);
	else
		sbeg = make_int(beg);

	/* If we are just after an undo boundary, and
	   point wasn't at start of deleted range, record where it was.  */
	if (at_boundary && BUFFERP(last_point_position_buffer)
	    && b == XBUFFER(last_point_position_buffer)
	    && last_point_position != XINT(sbeg))
		b->undo_list =
		    Fcons(make_int(last_point_position), b->undo_list);

	b->undo_list = Fcons(Fcons(make_string_from_buffer(b, beg,
							   length),
				   sbeg), b->undo_list);
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement does not change the number of characters.  */

void record_change(struct buffer *b, Bufpos beg, Charcount length)
{
	record_delete(b, beg, length);
	record_insert(b, beg, length);
}

/* Record that an EXTENT is about to be attached or detached in its buffer.
   This works much like a deletion or insertion, except that there's no string.
   The tricky part is that the buffer we operate on comes from EXTENT.
   Most extent changes happen as a side effect of string insertion and
   deletion; this call is solely for Fdetach_extent() and Finsert_extent().
   */
void record_extent(Lisp_Object extent, int attached)
{
	Lisp_Object obj = Fextent_object(extent);

	if (BUFFERP(obj)) {
		Lisp_Object token;
		struct buffer *b = XBUFFER(obj);
		if (!undo_prelude(b, 1))
			return;
		if (attached)
			token = extent;
		else
			token = list3(extent, Fextent_start_position(extent),
				      Fextent_end_position(extent));
		b->undo_list = Fcons(token, b->undo_list);
	} else
		return;
}

#if 0				/* FSFmacs */
/* Record a change in property PROP (whose old value was VAL)
   for LENGTH characters starting at position BEG in BUFFER.  */

record_property_change(Bufpos beg, Charcount length,
		       Lisp_Object prop, Lisp_Object value, Lisp_Object buffer)
{
	Lisp_Object lbeg, lend, entry;
	struct buffer *b = XBUFFER(buffer);

	if (!undo_prelude(b, 1))
		return;

	lbeg = make_int(beg);
	lend = make_int(beg + length);
	entry = Fcons(Qnil, Fcons(prop, Fcons(value, Fcons(lbeg, lend))));
	b->undo_list = Fcons(entry, b->undo_list);
}
#endif				/* FSFmacs */

DEFUN("undo-boundary", Fundo_boundary, 0, 0, 0,	/*
Mark a boundary between units of undo.
An undo command will stop at this point,
but another undo command will undo to the previous boundary.
*/
      ())
{
	if (EQ(current_buffer->undo_list, Qt))
		return Qnil;
	undo_boundary(current_buffer);
	return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.
   MINSIZE and MAXSIZE are the limits on size allowed, as described below.
   In practice, these are the values of undo-threshold and
   undo-high-threshold.  */

Lisp_Object truncate_undo_list(Lisp_Object list, int minsize, int maxsize)
{
	Lisp_Object prev, next, last_boundary;
	int size_so_far = 0;

	if (!(minsize > 0 || maxsize > 0))
		return list;

	prev = Qnil;
	next = list;
	last_boundary = Qnil;

	if (!CONSP(list))
		return (list);

	/* Always preserve at least the most recent undo record.
	   If the first element is an undo boundary, skip past it. */
	if (CONSP(next)
	    && NILP(XCAR(next))) {
		/* Add in the space occupied by this element and its chain link.  */
		size_so_far += sizeof(Lisp_Cons);

		/* Advance to next element.  */
		prev = next;
		next = XCDR(next);
	}
	while (CONSP(next)
	       && !NILP(XCAR(next))) {
		Lisp_Object elt;
		elt = XCAR(next);

		/* Add in the space occupied by this element and its chain link.  */
		size_so_far += sizeof(Lisp_Cons);
		if (CONSP(elt)) {
			size_so_far += sizeof(Lisp_Cons);
			if (STRINGP(XCAR(elt)))
				size_so_far += (sizeof(Lisp_String) - 1
						+ XSTRING_LENGTH(XCAR(elt)));
		}

		/* Advance to next element.  */
		prev = next;
		next = XCDR(next);
	}
	if (CONSP(next))
		last_boundary = prev;

	while (CONSP(next)) {
		Lisp_Object elt;
		elt = XCAR(next);

		/* When we get to a boundary, decide whether to truncate
		   either before or after it.  The lower threshold, MINSIZE,
		   tells us to truncate after it.  If its size pushes past
		   the higher threshold MAXSIZE as well, we truncate before it.  */
		if (NILP(elt)) {
			if (size_so_far > maxsize && maxsize > 0)
				break;
			last_boundary = prev;
			if (size_so_far > minsize && minsize > 0)
				break;
		}

		/* Add in the space occupied by this element and its chain link.  */
		size_so_far += sizeof(Lisp_Cons);
		if (CONSP(elt)) {
			size_so_far += sizeof(Lisp_Cons);
			if (STRINGP(XCAR(elt)))
				size_so_far += (sizeof(Lisp_String) - 1
						+ XSTRING_LENGTH(XCAR(elt)));
		}

		/* Advance to next element.  */
		prev = next;
		next = XCDR(next);
	}

	/* If we scanned the whole list, it is short enough; don't change it.  */
	if (NILP(next))
		return list;

	/* Truncate at the boundary where we decided to truncate.  */
	if (!NILP(last_boundary)) {
		XCDR(last_boundary) = Qnil;
		return list;
	} else
		return Qnil;
}

DEFUN("primitive-undo", Fprimitive_undo, 2, 2, 0,	/*
Undo COUNT records from the front of the list LIST.
Return what remains of the list.
*/
      (count, list))
{
	struct gcpro gcpro1, gcpro2;
	Lisp_Object next = Qnil;
	/* This function can GC */
	int arg;
	int speccount = specpdl_depth();

	record_unwind_protect(restore_inside_undo, make_int(inside_undo));
	inside_undo = 1;

#if 0				/* This is a good feature, but would make undo-start
				   unable to do what is expected.  */
	Lisp_Object tem;

	/* If the head of the list is a boundary, it is the boundary
	   preceding this command.  Get rid of it and don't count it.  */
	tem = Fcar(list);
	if (NILP(tem))
		list = Fcdr(list);
#endif

	CHECK_INT(count);
	arg = XINT(count);
	next = Qnil;
	GCPRO2(next, list);

	/* Don't let read-only properties interfere with undo.  */
	if (NILP(current_buffer->read_only))
		specbind(Qinhibit_read_only, Qt);

	while (arg > 0) {
		while (1) {
			if (NILP(list))
				break;
			else if (!CONSP(list))
				goto rotten;
			next = XCAR(list);
			list = XCDR(list);
			/* Exit inner loop at undo boundary.  */
			if (NILP(next))
				break;
			/* Handle an integer by setting point to that value.  */
			else if (INTP(next))
				BUF_SET_PT(current_buffer,
					   bufpos_clip_to_bounds(BUF_BEGV
								 (current_buffer),
								 XINT(next),
								 BUF_ZV
								 (current_buffer)));
			else if (CONSP(next)) {
				Lisp_Object car = XCAR(next);
				Lisp_Object cdr = XCDR(next);

				if (EQ(car, Qt)) {
					/* Element (t high . low) records previous modtime.  */
					Lisp_Object high, low;
					int mod_time;
					if (!CONSP(cdr))
						goto rotten;
					high = XCAR(cdr);
					low = XCDR(cdr);
					if (!INTP(high) || !INTP(low))
						goto rotten;
					mod_time =
					    (XINT(high) << 16) + XINT(low);
					/* If this records an obsolete save
					   (not matching the actual disk file)
					   then don't mark unmodified.  */
					if (mod_time != current_buffer->modtime)
						break;
#ifdef CLASH_DETECTION
					Funlock_buffer();
#endif				/* CLASH_DETECTION */
					/* may GC under ENERGIZE: */
					Fset_buffer_modified_p(Qnil, Qnil);
				} else if (EXTENTP(car)) {
					/* Element (extent start end) means that EXTENT was
					   detached, and we need to reattach it. */
					Lisp_Object extent_obj, start, end;

					extent_obj = car;
					start = Fcar(cdr);
					end = Fcar(Fcdr(cdr));

					if (!INTP(start) || !INTP(end))
						goto rotten;
					Fset_extent_endpoints(extent_obj, start,
							      end,
							      Fcurrent_buffer
							      ());
				}
#if 0				/* FSFmacs */
				else if (EQ(car, Qnil)) {
					/* Element (nil prop val beg . end) is property change.  */
					Lisp_Object beg, end, prop, val;

					prop = Fcar(cdr);
					cdr = Fcdr(cdr);
					val = Fcar(cdr);
					cdr = Fcdr(cdr);
					beg = Fcar(cdr);
					end = Fcdr(cdr);

					Fput_text_property(beg, end, prop, val,
							   Qnil);
				}
#endif				/* FSFmacs */
				else if (INTP(car) && INTP(cdr)) {
					/* Element (BEG . END) means range was inserted.  */

					if (XINT(car) < BUF_BEGV(current_buffer)
					    || XINT(cdr) >
					    BUF_ZV(current_buffer))
						error
						    ("Changes to be undone are outside visible portion of buffer");
					/* Set point first thing, so that undoing this undo
					   does not send point back to where it is now.  */
					Fgoto_char(car, Qnil);
					Fdelete_region(car, cdr, Qnil);
				} else if (STRINGP(car) && INTP(cdr)) {
					/* Element (STRING . POS) means STRING was deleted.  */
					Lisp_Object membuf = car;
					int pos = XINT(cdr);

					if (pos < 0) {
						if (-pos <
						    BUF_BEGV(current_buffer)
						    || -pos >
						    BUF_ZV(current_buffer))
							error
							    ("Changes to be undone are outside visible portion of buffer");
						BUF_SET_PT(current_buffer,
							   -pos);
						Finsert(1, &membuf);
					} else {
						if (pos <
						    BUF_BEGV(current_buffer)
						    || pos >
						    BUF_ZV(current_buffer))
							error
							    ("Changes to be undone are outside visible portion of buffer");
						BUF_SET_PT(current_buffer, pos);

						/* Insert before markers so that if the mark is
						   currently on the boundary of this deletion, it
						   ends up on the other side of the now-undeleted
						   text from point.  Since undo doesn't even keep
						   track of the mark, this isn't really necessary,
						   but it may lead to better behavior in certain
						   situations.

						   I'm doubtful that this is safe; you could mess
						   up the process-output mark in shell buffers, so
						   until I hear a compelling reason for this change,
						   I'm leaving it out. -jwz
						 */
						/* Finsert_before_markers (1, &membuf); */
						Finsert(1, &membuf);
						BUF_SET_PT(current_buffer, pos);
					}
				} else {
					goto rotten;
				}
			} else if (EXTENTP(next))
				Fdetach_extent(next);
			else {
			      rotten:
				signal_simple_continuable_error
				    ("Something rotten in the state of undo",
				     next);
			}
		}
		arg--;
	}

	UNGCPRO;
	return unbind_to(speccount, list);
}

void syms_of_undo(void)
{
	DEFSUBR(Fprimitive_undo);
	DEFSUBR(Fundo_boundary);
	defsymbol(&Qinhibit_read_only, "inhibit-read-only");
}

void reinit_vars_of_undo(void)
{
	inside_undo = 0;
}

void vars_of_undo(void)
{
	reinit_vars_of_undo();

	pending_boundary = Qnil;
	staticpro(&pending_boundary);
	last_undo_buffer = Qnil;
	staticpro(&last_undo_buffer);
}
