/* Indentation functions.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.

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


/* This file has been Mule-ized. */

/* Synched up with: 19.30.  Diverges significantly from FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "ui/device.h"
#include "extents.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/glyphs.h"
#include "ui/insdel.h"
#ifdef REGION_CACHE_NEEDS_WORK
#include "region-cache.h"
#endif
#include "ui/window.h"

Lisp_Object Qcoerce;

/* Indentation can insert tabs if this is non-zero;
   otherwise always uses spaces */
int indent_tabs_mode;

/* Avoid recalculation by remembering things in these variables. */

/* Last value returned by current_column.

   Some things set last_known_column_point to -1
   to mark the memoized value as invalid */
static int last_known_column;

/* Last buffer searched by current_column */
static struct buffer *last_known_column_buffer;

/* Value of point when current_column was called */
static Bufpos last_known_column_point;

/* Value of MODIFF when current_column was called */
static int last_known_column_modified;

static Bufpos last_visible_position(Bufpos pos, struct buffer *buf)
{
	Lisp_Object buffer;
	Lisp_Object value;

	XSETBUFFER(buffer, buf);
	value = Fprevious_single_property_change(make_int(pos), Qinvisible,
						 buffer, Qnil);
	if (NILP(value))
		return 0;	/* no visible position found */
	else
		/* #### bug bug bug!!! This will return the position of the beginning
		   of an invisible extent; this extent is very likely to be start-closed,
		   and thus the spaces inserted in `indent-to' will go inside the
		   invisible extent.

		   Not sure what the correct solution is here.  Rethink indent-to? */
		return XINT(value);
}

#ifdef REGION_CACHE_NEEDS_WORK

/* Allocate or free the width run cache, as requested by the current
   state of current_buffer's cache_long_line_scans variable.  */
static void width_run_cache_on_off(struct buffer *buf)
{
	if (NILP(buf->cache_long_line_scans)) {
		/* It should be off.  */
		if (buf->width_run_cache) {
			free_region_cache(buf->width_run_cache);
			buf->width_run_cache = 0;
			buf->width_table = Qnil;
		}
	} else {
		/* It should be on.  */
		if (buf->width_run_cache == 0) {
			buf->width_run_cache = new_region_cache();
			recompute_width_table(buf, buffer_display_table());
		}
	}
}

#endif				/* REGION_CACHE_NEEDS_WORK */

/* Cancel any recorded value of the horizontal position.  */

void invalidate_current_column(void)
{
	last_known_column_point = -1;
}

int column_at_point(struct buffer *buf, Bufpos init_pos, int cur_col)
{
	int col;
	int tab_seen;
	int tab_width = XINT(buf->tab_width);
	int post_tab;
	Bufpos pos = init_pos;
	Emchar c;

	if (tab_width <= 0 || tab_width > 1000)
		tab_width = 8;
	col = tab_seen = post_tab = 0;

	while (1) {
		if (pos <= BUF_BEGV(buf))
			break;

		pos--;
		c = BUF_FETCH_CHAR(buf, pos);
		if (c == '\t') {
			if (tab_seen)
				col =
				    ((col + tab_width) / tab_width) * tab_width;

			post_tab += col;
			col = 0;
			tab_seen = 1;
		} else if (c == '\n' ||
			   (EQ(buf->selective_display, Qt) && c == '\r'))
			break;
		else {
			/* #### This needs updating to handle the new redisplay. */
			/* #### FSFmacs looks at ctl_arrow, display tables.
			   We need to do similar. */
#if 0
			displayed_glyphs = glyphs_from_bufpos(sel_frame, buf,
							      XWINDOW
							      (selected_window),
							      pos, dp, 0, col,
							      0, 0, 0);
			col +=
			    (displayed_glyphs->columns -
			     (displayed_glyphs->begin_columns +
			      displayed_glyphs->end_columns));
#else				/* SXEmacs */
#ifdef MULE
			{
				Lisp_Object tmp = CHAR_CHARSET(c);
				col += XCHARSET_COLUMNS(tmp);
			}
#else
			col++;
#endif				/* MULE */
#endif				/* SXEmacs */
		}
	}

	if (tab_seen) {
		col = ((col + tab_width) / tab_width) * tab_width;
		col += post_tab;
	}

	if (cur_col) {
		last_known_column_buffer = buf;
		last_known_column = col;
		last_known_column_point = init_pos;
		last_known_column_modified = BUF_MODIFF(buf);
	}

	return col;
}

int string_column_at_point(Lisp_String * s, Bufpos init_pos, int tab_width)
{
	int col;
	int tab_seen;
	int post_tab;
	Bufpos pos = init_pos;
	Emchar c;

	if (tab_width <= 0 || tab_width > 1000)
		tab_width = 8;
	col = tab_seen = post_tab = 0;

	while (1) {
		if (pos <= 0)
			break;

		pos--;
		c = string_char(s, pos);
		if (c == '\t') {
			if (tab_seen)
				col =
				    ((col + tab_width) / tab_width) * tab_width;

			post_tab += col;
			col = 0;
			tab_seen = 1;
		} else if (c == '\n') {
			break;
		} else {
#ifdef MULE
			Lisp_Object tmp = CHAR_CHARSET(c);
			col += XCHARSET_COLUMNS(tmp);
#else
			col++;
#endif	/* MULE */
		}
	}

	if (tab_seen) {
		col = ((col + tab_width) / tab_width) * tab_width;
		col += post_tab;
	}

	return col;
}

int current_column(struct buffer *buf)
{
	if (buf == last_known_column_buffer
	    && BUF_PT(buf) == last_known_column_point
	    && BUF_MODIFF(buf) == last_known_column_modified)
		return last_known_column;

	return column_at_point(buf, BUF_PT(buf), 1);
}

DEFUN("current-column", Fcurrent_column, 0, 1, 0,	/*
Return the horizontal position of point.  Beginning of line is column 0.
This is calculated by adding together the widths of all the displayed
representations of the character between the start of the previous line
and point. (e.g. control characters will have a width of 2 or 4, tabs
will have a variable width.)
Ignores finite width of frame, which means that this function may return
values greater than (frame-width).
Whether the line is visible (if `selective-display' is t) has no effect;
however, ^M is treated as end of line when `selective-display' is t.
If BUFFER is nil, the current buffer is assumed.
*/
      (buffer))
{
	return make_int(current_column(decode_buffer(buffer, 0)));
}

DEFUN("indent-to", Findent_to, 1, 3, "NIndent to column: ",	/*
Indent from point with tabs and spaces until COLUMN is reached.
Optional second argument MINIMUM says always do at least MINIMUM spaces
even if that goes past COLUMN; by default, MINIMUM is zero.
If BUFFER is nil, the current buffer is assumed.
*/
      (column, minimum, buffer))
{
	/* This function can GC */
	int mincol;
	int fromcol;
	struct buffer *buf = decode_buffer(buffer, 0);
	int tab_width = XINT(buf->tab_width);
	Bufpos opoint = 0;

	CHECK_INT(column);
	if (NILP(minimum))
		minimum = Qzero;
	else
		CHECK_INT(minimum);

	XSETBUFFER(buffer, buf);

	fromcol = current_column(buf);
	mincol = fromcol + XINT(minimum);
	if (mincol < XINT(column))
		mincol = XINT(column);

	if (fromcol == mincol)
		return make_int(mincol);

	if (tab_width <= 0 || tab_width > 1000)
		tab_width = 8;

	if (!NILP(Fextent_at(make_int(BUF_PT(buf)), buffer, Qinvisible,
			     Qnil, Qnil))) {
		Bufpos last_visible = last_visible_position(BUF_PT(buf), buf);

		opoint = BUF_PT(buf);
		if (last_visible >= BUF_BEGV(buf))
			BUF_SET_PT(buf, last_visible);
		else
			error("Visible portion of buffer not modifiable");
	}

	if (indent_tabs_mode) {
		int n = mincol / tab_width - fromcol / tab_width;
		if (n != 0) {
			Finsert_char(make_char('\t'), make_int(n), Qnil,
				     buffer);

			fromcol = (mincol / tab_width) * tab_width;
		}
	}

	Finsert_char(make_char(' '), make_int(mincol - fromcol), Qnil, buffer);

	last_known_column_buffer = buf;
	last_known_column = mincol;
	last_known_column_point = BUF_PT(buf);
	last_known_column_modified = BUF_MODIFF(buf);

	/* Not in FSF: */
	if (opoint > 0)
		BUF_SET_PT(buf, opoint);

	return make_int(mincol);
}

int bi_spaces_at_point(struct buffer *b, Bytind bi_pos)
{
	Bytind bi_end = BI_BUF_ZV(b);
	int col = 0;
	Emchar c;
	int tab_width = XINT(b->tab_width);

	if (tab_width <= 0 || tab_width > 1000)
		tab_width = 8;

	while (bi_pos < bi_end &&
	       (c = BI_BUF_FETCH_CHAR(b, bi_pos),
		(c == '\t' ? (col += tab_width - col % tab_width)
		 : (c == ' ' ? ++col : 0))))
		INC_BYTIND(b, bi_pos);

	return col;
}

DEFUN("current-indentation", Fcurrent_indentation, 0, 1, 0,	/*
Return the indentation of the current line.
This is the horizontal position of the character
following any initial whitespace.
*/
      (buffer))
{
	struct buffer *buf = decode_buffer(buffer, 0);
	Bufpos pos = find_next_newline(buf, BUF_PT(buf), -1);

	XSETBUFFER(buffer, buf);

	if (!NILP(Fextent_at(make_int(pos), buffer, Qinvisible, Qnil, Qnil)))
		return Qzero;

	return make_int(bi_spaces_at_point(buf, bufpos_to_bytind(buf, pos)));
}

DEFUN("move-to-column", Fmove_to_column, 1, 3, 0,	/*
Move point to column COLUMN in the current line.
The column of a character is calculated by adding together the widths
as displayed of the previous characters in the line.
This function ignores line-continuation;
there is no upper limit on the column number a character can have
and horizontal scrolling has no effect.

If specified column is within a character, point goes after that character.
If it's past end of line, point goes to end of line.

A value of 'coerce for the second (optional) argument FORCE means if
COLUMN is in the middle of a tab character, change it to spaces.
Any other non-nil value means the same, plus if the line is too short to
reach column COLUMN, then add spaces/tabs to get there.

Returns the actual column that it moved to.
*/
      (column, force, buffer))
{
	/* This function can GC */
	Bufpos pos;
	struct buffer *buf = decode_buffer(buffer, 0);
	int col = current_column(buf);
	int goal;
	Bufpos end;
	int tab_width = XINT(buf->tab_width);

	int prev_col = 0;
	Emchar c = 0;

	XSETBUFFER(buffer, buf);
	if (tab_width <= 0 || tab_width > 1000)
		tab_width = 8;
	CHECK_NATNUM(column);
	goal = XINT(column);

      retry:
	pos = BUF_PT(buf);
	end = BUF_ZV(buf);

	/* If we're starting past the desired column,
	   back up to beginning of line and scan from there.  */
	if (col > goal) {
		pos = find_next_newline(buf, pos, -1);
		col = 0;
	}

	while (col < goal && pos < end) {
		c = BUF_FETCH_CHAR(buf, pos);
		if (c == '\n')
			break;
		if (c == '\r' && EQ(buf->selective_display, Qt))
			break;
		if (c == '\t') {
			prev_col = col;
			col += tab_width;
			col = col / tab_width * tab_width;
		} else {
			/* #### oh for the days of the complete new redisplay */
			/* #### FSFmacs looks at ctl_arrow, display tables.
			   We need to do similar. */
#if 0
			displayed_glyphs = glyphs_from_bufpos(selected_frame(),
							      buf,
							      XWINDOW
							      (Fselected_window
							       (Qnil)), pos, dp,
							      0, col, 0, 0, 0);
			col +=
			    (displayed_glyphs->columns -
			     (displayed_glyphs->begin_columns +
			      displayed_glyphs->end_columns));
#else				/* SXEmacs */
#ifdef MULE
			{
				Lisp_Object tmp = CHAR_CHARSET(c);
				col += XCHARSET_COLUMNS(tmp);
			}
#else
			col++;
#endif				/* MULE */
#endif				/* SXEmacs */
		}

		pos++;
	}

	BUF_SET_PT(buf, pos);

	/* If a tab char made us overshoot, change it to spaces
	   and scan through it again.  */
	if (!NILP(force) && col > goal && c == '\t' && prev_col < goal) {
		buffer_delete_range(buf, BUF_PT(buf) - 1, BUF_PT(buf), 0);
		Findent_to(make_int(col - 1), Qzero, buffer);
		buffer_insert_emacs_char(buf, ' ');
		goto retry;
	}

	/* If line ends prematurely, add space to the end.  */
	if (col < goal && !NILP(force) && !EQ(force, Qcoerce)) {
		col = goal;
		Findent_to(make_int(col), Qzero, buffer);
	}

	last_known_column_buffer = buf;
	last_known_column = col;
	last_known_column_point = BUF_PT(buf);
	last_known_column_modified = BUF_MODIFF(buf);

	return make_int(col);
}

#if 0				/* #### OK boys, this function needs to be present, I think.
				   It was there before the 19.12 redisplay rewrite. */

xxDEFUN("compute-motion", Fcompute_motion, 7, 7, 0,	/*
"Scan through the current buffer, calculating screen position.
Scan the current buffer forward from offset FROM,
assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--
to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--
and return the ending buffer position and screen location.

There are three additional arguments:

WIDTH is the number of columns available to display text;
this affects handling of continuation lines.
This is usually the value returned by `window-width', less one (to allow
for the continuation glyph).

OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
HSCROLL is the number of columns not being displayed at the left
margin; this is usually taken from a window's hscroll member.
TAB-OFFSET is the number of columns of the first tab that aren't
being displayed, perhaps because the line was continued within it.
If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.

WINDOW is the window to operate on.  Currently this is used only to
find the display table.  It does not matter what buffer WINDOW displays;
`compute-motion' always operates on the current buffer.

The value is a list of five elements:
(POS HPOS VPOS PREVHPOS CONTIN)
POS is the buffer position where the scan stopped.
VPOS is the vertical position where the scan stopped.
HPOS is the horizontal position where the scan stopped.

PREVHPOS is the horizontal position one character back from POS.
CONTIN is t if a line was continued after (or within) the previous character.

For example, to find the buffer position of column COL of line LINE
of a certain window, pass the window's starting location as FROM
and the window's upper-left coordinates as FROMPOS.
Pass the buffer's (point-max) as TO, to limit the scan to the end of the
visible section of the buffer, and pass LINE and COL as TOPOS.
							 */
	(from, frompos, to, topos, width, offsets, window)) {
	Lisp_Object bufpos, hpos, vpos, prevhpos, contin;
	struct position *pos;
	int hscroll, tab_offset;
	struct window *w = decode_window(window);

	CHECK_INT_COERCE_MARKER(from);
	CHECK_CONS(frompos);
	CHECK_INT(XCAR(frompos));
	CHECK_INT(XCDR(frompos));
	CHECK_INT_COERCE_MARKER(to);
	CHECK_CONS(topos);
	CHECK_INT(XCAR(topos));
	CHECK_INT(XCDR(topos));
	CHECK_INT(width);
	if (!NILP(offsets)) {
		CHECK_CONS(offsets);
		CHECK_INT(XCAR(offsets));
		CHECK_INT(XCDR(offsets));
		hscroll = XINT(XCAR(offsets));
		tab_offset = XINT(XCDR(offsets));
	} else
		hscroll = tab_offset = 0;

	pos = compute_motion(XINT(from), XINT(XCDR(frompos)),
			     XINT(XCAR(frompos)),
			     XINT(to), XINT(XCDR(topos)),
			     XINT(XCAR(topos)),
			     XINT(width), hscroll, tab_offset, w);

	XSETINT(bufpos, pos->bufpos);
	XSETINT(hpos, pos->hpos);
	XSETINT(vpos, pos->vpos);
	XSETINT(prevhpos, pos->prevhpos);

	return list5(bufpos, hpos, vpos, prevhpos, pos->contin ? Qt : Qnil);
}

#endif				/* 0 */

/* Helper for vmotion_1 - compute vertical pixel motion between
   START and END in the line start cache CACHE.  This just sums
   the line heights, including both the starting and ending lines.
*/
static int vpix_motion(line_start_cache_dynarr * cache, int start, int end)
{
	int i, vpix;

	assert(start <= end);
	assert(start >= 0);
	assert(end < Dynarr_length(cache));

	if (start<0 || end<0 || end>start) {
		/* Least bad thing in case of in fatal_failure, where
		   assert will not terminate this function... */
		return 0;
	}

	vpix = 0;
	for (i = start; i <= end; i++)
		vpix += Dynarr_atp(cache, i)->height;
	return vpix;
}

/*****************************************************************************
 vmotion_1

 Given a starting position ORIG, move point VTARGET lines in WINDOW.
 Returns the new value for point.  If the arg ret_vpos is not nil, it is
 taken to be a pointer to an int and the number of lines actually moved is
 returned in it.  If the arg ret_vpix is not nil, it is taken to be a
 pointer to an int and the vertical pixel height of the motion which
 took place is returned in it.
 ****************************************************************************/
static Bufpos
vmotion_1(struct window *w, Bufpos orig, int vtarget,
	  int *ret_vpos, int *ret_vpix)
{
	struct buffer *b = XBUFFER(w->buffer);
	int elt;

	elt = point_in_line_start_cache(w, orig, (vtarget < 0
						  ? -vtarget : vtarget));

	/* #### This assertion must be true before the if statements are hit
	   but may possibly be wrong after the call to
	   point_in_line_start_cache if orig is outside of the visible
	   region of the buffer.  Handle this. */
	assert(elt >= 0);

	/* Moving downward. */
	if (vtarget > 0) {
		int cur_line = Dynarr_length(w->line_start_cache) - 1 - elt;
		Bufpos ret_pt;

		if (cur_line > vtarget)
			cur_line = vtarget;

		/* The traditional FSF behavior is to return the end of buffer
		   position if we couldn't move far enough because we hit it.  */
		if (cur_line < vtarget)
			ret_pt = BUF_ZV(b);
		else
			ret_pt =
			    Dynarr_atp(w->line_start_cache,
				       cur_line + elt)->start;

		while (ret_pt > BUF_ZV(b) && cur_line > 0) {
			cur_line--;
			ret_pt =
			    Dynarr_atp(w->line_start_cache,
				       cur_line + elt)->start;
		}

		if (ret_vpos)
			*ret_vpos = cur_line;
		if (ret_vpix)
			*ret_vpix =
			    vpix_motion(w->line_start_cache, elt,
					cur_line + elt);
		return ret_pt;
	} else if (vtarget < 0) {
		if (elt < -vtarget) {
			if (ret_vpos)
				*ret_vpos = -elt;
			if (ret_vpix)
				*ret_vpix =
				    vpix_motion(w->line_start_cache, 0, elt);
			/* #### This should be BUF_BEGV (b), right? */
			return Dynarr_atp(w->line_start_cache, 0)->start;
		} else {
			if (ret_vpos)
				*ret_vpos = vtarget;
			if (ret_vpix)
				*ret_vpix =
				    vpix_motion(w->line_start_cache,
						elt + vtarget, elt);
			return Dynarr_atp(w->line_start_cache,
					  elt + vtarget)->start;
		}
	} else {
		/* No vertical motion requested so we just return the position
		   of the beginning of the current line. */
		if (ret_vpos)
			*ret_vpos = 0;
		if (ret_vpix)
			*ret_vpix = vpix_motion(w->line_start_cache, elt, elt);

		return Dynarr_atp(w->line_start_cache, elt)->start;
	}

	RETURN_NOT_REACHED(0)	/* shut up compiler */
}

/*****************************************************************************
 vmotion

 Given a starting position ORIG, move point VTARGET lines in WINDOW.
 Returns the new value for point.  If the arg ret_vpos is not nil, it is
 taken to be a pointer to an int and the number of lines actually moved is
 returned in it.
 ****************************************************************************/
Bufpos vmotion(struct window * w, Bufpos orig, int vtarget, int *ret_vpos)
{
	return vmotion_1(w, orig, vtarget, ret_vpos, NULL);
}

/* Helper for Fvertical_motion.
 */
static
Lisp_Object vertical_motion_1(Lisp_Object lines, Lisp_Object window, int pixels)
{
	Bufpos bufpos;
	Bufpos orig;
	int selected;
	int *vpos, *vpix;
	int value = 0;
	struct window *w;

	if (NILP(window))
		window = Fselected_window(Qnil);

	CHECK_LIVE_WINDOW(window);
	CHECK_INT(lines);

	selected = (EQ(window, Fselected_window(Qnil)));

	w = XWINDOW(window);

	orig = selected ? BUF_PT(XBUFFER(w->buffer))
	    : marker_position(w->pointm[CURRENT_DISP]);

	vpos = pixels ? NULL : &value;
	vpix = pixels ? &value : NULL;

	bufpos = vmotion_1(w, orig, XINT(lines), vpos, vpix);

	/* Note that the buffer's point is set, not the window's point. */
	if (selected)
		BUF_SET_PT(XBUFFER(w->buffer), bufpos);
	else
		set_marker_restricted(w->pointm[CURRENT_DISP],
				      make_int(bufpos), w->buffer);

	return make_int(value);
}

DEFUN("vertical-motion", Fvertical_motion, 1, 3, 0,	/*
Move to start of frame line LINES lines down.
If LINES is negative, this is moving up.
Optional second argument is WINDOW to move in,
the default is the selected window.

Sets point to position found; this may be start of line
or just the start of a continuation line.
If optional third argument PIXELS is nil, returns number
of lines moved; may be closer to zero than LINES if beginning
or end of buffer was reached.  If PIXELS is non-nil, the
vertical pixel height of the motion which took place is
returned instead of the actual number of lines moved.  A
motion of zero lines returns the height of the current line.

Note that `vertical-motion' sets WINDOW's buffer's point, not
WINDOW's point. (This differs from FSF Emacs, which buggily always
sets current buffer's point, regardless of WINDOW.)
*/
      (lines, window, pixels))
{
	return vertical_motion_1(lines, window, !NILP(pixels));
}

/*
 * Like vmotion() but requested and returned movement is in pixels.
 * HOW specifies the stopping condition.  Positive means move at least
 * PIXELS.  Negative means at most.  Zero means as close as possible.
 */
Bufpos
vmotion_pixels(Lisp_Object window, Bufpos start, int pixels, int how,
	       int *motion)
{
	struct window *w;
	Bufpos eobuf, bobuf;
	int defheight;
	int needed;
	int line, next;
	int remain, abspix, dirn;
	int elt, nelt;
	int i;
	line_start_cache_dynarr *cache;
	int previous = -1;
	int lines;

	if (NILP(window))
		window = Fselected_window(Qnil);

	CHECK_LIVE_WINDOW(window);
	w = XWINDOW(window);

	eobuf = BUF_ZV(XBUFFER(w->buffer));
	bobuf = BUF_BEGV(XBUFFER(w->buffer));

	default_face_height_and_width(window, &defheight, NULL);

	/* guess num lines needed in line start cache + a few extra */
	abspix = abs(pixels);
	needed = (abspix + defheight - 1) / defheight + 3;

	dirn = (pixels >= 0) ? 1 : -1;

	while (1) {
		elt = point_in_line_start_cache(w, start, needed);
		assert(elt >= 0);	/* in the cache */

		cache = w->line_start_cache;
		nelt = Dynarr_length(cache);

		*motion = 0;

		if (pixels == 0)
			/* No vertical motion requested so we just return the position
			   of the beginning of the current display line. */
			return Dynarr_atp(cache, elt)->start;

		if ((dirn < 0 && elt == 0 &&
		     Dynarr_atp(cache, elt)->start <= bobuf) ||
		    (dirn > 0 && elt == nelt - 1 &&
		     Dynarr_atp(cache, elt)->end >= eobuf))
			return Dynarr_atp(cache, elt)->start;

		remain = abspix;
		for (i = elt; (dirn > 0) ? (i < nelt) : (i > 0); i += dirn) {
			/* cache line we're considering moving over */
			int ii = (dirn > 0) ? i : i - 1;

			if (remain < 0)
				return Dynarr_atp(cache, i)->start;

			line = Dynarr_atp(cache, ii)->height;
			next = remain - line;

			/* is stopping condition satisfied? */
			if ((how > 0 && remain <= 0) ||	/* at least */
			    (how < 0 && next < 0) ||	/* at most */
			    (how == 0 && remain <= abs(next)))	/* closest */
				return Dynarr_atp(cache, i)->start;

			/* moving down and nowhere left to go? */
			if (dirn > 0 && Dynarr_atp(cache, ii)->end >= eobuf)
				return Dynarr_atp(cache, ii)->start;

			/* take the step */
			remain = next;
			*motion += dirn * line;

			/* moving up and nowhere left to go? */
			if (dirn < 0 && Dynarr_atp(cache, ii)->start <= bobuf)
				return Dynarr_atp(cache, ii)->start;
		}

		/* get here => need more cache lines.  try again. */
		assert(abs(*motion) > previous);	/* progress? */
		previous = abs(*motion);

		lines = (pixels < 0) ? elt : (nelt - elt);
		needed += (remain * lines + abspix - 1) / abspix + 3;
	}

	RETURN_NOT_REACHED(0)	/* shut up compiler */
}

DEFUN("vertical-motion-pixels", Fvertical_motion_pixels, 1, 3, 0,	/*
Move to start of frame line PIXELS vertical pixels down.
If PIXELS is negative, this is moving up.
The actual vertical motion in pixels is returned.

Optional second argument is WINDOW to move in,
the default is the selected window.

Optional third argument HOW specifies when to stop.  A value
less than zero indicates that the motion should be no more
than PIXELS.  A value greater than zero indicates that the
motion should be at least PIXELS.  Any other value indicates
that the motion should be as close as possible to PIXELS.
*/
      (pixels, window, how))
{
	Bufpos bufpos;
	Bufpos orig;
	int selected;
	int motion;
	int howto;
	struct window *w;

	if (NILP(window))
		window = Fselected_window(Qnil);

	CHECK_LIVE_WINDOW(window);
	CHECK_INT(pixels);

	selected = (EQ(window, Fselected_window(Qnil)));

	w = XWINDOW(window);

	orig = selected ? BUF_PT(XBUFFER(w->buffer))
	    : marker_position(w->pointm[CURRENT_DISP]);

	howto = INTP(how) ? XINT(how) : 0;

	bufpos = vmotion_pixels(window, orig, XINT(pixels), howto, &motion);

	if (selected)
		BUF_SET_PT(XBUFFER(w->buffer), bufpos);
	else
		set_marker_restricted(w->pointm[CURRENT_DISP],
				      make_int(bufpos), w->buffer);

	return make_int(motion);
}

void syms_of_indent(void)
{
	DEFSUBR(Fcurrent_indentation);
	DEFSUBR(Findent_to);
	DEFSUBR(Fcurrent_column);
	DEFSUBR(Fmove_to_column);
#if 0				/* #### */
	DEFSUBR(Fcompute_motion);
#endif
	DEFSUBR(Fvertical_motion);
	DEFSUBR(Fvertical_motion_pixels);

	defsymbol(&Qcoerce, "coerce");
}

void vars_of_indent(void)
{
	DEFVAR_BOOL("indent-tabs-mode", &indent_tabs_mode	/*
*Indentation can insert tabs if this is non-nil.
Setting this variable automatically makes it local to the current buffer.
								 */ );
	indent_tabs_mode = 1;
}
