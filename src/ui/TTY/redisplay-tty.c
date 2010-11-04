/* Communication module for TTY terminals.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.
   Copyright (C) 2007 Nelson Ferreira

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


/* Synched up with: Not completely synched with FSF.  Mostly divergent
   from FSF. */

/* This file has been Mule-ized. */

/* Written by Chuck Thompson. */
/* Color support added by Ben Wing and  completely changed by Nelson Ferreira */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-tty.h"
#include "events/events.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/glyphs.h"
#include "lstream.h"
#include "objects-tty.h"
#include "ui/redisplay.h"
#include "sysdep.h"
#include "ui/window.h"

#ifdef HAVE_CURSES_H
#ifdef CURSES_H_FILE
#include CURSES_H_FILE
#endif
#endif

#ifdef HAVE_TERMCAP_H
#ifdef TERMCAP_H_FILE
#include TERMCAP_H_FILE
#endif
#endif

/* These headers #define all kinds of common words like "columns"...
   What a bunch of losers.  If we were to include them, we'd have to
   include them last to prevent them from messing up our own header
   files (struct slot names, etc.).  But it turns out that there are
   other conflicts as well on some systems, so screw it: we'll just
   re-declare the routines we use and assume the code in this file is
   invoking them correctly. */
/* # include <curses.h> */
/* # include <term.h> */
#ifndef CURSES_H_FILE
EXTERN_C int tgetent(const char *, const char *);
EXTERN_C int tgetflag(const char *);
EXTERN_C int tgetnum(const char *);
EXTERN_C char *tgetstr(const char *, char **);
#endif

#ifndef TERMCAP_H_FILE
EXTERN_C void tputs(const char *, int, int (*)(int));
#endif

char *emacs_tparam(const char *string, char *outstring, int len, int arg1,
		   int arg2, int arg3, int arg4, int arg5, int arg6, int arg7,
		   int arg8, int arg9);

typedef int (*tputs_fun)(int);

#define FORCE_CURSOR_UPDATE(c) send_string_to_tty_console ((c), 0, 0)
#define OUTPUTN(c, a, n)			\
  do {						\
    cmputc_console = (c);			\
    FORCE_CURSOR_UPDATE(c);			\
    tputs ((a), (n), (tputs_fun)cmputc);	\
  } while (0)
#define OUTPUT1(c, a) OUTPUTN ((c), (a), 1)
#define OUTPUTN_IF(c, a, n)			\
  do {						\
    cmputc_console = (c);			\
    FORCE_CURSOR_UPDATE(c);			\
    if (a)					\
      tputs ((a), (n), (tputs_fun)cmputc);	\
  } while (0)
#define OUTPUT1_IF(c, a) OUTPUTN_IF ((c), (a), 1)

static void tty_output_emchar_dynarr(struct window *w,
				     struct display_line *dl,
				     Emchar_dynarr * buf, int xpos,
				     face_index findex, int cursor);
static void tty_output_bufbyte_string(struct window *w,
				      struct display_line *dl,
				      Bufbyte * str, Bytecount len,
				      int xpos, face_index findex, int cursor);
static void tty_turn_on_face(struct window *w, face_index findex);
static void tty_turn_off_face(struct window *w, face_index findex);
static void tty_turn_on_frame_face(struct frame *f, Lisp_Object face);
static void tty_turn_off_frame_face(struct frame *f, Lisp_Object face);

static void term_get_fkeys(Lisp_Object keymap, char **address);

extern int assume_colorterm;

/*****************************************************************************
 tty_text_width

 Non-Mule tty's don't have fonts (that we use at least), so everything
 is considered to be fixed width -- in other words, we return LEN.
 Under Mule, however, a character can still cover more than one
 column, so we use emchar_string_displayed_columns().
 ****************************************************************************/
static int
tty_text_width(struct frame *f, struct face_cachel *cachel, const Emchar * str,
	       Charcount len)
{
	return emchar_string_displayed_columns(str, len);
}

/*****************************************************************************
 tty_divider_height

 Return the width of the horizontal divider.  This is a function
 because divider_height is a console method.
 ****************************************************************************/
static int tty_divider_height(void)
{
	return 1;
}

/*****************************************************************************
 tty_eol_cursor_width

 Return the width of the end-of-line cursor.  This is a function
 because eol_cursor_width is a console method.
 ****************************************************************************/
static int tty_eol_cursor_width(void)
{
	return 1;
}

/*****************************************************************************
 tty_frame_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
#ifdef DEBUG_SXEMACS
void tty_frame_output_begin(struct frame *f);
void
#else
static void
#endif
tty_frame_output_begin(struct frame *f)
{
#ifndef HAVE_TERMIOS
	/* Termcap requires `ospeed' to be a global variable so we have to
	   always set it for whatever tty console we are actually currently
	   working with. */
	ospeed = DEVICE_TTY_DATA(XDEVICE(FRAME_DEVICE(f)))->ospeed;
#endif
}

/*****************************************************************************
 tty_frame_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
#ifdef DEBUG_SXEMACS
void tty_frame_output_end(struct frame *f);
void
#else
static void
#endif
tty_frame_output_end(struct frame *f)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	CONSOLE_TTY_CURSOR_X(c) = CONSOLE_TTY_FINAL_CURSOR_X(c);
	CONSOLE_TTY_CURSOR_Y(c) = CONSOLE_TTY_FINAL_CURSOR_Y(c);
	FORCE_CURSOR_UPDATE(c);
	Lstream_flush(XLSTREAM(CONSOLE_TTY_DATA(c)->outstream));
}

static void tty_set_final_cursor_coords(struct frame *f, int y, int x)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	CONSOLE_TTY_FINAL_CURSOR_X(c) = x;
	CONSOLE_TTY_FINAL_CURSOR_Y(c) = y;
}

/*****************************************************************************
 tty_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 ****************************************************************************/
static void
tty_output_display_block(struct window *w, struct display_line *dl, int block,
			 int start, int end, int start_pixpos,
			 int cursor_start, int cursor_width, int cursor_height)
{
	struct frame *f = XFRAME(w->frame);
	Emchar_dynarr *buf = NULL;

	struct display_block *db = Dynarr_atp(dl->display_blocks, block);
	rune_dynarr *rba = db->runes;
	struct rune *rb;

	int elt = start;
	face_index findex;
	int xpos;

	rb = Dynarr_atp(rba, elt);

	if (!rb) {
		/* Nothing to do so don't do anything. */
		return;
	} else {
		findex = rb->findex;
		xpos = rb->xpos;
	}

	if (end < 0)
		end = Dynarr_length(rba);

        buf = Dynarr_new(Emchar);

	while (elt < end && Dynarr_atp(rba, elt)->xpos < start_pixpos) {
		elt++;
		findex = Dynarr_atp(rba, elt)->findex;
		xpos = Dynarr_atp(rba, elt)->xpos;
	}

	while (elt < end) {
		rb = Dynarr_atp(rba, elt);

		if (rb->findex == findex && rb->type == RUNE_CHAR
		    && rb->object.chr.ch != '\n'
		    && (rb->cursor_type != CURSOR_ON
			|| NILP(w->text_cursor_visible_p))) {
			Dynarr_add(buf, rb->object.chr.ch);
			elt++;
		} else {
			if (Dynarr_length(buf)) {
				tty_output_emchar_dynarr(w, dl, buf, xpos,
							 findex, 0);
				xpos = rb->xpos;
			}
			Dynarr_reset(buf);

			if (rb->type == RUNE_CHAR) {
				findex = rb->findex;
				xpos = rb->xpos;

				if (rb->object.chr.ch == '\n') {
					/* Clear in case a cursor was formerly here. */

					Dynarr_add(buf, ' ');
					tty_output_emchar_dynarr(w, dl, buf,
								 rb->xpos,
								 DEFAULT_INDEX,
								 0);
					Dynarr_reset(buf);

					cmgoto(f, dl->ypos - 1, rb->xpos);

					elt++;
				} else if (rb->cursor_type == CURSOR_ON) {
					/* There is not a distinct eol cursor on tty's. */

					Dynarr_add(buf, rb->object.chr.ch);
					tty_output_emchar_dynarr(w, dl, buf,
								 xpos, findex,
								 0);
					Dynarr_reset(buf);

					cmgoto(f, dl->ypos - 1, xpos);

					xpos += rb->width;
					elt++;
				}
			}
			/* #### RUNE_HLINE is actually a little more complicated than this
			   but at the moment it is only used to draw a turned off
			   modeline and this will suffice for that. */
			else if (rb->type == RUNE_BLANK
				 || rb->type == RUNE_HLINE) {
				Emchar ch_to_add;
				int size = rb->width;

				if (rb->type == RUNE_BLANK)
					ch_to_add = ' ';
				else
					ch_to_add = '-';

				while (size--)
					Dynarr_add(buf, ch_to_add);
				tty_output_emchar_dynarr(w, dl, buf, rb->xpos,
							 findex, 0);

				if (xpos >= cursor_start
				    && cursor_start < xpos + Dynarr_length(buf))
				{
					cmgoto(f, dl->ypos - 1, cursor_start);
				}

				Dynarr_reset(buf);

				elt++;
				if (elt < end) {
					rb = Dynarr_atp(rba, elt);

					findex = rb->findex;
					xpos = rb->xpos;
				}
			} else if (rb->type == RUNE_DGLYPH) {
				Lisp_Object window;
				Lisp_Object instance;

				XSETWINDOW(window, w);
				instance =
				    glyph_image_instance(rb->object.dglyph.
							 glyph, window,
							 ERROR_ME_NOT, 1);

				if (IMAGE_INSTANCEP(instance)) {
					switch (XIMAGE_INSTANCE_TYPE(instance)) {
					case IMAGE_MONO_PIXMAP:
					case IMAGE_COLOR_PIXMAP:
					case IMAGE_SUBWINDOW:
					case IMAGE_WIDGET:
						/* just do nothing here */
						break;

					case IMAGE_NOTHING:
						/* nothing is as nothing does */
						break;

					case IMAGE_TEXT:
					case IMAGE_POINTER:
					case IMAGE_UNKNOWN:
					default:
						abort();
					}
					IMAGE_INSTANCE_OPTIMIZE_OUTPUT
					    (XIMAGE_INSTANCE(instance)) = 0;
				}

				xpos += rb->width;
				elt++;
			} else
				abort();
		}
	}

	if (Dynarr_length(buf))
		tty_output_emchar_dynarr(w, dl, buf, xpos, findex, 0);
	Dynarr_free(buf);

}

/*****************************************************************************
 tty_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void
tty_output_vertical_divider(struct window *w, int UNUSED(clearp))
{
	/* Divider width can either be 0 or 1 on TTYs */
	if (window_divider_width(w)) {
		struct frame *f = XFRAME(w->frame);
		struct console *c = XCONSOLE(FRAME_CONSOLE(f));
		int line;
		int y_top = WINDOW_TEXT_TOP(w);
		int y_bot = WINDOW_TEXT_BOTTOM(w);
		unsigned char divv = '|';

		tty_turn_on_face(w, MODELINE_INDEX);
		for (line = y_top; line < y_bot; line++) {
			cmgoto(f, line, WINDOW_TEXT_RIGHT(w));
			send_string_to_tty_console(c, &divv, 1);
			TTY_INC_CURSOR_X(c, 1);
		}

		/* Draw the divider in the modeline. */
		cmgoto(f, y_bot, WINDOW_TEXT_RIGHT(w));
		send_string_to_tty_console(c, &divv, 1);
		TTY_INC_CURSOR_X(c, 1);
		tty_turn_off_face(w, MODELINE_INDEX);
	}
	return;
}

/****************************************************************************
 tty_clear_region

 Clear the area in the box defined by the given parameters.
 ****************************************************************************/
static void
tty_clear_region(Lisp_Object window, struct device *d, struct frame *f,
		 face_index findex, int x, int y,
		 int width, int height, Lisp_Object fcolor, Lisp_Object bcolor,
		 Lisp_Object background_pixmap)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));
	int line;
	struct window *w = XWINDOW(window);

	tty_turn_on_face(w, findex);
	for (line = y; line < y + height; line++) {
		int col;

		cmgoto(f, line, x);

		if (window_is_leftmost(w)
		    && window_is_rightmost(w)
		    && TTY_SE(c).clr_to_eol) {
			OUTPUT1(c, TTY_SE(c).clr_to_eol);
		} else {
			unsigned char sp = ' ';
			/* #### Of course, this is all complete and utter crap. */
			for (col = x; col < x + width; col++)
				send_string_to_tty_console(c, &sp, 1);
			TTY_INC_CURSOR_X(c, width);
		}
	}
	tty_turn_off_face(w, findex);
	cmgoto(f, y, x);
}

/*****************************************************************************
 tty_clear_to_window_end

 Clear the area between ypos1 and ypos2.  Each margin area and the
 text area is handled separately since they may each have their own
 background color.
 ****************************************************************************/
static void tty_clear_to_window_end(struct window *w, int ypos1, int ypos2)
{
	struct frame *f = XFRAME(w->frame);
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));
	int x, width;

	x = WINDOW_TEXT_LEFT(w);
	width = WINDOW_TEXT_WIDTH(w);

	if (window_is_rightmost(w)) {
		/* #### Optimize to use clr_to_eol function of tty if available, if
		   the window is the entire width of the frame. */
		/* #### Is this actually an optimization? */
		int line;
		tty_turn_on_face(w, DEFAULT_INDEX);
		for (line = ypos1; line < ypos2; line++) {
			cmgoto(XFRAME(w->frame), line, x);
			OUTPUT1(c, TTY_SE(c).clr_to_eol);
		}
		tty_turn_off_face(w, DEFAULT_INDEX);
	} else {
		Lisp_Object window;

		XSETWINDOW(window, w);
		redisplay_clear_region(window, DEFAULT_INDEX, x, ypos1, width,
				       ypos2 - ypos1);
	}
}

/****************************************************************************
 tty_clear_frame

 Clear the entire frame.
 ****************************************************************************/
static void tty_clear_frame(struct frame *f)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	tty_turn_on_frame_face(f, Vdefault_face);
	if (TTY_SE(c).clr_frame) {
		OUTPUT1(c, TTY_SE(c).clr_frame);
		CONSOLE_TTY_REAL_CURSOR_X(c) = 0;
		CONSOLE_TTY_REAL_CURSOR_Y(c) = 0;
#ifdef NOT_SURE
		FRAME_CURSOR_X(f) = 0;
		FRAME_CURSOR_Y(f) = 0;
#endif
	} else {
#ifdef NOT_SURE
		internal_cursor_to(f, 0, 0);
		clear_to_end(f);
#else
		/* #### Not implemented. */
		stderr_out("Not yet.\n");
#endif
	}
	tty_turn_off_frame_face(f, Vdefault_face);
}

static void
tty_output_bufbyte_string(struct window *w, struct display_line *dl,
			  Bufbyte * str, Bytecount len, int xpos,
			  face_index findex, int cursor)
{
	struct frame *f = XFRAME(w->frame);
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	/* First position the cursor. */
	cmgoto(f, dl->ypos - 1, xpos);

	/* Enable any face properties. */
	tty_turn_on_face(w, findex);

	send_string_to_tty_console(c, str, len);
	TTY_INC_CURSOR_X(c, bufbyte_string_displayed_columns(str, len));

	/* Turn the face properties back off. */
	tty_turn_off_face(w, findex);
}

static Bufbyte_dynarr *tty_output_emchar_dynarr_dynarr;

/*****************************************************************************
 tty_output_emchar_dynarr

 Given a string and a starting position, output that string in the
 given face.  If cursor is true, draw a cursor around the string.
 ****************************************************************************/
static void
tty_output_emchar_dynarr(struct window *w, struct display_line *dl,
			 Emchar_dynarr * buf, int xpos, face_index findex,
			 int cursor)
{
	if (!tty_output_emchar_dynarr_dynarr)
		tty_output_emchar_dynarr_dynarr = Dynarr_new(Bufbyte);
	else
		Dynarr_reset(tty_output_emchar_dynarr_dynarr);

	convert_emchar_string_into_bufbyte_dynarr(Dynarr_atp(buf, 0),
						  Dynarr_length(buf),
						  tty_output_emchar_dynarr_dynarr);

	tty_output_bufbyte_string(w, dl,
				  Dynarr_atp(tty_output_emchar_dynarr_dynarr,
					     0),
				  Dynarr_length
				  (tty_output_emchar_dynarr_dynarr), xpos,
				  findex, cursor);
}

#include <semaphore.h>

static int          tty_mutex_inited = 0;
static sxe_mutex_t  tty_mutex;
static Lstream     *tty_stream = NULL;

static void tty_stream_putc( int c )
{
	if ( tty_stream )
		Lstream_putc(tty_stream, c);
}			

static void send_tty_escseq(struct console *c, char *escseq)
{
	if ( ! tty_mutex_inited ) {
		SXE_MUTEX_INIT(&tty_mutex);
		tty_mutex_inited = 1;
	}
	WITH_SXE_MUTEX(&tty_mutex, 
		       {
			       tty_stream = XLSTREAM(CONSOLE_TTY_DATA(c)->outstream);
			       tputs(escseq,1,(tputs_fun)tty_stream_putc);
			       tty_stream = NULL;
		       });

}



static void
set_foreground_to(struct console *c, Lisp_Object sym)
{
	Lisp_Object result;
	Bufbyte *escseq = 0;

	result = Ffind_tty_color(sym,c->selected_device,Qt);
	if ( !NILP(result) ) {
		Lisp_Object idx = XCAR(result);
		Lisp_Object bold  = XCAR(XCDR(result));

		if (TTY_SD(c).set_afore_ ) {
			/* inevitable warning? */
			escseq = (Bufbyte*) 
				emacs_tparam((TTY_SD(c).set_afore_),
					     NULL, 0,
					     XINT(idx),
					     0,0,0,0,0,0,0,0);
		}
		if (escseq) {
			if ( ! EQ(bold,Qnil) &&
			     CONSOLE_TTY_DATA(c)->is_bold_brighter ) {
				OUTPUT1_IF(c, TTY_SD(c).turn_on_bold_);
			}
			send_tty_escseq(c,(char*)escseq);
			free(escseq);
		}
	}
	return;
}

static void
set_background_to(struct console *c, Lisp_Object sym)
{
	Lisp_Object result;
	Bufbyte *escseq = 0;

	result = Ffind_tty_color(sym,c->selected_device,Qt);
	if ( !NILP(result) ) {
		Lisp_Object idx = XCAR(result);

		if (TTY_SD(c).set_aback_ ) {
			/* inevitable warning */
			escseq = (Bufbyte*) 
				emacs_tparam((TTY_SD(c).set_aback_),
					     NULL, 0,
					     XINT(idx),
					     0,0,0,0,0,0,0,0 );
		}
		if (escseq) {
			send_tty_escseq(c,(char*)escseq);
			free(escseq);
		}
	}
	return;
}

static void
tty_turn_on_face_1(struct console *c, int highlight_p,
		   int blinking_p, int dim_p, int underline_p,
		   int reverse_p, Lisp_Object cinst_fore,
		   Lisp_Object cinst_back)
{
	if (highlight_p) {
		OUTPUT1_IF(c, TTY_SD(c).turn_on_bold_);
	}

	if (blinking_p) {
		OUTPUT1_IF(c, TTY_SD(c).turn_on_blinking_);
	}

	if (dim_p) {
		OUTPUT1_IF(c, TTY_SD(c).turn_on_dim_);
	}

	if (underline_p) {
		/* #### punt for now if underline mode is glitchy */
		if (!TTY_FLAGS(c).underline_width) {
			OUTPUT1_IF(c, TTY_SD(c).begin_underline_);
		}
	}

	if (reverse_p) {
		/* #### punt for now if standout mode is glitchy */
		if (!TTY_FLAGS(c).standout_width) {
			OUTPUT1_IF(c, TTY_SD(c).begin_standout_);
		} else
			reverse_p = 0;
	}

	if (reverse_p) {
		Lisp_Object temp = cinst_fore;
		cinst_fore = cinst_back;
		cinst_back = temp;
	}


	int color_instancep_fore = COLOR_INSTANCEP(cinst_fore);
	int cinst_fore_eq_null_color = EQ(cinst_fore, Vthe_null_color_instance);
	if ( color_instancep_fore
	     && !cinst_fore_eq_null_color ) {
		Lisp_Object cinst_sym = COLOR_INSTANCE_TTY_SYMBOL
			(XCOLOR_INSTANCE(cinst_fore));
		set_foreground_to(c, cinst_sym);
	}

	int color_instancep_back = COLOR_INSTANCEP(cinst_back);
	int cinst_back_eq_null_color = EQ(cinst_back, Vthe_null_color_instance);
	if ( color_instancep_back
	     && !cinst_back_eq_null_color ) {
		Lisp_Object cinst_sym = COLOR_INSTANCE_TTY_SYMBOL
			(XCOLOR_INSTANCE(cinst_back));
		set_background_to(c, cinst_sym);
	}
}

/*****************************************************************************
 tty_turn_on_face

 Turn on all set properties of the given face.
 ****************************************************************************/
static void tty_turn_on_face(struct window *w, face_index findex)
{
	struct frame *f = XFRAME(w->frame);
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	tty_turn_on_face_1(c,
			   WINDOW_FACE_CACHEL_HIGHLIGHT_P(w, findex),
			   WINDOW_FACE_CACHEL_BLINKING_P(w, findex),
			   WINDOW_FACE_CACHEL_DIM_P(w, findex),
			   WINDOW_FACE_CACHEL_UNDERLINE_P(w, findex),
			   WINDOW_FACE_CACHEL_REVERSE_P(w, findex),
			   WINDOW_FACE_CACHEL_FOREGROUND(w, findex),
			   WINDOW_FACE_CACHEL_BACKGROUND(w, findex));
}

/*****************************************************************************
 tty_turn_off_face

 Turn off all set properties of the given face (revert to default
 face).  We assume that tty_turn_on_face has been called for the given
 face so that its properties are actually active.
 ****************************************************************************/
static void tty_turn_off_face(struct window *w, face_index findex)
{
	struct frame *f = XFRAME(w->frame);
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	if (WINDOW_FACE_CACHEL_REVERSE_P(w, findex)) {
		/* #### punt for now if standout mode is glitchy */
		if (!TTY_FLAGS(c).standout_width) {
			OUTPUT1_IF(c, TTY_SD(c).end_standout_);
		}
	}

	if (WINDOW_FACE_CACHEL_UNDERLINE_P(w, findex)) {
		/* #### punt for now if underline mode is glitchy */
		if (!TTY_FLAGS(c).underline_width) {
			OUTPUT1_IF(c, TTY_SD(c).end_underline_);
		}
	}

	if (WINDOW_FACE_CACHEL_HIGHLIGHT_P(w, findex) ||
	    WINDOW_FACE_CACHEL_BLINKING_P(w, findex) ||
	    WINDOW_FACE_CACHEL_DIM_P(w, findex) ||
	    !EQ(WINDOW_FACE_CACHEL_FOREGROUND(w, findex),
		Vthe_null_color_instance) ||
	    !EQ(WINDOW_FACE_CACHEL_BACKGROUND(w, findex),
		Vthe_null_color_instance)) {
		OUTPUT1_IF(c, TTY_SD(c).turn_off_attributes_);
	}
}

/*****************************************************************************
 tty_turn_on_frame_face

 Turn on all set properties of the given face.
 ****************************************************************************/
static void tty_turn_on_frame_face(struct frame *f, Lisp_Object face)
{
	Lisp_Object frame;
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	XSETFRAME(frame, f);
	tty_turn_on_face_1(c,
			   FACE_HIGHLIGHT_P(face, frame),
			   FACE_BLINKING_P(face, frame),
			   FACE_DIM_P(face, frame),
			   FACE_UNDERLINE_P(face, frame),
			   FACE_REVERSE_P(face, frame),
			   FACE_FOREGROUND(face, frame),
			   FACE_BACKGROUND(face, frame));
}

/*****************************************************************************
 tty_turn_off_frame_face

 Turn off all set properties of the given face (revert to default
 face).  We assume that tty_turn_on_face has been called for the given
 face so that its properties are actually active.
 ****************************************************************************/
static void tty_turn_off_frame_face(struct frame *f, Lisp_Object face)
{
	Lisp_Object frame;
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	XSETFRAME(frame, f);

	if (FACE_REVERSE_P(face, frame)) {
		/* #### punt for now if standout mode is glitchy */
		if (!TTY_FLAGS(c).standout_width) {
			OUTPUT1_IF(c, TTY_SD(c).end_standout_);
		}
	}

	if (FACE_UNDERLINE_P(face, frame)) {
		/* #### punt for now if underline mode is glitchy */
		if (!TTY_FLAGS(c).underline_width) {
			OUTPUT1_IF(c, TTY_SD(c).end_underline_);
		}
	}

	if (FACE_HIGHLIGHT_P(face, frame) ||
	    FACE_BLINKING_P(face, frame) ||
	    FACE_DIM_P(face, frame) ||
	    !EQ(FACE_FOREGROUND(face, frame), Vthe_null_color_instance) ||
	    !EQ(FACE_BACKGROUND(face, frame), Vthe_null_color_instance)) {
		OUTPUT1_IF(c, TTY_SD(c).turn_off_attributes_);
	}
}

/*****************************************************************************
 set_tty_modes

 Sets up various parameters on tty modes.
 ****************************************************************************/
void set_tty_modes(struct console *c)
{
	if (!CONSOLE_TTY_P(c))
		return;
	
	OUTPUT1_IF(c, (TTY_SD(c).init_motion_));
	OUTPUT1_IF(c, (TTY_SD(c).cursor_visible_));
	OUTPUT1_IF(c, (TTY_SD(c).keypad_on_));
}

/*****************************************************************************
 reset_tty_modes

 Restore default state of tty.
 ****************************************************************************/
void reset_tty_modes(struct console *c)
{
	if (!CONSOLE_TTY_P(c))
		return;

	OUTPUT1_IF(c, (TTY_SD(c).orig_pair_));
	OUTPUT1_IF(c, (TTY_SD(c).keypad_off_));
	OUTPUT1_IF(c, (TTY_SD(c).cursor_normal_));
	OUTPUT1_IF(c, (TTY_SD(c).end_motion_));

	{
		Lisp_Object frm = CONSOLE_SELECTED_FRAME(c);

		if (!NILP(frm))
			tty_frame_output_end(XFRAME(frm));
	}
}

/*****************************************************************************
 tty_redisplay_shutdown

 Clear the frame and position the cursor properly for exiting.
 ****************************************************************************/
void tty_redisplay_shutdown(struct console *c)
{
	Lisp_Object dev = CONSOLE_SELECTED_DEVICE(c);

	if (!NILP(dev)) {
		Lisp_Object frm = DEVICE_SELECTED_FRAME(XDEVICE(dev));

		if (!NILP(frm)) {
			struct frame *f = XFRAME(frm);

			/* Clear the bottom line of the frame. */
			redisplay_clear_region(FRAME_SELECTED_WINDOW(f),
					       DEFAULT_INDEX, 0, f->height,
					       f->width, 1);

			/* And then stick the cursor there. */
			tty_set_final_cursor_coords(f, f->height, 0);
			tty_frame_output_end(f);
		}
	}
}

/* #### Everything below here is old shit.  It should either be moved
   up or removed. */

#ifdef NOT_YET
/* FLAGS - these don't need to be console local since only one console
	   can be being updated at a time. */
static int insert_mode_on;	/* nonzero if in insert mode */
static int standout_mode_on;	/* nonzero if in standout mode */
static int underline_mode_on;	/* nonzero if in underline mode */
static int alternate_mode_on;	/* nonzero if in alternate char set */
static int attributes_on;	/* nonzero if any attributes on */

static void turn_on_insert(struct frame *f)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	if (!insert_mode_on)
		OUTPUT1_IF(c, TTY_SE(c).begin_ins_mode);
	insert_mode_on = 1;
}

static void turn_off_insert(struct frame *f)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	if (insert_mode_on)
		OUTPUT1(c, TTY_SE(c).end_ins_mode);
	insert_mode_on = 0;
}

static void internal_cursor_to(struct frame *f, int row, int col)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	if (!TTY_FLAGS(c).insert_mode_motion)
		turn_off_insert(f);
	if (!TTY_FLAGS(c).standout_motion) {
		turn_off_standout(f);
		turn_off_underline(f);
		turn_off_alternate(f);
	}

	cmgoto(f, row, col);
}

static void clear_to_end(struct frame *f)
{
	struct console *c = XCONSOLE(FRAME_CONSOLE(f));

	/* assumes cursor is already positioned */
	if (TTY_SE(c).clr_from_cursor) {
		OUTPUT1(c, TTY_SE(c).clr_from_cursor);
	} else {
		int line = FRAME_CURSOR_Y(f);

		while (line < FRAME_HEIGHT(f)) {
			internal_cursor_to(f, line, 0);
			OUTPUT1(c, TTY_SE(c).clr_to_eol);
		}
	}
}
#endif				/* NOT YET */

#if 0
/*
 *  clear from last visible line on window to window end (presumably
 *  the line above window's modeline
 */
static void tty_clear_window_end(struct window *w, int ystart, int yend)
{
	struct console *c = XCONSOLE(WINDOW_CONSOLE(w));
	int line;

	for (line = ystart; line < yend; line++) {
		cmgoto(XFRAME(w->frame), line, 0);
		OUTPUT1(c, TTY_SE(c).clr_to_eol);
	}
}

#endif				/* 0 */

static int tty_flash(struct device *d)
{
	struct console *c = XCONSOLE(DEVICE_CONSOLE(d));
	if (TTY_SD(c).visual_bell_) {
		OUTPUT1(c, TTY_SD(c).visual_bell_);
		Lstream_flush(XLSTREAM(CONSOLE_TTY_DATA(c)->outstream));
		return 1;
	} else
		return 0;
}

/*
 * tty_ring_bell - sound an audio beep.
 */
static void tty_ring_bell(struct device *d, int volume, int pitch, int duration)
{
	struct console *c = XCONSOLE(DEVICE_CONSOLE(d));

	if (volume) {
		OUTPUT1(c, TTY_SD(c).audio_bell_);
		Lstream_flush(XLSTREAM(CONSOLE_TTY_DATA(c)->outstream));
	}
}

int init_tty_for_redisplay(struct device *d, char *terminal_type)
{
	int force_colorterm = assume_colorterm || 
		getenv("COLOR_TERM") != NULL ||
		getenv("COLORTERM") != NULL;

	int status;
	/* According to the man page a terminfo/termcap cannot be
	   longer than 4096 so we should be golden here
	*/
	char entry_buffer[4098]; 
	char *bufptr;
	struct console *c = XCONSOLE(DEVICE_CONSOLE(d));
	char *ttype = terminal_type;

	/* What we should really do is allocate just enough space for
	   the actual strings that are stored; but this would require
	   doing this after all the tgetstr()s and adjusting all the
	   pointers. */
	CONSOLE_TTY_DATA(c)->term_entry_buffer =
		xmalloc_atomic(countof(entry_buffer));
	bufptr = CONSOLE_TTY_DATA(c)->term_entry_buffer;

	do {
#ifdef SIGTTOU
		EMACS_BLOCK_SIGNAL(SIGTTOU);
#endif
		status = tgetent(entry_buffer, ttype);
#ifdef SIGTTOU
		EMACS_UNBLOCK_SIGNAL(SIGTTOU);
#endif
		/* Under Linux at least, <0 is returned for TTY_TYPE_UNDEFINED. --ben 
		 */
		if (status <= 0) {
			if ( ! strcmp( ttype, "xterm-256color" ) ) {
				ttype = "xterm-color";
				continue;
			} else if ( ! strcmp( ttype, "xterm-color" ) ) {
				ttype = "xterm";
				continue;
			} else if ( ! strcmp( ttype, "xterm" ) ) {
				ttype = "vt100";
				continue;
			} 
			return TTY_TYPE_UNDEFINED;
		}
	} while( status <= 0 );

	if ( strcmp(terminal_type,ttype) ) {
		stderr_out("Could not find an entry for terminal type '");
		stderr_out("%s",terminal_type);
		stderr_out("' fallback using '");
		stderr_out("%s",ttype);
		stderr_out("'\n");
	}

	CONSOLE_TTY_DATA(c)->term_cmap = Qnil;
	CONSOLE_TTY_DATA(c)->term_crgb = Qnil;
	/*
	 * Establish the terminal size.
	 */
	/* First try to get the info from the system.  If that fails, check
	   the termcap entry. */
	get_tty_device_size(d, &CONSOLE_TTY_DATA(c)->width,
			    &CONSOLE_TTY_DATA(c)->height);

	if (CONSOLE_TTY_DATA(c)->width <= 0)
		CONSOLE_TTY_DATA(c)->width = tgetnum("co");
	if (CONSOLE_TTY_DATA(c)->height <= 0)
		CONSOLE_TTY_DATA(c)->height = tgetnum("li");

	if (CONSOLE_TTY_DATA(c)->width <= 0 || CONSOLE_TTY_DATA(c)->height <= 0)
		return TTY_SIZE_UNSPECIFIED;

	/*
	 * Initialize cursor motion information.
	 */

	/* local cursor movement */
	TTY_CM(c).up = tgetstr("up", &bufptr);
	TTY_CM(c).down = tgetstr("do", &bufptr);
	TTY_CM(c).left = tgetstr("le", &bufptr);
	TTY_CM(c).right = tgetstr("nd", &bufptr);
	TTY_CM(c).home = tgetstr("ho", &bufptr);
	TTY_CM(c).low_left = tgetstr("ll", &bufptr);
	TTY_CM(c).car_return = tgetstr("cr", &bufptr);

	/* absolute cursor motion */
	TTY_CM(c).abs = tgetstr("cm", &bufptr);
	TTY_CM(c).hor_abs = tgetstr("ch", &bufptr);
	TTY_CM(c).ver_abs = tgetstr("cv", &bufptr);

	/* Verify that the terminal is powerful enough to run Emacs */
	if (!TTY_CM(c).abs) {
		if (!TTY_CM(c).up || !TTY_CM(c).down
		    || !TTY_CM(c).left || !TTY_CM(c).right)
			return TTY_TYPE_INSUFFICIENT;
	}

	/* parameterized local cursor movement */
	TTY_CM(c).multi_up = tgetstr("UP", &bufptr);
	TTY_CM(c).multi_down = tgetstr("DO", &bufptr);
	TTY_CM(c).multi_left = tgetstr("LE", &bufptr);
	TTY_CM(c).multi_right = tgetstr("RI", &bufptr);

	/* scrolling */
	TTY_CM(c).scroll_forw = tgetstr("sf", &bufptr);
	TTY_CM(c).scroll_back = tgetstr("sr", &bufptr);
	TTY_CM(c).multi_scroll_forw = tgetstr("SF", &bufptr);
	TTY_CM(c).multi_scroll_back = tgetstr("SR", &bufptr);
	TTY_CM(c).set_scroll_region = tgetstr("cs", &bufptr);

	/*
	 * Initialize screen editing information.
	 */

	/* adding to the screen */
	TTY_SE(c).ins_line = tgetstr("al", &bufptr);
	TTY_SE(c).multi_ins_line = tgetstr("AL", &bufptr);
	TTY_SE(c).repeat = tgetstr("rp", &bufptr);
	TTY_SE(c).begin_ins_mode = tgetstr("im", &bufptr);
	TTY_SE(c).end_ins_mode = tgetstr("ei", &bufptr);
	TTY_SE(c).ins_char = tgetstr("ic", &bufptr);
	TTY_SE(c).multi_ins_char = tgetstr("IC", &bufptr);
	TTY_SE(c).insert_pad = tgetstr("ip", &bufptr);

	/* deleting from the screen */
	TTY_SE(c).clr_frame = tgetstr("cl", &bufptr);
	TTY_SE(c).clr_from_cursor = tgetstr("cd", &bufptr);
	TTY_SE(c).clr_to_eol = tgetstr("ce", &bufptr);
	TTY_SE(c).del_line = tgetstr("dl", &bufptr);
	TTY_SE(c).multi_del_line = tgetstr("DL", &bufptr);
	TTY_SE(c).del_char = tgetstr("dc", &bufptr);
	TTY_SE(c).multi_del_char = tgetstr("DC", &bufptr);
	TTY_SE(c).begin_del_mode = tgetstr("dm", &bufptr);
	TTY_SE(c).end_del_mode = tgetstr("ed", &bufptr);
	TTY_SE(c).erase_at_cursor = tgetstr("ec", &bufptr);

	/*
	 * Initialize screen display information.
	 */
	TTY_SD(c).begin_standout_   = tgetstr("so", &bufptr);
	TTY_SD(c).end_standout_     = tgetstr("se", &bufptr);
	TTY_SD(c).begin_underline_  = tgetstr("us", &bufptr);
	TTY_SD(c).end_underline_    = tgetstr("ue", &bufptr);
	TTY_SD(c).begin_alternate_  = tgetstr("as", &bufptr);
	TTY_SD(c).end_alternate_    = tgetstr("ae", &bufptr);
	TTY_SD(c).turn_on_reverse_  = tgetstr("mr", &bufptr);
	TTY_SD(c).turn_on_blinking_ = tgetstr("mb", &bufptr);
	TTY_SD(c).turn_on_bold_     = tgetstr("md", &bufptr);
	TTY_SD(c).turn_on_dim_      = tgetstr("mh", &bufptr);
	TTY_SD(c).turn_off_attributes_ = tgetstr("me", &bufptr);
	TTY_SD(c).set_aback_        = tgetstr("AB", &bufptr);
	TTY_SD(c).set_afore_        = tgetstr("AF", &bufptr);
	TTY_SD(c).orig_pair_        = tgetstr("op", &bufptr);

	TTY_SD(c).visual_bell_ = tgetstr("vb", &bufptr);
	TTY_SD(c).audio_bell_   = tgetstr("bl", &bufptr);
	if (!TTY_SD(c).audio_bell_) {
		/* If audio_bell doesn't get set, then assume C-g.  This is gross and
		   ugly but is what Emacs has done from time immortal. */
		TTY_SD(c).audio_bell_ = "\07";
	}

	TTY_SD(c).cursor_visible_ = tgetstr("ve", &bufptr);
	TTY_SD(c).cursor_normal_  = tgetstr("vs", &bufptr);
	TTY_SD(c).init_motion_    = tgetstr("ti", &bufptr);
	TTY_SD(c).end_motion_     = tgetstr("te", &bufptr);
	TTY_SD(c).keypad_on_      = tgetstr("ks", &bufptr);
	TTY_SD(c).keypad_off_     = tgetstr("ke", &bufptr);

	/*
	 * Initialize additional terminal information.
	 */
	TTY_FLAGS(c).must_write_spaces = tgetflag("in");
	TTY_FLAGS(c).insert_mode_motion = tgetflag("mi");
	TTY_FLAGS(c).standout_motion = tgetflag("ms");
	TTY_FLAGS(c).memory_above_frame = tgetflag("da");
	TTY_FLAGS(c).memory_below_frame = tgetflag("db");
	TTY_FLAGS(c).standout_width = tgetnum("sg");
	TTY_FLAGS(c).underline_width = tgetnum("ug");

	if (TTY_FLAGS(c).standout_width == -1)
		TTY_FLAGS(c).standout_width = 0;
	if (TTY_FLAGS(c).underline_width == -1)
		TTY_FLAGS(c).underline_width = 0;

	TTY_FLAGS(c).meta_key =
	    eight_bit_tty(d) ? tgetflag("km") || tgetflag("MT") ? 1 : 2 : 0;

	/*
	 * Setup the costs tables for this tty console.
	 */
	cm_cost_init(c);

#ifdef NOT_YET
	/*
	 * Initialize local flags.
	 */
	insert_mode_on = 0;
	standout_mode_on = 0;
	underline_mode_on = 0;
	alternate_mode_on = 0;
	attributes_on = 0;
#endif

	/*
	 * Attempt to initialize the function_key_map to
	 * some kind of sensible value
	 */

	term_get_fkeys(c->function_key_map, &bufptr);
	
	{
		/* check for ANSI set-foreground and set-background strings,
		   and assume color if so.

		   #### we should support the other (non-ANSI) ways of specifying
		   color, too. */

		char foobuf[500];
		char *UNUSED(fooptr) = foobuf;
		int colors = tgetnum("Co");
		int pairs =  tgetnum("pa");
		if ((TTY_SD(c).set_aback_ && TTY_SD(c).set_afore_) ||
		    ((colors > 0) || (pairs  > 0))) {
			DEVICE_CLASS(d) = Qcolor;
			/* These cases should be rare. Most termcap
			 * entries will have both colors and pairs
			 * defined. However let's play it safe :) 
			 */
			if ( colors == 0 && pairs > 0 ) {
				/* try to determine colors from pairs.
				 * Most common cases first.
				 */
				if ( pairs == 64 ) 
					colors = 8;
				else if ( pairs == 256 ) 
					colors = 16;
				else 
					/* Naive isqrt algorithm. */
					for( colors=1; 
                                             colors*colors < pairs; 
                                             colors ++ );
			}
			if ( pairs == 0 && colors > 0 )
				/* try to determine pairs from colors */
				pairs = colors * colors;
			CONSOLE_TTY_DATA(c)->maxcolors = colors;
			CONSOLE_TTY_DATA(c)->maxpairs = pairs;
			
			/* Most terminals that report 8 colors will
                           make bold brighter */
			CONSOLE_TTY_DATA(c)->is_bold_brighter = 
				(colors == 8 && TTY_SD(c).turn_on_bold_ ) 
                                ? 1 : 0; 
			/* Most terminals do not have brighter background
			   colors, however the REAL X11R6 xterm and rxvt do
			   allow reverse to achieve this.  Sadly there
			   is no way to autodetect. Since the Hue is very
                           different on the 8 basic colors and most users
                           will hopefully use xterm or rxvt.
			*/
			CONSOLE_TTY_DATA(c)->is_reverse_brighter = 
                                (colors == 8 && TTY_SD(c).turn_on_reverse_) 
                                ? 1 : 0; 
		}
		else if ( force_colorterm ) {
			DEVICE_CLASS(d) = Qcolor;
			TTY_SD(c).set_aback_ = "\e[4%p1%dm";
			TTY_SD(c).set_afore_ = "\e[3%p1%dm";
			CONSOLE_TTY_DATA(c)->maxcolors = 8;
			CONSOLE_TTY_DATA(c)->maxpairs = 64;
			CONSOLE_TTY_DATA(c)->is_bold_brighter = 
				(TTY_SD(c).turn_on_bold_ ) ? 1 : 0; 
			if ( ! 	TTY_SD(c).orig_pair_ ) 
				TTY_SD(c).orig_pair_ = "\e[39;49m";

		} else {
			DEVICE_CLASS(d) = Qmono;
			CONSOLE_TTY_DATA(c)->maxcolors = 2;
			CONSOLE_TTY_DATA(c)->maxpairs = 4;
			CONSOLE_TTY_DATA(c)->is_bold_brighter = 0; 
			CONSOLE_TTY_DATA(c)->is_reverse_brighter = 0; 
		}
	}

	return TTY_INIT_SUCCESS;
}

struct fkey_table {
	/* tgetstr will disobey this qualifier */
	char *cap;
	const char *name;
};

  /* Termcap capability names that correspond directly to X keysyms.
     Some of these (marked "terminfo") aren't supplied by old-style
     (Berkeley) termcap entries.  They're listed in X keysym order;
     except we put the keypad keys first, so that if they clash with
     other keys (as on the IBM PC keyboard) they get overridden.
   */

static struct fkey_table keys[] = {
	{"kh", "home"},		/* termcap */
	{"kl", "left"},		/* termcap */
	{"ku", "up"},		/* termcap */
	{"kr", "right"},	/* termcap */
	{"kd", "down"},		/* termcap */
	{"%8", "prior"},	/* terminfo */
	{"%5", "next"},		/* terminfo */
	{"@7", "end"},		/* terminfo */
	{"@1", "begin"},	/* terminfo */
	{"*6", "select"},	/* terminfo */
	{"%9", "print"},	/* terminfo */
	{"@4", "execute"},	/* terminfo --- actually the `command' key */
	/*
	 * "insert" --- see below
	 */
	{"&8", "undo"},		/* terminfo */
	{"%0", "redo"},		/* terminfo */
	{"%7", "menu"},		/* terminfo --- actually the `options' key */
	{"@0", "find"},		/* terminfo */
	{"@2", "cancel"},	/* terminfo */
	{"%1", "help"},		/* terminfo */
	/*
	 * "break" goes here, but can't be reliably intercepted with termcap
	 */
	{"&4", "reset"},	/* terminfo --- actually `restart' */
	/*
	 * "system" and "user" --- no termcaps
	 */
	{"kE", "clearline"},	/* terminfo */
	{"kA", "insertline"},	/* terminfo */
	{"kL", "deleteline"},	/* terminfo */
	{"kI", "insertchar"},	/* terminfo */
	{"kD", "delete"},	/* terminfo */
	{"kB", "backtab"},	/* terminfo */
	/*
	 * "kp-backtab", "kp-space", "kp-tab" --- no termcaps
	 */
	{"@8", "kp-enter"},	/* terminfo */
	/*
	 * "kp-f1", "kp-f2", "kp-f3" "kp-f4",
	 * "kp-multiply", "kp-add", "kp-separator",
	 * "kp-subtract", "kp-decimal", "kp-divide", "kp-0";
	 * --- no termcaps for any of these.
	 */
	{"K4", "kp-1"},		/* terminfo */
	/*
	 * "kp-2" --- no termcap
	 */
	{"K5", "kp-3"},		/* terminfo */
	/*
	 * "kp-4" --- no termcap
	 */
	{"K2", "kp-5"},		/* terminfo */
	/*
	 * "kp-6" --- no termcap
	 */
	{"K1", "kp-7"},		/* terminfo */
	/*
	 * "kp-8" --- no termcap
	 */
	{"K3", "kp-9"},		/* terminfo */
	/*
	 * "kp-equal" --- no termcap
	 */
	{"k1", "f1"},
	{"k2", "f2"},
	{"k3", "f3"},
	{"k4", "f4"},
	{"k5", "f5"},
	{"k6", "f6"},
	{"k7", "f7"},
	{"k8", "f8"},
	{"k9", "f9"},
};

static char **term_get_fkeys_arg;

static Lisp_Object term_get_fkeys_1(Lisp_Object keymap);
static Lisp_Object term_get_fkeys_error(Lisp_Object err, Lisp_Object arg);

/* Find the escape codes sent by the function keys for Vfunction_key_map.
   This function scans the termcap function key sequence entries, and
   adds entries to Vfunction_key_map for each function key it finds.  */

static void term_get_fkeys(Lisp_Object keymap, char **address)
{
	/* We run the body of the function (term_get_fkeys_1) and ignore all Lisp
	   errors during the call.  The only errors should be from Fdefine_key
	   when given a key sequence containing an invalid prefix key.  If the
	   termcap defines function keys which use a prefix that is already bound
	   to a command by the default bindings, we should silently ignore that
	   function key specification, rather than giving the user an error and
	   refusing to run at all on such a terminal.  */

	term_get_fkeys_arg = address;

	condition_case_1(Qerror,
			 term_get_fkeys_1, keymap, term_get_fkeys_error, Qnil);
}

static Lisp_Object term_get_fkeys_error(Lisp_Object err, Lisp_Object arg)
{
	return arg;
}

static Lisp_Object term_get_fkeys_1(Lisp_Object function_key_map)
{
	int i;

	char **address = term_get_fkeys_arg;

	for (i = 0; i < countof(keys); i++) {
		const char *sequence = tgetstr(keys[i].cap, address);
		if (sequence) {
			Fdefine_key(function_key_map,
				    build_ext_string(sequence, Qbinary),
				    vector1(intern(keys[i].name)));
		}
	}

	/* The uses of the "k0" capability are inconsistent; sometimes it
	   describes F10, whereas othertimes it describes F0 and "k;" describes F10.
	   We will attempt to politely accommodate both systems by testing for
	   "k;", and if it is present, assuming that "k0" denotes F0, otherwise F10.
	 */
	{
		const char *k_semi = tgetstr("k;", address);
		const char *k0 = tgetstr("k0", address);

		if (k_semi)
			Fdefine_key(function_key_map,
				    build_ext_string(k_semi, Qbinary),
				    vector1(intern("f10")));

		if (k0)
			Fdefine_key(function_key_map,
				    build_ext_string(k0, Qbinary),
				    vector1(intern(k_semi ? "f0" : "f10")));
	}

	/* Set up cookies for numbered function keys above f10. */
	{
		char fcap[3], fkey[4];

		fcap[0] = 'F';
		fcap[2] = '\0';
		for (i = 11; i < 64; i++) {
			if (i <= 19)
				fcap[1] = '1' + i - 11;
			else if (i <= 45)
				fcap[1] = 'A' + i - 20;
			else
				fcap[1] = 'a' + i - 46;

			{
				char *sequence = tgetstr(fcap, address);
				if (sequence) {
					sprintf(fkey, "f%d", i);
					Fdefine_key(function_key_map,
						    build_ext_string(sequence,
								     Qbinary),
						    vector1(intern(fkey)));
				}
			}
		}
	}

	/*
	 * Various mappings to try and get a better fit.
	 */
#define CONDITIONAL_REASSIGN(cap1, cap2, keyname) do {		\
    if (!tgetstr (cap1, address))				\
      {								\
	char *sequence = tgetstr (cap2, address);		\
	if (sequence)						\
	  Fdefine_key (function_key_map,			\
		       build_ext_string (sequence, Qbinary),	\
		       vector1 (intern (keyname)));		\
      }								\
  } while (0)

	/* if there's no key_next keycap, map key_npage to `next' keysym */
	CONDITIONAL_REASSIGN("%5", "kN", "next");
	/* if there's no key_prev keycap, map key_ppage to `previous' keysym */
	CONDITIONAL_REASSIGN("%8", "kP", "prior");
	/* if there's no key_dc keycap, map key_ic to `insert' keysym */
	CONDITIONAL_REASSIGN("kD", "kI", "insert");

	/* IBM has their own non-standard dialect of terminfo.
	   If the standard name isn't found, try the IBM name.  */
	CONDITIONAL_REASSIGN("kB", "KO", "backtab");
	CONDITIONAL_REASSIGN("@4", "kJ", "execute");	/* actually "action" */
	CONDITIONAL_REASSIGN("@4", "kc", "execute");	/* actually "command" */
	CONDITIONAL_REASSIGN("%7", "ki", "menu");
	CONDITIONAL_REASSIGN("@7", "kw", "end");
	CONDITIONAL_REASSIGN("F1", "k<", "f11");
	CONDITIONAL_REASSIGN("F2", "k>", "f12");
	CONDITIONAL_REASSIGN("%1", "kq", "help");
	CONDITIONAL_REASSIGN("*6", "kU", "select");
#undef CONDITIONAL_REASSIGN

	return Qnil;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_redisplay_tty(void)
{
	/* redisplay methods */
	CONSOLE_HAS_METHOD(tty, text_width);
	CONSOLE_HAS_METHOD(tty, output_display_block);
	CONSOLE_HAS_METHOD(tty, output_vertical_divider);
	CONSOLE_HAS_METHOD(tty, divider_height);
	CONSOLE_HAS_METHOD(tty, eol_cursor_width);
	CONSOLE_HAS_METHOD(tty, clear_to_window_end);
	CONSOLE_HAS_METHOD(tty, clear_region);
	CONSOLE_HAS_METHOD(tty, clear_frame);
	CONSOLE_HAS_METHOD(tty, frame_output_begin);
	CONSOLE_HAS_METHOD(tty, frame_output_end);
	CONSOLE_HAS_METHOD(tty, flash);
	CONSOLE_HAS_METHOD(tty, ring_bell);
	CONSOLE_HAS_METHOD(tty, set_final_cursor_coords);
}
