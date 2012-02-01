/* X output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
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


/* Synched up with:  Not in FSF. */

/* Author: Chuck Thompson */

/* Lots of work done by Ben Wing for Mule */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "EmacsFrame.h"
#include "EmacsFrameP.h"
#include "xgccache.h"
#include "glyphs-x.h"
#include "objects-x.h"

#include "buffer.h"
#include "debug.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/gutter.h"
#include "ui/redisplay.h"
#include "sysdep.h"
#include "ui/window.h"
#include <X11/bitmaps/gray>

#include "sysproc.h"		/* for select() */

#ifdef MULE
#include "mule/mule-ccl.h"
#include "mule/file-coding.h"	/* for CCL conversion */
#endif

/* Number of pixels below each line. */
int x_interline_space;		/* #### implement me */

#define EOL_CURSOR_WIDTH	5

static void x_output_vertical_divider(struct window *w, int clear);
static void x_output_blank(struct window *w, struct display_line *dl,
			   struct rune *rb, int start_pixpos,
			   int cursor_start, int cursor_width);
static void x_output_hline(struct window *w, struct display_line *dl,
			   struct rune *rb);
static void x_redraw_exposed_window(struct window *w, int x, int y,
				    int width, int height);
static void x_redraw_exposed_windows(Lisp_Object window, int x, int y,
				     int width, int height);
static void x_output_eol_cursor(struct window *w, struct display_line *dl,
				int xpos, face_index findex);
static void x_clear_frame(struct frame *f);
static void x_clear_frame_windows(Lisp_Object window);

     /* Note: We do not use the Xmb*() functions and XFontSets.
	Those functions are generally losing for a number of reasons:

	1) They only support one locale (e.g. you could display
	Japanese and ASCII text, but not mixed Japanese/Chinese
	text).  You could maybe call setlocale() frequently
	to try to deal with this, but that would generally
	fail because an XFontSet is tied to one locale and
	won't have the other character sets in it.
	2) Not all (or even very many) OS's support the useful
	locales.  For example, as far as I know SunOS and
	Solaris only support the Japanese locale if you get the
	special Asian-language version of the OS.  Yuck yuck
	yuck.  Linux doesn't support the Japanese locale at
	all.
	3) The locale support in X only exists in R5, not in R4.
	(Not sure how big of a problem this is: how many
	people are using R4?)
	4) Who knows if the multi-byte text format (which is locale-
	specific) is even the same for the same locale on
	different OS's?  It's not even documented anywhere that
	I can find what the multi-byte text format for the
	Japanese locale under SunOS and Solaris is, but I assume
	it's EUC.
      */

struct textual_run {
	Lisp_Object charset;
	unsigned char *ptr;
	int len;
	int dimension;
};

/* Separate out the text in DYN into a series of textual runs of a
   particular charset.  Also convert the characters as necessary into
   the format needed by XDrawImageString(), XDrawImageString16(), et
   al.  (This means converting to one or two byte format, possibly
   tweaking the high bits, and possibly running a CCL program.) You
   must pre-allocate the space used and pass it in. (This is done so
   you can alloca() the space.)  You need to allocate (2 * len) bytes
   of TEXT_STORAGE and (len * sizeof (struct textual_run)) bytes of
   RUN_STORAGE, where LEN is the length of the dynarr.

   Returns the number of runs actually used. */

static int
separate_textual_runs(unsigned char *text_storage,
		      struct textual_run *run_storage,
		      const Emchar * str, Charcount len)
{
	Lisp_Object prev_charset = Qunbound;	/* not Qnil because that is a
						   possible valid charset when
						   MULE is not defined */
	int runs_so_far = 0;
	int i;
#ifdef MULE
	struct ccl_program char_converter;
	int need_ccl_conversion = 0;
#endif

	for (i = 0; i < len; i++) {
		Emchar ch = str[i];
		Lisp_Object charset;
		int byte1, byte2;
		int dimension;
		int graphic;

		BREAKUP_CHAR(ch, charset, byte1, byte2);
		dimension = XCHARSET_DIMENSION(charset);
		graphic = XCHARSET_GRAPHIC(charset);

		if (!EQ(charset, prev_charset)) {
			run_storage[runs_so_far].ptr = text_storage;
			run_storage[runs_so_far].charset = charset;
			run_storage[runs_so_far].dimension = dimension;

			if (runs_so_far) {
				run_storage[runs_so_far - 1].len =
				    text_storage - run_storage[runs_so_far -
							       1].ptr;
				if (run_storage[runs_so_far - 1].dimension == 2)
					run_storage[runs_so_far - 1].len >>= 1;
			}
			runs_so_far++;
			prev_charset = charset;
#ifdef MULE
			{
				Lisp_Object ccl_prog =
				    XCHARSET_CCL_PROGRAM(charset);
				if ((!NILP(ccl_prog))
				    &&
				    (setup_ccl_program
				     (&char_converter, ccl_prog) >= 0))
					need_ccl_conversion = 1;
			}
#endif
		}

		if (graphic == 0) {
			byte1 &= 0x7F;
			byte2 &= 0x7F;
		} else if (graphic == 1) {
			byte1 |= 0x80;
			byte2 |= 0x80;
		}
#ifdef MULE
		if (need_ccl_conversion) {
			char_converter.reg[0] = XCHARSET_ID(charset);
			char_converter.reg[1] = byte1;
			char_converter.reg[2] = byte2;
			ccl_driver(&char_converter, 0, 0, 0, 0,
				   CCL_MODE_ENCODING);
			byte1 = char_converter.reg[1];
			byte2 = char_converter.reg[2];
		}
#endif
		*text_storage++ = (unsigned char)byte1;
		if (dimension == 2)
			*text_storage++ = (unsigned char)byte2;
	}

	if (runs_so_far) {
		run_storage[runs_so_far - 1].len =
		    text_storage - run_storage[runs_so_far - 1].ptr;
		if (run_storage[runs_so_far - 1].dimension == 2)
			run_storage[runs_so_far - 1].len >>= 1;
	}

	return runs_so_far;
}

/****************************************************************************/
/*                                                                          */
/*                           X output routines                              */
/*                                                                          */
/****************************************************************************/

static int
x_text_width_single_run(struct face_cachel *cachel, struct textual_run *run)
{
	Lisp_Object font_inst = FACE_CACHEL_FONT(cachel, run->charset);
	Lisp_Font_Instance *fi = XFONT_INSTANCE(font_inst);
	if (!fi->proportional_p)
		return fi->width * run->len;
	else {
		if (run->dimension == 2)
			return XTextWidth16(FONT_INSTANCE_X_FONT(fi),
					    (XChar2b *) run->ptr, run->len);
		else
			return XTextWidth(FONT_INSTANCE_X_FONT(fi),
					  (char *)run->ptr, run->len);
	}
}

/*
   x_text_width

   Given a string and a face, return the string's length in pixels when
   displayed in the font associated with the face.
   */

static int
x_text_width(struct frame *f, struct face_cachel *cachel, const Emchar * str,
	     Charcount len)
{
	int width_so_far = 0;
	unsigned char *text_storage = (unsigned char *)alloca(2 * len);
	struct textual_run *runs = alloca_array(struct textual_run, len);
	int nruns;
	int i;

	nruns = separate_textual_runs(text_storage, runs, str, len);

	for (i = 0; i < nruns; i++)
		width_so_far += x_text_width_single_run(cachel, runs + i);

	return width_so_far;
}

/*****************************************************************************
 x_divider_height

 Return the height of the horizontal divider.  This is a function because
 divider_height is a device method.

 #### If we add etched horizontal divider lines this will have to get
 smarter.
 ****************************************************************************/
static int x_divider_height(void)
{
	return 1;
}

/*****************************************************************************
 x_eol_cursor_width

 Return the width of the end-of-line cursor.  This is a function
 because eol_cursor_width is a device method.
 ****************************************************************************/
static int x_eol_cursor_width(void)
{
	return EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 x_window_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void x_window_output_begin(struct window *w)
{
}

/*****************************************************************************
 x_window_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void x_window_output_end(struct window *w)
{
	XFlush(DEVICE_X_DISPLAY(WINDOW_XDEVICE(w)));
}

/*****************************************************************************
 x_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 ****************************************************************************/
static void
x_output_display_block(struct window *w, struct display_line *dl, int block,
		       int start, int end, int start_pixpos, int cursor_start,
		       int cursor_width, int cursor_height)
{
	struct frame *f = XFRAME(w->frame);
	Emchar_dynarr *buf = NULL;
	Lisp_Object window;

	struct display_block *db = Dynarr_atp(dl->display_blocks, block);
	rune_dynarr *rba = db->runes;
	struct rune *rb;

	int elt = start;
	face_index findex;
	int xpos, width = 0;
	Lisp_Object charset = Qunbound;	/* Qnil is a valid charset when
					   MULE is not defined */

	XSETWINDOW(window, w);
	rb = Dynarr_atp(rba, start);

	if (!rb)
		/* Nothing to do so don't do anything. */
		return;

	findex = rb->findex;
	xpos = rb->xpos;
	if (rb->type == RUNE_CHAR)
		charset = CHAR_CHARSET(rb->object.chr.ch);

	if (end < 0)
		end = Dynarr_length(rba);

	buf = Dynarr_new (Emchar);

	while (elt < end) {
		rb = Dynarr_atp(rba, elt);

		if (rb->findex == findex && rb->type == RUNE_CHAR
		    && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
		    && EQ(charset, CHAR_CHARSET(rb->object.chr.ch))) {
			Dynarr_add(buf, rb->object.chr.ch);
			width += rb->width;
			elt++;
		} else {
			if (Dynarr_length(buf)) {
				x_output_string(w, dl, buf, xpos, 0,
						start_pixpos, width, findex, 0,
						cursor_start, cursor_width,
						cursor_height);
				xpos = rb->xpos;
				width = 0;
			}
			Dynarr_reset(buf);
			width = 0;

			if (rb->type == RUNE_CHAR) {
				findex = rb->findex;
				xpos = rb->xpos;
				charset = CHAR_CHARSET(rb->object.chr.ch);

				if (rb->cursor_type == CURSOR_ON) {
					if (rb->object.chr.ch == '\n') {
						x_output_eol_cursor(w, dl, xpos,
								    findex);
					} else {
						Dynarr_add(buf,
							   rb->object.chr.ch);
						x_output_string(w, dl, buf,
								xpos, 0,
								start_pixpos,
								rb->width,
								findex, 1,
								cursor_start,
								cursor_width,
								cursor_height);
						Dynarr_reset(buf);
					}

					xpos += rb->width;
					elt++;
				} else if (rb->object.chr.ch == '\n') {
					/* Clear in case a cursor was formerly here. */
					redisplay_clear_region(window, findex,
							       xpos,
							       DISPLAY_LINE_YPOS
							       (dl), rb->width,
							       DISPLAY_LINE_HEIGHT
							       (dl));
					elt++;
				}
			} else if (rb->type == RUNE_BLANK
				   || rb->type == RUNE_HLINE) {
				if (rb->type == RUNE_BLANK)
					x_output_blank(w, dl, rb, start_pixpos,
						       cursor_start,
						       cursor_width);
				else {
					/* #### Our flagging of when we need to redraw the
					   modeline shadows sucks.  Since RUNE_HLINE is only used
					   by the modeline at the moment it is a good bet
					   that if it gets redrawn then we should also
					   redraw the shadows.  This won't be true forever.
					   We borrow the shadow_thickness_changed flag for
					   now. */
					w->shadow_thickness_changed = 1;
					x_output_hline(w, dl, rb);
				}

				elt++;
				if (elt < end) {
					rb = Dynarr_atp(rba, elt);

					findex = rb->findex;
					xpos = rb->xpos;
				}
			} else if (rb->type == RUNE_DGLYPH) {
				Lisp_Object instance;
				struct display_box dbox;
				struct display_glyph_area dga;

				redisplay_calculate_display_boxes(dl, rb->xpos,
								  rb->object.
								  dglyph.
								  xoffset,
								  rb->object.
								  dglyph.
								  yoffset,
								  start_pixpos,
								  rb->width,
								  &dbox, &dga);

				XSETWINDOW(window, w);
				instance =
				    glyph_image_instance(rb->object.dglyph.
							 glyph, window,
							 ERROR_ME_NOT, 1);
				findex = rb->findex;

				if (IMAGE_INSTANCEP(instance)) {
					switch (XIMAGE_INSTANCE_TYPE(instance)) {
					case IMAGE_MONO_PIXMAP:
					case IMAGE_COLOR_PIXMAP:
						redisplay_output_pixmap(w,
									instance,
									&dbox,
									&dga,
									findex,
									cursor_start,
									cursor_width,
									cursor_height,
									0);
						break;

					case IMAGE_WIDGET:
						if (EQ
						    (XIMAGE_INSTANCE_WIDGET_TYPE
						     (instance), Qlayout)) {
							redisplay_output_layout
							    (window, instance,
							     &dbox, &dga,
							     findex,
							     cursor_start,
							     cursor_width,
							     cursor_height);
							break;
						}
					case IMAGE_SUBWINDOW:
						redisplay_output_subwindow(w,
									   instance,
									   &dbox,
									   &dga,
									   findex,
									   cursor_start,
									   cursor_width,
									   cursor_height);
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
		x_output_string(w, dl, buf, xpos, 0, start_pixpos, width,
				findex, 0, cursor_start, cursor_width,
				cursor_height);

	/* #### This is really conditionalized well for optimized
	   performance. */
	if (dl->modeline && !EQ(Qzero, w->modeline_shadow_thickness)
	    && (f->clear
		|| f->windows_structure_changed || w->shadow_thickness_changed))
		bevel_modeline(w, dl);

	Dynarr_free(buf);
}

/*****************************************************************************
 x_bevel_area

 Draw shadows for the given area in the given face.
 ****************************************************************************/
static void
x_bevel_area(struct window *w, face_index findex,
	     int x, int y, int width, int height,
	     int shadow_thickness, int edges, enum edge_style style)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);

	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	Pixel top_shadow_pixel, bottom_shadow_pixel, background_pixel;
	Lisp_Object tmp_pixel;
	XColor tmp_color;
	XGCValues gcv;
	GC top_shadow_gc, bottom_shadow_gc, background_gc;

	int use_pixmap = 0;
	int flip_gcs = 0;
	unsigned long mask;

	assert(shadow_thickness >= 0);
	memset(&gcv, ~0, sizeof(XGCValues));

	tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND(w, findex);
	tmp_color = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));

	/* First, get the GC's. */
	top_shadow_pixel = tmp_color.pixel;
	bottom_shadow_pixel = tmp_color.pixel;
	background_pixel = tmp_color.pixel;

	x_generate_shadow_pixels(f, &top_shadow_pixel, &bottom_shadow_pixel,
				 background_pixel, ef->core.background_pixel);

	tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND(w, findex);
	tmp_color = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));
	gcv.background = tmp_color.pixel;
	gcv.graphics_exposures = False;
	mask = GCForeground | GCBackground | GCGraphicsExposures;

	/* If we can't distinguish one of the shadows (the color is the same as the
	   background), it's better to use a pixmap to generate a dithered gray. */
	if (top_shadow_pixel == background_pixel ||
	    bottom_shadow_pixel == background_pixel)
		use_pixmap = 1;

	if (use_pixmap) {
		if (DEVICE_X_GRAY_PIXMAP(d) == None) {
			DEVICE_X_GRAY_PIXMAP(d) =
			    XCreatePixmapFromBitmapData(dpy, x_win,
							(char *)gray_bits,
							gray_width, gray_height,
							1, 0, 1);
		}

		tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND(w, findex);
		tmp_color = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));
		gcv.foreground = tmp_color.pixel;
		/* this is needed because the GC draws with a pixmap here */
		gcv.fill_style = FillOpaqueStippled;
		gcv.stipple = DEVICE_X_GRAY_PIXMAP(d);
		top_shadow_gc = gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv,
						(mask | GCStipple |
						 GCFillStyle));

		tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND(w, findex);
		tmp_color = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));
		bottom_shadow_pixel = tmp_color.pixel;

		flip_gcs = (bottom_shadow_pixel ==
			    WhitePixelOfScreen(DefaultScreenOfDisplay(dpy)));
	} else {
		gcv.foreground = top_shadow_pixel;
		top_shadow_gc =
		    gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, mask);
	}

	gcv.foreground = bottom_shadow_pixel;
	bottom_shadow_gc = gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, mask);

	if (use_pixmap && flip_gcs) {
		GC tmp_gc = bottom_shadow_gc;
		bottom_shadow_gc = top_shadow_gc;
		top_shadow_gc = tmp_gc;
	}

	gcv.foreground = background_pixel;
	background_gc = gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, mask);

	/* possibly revert the GC's This will give a depressed look to the
	   divider */
	if (style == EDGE_ETCHED_IN || style == EDGE_BEVEL_IN) {
		GC temp;

		temp = top_shadow_gc;
		top_shadow_gc = bottom_shadow_gc;
		bottom_shadow_gc = temp;
	}

	if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT)
		shadow_thickness /= 2;

	/* Draw the shadows around the divider line */
	x_output_shadows(f, x, y, width, height,
			 top_shadow_gc, bottom_shadow_gc,
			 background_gc, shadow_thickness, edges);

	if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT) {
		/* Draw the shadows around the divider line */
		x_output_shadows(f, x + shadow_thickness, y + shadow_thickness,
				 width - 2 * shadow_thickness,
				 height - 2 * shadow_thickness,
				 bottom_shadow_gc, top_shadow_gc, background_gc,
				 shadow_thickness, edges);
	}
}

/*****************************************************************************
 x_get_gc

 Given a number of parameters return a GC with those properties.
 ****************************************************************************/
static GC
x_get_gc(struct device *d, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
	 Lisp_Object bg_pmap, Lisp_Object lwidth)
{
	XGCValues gcv;
	unsigned long mask;

	memset(&gcv, ~0, sizeof(XGCValues));
	gcv.graphics_exposures = False;
	/* Make absolutely sure that we don't pick up a clipping region in
	   the GC returned by this function. */
	gcv.clip_mask = None;
	gcv.clip_x_origin = 0;
	gcv.clip_y_origin = 0;
	gcv.fill_style = FillSolid;
	mask = GCGraphicsExposures | GCClipMask | GCClipXOrigin | GCClipYOrigin;
	mask |= GCFillStyle;

	if (!NILP(font)) {
		gcv.font = FONT_INSTANCE_X_FONT(XFONT_INSTANCE(font))->fid;
		mask |= GCFont;
	}

	/* evil kludge! */
	if (!NILP(fg) && !COLOR_INSTANCEP(fg) && !INTP(fg)) {
		/* #### I fixed once case where this was getting it.  It was a
		   bad macro expansion (compiler bug). */
		stderr_out("Help! x_get_gc got a bogus fg value! fg = ");
		debug_print(fg);
		fg = Qnil;
	}

	if (!NILP(fg)) {
		if (COLOR_INSTANCEP(fg))
			gcv.foreground =
			    COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(fg)).pixel;
		else
			gcv.foreground = XINT(fg);
		mask |= GCForeground;
	}

	if (!NILP(bg)) {
		if (COLOR_INSTANCEP(bg))
			gcv.background =
			    COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(bg)).pixel;
		else
			gcv.background = XINT(bg);
		mask |= GCBackground;
	}

	/* This special case comes from a request to draw text with a face which has
	   the dim property. We'll use a stippled foreground GC. */
	if (EQ(bg_pmap, Qdim)) {
		assert(DEVICE_X_GRAY_PIXMAP(d) != None);

		gcv.fill_style = FillStippled;
		gcv.stipple = DEVICE_X_GRAY_PIXMAP(d);
		mask |= (GCFillStyle | GCStipple);
	} else if (IMAGE_INSTANCEP(bg_pmap)
		   && IMAGE_INSTANCE_PIXMAP_TYPE_P(XIMAGE_INSTANCE(bg_pmap))) {
		if (XIMAGE_INSTANCE_PIXMAP_DEPTH(bg_pmap) == 0) {
			gcv.fill_style = FillOpaqueStippled;
			gcv.stipple = XIMAGE_INSTANCE_X_PIXMAP(bg_pmap);
			mask |= (GCStipple | GCFillStyle);
		} else {
			gcv.fill_style = FillTiled;
			gcv.tile = XIMAGE_INSTANCE_X_PIXMAP(bg_pmap);
			mask |= (GCTile | GCFillStyle);
		}
	}

	if (!NILP(lwidth)) {
		gcv.line_width = XINT(lwidth);
		mask |= GCLineWidth;
	}

	return gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, mask);
}

/*****************************************************************************
 x_output_string

 Given a string and a starting position, output that string in the
 given face.  If cursor is true, draw a cursor around the string.
 Correctly handles multiple charsets in the string.

 The meaning of the parameters is something like this:

 W		Window that the text is to be displayed in.
 DL		Display line that this text is on.  The values in the
		structure are used to determine the vertical position and
		clipping range of the text.
 BUF		Dynamic array of Emchars specifying what is actually to be
		drawn.
 XPOS		X position in pixels where the text should start being drawn.
 XOFFSET	Number of pixels to be chopped off the left side of the
		text.  The effect is as if the text were shifted to the
		left this many pixels and clipped at XPOS.
 CLIP_START	Clip everything left of this X position.
 WIDTH		Clip everything right of XPOS + WIDTH.
 FINDEX		Index for the face cache element describing how to display
		the text.
 CURSOR		#### I don't understand this.  There's something
		strange and overcomplexified with this variable.
		Chuck, explain please?
 CURSOR_START	Starting X position of cursor.
 CURSOR_WIDTH	Width of cursor in pixels.
 CURSOR_HEIGHT	Height of cursor in pixels.

 Starting Y position of cursor is the top of the text line.
 The cursor is drawn sometimes whether or not CURSOR is set. ???
 ****************************************************************************/
void
x_output_string(struct window *w, struct display_line *dl,
		Emchar_dynarr * buf, int xpos, int xoffset, int clip_start,
		int width, face_index findex, int cursor,
		int cursor_start, int cursor_width, int cursor_height)
{
	/* General variables */
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);
	Lisp_Object device;
	Lisp_Object window;
	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));

	int clip_end;

	/* Cursor-related variables */
	int focus = EQ(w->frame, DEVICE_FRAME_WITH_FOCUS_REAL(d));
	int cursor_clip;
	Lisp_Object bar_cursor_value = symbol_value_in_buffer(Qbar_cursor,
							      WINDOW_BUFFER(w));
	struct face_cachel *cursor_cachel = 0;

	/* Text-related variables */
	Lisp_Object bg_pmap;
	GC bgc, gc;
	int height;
	int len = Dynarr_length(buf);
	unsigned char *text_storage = (unsigned char *)alloca(2 * len);
	struct textual_run *runs = alloca_array(struct textual_run, len);
	int nruns;
	int i;
	struct face_cachel *cachel = WINDOW_FACE_CACHEL(w, findex);

	XSETDEVICE(device, d);
	XSETWINDOW(window, w);

	if (width < 0)
		width =
		    x_text_width(f, cachel, Dynarr_atp(buf, 0),
				 Dynarr_length(buf));
	height = DISPLAY_LINE_HEIGHT(dl);

	/* Regularize the variables passed in. */

	if (clip_start < xpos)
		clip_start = xpos;
	clip_end = xpos + width;
	if (clip_start >= clip_end)
		/* It's all clipped out. */
		return;

	xpos -= xoffset;

	/* make sure the area we are about to display is subwindow free. */
	redisplay_unmap_subwindows_maybe(f, clip_start, DISPLAY_LINE_YPOS(dl),
					 clip_end - clip_start,
					 DISPLAY_LINE_HEIGHT(dl));

	nruns = separate_textual_runs(text_storage, runs, Dynarr_atp(buf, 0),
				      Dynarr_length(buf));

	cursor_clip = (cursor_start >= clip_start && cursor_start < clip_end);

	/* This cursor code is really a mess. */
	if (!NILP(w->text_cursor_visible_p)
	    && (cursor
		|| cursor_clip
		|| (cursor_width && (cursor_start + cursor_width >= clip_start)
		    && !NILP(bar_cursor_value)))) {
		/* These have to be in separate statements in order to avoid a
		   compiler bug. */
		face_index sucks =
		    get_builtin_face_cache_index(w, Vtext_cursor_face);
		cursor_cachel = WINDOW_FACE_CACHEL(w, sucks);

		/* We have to reset this since any call to WINDOW_FACE_CACHEL
		   may cause the cache to resize and any pointers to it to
		   become invalid. */
		cachel = WINDOW_FACE_CACHEL(w, findex);
	}
#ifdef HAVE_XIM
	if (cursor && focus && (cursor_start == clip_start) && cursor_height)
		XIM_SetSpotLocation(f, xpos - 2, dl->ypos + dl->descent - 2);
#endif				/* HAVE_XIM */

	bg_pmap = cachel->background_pixmap;
	if (!IMAGE_INSTANCEP(bg_pmap)
	    || !IMAGE_INSTANCE_PIXMAP_TYPE_P(XIMAGE_INSTANCE(bg_pmap)))
		bg_pmap = Qnil;

	if ((cursor && focus && NILP(bar_cursor_value)
	     && !NILP(w->text_cursor_visible_p)) || NILP(bg_pmap))
		bgc = 0;
	else
		bgc = x_get_gc(d, Qnil, cachel->foreground, cachel->background,
			       bg_pmap, Qnil);

	if (bgc)
		XFillRectangle(dpy, x_win, bgc, clip_start,
			       DISPLAY_LINE_YPOS(dl), clip_end - clip_start,
			       height);

	for (i = 0; i < nruns; i++) {
		Lisp_Object font = FACE_CACHEL_FONT(cachel, runs[i].charset);
		Lisp_Font_Instance *fi = XFONT_INSTANCE(font);
		int this_width;
		int need_clipping;

		if (EQ(font, Vthe_null_font_instance))
			continue;

		this_width = x_text_width_single_run(cachel, runs + i);
		need_clipping = (dl->clip || clip_start > xpos ||
				 clip_end < xpos + this_width);

		/* XDrawImageString only clears the area equal to the height of
		   the given font.  It is possible that a font is being displayed
		   on a line taller than it is, so this would cause us to fail to
		   clear some areas. */
		if ((int)fi->height < (int)(height + dl->clip + dl->top_clip)) {
			int clear_start = max(xpos, clip_start);
			int clear_end = min(xpos + this_width, clip_end);

			if (cursor) {
				int ypos1_line, ypos1_string, ypos2_line,
				    ypos2_string;

				ypos1_string = dl->ypos - fi->ascent;
				ypos2_string = dl->ypos + fi->descent;
				ypos1_line = DISPLAY_LINE_YPOS(dl);
				ypos2_line =
				    ypos1_line + DISPLAY_LINE_HEIGHT(dl);

				/* Make sure we don't clear below the real bottom of the
				   line. */
				if (ypos1_string > ypos2_line)
					ypos1_string = ypos2_line;
				if (ypos2_string > ypos2_line)
					ypos2_string = ypos2_line;

				if (ypos1_line < ypos1_string) {
					redisplay_clear_region(window, findex,
							       clear_start,
							       ypos1_line,
							       clear_end -
							       clear_start,
							       ypos1_string -
							       ypos1_line);
				}

				if (ypos2_line > ypos2_string) {
					redisplay_clear_region(window, findex,
							       clear_start,
							       ypos2_string,
							       clear_end -
							       clear_start,
							       ypos2_line -
							       ypos2_string);
				}
			} else {
				redisplay_clear_region(window, findex,
						       clear_start,
						       DISPLAY_LINE_YPOS(dl),
						       clear_end - clear_start,
						       height);
			}
		}

		if (cursor && cursor_cachel && focus && NILP(bar_cursor_value))
			gc = x_get_gc(d, font, cursor_cachel->foreground,
				      cursor_cachel->background, Qnil, Qnil);
		else if (cachel->dim) {
			/* Ensure the gray bitmap exists */
			if (DEVICE_X_GRAY_PIXMAP(d) == None)
				DEVICE_X_GRAY_PIXMAP(d) =
				    XCreateBitmapFromData(dpy, x_win,
							  (char *)gray_bits,
							  gray_width,
							  gray_height);

			/* Request a GC with the gray stipple pixmap to draw dimmed text */
			gc = x_get_gc(d, font, cachel->foreground,
				      cachel->background, Qdim, Qnil);
		} else
			gc = x_get_gc(d, font, cachel->foreground,
				      cachel->background, Qnil, Qnil);

		if (need_clipping) {
			XRectangle clip_box[1];

			clip_box[0].x = 0;
			clip_box[0].y = 0;
			clip_box[0].width = clip_end - clip_start;
			clip_box[0].height = height;

			XSetClipRectangles(dpy, gc, clip_start,
					   DISPLAY_LINE_YPOS(dl), clip_box, 1,
					   Unsorted);
		}

		if (runs[i].dimension == 1)
			(bgc ? XDrawString : XDrawImageString) (dpy, x_win, gc,
								xpos, dl->ypos,
								(char *)runs[i].
								ptr,
								runs[i].len);
		else
			(bgc ? XDrawString16 : XDrawImageString16) (dpy, x_win,
								    gc, xpos,
								    dl->ypos,
								    (XChar2b *)
								    runs[i].ptr,
								    runs[i].
								    len);

		/* We draw underlines in the same color as the text. */
		if (cachel->underline) {
			int upos, uthick;
			unsigned long upos_ext, uthick_ext;
			XFontStruct *xfont;

			xfont = FONT_INSTANCE_X_FONT(XFONT_INSTANCE(font));
			if (!XGetFontProperty
			    (xfont, XA_UNDERLINE_POSITION, &upos_ext))
				upos = dl->descent / 2;
			else
				upos = (int) upos_ext;

			if (!XGetFontProperty
			    (xfont, XA_UNDERLINE_THICKNESS, &uthick_ext))
				uthick = 1;
			else
				uthick = (int) uthick_ext;

			if (dl->ypos + upos < dl->ypos + dl->descent - dl->clip) {
				if (dl->ypos + upos + uthick >
				    dl->ypos + dl->descent - dl->clip)
					uthick = dl->descent - dl->clip - upos;

				if (uthick == 1) {
					XDrawLine(dpy, x_win, gc, xpos,
						  dl->ypos + upos,
						  xpos + this_width,
						  dl->ypos + upos);
				} else if (uthick > 1) {
					XFillRectangle(dpy, x_win, gc, xpos,
						       dl->ypos + upos,
						       this_width, uthick);
				}
			}
		}

		if (cachel->strikethru) {
			int ascent, descent, upos, uthick;
			unsigned long ascent_ext, descent_ext, uthick_ext;
			XFontStruct *xfont;

			xfont = FONT_INSTANCE_X_FONT(XFONT_INSTANCE(font));

			if (!XGetFontProperty
			    (xfont, XA_STRIKEOUT_ASCENT, &ascent_ext))
				ascent = xfont->ascent;
			else
				ascent = (int) ascent_ext;

			if (!XGetFontProperty
			    (xfont, XA_STRIKEOUT_DESCENT, &descent_ext))
				descent = xfont->descent;
			else
				descent = (int) descent_ext;

			if (!XGetFontProperty
			    (xfont, XA_UNDERLINE_THICKNESS, &uthick_ext))
				uthick = 1;
			else
				uthick = (int) uthick_ext;

			upos = ascent - ((ascent + descent) / 2) + 1;

			/* Generally, upos will be positive (above the baseline),so subtract */
			if (dl->ypos - upos < dl->ypos + dl->descent - dl->clip) {
				if (dl->ypos - upos + uthick >
				    dl->ypos + dl->descent - dl->clip)
					uthick = dl->descent - dl->clip + upos;

				if (uthick == 1) {
					XDrawLine(dpy, x_win, gc, xpos,
						  dl->ypos - upos,
						  xpos + this_width,
						  dl->ypos - upos);
				} else if (uthick > 1) {
					XFillRectangle(dpy, x_win, gc, xpos,
						       dl->ypos + upos,
						       this_width, uthick);
				}
			}
		}

		/* Restore the GC */
		if (need_clipping) {
			XSetClipMask(dpy, gc, None);
			XSetClipOrigin(dpy, gc, 0, 0);
		}

		/* If we are actually superimposing the cursor then redraw with just
		   the appropriate section highlighted. */
		if (cursor_clip && !cursor && focus && cursor_cachel) {
			GC cgc;
			XRectangle clip_box[1];

			cgc = x_get_gc(d, font, cursor_cachel->foreground,
				       cursor_cachel->background, Qnil, Qnil);

			clip_box[0].x = 0;
			clip_box[0].y = 0;
			clip_box[0].width = cursor_width;
			clip_box[0].height = height;

			XSetClipRectangles(dpy, cgc, cursor_start,
					   DISPLAY_LINE_YPOS(dl), clip_box, 1,
					   Unsorted);

			if (runs[i].dimension == 1)
				XDrawImageString(dpy, x_win, cgc, xpos,
						 dl->ypos, (char *)runs[i].ptr,
						 runs[i].len);
			else
				XDrawImageString16(dpy, x_win, cgc, xpos,
						   dl->ypos,
						   (XChar2b *) runs[i].ptr,
						   runs[i].len);

			XSetClipMask(dpy, cgc, None);
			XSetClipOrigin(dpy, cgc, 0, 0);
		}

		xpos += this_width;
	}

	/* Draw the non-focus box or bar-cursor as needed. */
	/* Can't this logic be simplified? */
	if (cursor_cachel && ((cursor && !focus && NILP(bar_cursor_value))
			      || (cursor_width
				  && (cursor_start + cursor_width >= clip_start)
				  && !NILP(bar_cursor_value)))) {
		int tmp_height, tmp_y;
		int bar_width = EQ(bar_cursor_value, Qt) ? 1 : 2;
		int need_clipping = (cursor_start < clip_start
				     || clip_end < cursor_start + cursor_width);

		/* #### This value is correct (as far as I know) because
		   all of the times we need to draw this cursor, we will
		   be called with exactly one character, so we know we
		   can always use runs[0].

		   This is bogus as all hell, however.  The cursor handling in
		   this function is way bogus and desperately needs to be
		   cleaned up. (In particular, the drawing of the cursor should
		   really really be separated out of this function.  This may be
		   a bit tricky now because this function itself does way too
		   much stuff, a lot of which needs to be moved into
		   redisplay.c) This is the only way to be able to easily add
		   new cursor types or (e.g.) make the bar cursor be able to
		   span two characters instead of overlaying just one. */
		int bogusly_obtained_ascent_value =
		    XFONT_INSTANCE(FACE_CACHEL_FONT(cachel, runs[0].charset))->
		    ascent;

		if (!NILP(bar_cursor_value)) {
			gc = x_get_gc(d, Qnil, cursor_cachel->background, Qnil,
				      Qnil, make_int(bar_width));
		} else {
			gc = x_get_gc(d, Qnil, cursor_cachel->background,
				      Qnil, Qnil, Qnil);
		}

		tmp_y = dl->ypos - bogusly_obtained_ascent_value;
		tmp_height = cursor_height;
		if (tmp_y + tmp_height > (int)(DISPLAY_LINE_YPOS(dl) + height)) {
			tmp_y = DISPLAY_LINE_YPOS(dl) + height - tmp_height;
			if (tmp_y < (int)DISPLAY_LINE_YPOS(dl))
				tmp_y = DISPLAY_LINE_YPOS(dl);
			tmp_height = DISPLAY_LINE_YPOS(dl) + height - tmp_y;
		}

		if (need_clipping) {
			XRectangle clip_box[1];
			clip_box[0].x = 0;
			clip_box[0].y = 0;
			clip_box[0].width = clip_end - clip_start;
			clip_box[0].height = tmp_height;
			XSetClipRectangles(dpy, gc, clip_start, tmp_y,
					   clip_box, 1, Unsorted);
		}

		if (!focus && NILP(bar_cursor_value)) {
			XDrawRectangle(dpy, x_win, gc, cursor_start, tmp_y,
				       cursor_width - 1, tmp_height - 1);
		} else if (focus && !NILP(bar_cursor_value)) {
			XDrawLine(dpy, x_win, gc, cursor_start + bar_width - 1,
				  tmp_y, cursor_start + bar_width - 1,
				  tmp_y + tmp_height - 1);
		}

		/* Restore the GC */
		if (need_clipping) {
			XSetClipMask(dpy, gc, None);
			XSetClipOrigin(dpy, gc, 0, 0);
		}
	}
}

void
x_output_x_pixmap(struct frame *f, Lisp_Image_Instance * p, int x,
		  int y, int xoffset, int yoffset,
		  int width, int height, unsigned long fg, unsigned long bg,
		  GC override_gc)
{
	struct device *d = XDEVICE(f->device);
	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));

	GC gc;
	XGCValues gcv;
	unsigned long pixmap_mask;

	if (!override_gc) {
		memset(&gcv, ~0, sizeof(XGCValues));
		gcv.graphics_exposures = False;
		gcv.foreground = fg;
		gcv.background = bg;
		pixmap_mask = GCForeground | GCBackground | GCGraphicsExposures;

		if (IMAGE_INSTANCE_X_MASK(p)) {
			gcv.function = GXcopy;
			gcv.clip_mask = IMAGE_INSTANCE_X_MASK(p);
			gcv.clip_x_origin = x - xoffset;
			gcv.clip_y_origin = y - yoffset;
			pixmap_mask |=
			    (GCFunction | GCClipMask | GCClipXOrigin |
			     GCClipYOrigin);
			/* Can't set a clip rectangle because we already have a mask.
			   Is it possible to get an equivalent effect by changing the
			   args to XCopyArea below rather than messing with a clip box?
			   - dkindred@cs.cmu.edu
			   Yes. We don't clip at all now - andy@xemacs.org
			 */
		}

		gc = gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, pixmap_mask);
	} else {
		gc = override_gc;
		/* override_gc might have a mask already--we don't want to nuke it.
		   Maybe we can insist that override_gc have no mask, or use
		   one of the suggestions above. */
	}

	/* depth of 0 means it's a bitmap, not a pixmap, and we should use
	   XCopyPlane (1 = current foreground color, 0 = background) instead
	   of XCopyArea, which means that the bits in the pixmap are actual
	   pixel values, instead of symbolic of fg/bg. */
	if (IMAGE_INSTANCE_PIXMAP_DEPTH(p) > 0) {
		XCopyArea(dpy,
			  IMAGE_INSTANCE_X_PIXMAP_SLICE
			  (p, IMAGE_INSTANCE_PIXMAP_SLICE(p)), x_win, gc,
			  xoffset, yoffset, width, height, x, y);
	} else {
		XCopyPlane(dpy, IMAGE_INSTANCE_X_PIXMAP_SLICE
			   (p, IMAGE_INSTANCE_PIXMAP_SLICE(p)), x_win, gc,
			   xoffset, yoffset, width, height, x, y, 1L);
	}
}

static void
x_output_pixmap(struct window *w, Lisp_Object image_instance,
		struct display_box *db, struct display_glyph_area *dga,
		face_index findex, int cursor_start, int cursor_width,
		int cursor_height, int bg_pixmap)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);
	Lisp_Image_Instance *p = XIMAGE_INSTANCE(image_instance);

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));

	/* Output the pixmap. */
	{
		Lisp_Object tmp_pixel;
		XColor tmp_bcolor, tmp_fcolor;

		tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND(w, findex);
		tmp_fcolor = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));
		tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND(w, findex);
		tmp_bcolor = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));

		x_output_x_pixmap(f, p, db->xpos, db->ypos,
				  dga->xoffset, dga->yoffset,
				  dga->width, dga->height,
				  tmp_fcolor.pixel, tmp_bcolor.pixel, 0);
	}

	/* Draw a cursor over top of the pixmap. */
	if (cursor_width && cursor_height && (cursor_start >= db->xpos)
	    && !NILP(w->text_cursor_visible_p)
	    && (cursor_start < db->xpos + dga->width)) {
		GC gc;
		int focus = EQ(w->frame, DEVICE_FRAME_WITH_FOCUS_REAL(d));
		struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL(w,
								       get_builtin_face_cache_index
								       (w,
									Vtext_cursor_face));

		gc = x_get_gc(d, Qnil, cursor_cachel->background, Qnil, Qnil,
			      Qnil);

		if (cursor_width > db->xpos + dga->width - cursor_start)
			cursor_width = db->xpos + dga->width - cursor_start;

		if (focus) {
			XFillRectangle(dpy, x_win, gc, cursor_start, db->ypos,
				       cursor_width, cursor_height);
		} else {
			XDrawRectangle(dpy, x_win, gc, cursor_start, db->ypos,
				       cursor_width, cursor_height);
		}
	}
}

/*****************************************************************************
 x_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void x_output_vertical_divider(struct window *w, int clear)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	Lisp_Object tmp_pixel;
	XColor tmp_color;
	XGCValues gcv;
	GC background_gc;
	enum edge_style style;

	unsigned long mask;
	int x, yy1, yy2, width, shadow_thickness, spacing, line_width;
	face_index div_face =
	    get_builtin_face_cache_index(w, Vvertical_divider_face);

	width = window_divider_width(w);
	shadow_thickness = XINT(w->vertical_divider_shadow_thickness);
	spacing = XINT(w->vertical_divider_spacing);
	line_width = XINT(w->vertical_divider_line_width);
	x = WINDOW_RIGHT(w) - width;
	yy1 = WINDOW_TOP(w);
	yy2 = WINDOW_BOTTOM(w);

	memset(&gcv, ~0, sizeof(XGCValues));

	tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND(w, div_face);
	tmp_color = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));

	/* First, get the GC's. */
	gcv.background = tmp_color.pixel;
	gcv.foreground = tmp_color.pixel;
	gcv.graphics_exposures = False;
	mask = GCForeground | GCBackground | GCGraphicsExposures;
	background_gc = gc_cache_lookup(DEVICE_X_GC_CACHE(d), &gcv, mask);

	/* Clear the divider area first.  This needs to be done when a
	   window split occurs. */
	if (clear)
		XClearArea(dpy, x_win, x, yy1, width, yy2 - yy1, False);

	/* Draw the divider line. */
	XFillRectangle(dpy, x_win, background_gc,
		       x + spacing + shadow_thickness,
		       yy1, line_width, yy2 - yy1);

	if (shadow_thickness < 0) {
		shadow_thickness = -shadow_thickness;
		style = EDGE_BEVEL_IN;
	} else {
		style = EDGE_BEVEL_OUT;
	}

	/* Draw the shadows around the divider line */
	x_bevel_area(w, div_face, x + spacing, yy1,
		     width - 2 * spacing, yy2 - yy1,
		     shadow_thickness, EDGE_ALL, style);
}

/*****************************************************************************
 x_output_blank

 Output a blank by clearing the area it covers in the foreground color
 of its face.
 ****************************************************************************/
static void
x_output_blank(struct window *w, struct display_line *dl, struct rune *rb,
	       int start_pixpos, int cursor_start, int cursor_width)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC gc;
	struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL(w,
							       get_builtin_face_cache_index
							       (w,
								Vtext_cursor_face));
	Lisp_Object bg_pmap;
	Lisp_Object buffer = WINDOW_BUFFER(w);
	Lisp_Object bar_cursor_value = symbol_value_in_buffer(Qbar_cursor,
							      buffer);

	int x = rb->xpos;
	int y = DISPLAY_LINE_YPOS(dl);
	int width = rb->width;
	int height = DISPLAY_LINE_HEIGHT(dl);

	/* Unmap all subwindows in the area we are going to blank. */
	redisplay_unmap_subwindows_maybe(f, x, y, width, height);

	if (start_pixpos > x) {
		if (start_pixpos >= (x + width))
			return;
		else {
			width -= (start_pixpos - x);
			x = start_pixpos;
		}
	}

	bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP(w, rb->findex);
	if (!IMAGE_INSTANCEP(bg_pmap)
	    || !IMAGE_INSTANCE_PIXMAP_TYPE_P(XIMAGE_INSTANCE(bg_pmap)))
		bg_pmap = Qnil;

	if (NILP(bg_pmap))
		gc = x_get_gc(d, Qnil,
			      WINDOW_FACE_CACHEL_BACKGROUND(w, rb->findex),
			      Qnil, Qnil, Qnil);
	else
		gc = x_get_gc(d, Qnil,
			      WINDOW_FACE_CACHEL_FOREGROUND(w, rb->findex),
			      WINDOW_FACE_CACHEL_BACKGROUND(w, rb->findex),
			      bg_pmap, Qnil);

	XFillRectangle(dpy, x_win, gc, x, y, width, height);

	/* If this rune is marked as having the cursor, then it is actually
	   representing a tab. */
	if (!NILP(w->text_cursor_visible_p)
	    && (rb->cursor_type == CURSOR_ON
		|| (cursor_width && (cursor_start + cursor_width > x)
		    && cursor_start < (x + width)))) {
		int cursor_height, cursor_y;
		int focus = EQ(w->frame, DEVICE_FRAME_WITH_FOCUS_REAL(d));
		Lisp_Font_Instance *fi;

		fi = XFONT_INSTANCE(FACE_CACHEL_FONT
				    (WINDOW_FACE_CACHEL(w, rb->findex),
				     Vcharset_ascii));

		gc = x_get_gc(d, Qnil, cursor_cachel->background, Qnil, Qnil,
			      Qnil);

		cursor_y = dl->ypos - fi->ascent;
		cursor_height = fi->height;
		if (cursor_y + cursor_height > y + height)
			cursor_height = y + height - cursor_y;

		if (focus) {
			if (NILP(bar_cursor_value)) {
				XFillRectangle(dpy, x_win, gc, cursor_start,
					       cursor_y, fi->width,
					       cursor_height);
			} else {
				int bar_width =
				    EQ(bar_cursor_value, Qt) ? 1 : 2;

				gc = x_get_gc(d, Qnil,
					      cursor_cachel->background, Qnil,
					      Qnil, make_int(bar_width));
				XDrawLine(dpy, x_win, gc,
					  cursor_start + bar_width - 1,
					  cursor_y,
					  cursor_start + bar_width - 1,
					  cursor_y + cursor_height - 1);
			}
		} else if (NILP(bar_cursor_value)) {
			XDrawRectangle(dpy, x_win, gc, cursor_start, cursor_y,
				       fi->width - 1, cursor_height - 1);
		}
	}
}

/*****************************************************************************
 x_output_hline

 Output a horizontal line in the foreground of its face.
 ****************************************************************************/
static void
x_output_hline(struct window *w, struct display_line *dl, struct rune *rb)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC gc;

	int x = rb->xpos;
	int width = rb->width;
	int height = DISPLAY_LINE_HEIGHT(dl);
	int ypos1, ypos2, ypos3, ypos4;

	ypos1 = DISPLAY_LINE_YPOS(dl);
	ypos2 = ypos1 + rb->object.hline.yoffset;
	ypos3 = ypos2 + rb->object.hline.thickness;
	ypos4 = dl->ypos + dl->descent - dl->clip;

	/* First clear the area not covered by the line. */
	if (height - rb->object.hline.thickness > 0) {
		gc = x_get_gc(d, Qnil,
			      WINDOW_FACE_CACHEL_FOREGROUND(w, rb->findex),
			      Qnil, Qnil, Qnil);

		if (ypos2 - ypos1 > 0)
			XFillRectangle(dpy, x_win, gc, x, ypos1, width,
				       ypos2 - ypos1);
		if (ypos4 - ypos3 > 0)
			XFillRectangle(dpy, x_win, gc, x, ypos1, width,
				       ypos2 - ypos1);
	}

	/* Now draw the line. */
	gc = x_get_gc(d, Qnil, WINDOW_FACE_CACHEL_BACKGROUND(w, rb->findex),
		      Qnil, Qnil, Qnil);

	if (ypos2 < ypos1)
		ypos2 = ypos1;
	if (ypos3 > ypos4)
		ypos3 = ypos4;

	if (ypos3 - ypos2 > 0)
		XFillRectangle(dpy, x_win, gc, x, ypos2, width, ypos3 - ypos2);
}

/*****************************************************************************
 x_output_shadows

 Draw a shadow around the given area using the given GC's.  It is the
 callers responsibility to set the GC's appropriately.
 ****************************************************************************/
void
x_output_shadows(struct frame *f, int x, int y, int width, int height,
		 GC top_shadow_gc, GC bottom_shadow_gc, GC background_gc,
		 int shadow_thickness, int edges)
{
	struct device *d = XDEVICE(f->device);

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));

	XSegment top_shadow[20], bottom_shadow[20];
	int elt;

	if (shadow_thickness > 10)
		shadow_thickness = 10;
	else if (shadow_thickness < 0)
		shadow_thickness = 0;
	if (shadow_thickness > (width / 2))
		shadow_thickness = width / 2;
	if (shadow_thickness > (height / 2))
		shadow_thickness = height / 2;

	for (elt = 0; elt < shadow_thickness; elt++) {
		int seg1 = elt;
		int seg2 = (edges & EDGE_TOP) ? elt + shadow_thickness : elt;
		int bot_seg2 =
		    (edges & EDGE_BOTTOM) ? elt + shadow_thickness : elt;

		if (edges & EDGE_TOP) {
			top_shadow[seg1].x1 = x + elt;
			top_shadow[seg1].x2 = x + width - elt - 1;
			top_shadow[seg1].y1 = top_shadow[seg1].y2 = y + elt;
		}
		if (edges & EDGE_LEFT) {
			top_shadow[seg2].x1 = top_shadow[seg2].x2 = x + elt;
			top_shadow[seg2].y1 = y + elt;
			top_shadow[seg2].y2 = y + height - elt - 1;
		}
		if (edges & EDGE_BOTTOM) {
			bottom_shadow[seg1].x1 = x + elt;
			bottom_shadow[seg1].x2 = x + width - elt - 1;
			bottom_shadow[seg1].y1 = bottom_shadow[seg1].y2 =
			    y + height - elt - 1;
		}
		if (edges & EDGE_RIGHT) {
			bottom_shadow[bot_seg2].x1 =
			    bottom_shadow[bot_seg2].x2 = x + width - elt - 1;
			bottom_shadow[bot_seg2].y1 = y + elt;
			bottom_shadow[bot_seg2].y2 = y + height - elt - 1;
		}
	}

	XDrawSegments(dpy, x_win, top_shadow_gc, top_shadow,
		      ((edges & EDGE_TOP) ? shadow_thickness : 0)
		      + ((edges & EDGE_LEFT) ? shadow_thickness : 0));
	XDrawSegments(dpy, x_win, bottom_shadow_gc, bottom_shadow,
		      ((edges & EDGE_BOTTOM) ? shadow_thickness : 0)
		      + ((edges & EDGE_RIGHT) ? shadow_thickness : 0));
}

/*****************************************************************************
 x_generate_shadow_pixels

 Given three pixels (top shadow, bottom shadow, background) massage
 the top and bottom shadow colors to guarantee that they differ.  The
 background pixels are not allowed to be modified.

 This function modifies its parameters.

 This code is modified from code blatantly stolen from lwlib/xlwmenu.c
 ****************************************************************************/
#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

void
x_generate_shadow_pixels(struct frame *f, unsigned long *top_shadow,
			 unsigned long *bottom_shadow,
			 unsigned long background,
			 unsigned long core_background)
{
	struct device *d = XDEVICE(f->device);
	Display *dpy = DEVICE_X_DISPLAY(d);
	Colormap cmap = DEVICE_X_COLORMAP(d);
	Visual *visual = DEVICE_X_VISUAL(d);

	XColor topc, botc;
	int top_frobbed = 0, bottom_frobbed = 0;

	/* If the top shadow is the same color as the background, try to
	   adjust it. */
	if (*top_shadow == background) {
		topc.pixel = background;
		XQueryColor(dpy, cmap, &topc);
		/* don't overflow/wrap! */
		topc.red = MINL(65535, (unsigned long)topc.red * 6 / 5);
		topc.green = MINL(65535, (unsigned long)topc.green * 6 / 5);
		topc.blue = MINL(65535, (unsigned long)topc.blue * 6 / 5);
		if (allocate_nearest_color(dpy, cmap, visual, &topc)) {
			*top_shadow = topc.pixel;
			top_frobbed = 1;
		}
	}

	/* If the bottom shadow is the same color as the background, try to
	   adjust it. */
	if (*bottom_shadow == background) {
		botc.pixel = background;
		XQueryColor(dpy, cmap, &botc);
		botc.red = (unsigned short)((unsigned long)botc.red * 3 / 5);
		botc.green =
		    (unsigned short)((unsigned long)botc.green * 3 / 5);
		botc.blue = (unsigned short)((unsigned long)botc.blue * 3 / 5);
		if (allocate_nearest_color(dpy, cmap, visual, &botc)) {
			*bottom_shadow = botc.pixel;
			bottom_frobbed = 1;
		}
	}

	/* If we had to adjust both shadows, then we have to do some
	   additional work. */
	if (top_frobbed && bottom_frobbed) {
		int top_avg =
		    ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
		int bot_avg =
		    ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
		if (bot_avg > top_avg) {
			Pixel tmp = *top_shadow;

			*top_shadow = *bottom_shadow;
			*bottom_shadow = tmp;
		} else if (topc.pixel == botc.pixel) {
			if (botc.pixel == background)
				*top_shadow = core_background;
			else
				*bottom_shadow = background;
		}
	}
}

/*****************************************************************************
 x_redraw_exposed_window

 Given a bounding box for an area that needs to be redrawn, determine
 what parts of what lines are contained within and re-output their
 contents.
 ****************************************************************************/
static void
x_redraw_exposed_window(struct window *w, int x, int y, int width, int height)
{
	struct frame *f = XFRAME(w->frame);
	int line;
	int start_x, start_y, end_x, end_y;
	int orig_windows_structure_changed;

	display_line_dynarr *cdla = window_display_lines(w, CURRENT_DISP);

	if (!NILP(w->vchild)) {
		x_redraw_exposed_windows(w->vchild, x, y, width, height);
		return;
	} else if (!NILP(w->hchild)) {
		x_redraw_exposed_windows(w->hchild, x, y, width, height);
		return;
	}

	/* If the window doesn't intersect the exposed region, we're done here. */
	if (x >= WINDOW_RIGHT(w) || (x + width) <= WINDOW_LEFT(w)
	    || y >= WINDOW_BOTTOM(w) || (y + height) <= WINDOW_TOP(w)) {
		return;
	} else {
		start_x = max(WINDOW_LEFT(w), x);
		end_x = min(WINDOW_RIGHT(w), (x + width));
		start_y = max(WINDOW_TOP(w), y);
		end_y = min(WINDOW_BOTTOM(w), y + height);

		/* We do this to make sure that the 3D modelines get redrawn if
		   they are in the exposed region. */
		orig_windows_structure_changed = f->windows_structure_changed;
		f->windows_structure_changed = 1;
	}

	redisplay_clear_top_of_window(w);
	if (window_needs_vertical_divider(w)) {
		x_output_vertical_divider(w, 0);
	}

	for (line = 0; line < Dynarr_length(cdla); line++) {
		struct display_line *cdl = Dynarr_atp(cdla, line);
		int top_y = cdl->ypos - cdl->ascent;
		int bottom_y = cdl->ypos + cdl->descent;

		if (bottom_y >= start_y) {
			if (top_y > end_y) {
				if (line == 0)
					continue;
				else
					break;
			} else {
				output_display_line(w, 0, cdla, line, start_x,
						    end_x);
			}
		}
	}

	f->windows_structure_changed = orig_windows_structure_changed;

	/* If there have never been any face cache_elements created, then this
	   expose event doesn't actually have anything to do. */
	if (Dynarr_largest(w->face_cachels))
		redisplay_clear_bottom_of_window(w, cdla, start_y, end_y);
}

/*****************************************************************************
 x_redraw_exposed_windows

 For each window beneath the given window in the window hierarchy,
 ensure that it is redrawn if necessary after an Expose event.
 ****************************************************************************/
static void
x_redraw_exposed_windows(Lisp_Object window, int x, int y, int width,
			 int height)
{
	for (; !NILP(window); window = XWINDOW(window)->next)
		x_redraw_exposed_window(XWINDOW(window), x, y, width, height);
}

/*****************************************************************************
 x_redraw_exposed_area

 For each window on the given frame, ensure that any area in the
 Exposed area is redrawn.
 ****************************************************************************/
void x_redraw_exposed_area(struct frame *f, int x, int y, int width, int height)
{
	/* If any window on the frame has had its face cache reset then the
	   redisplay structures are effectively invalid.  If we attempt to
	   use them we'll blow up.  We mark the frame as changed to ensure
	   that redisplay will do a full update.  This probably isn't
	   necessary but it can't hurt. */

#ifdef HAVE_TOOLBARS
	/* #### We would rather put these off as well but there is currently
	   no combination of flags which will force an unchanged toolbar to
	   redraw anyhow. */
	MAYBE_FRAMEMETH(f, redraw_exposed_toolbars, (f, x, y, width, height));
#endif
	redraw_exposed_gutters(f, x, y, width, height);

	if (!f->window_face_cache_reset) {
		x_redraw_exposed_windows(f->root_window, x, y, width, height);

		XFlush(DEVICE_X_DISPLAY(XDEVICE(f->device)));
	} else
		MARK_FRAME_CHANGED(f);
}

/****************************************************************************
 x_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
x_clear_region(Lisp_Object locale, struct device *d, struct frame *f,
	       face_index findex, int x, int y, int width, int height,
	       Lisp_Object fcolor, Lisp_Object bcolor,
	       Lisp_Object background_pixmap)
{
	Display *dpy;
	Window x_win;
	GC gc = NULL;

	dpy = DEVICE_X_DISPLAY(d);
	x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));

	if (!UNBOUNDP(background_pixmap)) {
		gc = x_get_gc(d, Qnil, fcolor, bcolor, background_pixmap, Qnil);
	}

	if (gc)
		XFillRectangle(dpy, x_win, gc, x, y, width, height);
	else
		XClearArea(dpy, x_win, x, y, width, height, False);
}

/*****************************************************************************
 x_output_eol_cursor

 Draw a cursor at the end of a line.  The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
x_output_eol_cursor(struct window *w, struct display_line *dl, int xpos,
		    face_index findex)
{
	struct frame *f = XFRAME(w->frame);
	struct device *d = XDEVICE(f->device);
	Lisp_Object window;

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC gc;
	face_index elt = get_builtin_face_cache_index(w, Vtext_cursor_face);
	struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL(w, elt);

	int focus = EQ(w->frame, DEVICE_FRAME_WITH_FOCUS_REAL(d));
	Lisp_Object bar_cursor_value = symbol_value_in_buffer(Qbar_cursor,
							      WINDOW_BUFFER(w));

	int x = xpos;
	int y = DISPLAY_LINE_YPOS(dl);
	int width = EOL_CURSOR_WIDTH;
	int height = DISPLAY_LINE_HEIGHT(dl);
	int cursor_height, cursor_y;
	int defheight, defascent;

	XSETWINDOW(window, w);
	redisplay_clear_region(window, findex, x, y, width, height);

	if (NILP(w->text_cursor_visible_p))
		return;

	gc = x_get_gc(d, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil);

	default_face_font_info(window, &defascent, 0, &defheight, 0, 0);

	/* make sure the cursor is entirely contained between y and y+height */
	cursor_height = min(defheight, height);
	cursor_y = max(y, min(y + height - cursor_height,
			      dl->ypos - defascent));

	if (focus) {
#ifdef HAVE_XIM
		XIM_SetSpotLocation(f, x - 2, cursor_y + cursor_height - 2);
#endif				/* HAVE_XIM */

		if (NILP(bar_cursor_value)) {
			XFillRectangle(dpy, x_win, gc, x, cursor_y, width,
				       cursor_height);
		} else {
			int bar_width = EQ(bar_cursor_value, Qt) ? 1 : 2;

			gc = x_get_gc(d, Qnil, cursor_cachel->background, Qnil,
				      Qnil, make_int(bar_width));
			XDrawLine(dpy, x_win, gc, x + bar_width - 1, cursor_y,
				  x + bar_width - 1,
				  cursor_y + cursor_height - 1);
		}
	} else if (NILP(bar_cursor_value)) {
		XDrawRectangle(dpy, x_win, gc, x, cursor_y, width - 1,
			       cursor_height - 1);
	}
}

static void x_clear_frame_window(Lisp_Object window)
{
	struct window *w = XWINDOW(window);

	if (!NILP(w->vchild)) {
		x_clear_frame_windows(w->vchild);
		return;
	}

	if (!NILP(w->hchild)) {
		x_clear_frame_windows(w->hchild);
		return;
	}

	redisplay_clear_to_window_end(w, WINDOW_TEXT_TOP(w),
				      WINDOW_TEXT_BOTTOM(w));
}

static void x_clear_frame_windows(Lisp_Object window)
{
	for (; !NILP(window); window = XWINDOW(window)->next)
		x_clear_frame_window(window);
}

static void x_clear_frame(struct frame *f)
{
	struct device *d = XDEVICE(f->device);
	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	int x, y, width, height;
	Lisp_Object frame;

	x = FRAME_LEFT_BORDER_START(f);
	width = (FRAME_PIXWIDTH(f) - FRAME_REAL_LEFT_TOOLBAR_WIDTH(f) -
		 FRAME_REAL_RIGHT_TOOLBAR_WIDTH(f) -
		 2 * FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH(f) -
		 2 * FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH(f));
	/* #### This adjustment by 1 should be being done in the macros.
	   There is some small differences between when the menubar is on
	   and off that we still need to deal with. */
	y = FRAME_TOP_BORDER_START(f) - 1;
	height = (FRAME_PIXHEIGHT(f) - FRAME_REAL_TOP_TOOLBAR_HEIGHT(f) -
		  FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT(f) -
		  2 * FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH(f) -
		  2 * FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH(f)) + 1;

	XClearArea(dpy, x_win, x, y, width, height, False);

	XSETFRAME(frame, f);

	if (!UNBOUNDP(FACE_BACKGROUND_PIXMAP(Vdefault_face, frame))
	    || !UNBOUNDP(FACE_BACKGROUND_PIXMAP(Vleft_margin_face, frame))
	    || !UNBOUNDP(FACE_BACKGROUND_PIXMAP(Vright_margin_face, frame))) {
		x_clear_frame_windows(f->root_window);
	}

	XFlush(DEVICE_X_DISPLAY(d));
}

/* briefly swap the foreground and background colors.
 */

static int x_flash(struct device *d)
{
	Display *dpy;
	Window win;
	XGCValues gcv;
	GC gc;
	XColor tmp_fcolor, tmp_bcolor;
	Lisp_Object tmp_pixel, frame;
	struct frame *f = device_selected_frame(d);
	struct window *w = XWINDOW(FRAME_ROOT_WINDOW(f));
	Widget shell = FRAME_X_SHELL_WIDGET(f);
	int flash_height;

	XSETFRAME(frame, f);

	tmp_pixel = FACE_FOREGROUND(Vdefault_face, frame);
	tmp_fcolor = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));
	tmp_pixel = FACE_BACKGROUND(Vdefault_face, frame);
	tmp_bcolor = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(tmp_pixel));

	dpy = XtDisplay(shell);
	win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	memset(&gcv, ~0, sizeof(XGCValues));	/* initialize all slots to ~0 */
	gcv.foreground = (tmp_fcolor.pixel ^ tmp_bcolor.pixel);
	gcv.function = GXxor;
	gcv.graphics_exposures = False;
	gc = gc_cache_lookup(DEVICE_X_GC_CACHE(XDEVICE(f->device)), &gcv,
			     (GCForeground | GCFunction | GCGraphicsExposures));
	default_face_height_and_width(frame, &flash_height, 0);

	/* If window is tall, flash top and bottom line.  */
	if (EQ(Vvisible_bell, Qtop_bottom)
	    && w->pixel_height > 3 * flash_height) {
		XFillRectangle(dpy, win, gc, w->pixel_left, w->pixel_top,
			       w->pixel_width, flash_height);
		XFillRectangle(dpy, win, gc, w->pixel_left,
			       w->pixel_top + w->pixel_height - flash_height,
			       w->pixel_width, flash_height);
	} else
		/* If it is short, flash it all.  */
		XFillRectangle(dpy, win, gc, w->pixel_left, w->pixel_top,
			       w->pixel_width, w->pixel_height);

	XSync(dpy, False);

#ifdef HAVE_SELECT
	{
		int usecs = 100000;
		struct timeval tv;
		tv.tv_sec = usecs / 1000000L;
		tv.tv_usec = usecs % 1000000L;
		/* I'm sure someone is going to complain about this... */
		select(0, 0, 0, 0, &tv);
	}
#else
#ifdef HAVE_POLL
	poll(0, 0, 100);
#else				/* !HAVE_POLL */
	bite me
#endif				/* HAVE_POLL */
#endif				/* HAVE_SELECT */
	    /* If window is tall, flash top and bottom line.  */
	if (EQ(Vvisible_bell, Qtop_bottom)
	    && w->pixel_height > 3 * flash_height) {
		XFillRectangle(dpy, win, gc, w->pixel_left, w->pixel_top,
			       w->pixel_width, flash_height);
		XFillRectangle(dpy, win, gc, w->pixel_left,
			       w->pixel_top + w->pixel_height - flash_height,
			       w->pixel_width, flash_height);
	} else
		/* If it is short, flash it all.  */
		XFillRectangle(dpy, win, gc, w->pixel_left, w->pixel_top,
			       w->pixel_width, w->pixel_height);

	XSync(dpy, False);

	return 1;
}

/* Make audible bell.  */

static void x_ring_bell(struct device *d, int volume, int pitch, int duration)
{
	Display *display = DEVICE_X_DISPLAY(d);

	if (volume < 0)
		volume = 0;
	else if (volume > 100)
		volume = 100;
	if (pitch < 0 && duration < 0) {
		XBell(display, (volume * 2) - 100);
		XFlush(display);
	} else {
		XKeyboardState state;
		XKeyboardControl ctl;
		XSync(display, 0);
		/* #### grab server? */
		XGetKeyboardControl(display, &state);

		ctl.bell_pitch = (pitch >= 0 ? pitch : (int)state.bell_pitch);
		ctl.bell_duration =
		    (duration >= 0 ? duration : (int)state.bell_duration);
		XChangeKeyboardControl(display, KBBellPitch | KBBellDuration,
				       &ctl);

		XBell(display, (volume * 2) - 100);

		ctl.bell_pitch = state.bell_pitch;
		ctl.bell_duration = state.bell_duration;
		XChangeKeyboardControl(display, KBBellPitch | KBBellDuration,
				       &ctl);

		/* #### ungrab server? */
		XSync(display, 0);
	}
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_redisplay_x(void)
{
	/* redisplay methods */
	CONSOLE_HAS_METHOD(x, text_width);
	CONSOLE_HAS_METHOD(x, output_display_block);
	CONSOLE_HAS_METHOD(x, divider_height);
	CONSOLE_HAS_METHOD(x, eol_cursor_width);
	CONSOLE_HAS_METHOD(x, output_vertical_divider);
	CONSOLE_HAS_METHOD(x, clear_region);
	CONSOLE_HAS_METHOD(x, clear_frame);
	CONSOLE_HAS_METHOD(x, window_output_begin);
	CONSOLE_HAS_METHOD(x, window_output_end);
	CONSOLE_HAS_METHOD(x, flash);
	CONSOLE_HAS_METHOD(x, ring_bell);
	CONSOLE_HAS_METHOD(x, bevel_area);
	CONSOLE_HAS_METHOD(x, output_string);
	CONSOLE_HAS_METHOD(x, output_pixmap);
}
