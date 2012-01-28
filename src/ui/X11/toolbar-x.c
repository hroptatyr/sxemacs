/* toolbar implementation -- X interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.

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

/* This file Mule-ized (more like Mule-verified) by Ben Wing, 7-8-00. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "glyphs-x.h"
#include "objects-x.h"
#include "EmacsFrame.h"
#include "EmacsFrameP.h"

#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/toolbar.h"
#include "ui/window.h"

static void
x_draw_blank_toolbar_button(struct frame *f, int x, int y, int width,
			    int height, int threed, int border_width,
			    int vertical)
{
	struct device *d = XDEVICE(f->device);
	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;
	int sx = x, sy = y, swidth = width, sheight = height;

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC top_shadow_gc, bottom_shadow_gc, background_gc;

	background_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);

	if (threed) {
		top_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC(f);
		bottom_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f);
	} else {
		top_shadow_gc = background_gc;
		bottom_shadow_gc = background_gc;
	}

	if (vertical) {
		sx += border_width;
		swidth -= 2 * border_width;
	} else {
		sy += border_width;
		sheight -= 2 * border_width;
	}

	/* Draw the outline. */
	x_output_shadows(f, sx, sy, swidth, sheight, top_shadow_gc,
			 bottom_shadow_gc, background_gc, shadow_thickness,
			 EDGE_ALL);

	/* Blank the middle. */
	XFillRectangle(dpy, x_win, background_gc, sx + shadow_thickness,
		       sy + shadow_thickness, swidth - shadow_thickness * 2,
		       sheight - shadow_thickness * 2);

	/* Do the border */
	XFillRectangle(dpy, x_win, background_gc, x, y,
		       (vertical ? border_width : width),
		       (vertical ? height : border_width));
	XFillRectangle(dpy, x_win, background_gc,
		       (vertical ? sx + swidth : x),
		       (vertical ? y : sy + sheight),
		       (vertical ? border_width : width),
		       (vertical ? height : border_width));
}

static void x_output_toolbar_button(struct frame *f, Lisp_Object button)
{
	struct device *d = XDEVICE(f->device);
	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;
	int x_adj, y_adj, width_adj, height_adj;

	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC top_shadow_gc, bottom_shadow_gc, background_gc;
	Lisp_Object instance, frame, window, glyph;
	struct toolbar_button *tb = XTOOLBAR_BUTTON(button);
	Lisp_Image_Instance *p;
	struct window *w;
	int vertical = tb->vertical;
	int border_width = tb->border_width;

	if (vertical) {
		x_adj = border_width;
		width_adj = -2 * border_width;
		y_adj = 0;
		height_adj = 0;
	} else {
		x_adj = 0;
		width_adj = 0;
		y_adj = border_width;
		height_adj = -2 * border_width;
	}

	XSETFRAME(frame, f);
	window = FRAME_LAST_NONMINIBUF_WINDOW(f);
	w = XWINDOW(window);

	glyph = get_toolbar_button_glyph(w, tb);

	if (tb->enabled) {
		if (tb->down) {
			top_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f);
			bottom_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC(f);
		} else {
			top_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC(f);
			bottom_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f);
		}
	} else {
		top_shadow_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);
		bottom_shadow_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);
	}
	background_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);

	/* Draw the outline. */
	x_output_shadows(f, tb->x + x_adj, tb->y + y_adj,
			 tb->width + width_adj, tb->height + height_adj,
			 top_shadow_gc,
			 bottom_shadow_gc, background_gc, shadow_thickness,
			 EDGE_ALL);

	/* Clear the pixmap area. */
	XFillRectangle(dpy, x_win, background_gc,
		       tb->x + x_adj + shadow_thickness,
		       tb->y + y_adj + shadow_thickness,
		       tb->width + width_adj - shadow_thickness * 2,
		       tb->height + height_adj - shadow_thickness * 2);

	/* Do the border. */
	XFillRectangle(dpy, x_win, background_gc, tb->x, tb->y,
		       (vertical ? border_width : tb->width),
		       (vertical ? tb->height : border_width));

	XFillRectangle(dpy, x_win, background_gc,
		       (vertical ? tb->x + tb->width - border_width : tb->x),
		       (vertical ? tb->y : tb->y + tb->height - border_width),
		       (vertical ? border_width : tb->width),
		       (vertical ? tb->height : border_width));

	background_gc = FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f);

	/* #### It is currently possible for users to trash us by directly
	   changing the toolbar glyphs.  Avoid crashing in that case. */
	if (GLYPHP(glyph))
		instance = glyph_image_instance(glyph, window, ERROR_ME_NOT, 1);
	else
		instance = Qnil;

	if (IMAGE_INSTANCEP(instance)) {
		int width = tb->width + width_adj - shadow_thickness * 2;
		int height = tb->height + height_adj - shadow_thickness * 2;
		int x_offset = x_adj + shadow_thickness;
		int y_offset = y_adj + shadow_thickness;

		p = XIMAGE_INSTANCE(instance);

		if (IMAGE_INSTANCE_PIXMAP_TYPE_P(p)) {
			if (width > (int)IMAGE_INSTANCE_PIXMAP_WIDTH(p)) {
				x_offset +=
				    ((int)
				     (width - IMAGE_INSTANCE_PIXMAP_WIDTH(p))
				     / 2);
				width = IMAGE_INSTANCE_PIXMAP_WIDTH(p);
			}
			if (height > (int)IMAGE_INSTANCE_PIXMAP_HEIGHT(p)) {
				y_offset +=
				    ((int)
				     (height - IMAGE_INSTANCE_PIXMAP_HEIGHT(p))
				     / 2);
				height = IMAGE_INSTANCE_PIXMAP_HEIGHT(p);
			}

			x_output_x_pixmap(f, XIMAGE_INSTANCE(instance),
					  tb->x + x_offset, tb->y + y_offset, 0,
					  0, width, height, 0, 0,
					  background_gc);
		} else if (IMAGE_INSTANCE_TYPE(p) == IMAGE_TEXT) {
			/* #### We need to make the face used configurable. */
			struct face_cachel *cachel =
			    WINDOW_FACE_CACHEL(w, DEFAULT_INDEX);
			struct display_line dl;
			Lisp_Object string = IMAGE_INSTANCE_TEXT_STRING(p);
			unsigned char charsets[NUM_LEADING_BYTES];
			Emchar_dynarr *buf;
			struct font_metric_info fm;

			/* This could be true if we were called via the Expose event
			   handler.  Mark the button as dirty and return
			   immediately. */
			if (f->window_face_cache_reset) {
				tb->dirty = 1;
				MARK_TOOLBAR_CHANGED;
				return;
			}
			buf = Dynarr_new(Emchar);
			convert_bufbyte_string_into_emchar_dynarr
			    (XSTRING_DATA(string), XSTRING_LENGTH(string), buf);
			find_charsets_in_emchar_string(charsets,
						       Dynarr_atp(buf, 0),
						       Dynarr_length(buf));
			ensure_face_cachel_complete(cachel, window, charsets);
			face_cachel_charset_font_metric_info(cachel, charsets,
							     &fm);

			dl.ascent = fm.ascent;
			dl.descent = fm.descent;
			dl.ypos = tb->y + y_offset + fm.ascent;
			dl.top_clip = 0;

			if (fm.ascent + fm.descent <= height) {
				dl.ypos +=
				    (height - fm.ascent - fm.descent) / 2;
				dl.clip = 0;
			} else {
				dl.clip = fm.ascent + fm.descent - height;
			}

			x_output_string(w, &dl, buf, tb->x + x_offset, 0, 0,
					width, DEFAULT_INDEX, 0, 0, 0, 0);
			Dynarr_free(buf);
		}

		/* We silently ignore the image if it isn't a pixmap or text. */
	}

	tb->dirty = 0;
}

static int
x_get_button_size(struct frame *f, Lisp_Object window,
		  struct toolbar_button *tb, int vert, int pos)
{
	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;
	int size;

	if (tb->blank) {
		if (!NILP(tb->down_glyph))
			size = XINT(tb->down_glyph);
		else
			size = DEFAULT_TOOLBAR_BLANK_SIZE;
	} else {
		struct window *w = XWINDOW(window);
		Lisp_Object glyph = get_toolbar_button_glyph(w, tb);

		/* Unless, of course, the user has done something stupid like
		   change the glyph out from under us.  Use a blank placeholder
		   in that case. */
		if (NILP(glyph))
			return XINT(f->toolbar_size[pos]);

		if (vert)
			size = glyph_height(glyph, window);
		else
			size = glyph_width(glyph, window);
	}

	if (!size) {
		/* If the glyph doesn't have a size we'll insert a blank
		   placeholder instead. */
		return XINT(f->toolbar_size[pos]);
	}

	size += shadow_thickness * 2;

	return (size);
}

#define X_OUTPUT_BUTTONS_LOOP(left)					\
  do {									\
    while (!NILP (button))						\
      {									\
	struct toolbar_button *tb = XTOOLBAR_BUTTON (button);		\
	int size, height, width;					\
									\
	if (left && tb->pushright)					\
	  break;							\
									\
	size = x_get_button_size (f, window, tb, vert, pos);		\
									\
	if (vert)							\
	  {								\
	    width = bar_width;						\
	    if (y + size > max_pixpos)					\
	      height = max_pixpos - y;					\
	    else							\
	      height = size;						\
	  }								\
	else								\
	  {								\
	    if (x + size > max_pixpos)					\
	      width = max_pixpos - x;					\
	    else							\
	      width = size;						\
	    height = bar_height;					\
	  }								\
									\
	if (tb->x != x							\
	    || tb->y != y						\
	    || tb->width != width					\
	    || tb->height != height					\
	    || tb->dirty						\
	    || f->clear) /* This is clearly necessary. */		\
	  {								\
	    if (width && height)					\
	      {								\
		tb->x = x;						\
		tb->y = y;						\
		tb->width = width;					\
		tb->height = height;					\
		tb->border_width = border_width;			\
		tb->vertical = vert;					\
									\
		if (tb->blank || NILP (tb->up_glyph))			\
		  {							\
		    int threed = (EQ (Qt, tb->up_glyph) ? 1 : 0);	\
		    x_draw_blank_toolbar_button (f, x, y, width,	\
						 height, threed,	\
						 border_width, vert);	\
		  }							\
		else							\
		  x_output_toolbar_button (f, button);			\
	      }								\
	  }								\
									\
	if (vert)							\
	  y += height;							\
	else								\
	  x += width;							\
									\
	if ((vert && y == max_pixpos) || (!vert && x == max_pixpos))	\
	  button = Qnil;						\
	else								\
	  button = tb->next;						\
      }									\
  } while (0)

#define SET_TOOLBAR_WAS_VISIBLE_FLAG(frame, pos, flag)			\
  do {									\
    switch (pos)							\
      {									\
      case TOP_TOOLBAR:							\
	(frame)->top_toolbar_was_visible = flag;			\
	break;								\
      case BOTTOM_TOOLBAR:						\
	(frame)->bottom_toolbar_was_visible = flag;			\
	break;								\
      case LEFT_TOOLBAR:						\
	(frame)->left_toolbar_was_visible = flag;			\
	break;								\
      case RIGHT_TOOLBAR:						\
	(frame)->right_toolbar_was_visible = flag;			\
	break;								\
      default:								\
	abort ();							\
      }									\
  } while (0)

static void x_output_toolbar(struct frame *f, enum toolbar_pos pos)
{
	struct device *d = XDEVICE(f->device);
	int x, y, bar_width, bar_height, vert;
	int max_pixpos, right_size, right_start, blank_size;
	int border_width = FRAME_REAL_TOOLBAR_BORDER_WIDTH(f, pos);
	Lisp_Object button, window;
	Display *dpy = DEVICE_X_DISPLAY(d);
	Window x_win = XtWindow(FRAME_X_TEXT_WIDGET(f));
	GC background_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);

	get_toolbar_coords(f, pos, &x, &y, &bar_width, &bar_height, &vert, 1);
	window = FRAME_LAST_NONMINIBUF_WINDOW(f);

	/* Do the border */
	XFillRectangle(dpy, x_win, background_gc, x, y,
		       (vert ? bar_width : border_width),
		       (vert ? border_width : bar_height));
	XFillRectangle(dpy, x_win, background_gc,
		       (vert ? x : x + bar_width - border_width),
		       (vert ? y + bar_height - border_width : y),
		       (vert ? bar_width : border_width),
		       (vert ? border_width : bar_height));

	if (vert) {
		max_pixpos = y + bar_height - border_width;
		y += border_width;
	} else {
		max_pixpos = x + bar_width - border_width;
		x += border_width;
	}

	button = FRAME_TOOLBAR_BUTTONS(f, pos);
	right_size = 0;

	/* First loop over all of the buttons to determine how much room we
	   need for left hand and right hand buttons.  This loop will also
	   make sure that all instances are instantiated so when we actually
	   output them they will come up immediately. */
	while (!NILP(button)) {
		struct toolbar_button *tb = XTOOLBAR_BUTTON(button);
		int size = x_get_button_size(f, window, tb, vert, pos);

		if (tb->pushright)
			right_size += size;

		button = tb->next;
	}

	button = FRAME_TOOLBAR_BUTTONS(f, pos);

	/* Loop over the left buttons, updating and outputting them. */
	X_OUTPUT_BUTTONS_LOOP(1);

	/* Now determine where the right buttons start. */
	right_start = max_pixpos - right_size;
	if (right_start < (vert ? y : x))
		right_start = (vert ? y : x);

	/* Output the blank which goes from the end of the left buttons to
	   the start of the right. */
	blank_size = right_start - (vert ? y : x);
	if (blank_size) {
		int height, width;

		if (vert) {
			width = bar_width;
			height = blank_size;
		} else {
			width = blank_size;
			height = bar_height;
		}

		/*
		 * Use a 3D pushright separator only if there isn't a toolbar
		 * border.  A flat separator meshes with the border and looks
		 * better.
		 */
		x_draw_blank_toolbar_button(f, x, y, width, height,
					    !border_width, border_width, vert);

		if (vert)
			y += height;
		else
			x += width;
	}

	/* Loop over the right buttons, updating and outputting them. */
	X_OUTPUT_BUTTONS_LOOP(0);

	if (!vert) {
		Lisp_Object frame;

		XSETFRAME(frame, f);
		redisplay_clear_region(frame,
				       DEFAULT_INDEX, FRAME_PIXWIDTH(f) - 1, y,
				       1, bar_height);
	}

	SET_TOOLBAR_WAS_VISIBLE_FLAG(f, pos, 1);

	XFlush(DEVICE_X_DISPLAY(d));
}

static void
x_clear_toolbar(struct frame *f, enum toolbar_pos pos, int thickness_change)
{
	Lisp_Object frame;
	struct device *d = XDEVICE(f->device);
	int x, y, width, height, vert;

	get_toolbar_coords(f, pos, &x, &y, &width, &height, &vert, 1);
	XSETFRAME(frame, f);

	/* The thickness_change parameter is used by the toolbar resize routines
	   to clear any excess toolbar if the size shrinks. */
	if (thickness_change < 0) {
		if (pos == LEFT_TOOLBAR || pos == RIGHT_TOOLBAR) {
			x = x + width + thickness_change;
			width = -thickness_change;
		} else {
			y = y + height + thickness_change;
			height = -thickness_change;
		}
	}

	SET_TOOLBAR_WAS_VISIBLE_FLAG(f, pos, 0);

	redisplay_clear_region(frame, DEFAULT_INDEX, x, y, width, height);
	XFlush(DEVICE_X_DISPLAY(d));
}

static void x_output_frame_toolbars(struct frame *f)
{
	assert(FRAME_X_P(f));

	if (FRAME_REAL_TOP_TOOLBAR_VISIBLE(f))
		x_output_toolbar(f, TOP_TOOLBAR);
	if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE(f))
		x_output_toolbar(f, BOTTOM_TOOLBAR);
	if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE(f))
		x_output_toolbar(f, LEFT_TOOLBAR);
	if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE(f))
		x_output_toolbar(f, RIGHT_TOOLBAR);
}

static void x_clear_frame_toolbars(struct frame *f)
{
	assert(FRAME_X_P(f));

	if (f->top_toolbar_was_visible && !FRAME_REAL_TOP_TOOLBAR_VISIBLE(f))
		x_clear_toolbar(f, TOP_TOOLBAR, 0);
	if (f->bottom_toolbar_was_visible
	    && !FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE(f))
		x_clear_toolbar(f, BOTTOM_TOOLBAR, 0);
	if (f->left_toolbar_was_visible && !FRAME_REAL_LEFT_TOOLBAR_VISIBLE(f))
		x_clear_toolbar(f, LEFT_TOOLBAR, 0);
	if (f->right_toolbar_was_visible
	    && !FRAME_REAL_RIGHT_TOOLBAR_VISIBLE(f))
		x_clear_toolbar(f, RIGHT_TOOLBAR, 0);
}

static void
x_redraw_exposed_toolbar(struct frame *f, enum toolbar_pos pos, int x, int y,
			 int width, int height)
{
	int bar_x, bar_y, bar_width, bar_height, vert;
	Lisp_Object button = FRAME_TOOLBAR_BUTTONS(f, pos);

	get_toolbar_coords(f, pos, &bar_x, &bar_y, &bar_width, &bar_height,
			   &vert, 1);

	if (((y + height) < bar_y) || (y > (bar_y + bar_height)))
		return;
	if (((x + width) < bar_x) || (x > (bar_x + bar_width)))
		return;

	while (!NILP(button)) {
		struct toolbar_button *tb = XTOOLBAR_BUTTON(button);

		if (vert) {
			if (((tb->y + tb->height) > y)
			    && (tb->y < (y + height)))
				tb->dirty = 1;

			/* If this is true we have gone past the exposed region. */
			if (tb->y > (y + height))
				break;
		} else {
			if (((tb->x + tb->width) > x) && (tb->x < (x + width)))
				tb->dirty = 1;

			/* If this is true we have gone past the exposed region. */
			if (tb->x > (x + width))
				break;
		}

		button = tb->next;
	}

	/* Even if none of the buttons is in the area, the blank region at
	   the very least must be because the first thing we did is verify
	   that some portion of the toolbar is in the exposed region. */
	x_output_toolbar(f, pos);
}

static void
x_redraw_exposed_toolbars(struct frame *f, int x, int y, int width, int height)
{
	assert(FRAME_X_P(f));

	if (FRAME_REAL_TOP_TOOLBAR_VISIBLE(f))
		x_redraw_exposed_toolbar(f, TOP_TOOLBAR, x, y, width, height);

	if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE(f))
		x_redraw_exposed_toolbar(f, BOTTOM_TOOLBAR, x, y, width,
					 height);

	if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE(f))
		x_redraw_exposed_toolbar(f, LEFT_TOOLBAR, x, y, width, height);

	if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE(f))
		x_redraw_exposed_toolbar(f, RIGHT_TOOLBAR, x, y, width, height);
}

static void x_redraw_frame_toolbars(struct frame *f)
{
	/* There are certain startup paths that lead to update_EmacsFrame in
	   faces.c being called before a new frame is fully initialized.  In
	   particular before we have actually mapped it.  That routine can
	   call this one.  So, we need to make sure that the frame is
	   actually ready before we try and draw all over it. */

	if (XtIsRealized(FRAME_X_SHELL_WIDGET(f)))
		x_redraw_exposed_toolbars(f, 0, 0, FRAME_PIXWIDTH(f),
					  FRAME_PIXHEIGHT(f));
}

static void x_initialize_frame_toolbar_gcs(struct frame *f)
{
	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	EmacsFramePart *efp = &(ef->emacs_frame);
	XGCValues gcv;
	unsigned long flags =
	    (GCForeground | GCBackground | GCGraphicsExposures);

	/*
	 * If backgroundToolBarColor is specified, use it.
	 * Otherwise use the background resource.
	 */
	if (efp->background_toolbar_pixel == (Pixel) (-1))
		efp->background_toolbar_pixel = efp->background_pixel;

	/*
	 * ####
	 * If foregroundToolBarColor is specified, use it.
	 * Otherwise use the foreground resource.
	 *
	 * The foreground pixel is currently unused, but will likely be
	 * used when toolbar captions are generated by the toolbar code
	 * instead being incorporated into the icon image.
	 */
	if (efp->foreground_toolbar_pixel == (Pixel) (-1))
		efp->foreground_toolbar_pixel = efp->foreground_pixel;

	gcv.foreground = efp->background_toolbar_pixel;
	gcv.background = ef->core.background_pixel;
	gcv.graphics_exposures = False;
	FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f) =
	    XtGetGC((Widget) ef, flags, &gcv);

	if (efp->top_toolbar_shadow_pixel == efp->bottom_toolbar_shadow_pixel) {
		efp->top_toolbar_shadow_pixel = efp->background_toolbar_pixel;
		efp->bottom_toolbar_shadow_pixel =
		    efp->background_toolbar_pixel;
	}

	x_generate_shadow_pixels(f, &efp->top_toolbar_shadow_pixel,
				 &efp->bottom_toolbar_shadow_pixel,
				 efp->background_toolbar_pixel,
				 ef->core.background_pixel);

	gcv.foreground = efp->top_toolbar_shadow_pixel;
	gcv.background = ef->core.background_pixel;
	gcv.graphics_exposures = False;
	flags = GCForeground | GCBackground | GCGraphicsExposures;
	if (efp->top_toolbar_shadow_pixmap) {
		gcv.fill_style = FillOpaqueStippled;
		gcv.stipple = efp->top_toolbar_shadow_pixmap;
		flags |= GCStipple | GCFillStyle;
	}
	FRAME_X_TOOLBAR_TOP_SHADOW_GC(f) = XtGetGC((Widget) ef, flags, &gcv);

	gcv.foreground = efp->bottom_toolbar_shadow_pixel;
	gcv.background = ef->core.background_pixel;
	gcv.graphics_exposures = False;
	flags = GCForeground | GCBackground | GCGraphicsExposures;
	if (efp->bottom_toolbar_shadow_pixmap) {
		gcv.fill_style = FillOpaqueStippled;
		gcv.stipple = efp->bottom_toolbar_shadow_pixmap;
		flags |= GCStipple | GCFillStyle;
	}
	FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f) = XtGetGC((Widget) ef, flags, &gcv);

#ifdef HAVE_XPM
	FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f) =
	    FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f);
#else
	{
		struct device *d = XDEVICE(f->device);
		Display *dpy = DEVICE_X_DISPLAY(d);

		gcv.background =
		    WhitePixelOfScreen(DefaultScreenOfDisplay(dpy));
		gcv.foreground =
		    BlackPixelOfScreen(DefaultScreenOfDisplay(dpy));
		gcv.graphics_exposures = False;
		flags = GCForeground | GCBackground | GCGraphicsExposures;
		FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f) =
		    XtGetGC((Widget) ef, flags, &gcv);
	}
#endif
}

static void x_release_frame_toolbar_gcs(struct frame *f)
{
	Widget ew = (Widget) FRAME_X_TEXT_WIDGET(f);
	XtReleaseGC(ew, FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f));
	/* If compiled with XPM support, this is a pointer to the same GC as
	   FRAME_X_BLANK_BACKGROUND_GC so we need to make sure we don't
	   release it twice. */
#ifndef HAVE_XPM
	XtReleaseGC(ew, FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f));
#endif
	XtReleaseGC(ew, FRAME_X_TOOLBAR_TOP_SHADOW_GC(f));
	XtReleaseGC(ew, FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f));

	/* Seg fault if we try and use these again. */
	FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f) = (GC) - 1;
	FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f) = (GC) - 1;
	FRAME_X_TOOLBAR_TOP_SHADOW_GC(f) = (GC) - 1;
	FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f) = (GC) - 1;
}

static void x_initialize_frame_toolbars(struct frame *f)
{
	EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);

	if (ef->emacs_frame.toolbar_shadow_thickness < MINIMUM_SHADOW_THICKNESS)
		Xt_SET_VALUE(FRAME_X_TEXT_WIDGET(f),
			     XtNtoolBarShadowThickness,
			     MINIMUM_SHADOW_THICKNESS);

	x_initialize_frame_toolbar_gcs(f);
}

/* This only calls one function but we go ahead and create this in
   case we ever do decide that we need to do more work. */
static void x_free_frame_toolbars(struct frame *f)
{
	x_release_frame_toolbar_gcs(f);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_toolbar_x(void)
{
	CONSOLE_HAS_METHOD(x, output_frame_toolbars);
	CONSOLE_HAS_METHOD(x, clear_frame_toolbars);
	CONSOLE_HAS_METHOD(x, initialize_frame_toolbars);
	CONSOLE_HAS_METHOD(x, free_frame_toolbars);
	CONSOLE_HAS_METHOD(x, output_toolbar_button);
	CONSOLE_HAS_METHOD(x, redraw_exposed_toolbars);
	CONSOLE_HAS_METHOD(x, redraw_frame_toolbars);
}
