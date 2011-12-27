/* X-specific glyphs and related.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing
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

#ifndef INCLUDED_glyphs_x_h_
#define INCLUDED_glyphs_x_h_

#include "ui/glyphs.h"

#ifdef HAVE_X_WINDOWS

#include "xintrinsic.h"
#include "ui/lwlib/lwlib.h"
#include "ui/lwlib/lwlib-utils.h"

/****************************************************************************
 *                         Image-Instance Object                            *
 ****************************************************************************/

struct x_image_instance_data {
	Pixmap *pixmaps;
	Cursor cursor;

	/* If depth>0, then that means that other colors were allocated when
	   this pixmap was loaded.  These are they; we need to free them when
	   finalizing the image instance. */
	Colormap colormap;
	unsigned long *pixels;
	int npixels;

	/* Should we hang on to the extra info from the XpmAttributes, like
	   the textual color table and the comments?   Is that useful? */
};

#define X_IMAGE_INSTANCE_DATA(i) ((struct x_image_instance_data *) (i)->data)

#define IMAGE_INSTANCE_X_PIXMAP(i) (X_IMAGE_INSTANCE_DATA (i)->pixmaps[0])
#define IMAGE_INSTANCE_X_PIXMAP_SLICE(i,slice) \
     (X_IMAGE_INSTANCE_DATA (i)->pixmaps[slice])
#define IMAGE_INSTANCE_X_PIXMAP_SLICES(i) \
     (X_IMAGE_INSTANCE_DATA (i)->pixmaps)
#define IMAGE_INSTANCE_X_MASK(i) \
	(Pixmap)(IMAGE_INSTANCE_PIXMAP_MASK (i))
#define IMAGE_INSTANCE_X_CURSOR(i) (X_IMAGE_INSTANCE_DATA (i)->cursor)
#define IMAGE_INSTANCE_X_COLORMAP(i) (X_IMAGE_INSTANCE_DATA (i)->colormap)
#define IMAGE_INSTANCE_X_PIXELS(i) (X_IMAGE_INSTANCE_DATA (i)->pixels)
#define IMAGE_INSTANCE_X_NPIXELS(i) (X_IMAGE_INSTANCE_DATA (i)->npixels)

#define XIMAGE_INSTANCE_X_PIXMAP(i) \
  IMAGE_INSTANCE_X_PIXMAP (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_PIXMAP_SLICES(i) \
  IMAGE_INSTANCE_X_PIXMAP_SLICES (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_PIXMAP_SLICE(i) \
  IMAGE_INSTANCE_X_PIXMAP_SLICE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_MASK(i) \
  IMAGE_INSTANCE_X_MASK (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_CURSOR(i) \
  IMAGE_INSTANCE_X_CURSOR (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_PIXELS(i) \
  IMAGE_INSTANCE_X_PIXELS (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_NPIXELS(i) \
  IMAGE_INSTANCE_X_NPIXELS (XIMAGE_INSTANCE (i))

/****************************************************************************
 *                            Subwindow Object                              *
 ****************************************************************************/

struct x_subwindow_data {
	union {
		struct {
			Display *display;
			Window parent_window;
			Window clip_window;
		} sub;
		struct {
			Widget clip_window;
			Position x_offset;
			Position y_offset;
			LWLIB_ID id;
		} wid;
	} data;
};

#define X_SUBWINDOW_INSTANCE_DATA(i) ((struct x_subwindow_data *) (i)->data)

#define IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.sub.display)
#define IMAGE_INSTANCE_X_SUBWINDOW_PARENT(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.sub.parent_window)
#define IMAGE_INSTANCE_X_CLIPWINDOW(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.sub.clip_window)
#define IMAGE_INSTANCE_X_WIDGET_XOFFSET(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.wid.x_offset)
#define IMAGE_INSTANCE_X_WIDGET_YOFFSET(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.wid.y_offset)
#define IMAGE_INSTANCE_X_WIDGET_LWID(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.wid.id)
#define IMAGE_INSTANCE_X_CLIPWIDGET(i) \
  (X_SUBWINDOW_INSTANCE_DATA (i)->data.wid.clip_window)
#define IMAGE_INSTANCE_X_SUBWINDOW_ID(i) \
	(*(Window*)((void*)&IMAGE_INSTANCE_SUBWINDOW_ID(i)))
#define IMAGE_INSTANCE_X_WIDGET_ID(i) \
	(*(Widget*)((void*)&IMAGE_INSTANCE_SUBWINDOW_ID(i)))

#define XIMAGE_INSTANCE_X_SUBWINDOW_PARENT(i) \
  IMAGE_INSTANCE_X_SUBWINDOW_PARENT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_SUBWINDOW_DISPLAY(i) \
  IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_WIDGET_XOFFSET(i) \
  IMAGE_INSTANCE_X_WIDGET_XOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_WIDGET_YOFFSET(i) \
  IMAGE_INSTANCE_X_WIDGET_YOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_WIDGET_LWID(i) \
  IMAGE_INSTANCE_X_WIDGET_LWID (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_CLIPWIDGET(i) \
  IMAGE_INSTANCE_X_CLIPWIDGET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_CLIPWINDOW(i) \
  IMAGE_INSTANCE_X_CLIPWINDOW (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_X_WIDGET_ID(i) \
  IMAGE_INSTANCE_X_WIDGET_ID (XIMAGE_INSTANCE (i))

#define DOMAIN_X_WIDGET(domain) \
  ((IMAGE_INSTANCEP (domain) && \
  X_SUBWINDOW_INSTANCE_DATA (XIMAGE_INSTANCE (domain))) ? \
   XIMAGE_INSTANCE_X_WIDGET_ID (domain) : \
   FRAME_X_CONTAINER_WIDGET (f) (DOMAIN_XFRAME (domain)))

#endif				/* HAVE_X_WINDOWS */
#endif				/* INCLUDED_glyphs_x_h_ */
