/* mswindows-specific glyphs and related.
   Copyright (C) 1998 Andy Piper

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with:  Not in FSF. */

#ifndef INCLUDED_glyphs_msw_h_
#define INCLUDED_glyphs_msw_h_

#ifdef HAVE_MS_WINDOWS

#include <windows.h>
#include "glyphs.h"

/****************************************************************************
 *                         Image-Instance Object                            *
 ****************************************************************************/

struct mswindows_image_instance_data
{
  HBITMAP* bitmaps;
  HICON icon;
  int real_width, real_height;
};

#define MSWINDOWS_IMAGE_INSTANCE_DATA(i) \
((struct mswindows_image_instance_data *) (i)->data)

#define IMAGE_INSTANCE_MSWINDOWS_BITMAP(i) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->bitmaps[0])
#define IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICE(i,slice) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->bitmaps[slice])
#define IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICES(i) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->bitmaps)
#define IMAGE_INSTANCE_MSWINDOWS_MASK(i) \
     (*(HBITMAP*)&(IMAGE_INSTANCE_PIXMAP_MASK (i)))		/* Make it lvalue */
#define IMAGE_INSTANCE_MSWINDOWS_ICON(i) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->icon)
#define IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_WIDTH(i) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->real_width)
#define IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_HEIGHT(i) \
     (MSWINDOWS_IMAGE_INSTANCE_DATA (i)->real_height)

#define XIMAGE_INSTANCE_MSWINDOWS_BITMAP(i) \
  IMAGE_INSTANCE_MSWINDOWS_BITMAP (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICE(i,slice) \
  IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICE (XIMAGE_INSTANCE (i,slice))
#define XIMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICES(i) \
  IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICES (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_MSWINDOWS_MASK(i) \
  IMAGE_INSTANCE_MSWINDOWS_MASK (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_MSWINDOWS_ICON(i) \
  IMAGE_INSTANCE_MSWINDOWS_ICON (XIMAGE_INSTANCE (i))

int
mswindows_resize_dibitmap_instance (Lisp_Image_Instance* ii,
				    struct frame* f,
				    int newx, int newy);
HBITMAP
mswindows_create_resized_bitmap (Lisp_Image_Instance* ii,
				 struct frame* f,
				 int newx, int newy);
HBITMAP
mswindows_create_resized_mask (Lisp_Image_Instance* ii,
			       struct frame* f,
			       int newx, int newy);
void
mswindows_initialize_image_instance_icon (Lisp_Image_Instance* image,
					  int cursor);

#define WIDGET_INSTANCE_MSWINDOWS_HANDLE(i) \
     (HWND) (IMAGE_INSTANCE_SUBWINDOW_ID (i))

#define XWIDGET_INSTANCE_MSWINDOWS_HANDLE(i) \
  WIDGET_INSTANCE_MSWINDOWS_HANDLE (XIMAGE_INSTANCE (i))

struct mswindows_subwindow_data
{
  HWND clip_window;
};

#define MSWINDOWS_SUBWINDOW_DATA(i) \
  ((struct mswindows_subwindow_data *) (i)->data)
#define IMAGE_INSTANCE_MSWINDOWS_CLIPWINDOW(i) \
     (MSWINDOWS_SUBWINDOW_DATA (i)->clip_window)

#define XIMAGE_INSTANCE_MSWINDOWS_SUBWINDOW_DATA(i) \
  MSWINDOWS_SUBWINDOW_DATA (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_MSWINDOWS_CLIPWINDOW(i) \
  IMAGE_INSTANCE_MSWINDOWS_CLIPWINDOW (XIMAGE_INSTANCE (i))

#define DOMAIN_MSWINDOWS_HANDLE(domain) \
  ((IMAGE_INSTANCEP (domain) && \
  XIMAGE_INSTANCE_MSWINDOWS_SUBWINDOW_DATA (domain)) ? \
   XWIDGET_INSTANCE_MSWINDOWS_HANDLE (domain) : \
   FRAME_MSWINDOWS_HANDLE (DOMAIN_XFRAME (domain)))

#endif /* HAVE_MS_WINDOWS */

#endif /* INCLUDED_glyphs_msw_h_ */
