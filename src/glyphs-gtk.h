/* Gtk-specific glyphs and related.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing
   Copyright (C) 1995 Sun Microsystems, Inc.

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
/* Gtk version by William Perry */

#ifndef _XEMACS_GLYPHS_GTK_H_
#define _XEMACS_GLYPHS_GTK_H_

#include "glyphs.h"

#ifdef HAVE_GTK

#include <gtk/gtk.h>

/****************************************************************************
 *                         Image-Instance Object                            *
 ****************************************************************************/

struct gtk_image_instance_data
{
  GdkPixmap **pixmaps;
  GdkCursor *cursor;

  /* If depth>0, then that means that other colors were allocated when
     this pixmap was loaded.  These are they; we need to free them when
     finalizing the image instance. */
  GdkColormap *colormap;
  unsigned long *pixels;
  int npixels;

  /* Should we hang on to the extra info from the XpmAttributes, like
     the textual color table and the comments?   Is that useful? */
};

struct gtk_subwindow_data
{
  union
  {
    struct
    {
      GtkWidget *parent_window;
      GtkWidget *clip_window;
    } sub;
    struct
    {
      GtkWidget *clip_window;
      Lisp_Object widget;
      guint x_offset;
      guint y_offset;
      gboolean added_to_fixed;
    } wid;
  } data;
};

void init_image_instance_from_gdk_pixmap (struct Lisp_Image_Instance *ii,
					  struct device *device,
					  GdkPixmap *gdk_pixmap,
					  int dest_mask,
					  Lisp_Object instantiator);

#define GTK_IMAGE_INSTANCE_DATA(i) ((struct gtk_image_instance_data *) (i)->data)

#define IMAGE_INSTANCE_GTK_PIXMAP(i) (GTK_IMAGE_INSTANCE_DATA (i)->pixmaps[0])
#define IMAGE_INSTANCE_GTK_PIXMAP_SLICE(i,slice) \
     (GTK_IMAGE_INSTANCE_DATA (i)->pixmaps[slice])
#define IMAGE_INSTANCE_GTK_PIXMAP_SLICES(i) \
     (GTK_IMAGE_INSTANCE_DATA (i)->pixmaps)
#define IMAGE_INSTANCE_GTK_MASK(i) (GdkPixmap*)(IMAGE_INSTANCE_PIXMAP_MASK (i))
#define IMAGE_INSTANCE_GTK_CURSOR(i) (GTK_IMAGE_INSTANCE_DATA (i)->cursor)
#define IMAGE_INSTANCE_GTK_COLORMAP(i) (GTK_IMAGE_INSTANCE_DATA (i)->colormap)
#define IMAGE_INSTANCE_GTK_PIXELS(i) (GTK_IMAGE_INSTANCE_DATA (i)->pixels)
#define IMAGE_INSTANCE_GTK_NPIXELS(i) (GTK_IMAGE_INSTANCE_DATA (i)->npixels)

#define XIMAGE_INSTANCE_GTK_PIXMAP(i) \
  IMAGE_INSTANCE_GTK_PIXMAP (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_PIXMAP_SLICE(i) \
  IMAGE_INSTANCE_GTK_PIXMAP_SLICE (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_PIXMAP_SLICES(i) \
  IMAGE_INSTANCE_GTK_PIXMAP_SLICES (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_MASK(i) \
  IMAGE_INSTANCE_GTK_MASK (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_CURSOR(i) \
  IMAGE_INSTANCE_GTK_CURSOR (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_PIXELS(i) \
  IMAGE_INSTANCE_GTK_PIXELS (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_NPIXELS(i) \
  IMAGE_INSTANCE_GTK_NPIXELS (XIMAGE_INSTANCE (i))

/* Subwindow / widget stuff */
#define GTK_SUBWINDOW_INSTANCE_DATA(i) ((struct gtk_subwindow_data *) (i)->data)

#define IMAGE_INSTANCE_GTK_SUBWINDOW_PARENT(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.sub.parent_window)
#define IMAGE_INSTANCE_GTK_CLIPWINDOW(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.sub.clip_window)
#define IMAGE_INSTANCE_GTK_WIDGET_XOFFSET(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.wid.x_offset)
#define IMAGE_INSTANCE_GTK_WIDGET_YOFFSET(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.wid.y_offset)
#define IMAGE_INSTANCE_GTK_WIDGET_LWID(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.wid.id)
#define IMAGE_INSTANCE_GTK_CLIPWIDGET(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.wid.clip_window)
#define IMAGE_INSTANCE_GTK_ALREADY_PUT(i) \
  (GTK_SUBWINDOW_INSTANCE_DATA (i)->data.wid.added_to_fixed)
#define IMAGE_INSTANCE_GTK_SUBWINDOW_ID(i) \
  ((GdkWindow *) & IMAGE_INSTANCE_SUBWINDOW_ID (i))
#define IMAGE_INSTANCE_GTK_WIDGET_ID(i) \
  ((GtkWidget *) & IMAGE_INSTANCE_SUBWINDOW_ID (i))

#define XIMAGE_INSTANCE_GTK_SUBWINDOW_PARENT(i) \
  IMAGE_INSTANCE_GTK_SUBWINDOW_PARENT (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_SUBWINDOW_DISPLAY(i) \
  IMAGE_INSTANCE_GTK_SUBWINDOW_DISPLAY (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_WIDGET_XOFFSET(i) \
  IMAGE_INSTANCE_GTK_WIDGET_XOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_WIDGET_YOFFSET(i) \
  IMAGE_INSTANCE_GTK_WIDGET_YOFFSET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_WIDGET_LWID(i) \
  IMAGE_INSTANCE_GTK_WIDGET_LWID (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_CLIPWIDGET(i) \
  IMAGE_INSTANCE_GTK_CLIPWIDGET (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_CLIPWINDOW(i) \
  IMAGE_INSTANCE_GTK_CLIPWINDOW (XIMAGE_INSTANCE (i))
#define XIMAGE_INSTANCE_GTK_WIDGET_ID(i) \
  IMAGE_INSTANCE_GTK_WIDGET_ID (XIMAGE_INSTANCE (i))

#define DOMAIN_GTK_WIDGET(domain) \
  ((IMAGE_INSTANCEP (domain) && \
  GTK_SUBWINDOW_INSTANCE_DATA (XIMAGE_INSTANCE (domain))) ? \
   XIMAGE_INSTANCE_GTK_WIDGET_ID (domain) : \
   FRAME_GTK_CONTAINER_WIDGET (f) (DOMAIN_XFRAME (domain)))

#endif /* HAVE_GTK */
#endif /* _XEMACS_GLYPHS_GTK_H_ */
