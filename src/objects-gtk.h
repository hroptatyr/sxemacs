/* Gtk-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.

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

#ifndef _XEMACS_OBJECTS_GTK_H_
#define _XEMACS_OBJECTS_GTK_H_

#include "objects.h"

#ifdef HAVE_GTK

/*****************************************************************************
 Color-Instance
 ****************************************************************************/

struct gtk_color_instance_data
{
  GdkColor *color;
  char dealloc_on_gc;
};

#define GTK_COLOR_INSTANCE_DATA(c) ((struct gtk_color_instance_data *) (c)->data)
#define COLOR_INSTANCE_GTK_COLOR(c) (GTK_COLOR_INSTANCE_DATA (c)->color)
#define COLOR_INSTANCE_GTK_DEALLOC(c) (GTK_COLOR_INSTANCE_DATA (c)->dealloc_on_gc)

int allocate_nearest_color (GdkColormap *screen_colormap, GdkVisual *visual,
							GdkColor *color_def);
int gtk_parse_nearest_color (struct device *d, GdkColor *color, Bufbyte *name,
							 Bytecount len, Error_behavior errb);

/*****************************************************************************
 Font-Instance
 ****************************************************************************/

struct gtk_font_instance_data
{
  /* Gtk-specific information */
  Lisp_Object truename;
  GdkFont *font;
};

#define GTK_FONT_INSTANCE_DATA(f) ((struct gtk_font_instance_data *) (f)->data)
#define FONT_INSTANCE_GTK_FONT(f) (GTK_FONT_INSTANCE_DATA (f)->font)
#define FONT_INSTANCE_GTK_TRUENAME(f) (GTK_FONT_INSTANCE_DATA (f)->truename)

#endif /* HAVE_GTK */
#endif /* _XEMACS_OBJECTS_GTK_H_ */
