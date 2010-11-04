/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* This file Mule-ized (more like Mule-verified) by Ben Wing, 7-10-00. */

#ifndef INCLUDED_objects_x_h_
#define INCLUDED_objects_x_h_

#include "ui/objects.h"

#ifdef HAVE_X_WINDOWS

/*****************************************************************************
 Color-Instance
 ****************************************************************************/

struct x_color_instance_data {
	XColor color;
	char dealloc_on_gc;
};

#define X_COLOR_INSTANCE_DATA(c) ((struct x_color_instance_data *) (c)->data)
#define COLOR_INSTANCE_X_COLOR(c) (X_COLOR_INSTANCE_DATA (c)->color)
#define COLOR_INSTANCE_X_DEALLOC(c) (X_COLOR_INSTANCE_DATA (c)->dealloc_on_gc)

int allocate_nearest_color(Display * display, Colormap screen_colormap,
			   Visual * visual, XColor * color_def);

/*****************************************************************************
 Font-Instance
 ****************************************************************************/

struct x_font_instance_data {
	/* X-specific information */
	Lisp_Object truename;
	XFontStruct *font;
};

#define X_FONT_INSTANCE_DATA(f) ((struct x_font_instance_data *) (f)->data)
#define FONT_INSTANCE_X_FONT(f) (X_FONT_INSTANCE_DATA (f)->font)
#define FONT_INSTANCE_X_TRUENAME(f) (X_FONT_INSTANCE_DATA (f)->truename)

#endif				/* HAVE_X_WINDOWS */

#endif				/* INCLUDED_objects_x_h_ */
