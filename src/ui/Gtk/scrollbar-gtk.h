/* Define Gtk-specific scrollbar instance.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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

#ifndef _XEMACS_SCROLLBAR_GTK_H_
#define _XEMACS_SCROLLBAR_GTK_H_

#if defined (HAVE_GTK) && defined (HAVE_SCROLLBARS)

#include "ui/scrollbar.h"

typedef struct _scrollbar_values {
	int line_increment;
	int page_increment;

	int minimum;
	int maximum;

	int slider_size;
	int slider_position;

	int scrollbar_width, scrollbar_height;
	int scrollbar_x, scrollbar_y;
} scrollbar_values;

struct gtk_scrollbar_data {
	/* Unique scrollbar identifier and name. */
	unsigned int id;

	/* Is set if we have already set the backing_store attribute correctly */
	char backing_store_initialized;

	/* Positioning and sizing information for scrollbar and slider. */
	scrollbar_values pos_data;

	/* Pointer to the scrollbar widget this structure describes. */
	GtkWidget *widget;

	gfloat last_value;

	/* Recorded starting position for Motif-like scrollbar drags. */
	int vdrag_orig_value;
	Bufpos vdrag_orig_window_start;
};

#define SCROLLBAR_GTK_DATA(i) ((struct gtk_scrollbar_data *) ((i)->scrollbar_data))

#define SCROLLBAR_GTK_ID(i) (SCROLLBAR_GTK_DATA (i)->id)
#define SCROLLBAR_GTK_BACKING_STORE_INITIALIZED(i) \
  (SCROLLBAR_GTK_DATA (i)->backing_store_initialized)
#define SCROLLBAR_GTK_POS_DATA(i) (SCROLLBAR_GTK_DATA (i)->pos_data)
#define SCROLLBAR_GTK_WIDGET(i) (SCROLLBAR_GTK_DATA (i)->widget)
#define SCROLLBAR_GTK_LAST_VALUE(i) SCROLLBAR_GTK_DATA (i)->last_value

#define SCROLLBAR_GTK_VDRAG_ORIG_VALUE(i) \
  (SCROLLBAR_GTK_DATA (i)->vdrag_orig_value)
#define SCROLLBAR_GTK_VDRAG_ORIG_WINDOW_START(i) \
  (SCROLLBAR_GTK_DATA (i)->vdrag_orig_window_start)

void gtk_update_frame_scrollbars(struct frame *f);
void gtk_set_scrollbar_pointer(struct frame *f, Lisp_Object cursor);

#endif				/* HAVE_GDK and HAVE_SCROLLBARS */
#endif				/* _XEMACS_SCROLLBAR_GTK_H_ */
