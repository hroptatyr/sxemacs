/* Define X-specific scrollbar instance.
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

#ifndef INCLUDED_scrollbar_x_h_
#define INCLUDED_scrollbar_x_h_

#if defined (HAVE_X_WINDOWS) && defined (HAVE_SCROLLBARS)

#include "ui/scrollbar.h"

struct x_scrollbar_data {
	/* Unique scrollbar identifier and name. */
	unsigned int id;
	char *name;

	/* Is set if we have already set the backing_store attribute correctly */
	char backing_store_initialized;

	/* Positioning and sizing information for scrollbar and slider. */
	scrollbar_values pos_data;

	/* Pointer to the scrollbar widget this structure describes. */
	Widget widget;

#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID) || \
    defined (LWLIB_SCROLLBARS_ATHENA3D)
	/* Recorded starting position for Motif-like scrollbar drags. */
	int vdrag_orig_value;
	Bufpos vdrag_orig_window_start;
#endif
};

#define SCROLLBAR_X_DATA(i) ((struct x_scrollbar_data *) ((i)->scrollbar_data))

#define SCROLLBAR_X_ID(i) (SCROLLBAR_X_DATA (i)->id)
#define SCROLLBAR_X_NAME(i) (SCROLLBAR_X_DATA (i)->name)
#define SCROLLBAR_X_BACKING_STORE_INITIALIZED(i) \
  (SCROLLBAR_X_DATA (i)->backing_store_initialized)
#define SCROLLBAR_X_POS_DATA(i) (SCROLLBAR_X_DATA (i)->pos_data)
#define SCROLLBAR_X_WIDGET(i) (SCROLLBAR_X_DATA (i)->widget)

#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID) || \
    defined (LWLIB_SCROLLBARS_ATHENA3D)
#define SCROLLBAR_X_VDRAG_ORIG_VALUE(i) \
  (SCROLLBAR_X_DATA (i)->vdrag_orig_value)
#define SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(i) \
  (SCROLLBAR_X_DATA (i)->vdrag_orig_window_start)
#endif

void x_update_frame_scrollbars(struct frame *f);
void x_set_scrollbar_pointer(struct frame *f, Lisp_Object cursor);

#endif				/* HAVE_X_WINDOWS and HAVE_SCROLLBARS */

#endif				/* INCLUDED_scrollbar_x_h_ */
