/* Define general toolbar support.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
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

#ifndef INCLUDED_toolbar_h_
#define INCLUDED_toolbar_h_

#ifdef HAVE_TOOLBARS

#include "specifier.h"

#define FRAME_TOOLBAR_BUTTONS(frame, pos)	\
  ((frame)->toolbar_buttons[pos])
#define FRAME_CURRENT_TOOLBAR_SIZE(frame, pos)	\
  ((frame)->current_toolbar_size[pos])
#define DEVICE_SUPPORTS_TOOLBARS_P(d)		\
  HAS_DEVMETH_P (d, output_frame_toolbars)

struct toolbar_button {
	struct lcrecord_header header;

	Lisp_Object next;
	Lisp_Object frame;

	Lisp_Object up_glyph;
	Lisp_Object down_glyph;
	Lisp_Object disabled_glyph;

	Lisp_Object cap_up_glyph;
	Lisp_Object cap_down_glyph;
	Lisp_Object cap_disabled_glyph;

	Lisp_Object callback;
	Lisp_Object enabled_p;
	Lisp_Object help_string;

	char enabled;
	char down;
	char pushright;
	char blank;

	int x, y;
	int width, height;
	int dirty;
	/* is this button in a left or right toolbar? */
	int vertical;
	/* border_width when this button was laid out */
	int border_width;
};

DECLARE_LRECORD(toolbar_button, struct toolbar_button);
#define XTOOLBAR_BUTTON(x) XRECORD (x, toolbar_button, struct toolbar_button)
#define XSETTOOLBAR_BUTTON(x, p) XSETRECORD (x, p, toolbar_button)
#define TOOLBAR_BUTTONP(x) RECORDP (x, toolbar_button)
#define CHECK_TOOLBAR_BUTTON(x) CHECK_RECORD (x, toolbar_button)
#define CONCHECK_TOOLBAR_BUTTON(x) CONCHECK_RECORD (x, toolbar_button)

void get_toolbar_coords(struct frame *f, enum toolbar_pos pos, int *x,
			int *y, int *width, int *height, int *vert,
			int for_layout);
Lisp_Object toolbar_button_at_pixpos(struct frame *f, int x_coord, int y_coord);
DECLARE_SPECIFIER_TYPE(toolbar);
#define XTOOLBAR_SPECIFIER(x) XSPECIFIER_TYPE (x, toolbar)
#define XSETTOOLBAR_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, toolbar)
#define TOOLBAR_SPECIFIERP(x) SPECIFIER_TYPEP (x, toolbar)
#define CHECK_TOOLBAR_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, toolbar)
#define CONCHECK_TOOLBAR_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, toolbar)

#define MSWINDOWS_DEFAULT_TOOLBAR_HEIGHT	37
#define MSWINDOWS_DEFAULT_TOOLBAR_WIDTH		40
#define MSWINDOWS_DEFAULT_TOOLBAR_BORDER_WIDTH	0
#define DEFAULT_TOOLBAR_HEIGHT	37
#define DEFAULT_TOOLBAR_WIDTH		40
#define DEFAULT_TOOLBAR_BLANK_SIZE	8
#define DEFAULT_TOOLBAR_BORDER_WIDTH	0
#define MINIMUM_SHADOW_THICKNESS	1

extern Lisp_Object Vtoolbar_size[4];
extern Lisp_Object Vtoolbar_border_width[4];
void update_frame_toolbars(struct frame *f);
void update_frame_toolbars_geometry(struct frame *f);
void init_frame_toolbars(struct frame *f);
void init_device_toolbars(struct device *d);
void init_global_toolbars(struct device *d);
void free_frame_toolbars(struct frame *f);
Lisp_Object get_toolbar_button_glyph(struct window *w,
				     struct toolbar_button *tb);
void mark_frame_toolbar_buttons_dirty(struct frame *f, enum toolbar_pos pos);

#endif				/* HAVE_TOOLBARS */

#endif				/* INCLUDED_toolbar_h_ */
