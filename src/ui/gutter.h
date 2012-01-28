/* Define general gutter support.
   Copyright (C) 1999 Andy Piper.

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

#ifndef INCLUDED_gutter_h_
#define INCLUDED_gutter_h_

#include "specifier.h"

#define DEVICE_SUPPORTS_GUTTERS_P(d) HAS_DEVMETH_P (d, output_frame_gutters)

DECLARE_SPECIFIER_TYPE(gutter);
#define XGUTTER_SPECIFIER(x) XSPECIFIER_TYPE (x, gutter)
#define XSETGUTTER_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, gutter)
#define GUTTER_SPECIFIERP(x) SPECIFIER_TYPEP (x, gutter)
#define CHECK_GUTTER_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, gutter)
#define CONCHECK_GUTTER_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, gutter)

#define DEFAULT_GUTTER_WIDTH		40
#define DEFAULT_GUTTER_BORDER_WIDTH	2

enum gutter_pos {
	TOP_GUTTER = 0,
	BOTTOM_GUTTER = 1,
	LEFT_GUTTER = 2,
	RIGHT_GUTTER = 3
};

/* Iterate over all possible gutter positions */
#define GUTTER_POS_LOOP(var) \
for (var = (enum gutter_pos) 0; var < 4; var = (enum gutter_pos) (var + 1))

extern Lisp_Object Qgutter;

extern Lisp_Object Vgutter_size[4];
extern Lisp_Object Vgutter_border_width[4];
void update_frame_gutters(struct frame *f);
void update_frame_gutter_geometry(struct frame *f);
void mark_gutters(struct frame *f);
void init_frame_gutters(struct frame *f);
void init_device_gutters(struct device *d);
void init_global_gutters(struct device *d);
void free_frame_gutters(struct frame *f);
void redraw_exposed_gutters(struct frame *f, int x, int y, int width,
			    int height);
void reset_gutter_display_lines(struct frame *f);
void gutter_extent_signal_changed_region_maybe(Lisp_Object obj,
					       Bufpos start, Bufpos end);
int display_boxes_in_gutter_p(struct frame *f, struct display_box *db,
			      struct display_glyph_area *dga);

#define WINDOW_GUTTER_BORDER_WIDTH(w, pos) \
  (INTP ((w)->gutter_border_width[pos]) ? XINT ((w)->gutter_border_width[pos]) : 0)
#define WINDOW_GUTTER_SIZE(w, pos) \
  (INTP ((w)->gutter_size[pos]) ? XINT ((w)->gutter_size[pos]) : 0)
#define WINDOW_GUTTER_SIZE_INTERNAL(w, pos) \
  (INTP ((w)->real_gutter_size[pos]) ? XINT ((w)->real_gutter_size[pos]) : 0)
#define WINDOW_GUTTER_VISIBLE(w, pos) \
  ((w)->gutter_visible_p[pos])
#define WINDOW_GUTTER(w, pos) \
  ((w)->real_gutter[pos])
#define RAW_WINDOW_GUTTER(w, pos) \
  ((w)->gutter[pos])

#define WINDOW_REAL_GUTTER_SIZE(w, pos)	\
  (!NILP (WINDOW_GUTTER_VISIBLE (w, pos))		\
   ? WINDOW_GUTTER_SIZE_INTERNAL (w, pos)	\
   : 0)
#define WINDOW_REAL_GUTTER_VISIBLE(f, pos)	\
   (WINDOW_REAL_GUTTER_SIZE (f, pos) > 0)
#define WINDOW_REAL_GUTTER_BORDER_WIDTH(f, pos)	\
  ((!NILP (WINDOW_GUTTER_VISIBLE (f, pos))		\
   && WINDOW_GUTTER_SIZE_INTERNAL (f,pos) > 0)	\
   ? WINDOW_GUTTER_BORDER_WIDTH (f, pos)	\
   : 0)
#define WINDOW_REAL_GUTTER_BOUNDS(f, pos)	\
   (WINDOW_REAL_GUTTER_SIZE (f,pos) +		\
    2 * WINDOW_REAL_GUTTER_BORDER_WIDTH (f,pos))

/* these macros predicate size on position and type of window */
#define WINDOW_REAL_TOP_GUTTER_BOUNDS(w)	\
   WINDOW_REAL_GUTTER_BOUNDS (w,TOP_GUTTER)
#define WINDOW_REAL_BOTTOM_GUTTER_BOUNDS(w)	\
   WINDOW_REAL_GUTTER_BOUNDS (w,BOTTOM_GUTTER)
#define WINDOW_REAL_LEFT_GUTTER_BOUNDS(w)	\
   WINDOW_REAL_GUTTER_BOUNDS (w,LEFT_GUTTER)
#define WINDOW_REAL_RIGHT_GUTTER_BOUNDS(w)	\
   WINDOW_REAL_GUTTER_BOUNDS (w,RIGHT_GUTTER)

#define FRAME_GUTTER_VISIBLE(f, pos) \
   WINDOW_REAL_GUTTER_VISIBLE (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), pos)
#define FRAME_GUTTER_SIZE(f, pos) \
   WINDOW_REAL_GUTTER_SIZE (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), pos)
#define FRAME_GUTTER_BOUNDS(f, pos) \
   WINDOW_REAL_GUTTER_BOUNDS (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), pos)
#define FRAME_GUTTER_BORDER_WIDTH(f, pos) \
   WINDOW_REAL_GUTTER_BORDER_WIDTH (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), pos)

#define FRAME_GUTTER(f, pos) \
WINDOW_GUTTER (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), pos)

/* these macros predicate size on position and type of window */
#define FRAME_TOP_GUTTER_BOUNDS(f) \
   WINDOW_REAL_GUTTER_BOUNDS (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), TOP_GUTTER)
#define FRAME_BOTTOM_GUTTER_BOUNDS(f) \
   WINDOW_REAL_GUTTER_BOUNDS (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), BOTTOM_GUTTER)
#define FRAME_LEFT_GUTTER_BOUNDS(f) \
   WINDOW_REAL_GUTTER_BOUNDS (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), LEFT_GUTTER)
#define FRAME_RIGHT_GUTTER_BOUNDS(f) \
   WINDOW_REAL_GUTTER_BOUNDS (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f)), RIGHT_GUTTER)

#endif				/* INCLUDED_gutter_h_ */
