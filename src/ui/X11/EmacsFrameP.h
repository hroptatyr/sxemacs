/* Private header for the Emacs frame widget.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

#ifndef INCLUDED_EmacsFrameP_h_
#define INCLUDED_EmacsFrameP_h_

#include "xintrinsicp.h"
#include <X11/CoreP.h>
#ifdef LWLIB_USES_MOTIF
#include "ui/X11/xmprimitivep.h"
#endif
#include "EmacsFrame.h"

typedef struct {

#ifdef LWLIB_USES_MOTIF
#if XmVERSION < 2
	/* It's easy to erroneously compile with Motif 1 headers, but link
	   with Motif 2 libraries.

	   For example, AIX stupidly provides Motif headers in
	   /usr/dt/include, but fails to provide the corresponding libraries
	   in /usr/dt/lib.

	   We actually try to survive such a version mismatch, since Motif 2
	   is _almost_ binary compatible with Motif 1.  Motif 2's
	   XmPrimitivePart has some trailing data members which overlay the
	   start of this struct.  We create dummy members to make space for
	   XmPrimitivePart's added members.  These must, of course, be at
	   the beginning of this struct.

	   Delete this kludge when no one has Motif1 on their system anymore,
	   perhaps in 2010. */
	XtCallbackList Motif2_dummy_convert_callback;
	XtCallbackList Motif2_dummy_popup_handler_callback;
	long Motif2_dummy_layout_direction;
#endif
#endif

	struct frame *frame;	/* the *emacs* frame object */

	/* Resources that can't be done from lisp.
	 */
	char *geometry;		/* geometry spec of this frame */
	Boolean iconic;		/* whether this frame is iconic */

	/* The rest of this is crap and should be deleted.
	 */
	Boolean minibuffer;	/* 0: normal frames with minibuffers.
				 * 1: frames without minibuffers
				 * 2: minibuffer only. */
	Boolean unsplittable;	/* frame can only have one window */

	int internal_border_width;	/* internal borders */
	int scrollbar_width;	/* width of frame vertical sb's */
	int scrollbar_height;	/* height of frame horizontal sb's */
	int top_toolbar_height;	/* height of top toolbar */
	int bottom_toolbar_height;	/* height of bottom toolbar */
	int left_toolbar_width;	/* width of left toolbar */
	int right_toolbar_width;	/* width of right toolbar */
	int top_toolbar_border_width;	/* border width */
	int bottom_toolbar_border_width;	/* ... of bottom toolbar */
	int left_toolbar_border_width;	/* ... of left toolbar */
	int right_toolbar_border_width;	/* ... of right toolbar */
	Pixel top_toolbar_shadow_pixel;
	Pixel bottom_toolbar_shadow_pixel;
	Pixel background_toolbar_pixel;
	Pixel foreground_toolbar_pixel;
	Pixmap top_toolbar_shadow_pixmap;
	Pixmap bottom_toolbar_shadow_pixmap;
	Dimension toolbar_shadow_thickness;
	unsigned char scrollbar_placement;
	int interline;		/* skips between lines */

	XFontStruct *font;	/* font */
	Pixel foreground_pixel;	/* foreground */
	Pixel background_pixel;	/* background */

	Pixel cursor_color;	/* text cursor color */
	Boolean bar_cursor;	/* 1 if bar, 0 if block */

	Boolean visual_bell;	/* flash instead of beep */
	int bell_volume;	/* how loud is beep */

	Boolean menubar_p;	/* initially show a menubar? */
	Boolean initially_unmapped;	/* inhibit initial window mapping */
	Boolean use_backing_store;	/* backing store for menubar & ew? */

	Dimension preferred_width;	/* if non-zero, preferred size for */
	Dimension preferred_height;	/* QueryGeometry() */
	/* private state */

} EmacsFramePart;

typedef struct _EmacsFrameRec {	/* full instance record */
	CorePart core;
#ifdef LWLIB_USES_MOTIF
	XmPrimitivePart primitive;
#endif
	EmacsFramePart emacs_frame;
} EmacsFrameRec;

typedef struct {		/* new fields for EmacsFrame class */
	int dummy;
} EmacsFrameClassPart;

typedef struct _EmacsFrameClassRec {	/* full class record declaration */
	CoreClassPart core_class;
#ifdef LWLIB_USES_MOTIF
	XmPrimitiveClassPart primitive_class;
#endif
	EmacsFrameClassPart emacs_frame_class;
} EmacsFrameClassRec;

extern EmacsFrameClassRec emacsFrameClassRec;	/* class pointer */

#endif				/* INCLUDED_EmacsFrameP_h_ */
