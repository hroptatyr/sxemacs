/* Tabs Widget for SXEmacs.
   Copyright (C) 1999 Edward A. Falk

This file is part of SXEmacs.

SXEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

SXEmacs is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: TabsP.h 1.8 */

/*
 * TabsP.h - Private definitions for Index Tabs widget
 */

#ifndef _TabsP_h
#define _TabsP_h

/***********************************************************************
 *
 * Tabs Widget Private Data
 *
 ***********************************************************************/

#include <X11/IntrinsicP.h>

#ifdef        NEED_MOTIF
#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#endif

#include "xlwtabs.h"

/* New fields for the Tabs widget class record */
typedef struct {
	XtPointer extension;
} TabsClassPart;

/* Full class record declaration */
typedef struct _TabsClassRec {
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
#ifdef	NEED_MOTIF
	XmManagerClassPart manager_class;
#endif
	TabsClassPart tabs_class;
} TabsClassRec;

extern TabsClassRec tabsClassRec;

/****************************************************************
 *
 * instance record declaration
 *
 ****************************************************************/

/* New fields for the Tabs widget record */
typedef struct {
	/* resources */
	XFontStruct *font;
	Dimension internalHeight, internalWidth;
	Widget topWidget;
	XtCallbackList callbacks;
	XtCallbackList popdownCallbacks;
	Boolean selectInsensitive;
	Boolean be_nice_to_cmap;
	int top_shadow_contrast;
	int bot_shadow_contrast;
	int insensitive_contrast;

	/* private state */
	Widget hilight;
	GC foregroundGC;
	GC backgroundGC;
	GC greyGC;
	GC topGC;
	GC botGC;
	Dimension tab_height;	/* height of tabs (all the same) */
	/* Note: includes top shadow only */
	Dimension tab_total;	/* total height of all tabs */
	Dimension child_width, child_height;	/* child size, including borders */
	Dimension max_cw, max_ch;	/* max child preferred size */
	Cardinal numRows;
	Cardinal realRows;	/* XEmacs addition */
	XtGeometryMask last_query_mode;
	Boolean needs_layout;
	Pixmap grey50;		/* TODO: cache this elsewhere */
} TabsPart;

typedef struct _TabsRec {
	CorePart core;
	CompositePart composite;
	ConstraintPart constraint;
#ifdef	NEED_MOTIF
	XmManagerPart manager;
#endif
	TabsPart tabs;
} TabsRec;

/****************************************************************
 *
 * constraint record declaration
 *
 ****************************************************************/

typedef struct _TabsConstraintsPart {
	/* resources */
	String label;
	Pixmap left_bitmap;
	Pixel foreground;
	Boolean resizable;

	/* private state */
	Pixel grey;
	Boolean greyAlloc;
	Boolean visible;	/* XEmacs change */
	Dimension width;	/* tab width */
	Position x, y;		/* tab base position */
	short row;		/* tab row */
	Position l_x, l_y;	/* label position */
	Position lbm_x, lbm_y;	/* bitmap position */
	unsigned int lbm_width, lbm_height, lbm_depth;
} TabsConstraintsPart;

typedef struct _TabsConstraintsRec {
#ifdef	NEED_MOTIF
	XmManagerConstraintPart manager;
#endif
	TabsConstraintsPart tabs;
} TabsConstraintsRec, *TabsConstraints;

#endif				/* _TabsP_h */
