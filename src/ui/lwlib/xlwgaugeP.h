/* Gauge Widget for SXEmacs.
   Copyright (C) 1999 Edward A. Falk

This file is part of SXEmacs.

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

/*
 * GaugeP.h - Gauge widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   July 9, 1997
 */

#ifndef _XawGaugeP_h
#define _XawGaugeP_h

/***********************************************************************
 *
 * Gauge Widget Private Data
 *
 * Gauge has little in common with the label widget, but can make use
 * of some label resources, so is subclassed from label.
 *
 ***********************************************************************/

#include "xlwgauge.h"
#include ATHENA_LabelP_h_

/* New fields for the Gauge widget class record */

typedef struct {
	XtPointer extension;
} GaugeClassPart;

/* Full class record declaration */
typedef struct _GaugeClassRec {
	CoreClassPart core_class;
	SimpleClassPart simple_class;
#ifdef	_ThreeDP_h
	ThreeDClassPart threeD_class;
#endif
	LabelClassPart label_class;
	GaugeClassPart gauge_class;
} GaugeClassRec;

extern GaugeClassRec gaugeClassRec;

/* New fields for the Gauge widget record */
typedef struct {
	/* resources */
	int value, v0, v1;
	int ntics, nlabels;
	String *labels;
	XtOrientation orientation;
	Boolean autoScaleUp;	/* scales automatically */
	Boolean autoScaleDown;	/* scales automatically */
	int update;		/* update interval */
	XtCallbackList getValue;	/* proc to call to fetch a point */

	/* private state */
	Dimension gmargin;	/* edges <-> gauge */
	Dimension tmargin;	/* top (left) edge <-> tic marks */
	Dimension lmargin;	/* tic marks <-> labels */
	Dimension margin0;	/* left/bottom margin */
	Dimension margin1;	/* right/top margin */
	XtIntervalId intervalId;
	Atom selected;
	String selstr;		/* selection string, if any */
	GC inverse_GC;
} GaugePart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _GaugeRec {
	CorePart core;
	SimplePart simple;
#ifdef	_ThreeDP_h
	ThreeDPart threeD;
#endif
	LabelPart label;
	GaugePart gauge;
} GaugeRec;

#endif				/* _XawGaugeP_h */
