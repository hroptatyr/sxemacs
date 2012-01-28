/* Radio Widget for SXEmacs.
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
 * RadioP.h - Private definitions for Radio widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   June 30, 1997
 *
 */

#ifndef _XawRadioP_h
#define _XawRadioP_h

#include "xlwradio.h"
#include ATHENA_ToggleP_h_

/***********************************************************************
 *
 * Radio Widget Private Data
 *
 ***********************************************************************/

/* Already in Xaw/ToggleP.h, and not used by us.
  #define streq(a, b) ( strcmp((a), (b)) == 0 )
*/

typedef void (*XawDiamondProc) (Widget);

void RadioSet(Widget w, XEvent * event, String * params,	/* unused */
	      Cardinal * num_params);	/* unused */

void RadioUnset(Widget w, XEvent * event, String * params,	/* unused */
		Cardinal * num_params);	/* unused */

/************************************
 *
 *  Class structure
 *
 ***********************************/

   /* New fields for the Radio widget class record */
typedef struct _RadioClass {
	Dimension dsize;	/* diamond size */
	XawDiamondProc drawDiamond;
	/* TODO: 3-d and xaw-xpm features? */
	XtPointer extension;
} RadioClassPart;

#define	XtInheritDrawDiamond	((XawDiamondProc)_XtInherit)

   /* Full class record declaration */
typedef struct _RadioClassRec {
	CoreClassPart core_class;
	SimpleClassPart simple_class;
#ifdef	_ThreeDP_h
	ThreeDClassPart threeD_class;
#endif
	LabelClassPart label_class;
	CommandClassPart command_class;
	ToggleClassPart toggle_class;
	RadioClassPart radio_class;
} RadioClassRec;

extern RadioClassRec radioClassRec;

/***************************************
 *
 *  Instance (widget) structure
 *
 **************************************/

    /* New fields for the Radio widget record */
typedef struct {
	/* resources */
	/* TODO: 3-d and xaw-xpm features? */

	/* private data */
	XtPointer extension;
} RadioPart;

   /* Full widget declaration */
typedef struct _RadioRec {
	CorePart core;
	SimplePart simple;
#ifdef	_ThreeDP_h
	ThreeDPart threeD;
#endif
	LabelPart label;
	CommandPart command;
	TogglePart toggle;
	RadioPart radio;
} RadioRec;

#endif				/* _XawRadioP_h */
