/* Checkbox Widget for SXEmacs.
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
 * CheckboxP.h - Private definitions for Checkbox widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   June 30, 1997
 */

#ifndef _XawCheckboxP_h
#define _XawCheckboxP_h

#include "xlwcheckbox.h"
#include "xlwradioP.h"

/************************************
 *
 *  Class structure
 *
 ***********************************/

   /* New fields for the Checkbox widget class record */
typedef struct _CheckboxClass {
	XtPointer extension;
} CheckboxClassPart;

   /* Full class record declaration */
typedef struct _CheckboxClassRec {
	CoreClassPart core_class;
	SimpleClassPart simple_class;
#ifdef	_ThreeDP_h
	ThreeDClassPart threeD_class;
#endif
	LabelClassPart label_class;
	CommandClassPart command_class;
	ToggleClassPart toggle_class;
	RadioClassPart radio_class;
	CheckboxClassPart checkbox_class;
} CheckboxClassRec;

extern CheckboxClassRec checkboxClassRec;

/***************************************
 *
 *  Instance (widget) structure
 *
 **************************************/

    /* New fields for the Checkbox widget record */
typedef struct {
	/* resources */
	Boolean tristate;

	/* private data */
	Boolean pressed;
	Pixmap checkmark;	/* TODO: share these via xmu? */
	GC checkmark_GC;
	XtPointer extension;
} CheckboxPart;

   /* Full widget declaration */
typedef struct _CheckboxRec {
	CorePart core;
	SimplePart simple;
#ifdef	_ThreeDP_h
	ThreeDPart threeD;
#endif
	LabelPart label;
	CommandPart command;
	TogglePart toggle;
	RadioPart radio;
	CheckboxPart checkbox;
} CheckboxRec;

#endif				/* _XawCheckboxP_h */
