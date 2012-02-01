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

/* Synched up with: Checkbox.h 1.1 */

/*
 * Checkbox.h - Checkbox widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   June 30, 1997
 */

#ifndef _XawCheckbox_h
#define _XawCheckbox_h

/***********************************************************************
 *
 * Checkbox Widget
 *
 * The Checkbox widget is identical to the Radio widget in behavior but
 * not in appearance.  The Checkbox widget looks like a small diamond
 * shaped button to the left of the label.
 *
 ***********************************************************************/

#include "xlwradio.h"

/* Resources:

 Name			Class		RepType		Default Value
 ----			-----		-------		-------------
 tristate		Tristate	Boolean		FALSE

 radioGroup		RadioGroup	Widget		NULL
 radioData		RadioData	Pointer		(XPointer) Widget
 state			State		Boolean		Off
 background		Background	Pixel		XtDefaultBackground
 bitmap			Pixmap		Pixmap		None
 border			BorderColor	Pixel		XtDefaultForeground
 borderWidth		BorderWidth	Dimension	1
 callback		Callback	Pointer		NULL
 cursor			Cursor		Cursor		None
 destroyCallback	Callback	Pointer		NULL
 font			Font		XFontStructx*	XtDefaultFont
 foreground		Foreground	Pixel		XtDefaultForeground
 height			Height		Dimension	text height
 highlightThickness	Thickness	Dimension	2
 insensitiveBorder	sensitive	Pixmap		Gray
 internalHeight		Height		Dimension	2
 internalWidth		Width		Dimension	4
 justify		Justify		XtJustify	XtJustifyCenter
 label			Label		String		NULL
 mappedWhenManaged	MappedWhenManaged Boolean	True
 resize			Resize		Boolean		True
 sensitive		Sensitive	Boolean		True
 width			Width		Dimension	text width
 x			Position	Position	0
 y			Position	Position	0

*/

/*
 * These should be in StringDefs.h but aren't so we will define
 * them here if they are needed.
 */

#define	XtCTristate	"Tristate"

#define	XtNtristate	"tristate"

extern WidgetClass checkboxWidgetClass;

typedef struct _CheckboxClassRec *CheckboxWidgetClass;
typedef struct _CheckboxRec *CheckboxWidget;

/************************************************************
 *
 * Public Functions
 *
 ************************************************************/

#endif				/* _XawCheckbox_h */
