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

/* Synched up with: Radio.h 1.1 */

/*
 * Radio.h - Radio button widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   June 30, 1997
 */

#ifndef _XawRadio_h
#define _XawRadio_h

/***********************************************************************
 *
 * Radio Widget
 *
 * The Radio widget is identical to the Toggle widget in behavior but
 * not in appearance.  The Radio widget looks like a small diamond
 * shaped button to the left of the label.
 *
 ***********************************************************************/

#include ATHENA_Toggle_h_

/* Resources:

 Name			Class		RepType		Default Value
 ----			-----		-------		-------------
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

extern WidgetClass radioWidgetClass;

typedef struct _RadioClassRec *RadioWidgetClass;
typedef struct _RadioRec *RadioWidget;

/************************************************************
 *
 * Public Functions
 *
 ************************************************************/

#endif				/* _XawRadio_h */
