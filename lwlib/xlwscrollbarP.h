/* Implements a lightweight scrollbar widget.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

The Lucid Widget Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Created by Douglas Keller <dkeller@vnet.ibm.com> */

#ifndef _XlwScrollBarP_h
#define _XlwScrollBarP_h

/*
** Widget class
*/
typedef struct {
	int dummy_field;	/* keep compiler happy */
} XlwScrollBarClassPart;

typedef struct _XlwScrollbarClassRec {
	CoreClassPart core_class;
	XlwScrollBarClassPart scrollbar_class;
} XlwScrollBarClassRec;

enum XlwScrollbarArm {
	ARM_NONE,
	ARM_SLIDER,
	ARM_UP,
	ARM_DOWN,
	ARM_PAGEUP,
	ARM_PAGEDOWN
};

enum XlwScrollbarForcedScroll {
	FORCED_SCROLL_NONE,
	FORCED_SCROLL_DOWNRIGHT,
	FORCED_SCROLL_UPLEFT
};

/*
** Widget instance
*/
typedef struct {
	/* resources */
	XtCallbackList valueChangedCBL;
	XtCallbackList incrementCBL;
	XtCallbackList decrementCBL;
	XtCallbackList pageIncrementCBL;
	XtCallbackList pageDecrementCBL;
	XtCallbackList toTopCBL;
	XtCallbackList toBottomCBL;
	XtCallbackList dragCBL;

	Pixel foreground;

	Pixel topShadowColor;
	Pixel bottomShadowColor;

	Pixel troughColor;

	Pixel armColor;
	Pixel armTopShadowColor;
	Pixel armBottomShadowColor;

	Pixmap topShadowPixmap;
	Pixmap bottomShadowPixmap;

	int shadowThickness;

	Boolean showArrows;

	int minimum;
	int maximum;
	int sliderSize;
	int value;
	int pageIncrement;
	int increment;

	int initialDelay;
	int repeatDelay;

	unsigned char orientation;

	char *sliderStyle;
	char *knobStyle;
	char *arrowPosition;

	/* private */
	Pixmap grayPixmap;

	GC backgroundGC;
	GC topShadowGC;
	GC bottomShadowGC;

	int above, ss, below;
	int lastY;

	enum XlwScrollbarArm armed;

	enum XlwScrollbarForcedScroll forced_scroll;

	int savedValue;

	Boolean fullRedrawNext;

	Boolean timerActive;
	XtIntervalId timerId;

} XlwScrollBarPart;

typedef struct _XlwScrollBarRec {
	CorePart core;
	XlwScrollBarPart sb;
} XlwScrollBarRec;

#endif				/* _XlwScrollBarP_h */
