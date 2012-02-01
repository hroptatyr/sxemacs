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

#ifndef INCLUDED_xlwscrollbar_h_
#define INCLUDED_xlwscrollbar_h_

#include <X11/Core.h>

/* Motif-compatible resource names */
#ifndef _XmStrDefs_h_

#define XmNbackground		"background"
#define XmNbottomShadowColor	"bottomShadowColor"
#define XmNbottomShadowPixmap	"bottomShadowPixmap"
#define XmNforeground		"foreground"
#define XmNincrement		"increment"
#define XmNinitialDelay		"initialDelay"
#define XmNmaximum		"maximum"
#define XmNminimum		"minimum"
#define XmNpageIncrement	"pageIncrement"
#define XmNrepeatDelay		"repeatDelay"
#define XmNshadowThickness	"shadowThickness"
#define XmNborderWidth		"borderWidth"
#define XmNshowArrows		"showArrows"
#define XmNsliderSize		"sliderSize"
#define XmNtopShadowColor	"topShadowColor"
#define XmNtopShadowPixmap	"topShadowPixmap"
#define XmNtroughColor		"troughColor"
#define XmNvalue		"value"
#define XmNvalueChangedCallback	"valueChangedCallback"
#define XmNincrementCallback	"incrementCallback"
#define XmNdecrementCallback	"decrementCallback"
#define XmNpageIncrementCallback "pageIncrementCallback"
#define XmNpageDecrementCallback "pageDecrementCallback"
#define XmNtoTopCallback	"toTopCallback"
#define XmNtoBottomCallback	"toBottomCallback"
#define XmNdragCallback		"dragCallback"
#define XmNorientation		"orientation"

#define XmCBackground		"Background"
#define XmCBottomShadowColor	"BottomShadowColor"
#define XmCBottomShadowPixmap	"BottomShadowPixmap"
#define XmCTopShadowPixmap	"TopShadowPixmap"
#define XmCForeground		"Foreground"
#define XmCIncrement		"Increment"
#define XmCInitialDelay		"InitialDelay"
#define XmCMaximum		"Maximum"
#define XmCMinimum		"Minimum"
#define XmCPageIncrement	"PageIncrement"
#define XmCRepeatDelay		"RepeatDelay"
#define XmCShadowThickness	"ShadowThickness"
#define XmCBorderWidth		"BorderWidth"
#define XmCShowArrows		"ShowArrows"
#define XmCSliderSize		"SliderSize"
#define XmCTopShadowColor	"TopShadowColor"
#define XmCTroughColor		"TroughColor"
#define XmCValue		"Value"
#define XmCValueChangedCallback	"ValueChangedCallback"
#define XmCIncrementCallback	"IncrementCallback"
#define XmCDecrementCallback	"DecrementCallback"
#define XmCPageIncrementCallback "PageIncrementCallback"
#define XmCPageDecrementCallback "PageDecrementCallback"
#define XmCToTopCallback	"ToTopCallback"
#define XmCToBottomCallback	"ToBottomCallback"
#define XmCDragCallback		"DragCallback"
#define XmCOrientation		"Orientation"

#endif				/* _XmStrDefs_h_ */

/* New resources that Motif does not have.
   Maybe we should use a different prefix. */

/* "knob" is obsolete; use "slider" instead. */
#define XmNknobStyle		"knobStyle"
#define XmCKnobStyle		"KnobStyle"

#define XmNsliderStyle		"sliderStyle"
#define XmCSliderStyle		"SliderStyle"

#define XmNarrowPosition	"arrowPosition"
#define XmCArrowPosition	"ArrowPosition"

#ifndef _Xm_h

enum {
	XmCR_NONE,
	XmCR_VALUE_CHANGED = 2,
	XmCR_INCREMENT,
	XmCR_DECREMENT,
	XmCR_PAGE_INCREMENT,
	XmCR_PAGE_DECREMENT,
	XmCR_TO_TOP,
	XmCR_TO_BOTTOM,
	XmCR_DRAG
};

enum {
	XmNO_ORIENTATION,
	XmVERTICAL,
	XmHORIZONTAL
};

#endif				/* ! _Xm_h */

extern WidgetClass xlwScrollBarWidgetClass;

typedef struct _XlwScrollBarClassRec *XlwScrollBarWidgetClass;
typedef struct _XlwScrollBarRec *XlwScrollBarWidget;

typedef struct {
	int reason;
	XEvent *event;
	int value;
	int pixel;
} XlwScrollBarCallbackStruct;

void XlwScrollBarGetValues(Widget widget, int *value, int *sliderSize,
			   int *increment, int *pageIncrement);

void XlwScrollBarSetValues(Widget widget, int value, int sliderSize,
			   int increment, int pageIncrement, Boolean notify);

#endif				/* INCLUDED_xlwscrollbar_h_ */
