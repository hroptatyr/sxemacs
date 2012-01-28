/* Implements a lightweight scrollbar widget.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1997 Sun Microsystems, Inc.

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
/* Lots of hacking by Martin Buchholz */

/*
 * Athena-style scrollbar button bindings added on Sun Dec 24 22:03:57 1995
 * by Jonathan Stigelman <Stig@hackvan.com>...   Ho ho ho!
 *
 * To use them, put this resource in your .Xdefaults
 *
 * Emacs*XlwScrollBar.translations: #override \n\
 *   <Btn1Down>:     PageDownOrRight()	  \n\
 *   <Btn3Down>:     PageUpOrLeft()	\n\
 *   <Btn3Up>:	Release()
 *
 */

/*
 * Resources Supported:
 *     XmNforeground
 *     XmNbackground
 *     XmNtopShadowColor
 *     XmNtopShadowPixmap
 *     XmNbottomShadowColor
 *     XmNbottomShadowPixmap
 *     XmNtroughColor
 *     XmNshadowThickness
 *     XmNshowArrows
 *     XmNorientation
 *     XmNborderWidth
 *
 *     XmNminimum
 *     XmNmaximum
 *     XmNvalue
 *     XmNincrement
 *     XmNpageIncrement
 *
 *     XmNvalueChangedCallback
 *     XmNincrementCallback
 *     XmNdecrementCallback
 *     XmNpageIncrementCallback
 *     XmNpageDecrementCallback
 *     XmNtoTopCallback
 *     XmNtoBottomCallback
 *     XmNdragCallback
 *
 *     XmNsliderStyle    - values can be: "plain" or "dimple"
 *     XmNarrowPosition  - values can be: "opposite" or "same"
 *
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/bitmaps/gray>

#include <assert.h>

#include "xlwscrollbarP.h"
#include "xlwscrollbar.h"

#ifdef USE_DEBUG_MALLOC
#include <dmalloc.h>
#endif

#define DBUG(x)

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		  ? ((unsigned long) (x)) : ((unsigned long) (y)))

#define VERT(w) ((w)->sb.orientation == XmVERTICAL)

#define SS_MIN 8

typedef enum {
	BUTTON_NONE,
	BUTTON_SLIDER,
	BUTTON_UP_ARROW,
	BUTTON_DOWN_ARROW,
	BUTTON_TROUGH_ABOVE,
	BUTTON_TROUGH_BELOW
} button_where;

typedef enum {
	SLIDER_PLAIN,
	SLIDER_DIMPLE
} SliderStyle;

/*-------------------------- Resources ----------------------------------*/
#define offset(field) XtOffset(XlwScrollBarWidget, field)

static XtResource resources[] = {
	{XmNforeground, XmCForeground, XtRPixel, sizeof(Pixel),
	 offset(sb.foreground), XtRImmediate, (XtPointer) XtDefaultForeground}
	,

	{XmNtopShadowColor, XmCTopShadowColor, XtRPixel,
	 sizeof(Pixel), offset(sb.topShadowColor), XtRImmediate, (XtPointer) ~ 0}
	,
	{XmNbottomShadowColor, XmCBottomShadowColor, XtRPixel,
	 sizeof(Pixel), offset(sb.bottomShadowColor), XtRImmediate,
	 (XtPointer) ~ 0}
	,

	{XmNtopShadowPixmap, XmCTopShadowPixmap, XtRPixmap,
	 sizeof(Pixmap), offset(sb.topShadowPixmap), XtRImmediate,
	 (XtPointer) None}
	,
	{XmNbottomShadowPixmap, XmCBottomShadowPixmap,
	 XtRPixmap, sizeof(Pixmap), offset(sb.bottomShadowPixmap),
	 XtRImmediate, (XtPointer) None}
	,

	{XmNtroughColor, XmCTroughColor, XtRPixel, sizeof(Pixel),
	 offset(sb.troughColor), XtRImmediate, (XtPointer) ~ 0}
	,

	{XmNshadowThickness, XmCShadowThickness, XtRInt,
	 sizeof(int), offset(sb.shadowThickness), XtRImmediate, (XtPointer) 2},

	{XmNborderWidth, XmCBorderWidth, XtRDimension,
	 sizeof(Dimension), offset(core.border_width), XtRImmediate,
	 (XtPointer) 0}
	,

	{XmNshowArrows, XmCShowArrows, XtRBoolean,
	 sizeof(Boolean), offset(sb.showArrows), XtRImmediate, (XtPointer) True}
	,

	{XmNinitialDelay, XmCInitialDelay, XtRInt, sizeof(int),
	 offset(sb.initialDelay), XtRImmediate, (XtPointer) 250},
	{XmNrepeatDelay, XmCRepeatDelay, XtRInt, sizeof(int),
	 offset(sb.repeatDelay), XtRImmediate, (XtPointer) 50},

	{XmNorientation, XmCOrientation, XtROrientation,
	 sizeof(unsigned char), offset(sb.orientation), XtRImmediate,
	 (XtPointer) XmVERTICAL},

	{XmNminimum, XmCMinimum, XtRInt, sizeof(int),
	 offset(sb.minimum), XtRImmediate, (XtPointer) 0},
	{XmNmaximum, XmCMaximum, XtRInt, sizeof(int),
	 offset(sb.maximum), XtRImmediate, (XtPointer) 100},
	{XmNvalue, XmCValue, XtRInt, sizeof(int),
	 offset(sb.value), XtRImmediate, (XtPointer) 0},
	{XmNsliderSize, XmCSliderSize, XtRInt, sizeof(int),
	 offset(sb.sliderSize), XtRImmediate, (XtPointer) 10},
	{XmNincrement, XmCIncrement, XtRInt, sizeof(int),
	 offset(sb.increment), XtRImmediate, (XtPointer) 1},
	{XmNpageIncrement, XmCPageIncrement, XtRInt, sizeof(int),
	 offset(sb.pageIncrement), XtRImmediate, (XtPointer) 10},

	{XmNvalueChangedCallback, XmCValueChangedCallback,
	 XtRCallback, sizeof(XtPointer), offset(sb.valueChangedCBL),
	 XtRCallback, NULL}
	,
	{XmNincrementCallback, XmCIncrementCallback,
	 XtRCallback, sizeof(XtPointer), offset(sb.incrementCBL),
	 XtRCallback, NULL}
	,
	{XmNdecrementCallback, XmCDecrementCallback,
	 XtRCallback, sizeof(XtPointer), offset(sb.decrementCBL),
	 XtRCallback, NULL}
	,
	{XmNpageIncrementCallback, XmCPageIncrementCallback,
	 XtRCallback, sizeof(XtPointer), offset(sb.pageIncrementCBL),
	 XtRCallback, NULL}
	,
	{XmNpageDecrementCallback, XmCPageDecrementCallback,
	 XtRCallback, sizeof(XtPointer), offset(sb.pageDecrementCBL),
	 XtRCallback, NULL}
	,
	{XmNtoTopCallback, XmCToTopCallback, XtRCallback,
	 sizeof(XtPointer), offset(sb.toTopCBL), XtRCallback, NULL}
	,
	{XmNtoBottomCallback, XmCToBottomCallback, XtRCallback,
	 sizeof(XtPointer), offset(sb.toBottomCBL), XtRCallback, NULL}
	,
	{XmNdragCallback, XmCDragCallback, XtRCallback,
	 sizeof(XtPointer), offset(sb.dragCBL), XtRCallback, NULL}
	,

	/* "knob" is obsolete; use "slider" instead. */
	{XmNsliderStyle, XmCSliderStyle, XtRString, sizeof(char *),
	 offset(sb.sliderStyle), XtRImmediate, NULL},
	{XmNknobStyle, XmCKnobStyle, XtRString, sizeof(char *),
	 offset(sb.knobStyle), XtRImmediate, NULL},

	{XmNarrowPosition, XmCArrowPosition, XtRString, sizeof(char *),
	 offset(sb.arrowPosition), XtRImmediate, NULL},
};

/*-------------------------- Prototypes ---------------------------------*/

/* Actions */
typedef void Action(Widget w, XEvent * event, String * parms,
		    Cardinal * num_parms);
static Action Select, PageUpOrLeft, PageDownOrRight, Drag, Release, Jump, Abort;

/* Methods */
static void Initialize(Widget treq, Widget tnew, ArgList args,
		       Cardinal * num_args);
static Boolean SetValues(Widget current, Widget request, Widget nw,
			 ArgList args, Cardinal * num_args);
static void Destroy(Widget widget);
static void Redisplay(Widget widget, XEvent * event, Region region);
static void Resize(Widget widget);
static void Realize(Widget widget, XtValueMask * valuemask,
		    XSetWindowAttributes * attr);

/* Private */

/*-------------------------- Actions Table ------------------------------*/
static XtActionsRec actions[] = {
	{"Select", Select},
	{"PageDownOrRight", PageDownOrRight},
	{"PageUpOrLeft", PageUpOrLeft},
	{"Drag", Drag},
	{"Release", Release},
	{"Jump", Jump},
	{"Abort", Abort},
};

/*--------------------- Default Translation Table -----------------------*/
static char default_translations[] =
    "<Btn1Down>:    Select()\n"
    "<Btn1Motion>:  Drag()\n"
    "<Btn1Up>:      Release()\n"
    "<Btn2Down>:    Jump()\n"
    "<Btn2Motion>:  Drag()\n"
    "<Btn2Up>:      Release()\n" "<Key>Delete:   Abort()";

/*------------------- Class record initialization -----------------------*/
XlwScrollBarClassRec xlwScrollBarClassRec = {
	/* core_class fields */
	{
	 /* superclass          */ (WidgetClass) & coreClassRec,
	 /* class_name          */ "XlwScrollBar",
	 /* widget_size         */ sizeof(XlwScrollBarRec),
	 /* class_initialize    */ NULL,
	 /* class_part_init     */ NULL,
	 /* class_inited        */ False,
	 /* initialize          */ Initialize,
	 /* initialize_hook     */ NULL,
	 /* realize             */ Realize,
	 /* actions             */ actions,
	 /* num_actions         */ XtNumber(actions),
	 /* resources           */ resources,
	 /* num_resources       */ XtNumber(resources),
	 /* xrm_class           */ NULLQUARK,
	 /* compress_motion     */ True,
	 /* compress_exposure   */ XtExposeCompressMultiple,
	 /* compress_enterleave */ True,
	 /* visible_interest    */ False,
	 /* destroy             */ Destroy,
	 /* resize              */ Resize,
	 /* expose              */ Redisplay,
	 /* set_values          */ SetValues,
	 /* set_values_hook     */ NULL,
	 /* set_values_almost   */ XtInheritSetValuesAlmost,
	 /* get_values_hook     */ NULL,
	 /* accept_focus        */ NULL,
	 /* version             */ XtVersionDontCheck,
	 /* callback_private    */ NULL,
	 /* tm_table            */ default_translations,
	 /* query_geometry      */ NULL,
	 }
	,
	/* scrollbar_class fields */
	{
	 /* dummy_field         */ 0,
	 }
	,
};

WidgetClass xlwScrollBarWidgetClass = (WidgetClass) & xlwScrollBarClassRec;

/*-------------------------- Debug Functions ----------------------------*/

#ifdef SHOW_CLEAR
static void
myXClearArea(Display * dpy, Drawable d, int x, int y, int w, int h,
	     Boolean exp, XlwScrollBarWidget widget)
{
	XFillRectangle(dpy, d, widget->sb.topShadowGC, x, y, w, h);
	XSync(dpy, False);
	sleep(2);
	XClearArea(dpy, d, x, y, w, h, exp);
}

#define XClearArea(dpy,win,x,y,width,height,exp) myXClearArea(dpy,win,x,y,width,height,exp,w)
#endif

#ifdef CHECK_VALUES
static void check(XlwScrollBarWidget w)
{
	int height = widget_h(w);
	if (w->sb.showArrows)
		height -= (2 * arrow_h(w));

	if ((w->sb.above + w->sb.ss + w->sb.below > height) ||
	    (w->sb.value < w->sb.minimum) ||
	    (w->sb.value > w->sb.maximum - w->sb.sliderSize)) {
		printf("above=%d ss=%d below=%d height=%d\n",
		       w->sb.above, w->sb.ss, w->sb.below, height);
		printf("value=%d min=%d max=%d ss=%d max-ss=%d\n",
		       w->sb.value, w->sb.minimum, w->sb.maximum,
		       w->sb.sliderSize, w->sb.maximum - w->sb.sliderSize);
		abort();
	}
}

#  define CHECK(w) check(w)
#else
#  define CHECK(w)
#endif

/*-------------------------- Static functions ---------------------------*/

static void
call_callbacks(XlwScrollBarWidget w, int reason,
	       int value, int pixel, XEvent * event)
{
	XlwScrollBarCallbackStruct cbs;
	Boolean called_anything;

	cbs.reason = reason;
	cbs.event = event;
	cbs.value = value;
	cbs.pixel = pixel;

	called_anything = False;

	switch (reason) {
	case XmCR_VALUE_CHANGED:
		XtCallCallbackList((Widget) w, w->sb.valueChangedCBL, &cbs);
		called_anything = True;
		break;
	case XmCR_INCREMENT:
		if (w->sb.incrementCBL) {
			XtCallCallbackList((Widget) w, w->sb.incrementCBL,
					   &cbs);
			called_anything = True;
		}
		break;
	case XmCR_DECREMENT:
		if (w->sb.decrementCBL) {
			XtCallCallbackList((Widget) w, w->sb.decrementCBL,
					   &cbs);
			called_anything = True;
		}
		break;
	case XmCR_PAGE_INCREMENT:
		if (w->sb.incrementCBL) {
			XtCallCallbackList((Widget) w, w->sb.pageIncrementCBL,
					   &cbs);
			called_anything = True;
		}
		break;
	case XmCR_PAGE_DECREMENT:
		if (w->sb.decrementCBL) {
			XtCallCallbackList((Widget) w, w->sb.pageDecrementCBL,
					   &cbs);
			called_anything = True;
		}
		break;
	case XmCR_TO_TOP:
		if (w->sb.toTopCBL) {
			XtCallCallbackList((Widget) w, w->sb.toTopCBL, &cbs);
			called_anything = True;
		}
		break;
	case XmCR_TO_BOTTOM:
		if (w->sb.toBottomCBL) {
			XtCallCallbackList((Widget) w, w->sb.toBottomCBL, &cbs);
			called_anything = True;
		}
		break;
	case XmCR_DRAG:
		if (w->sb.dragCBL) {
			XtCallCallbackList((Widget) w, w->sb.dragCBL, &cbs);
		}
		called_anything = True;	/* Special Case */
		break;
	default:
		/* Since called_anything will definitely be False
		   here, the fall through if will do the proper
		   thing.. So, nothing to do here
		*/
		break;
	}

	if (!called_anything) {
		cbs.reason = XmCR_VALUE_CHANGED;
		XtCallCallbackList((Widget) w, w->sb.valueChangedCBL, &cbs);
	}
}

/* Widget sizes minus the shadow and highlight area */

static int widget_x(XlwScrollBarWidget w)
{
	return w->sb.shadowThickness;
}

static int widget_y(XlwScrollBarWidget w)
{
	return w->sb.shadowThickness;
}

static int widget_w(XlwScrollBarWidget w)
{
	int x = w->sb.shadowThickness;
	int width = (VERT(w) ? w->core.width : w->core.height) - (2 * x);
	return width > 1 ? width : 1;
}

static int widget_h(XlwScrollBarWidget w)
{
	int y = w->sb.shadowThickness;
	int height = (VERT(w) ? w->core.height : w->core.width) - (2 * y);

	return height > 1 ? height : 1;
}

static int arrow_h(XlwScrollBarWidget w)
{
	int width = widget_w(w);
	int minimum_size = ((widget_h(w) - SS_MIN) / 2) - 1;
	return minimum_size < width ? minimum_size : width;
}

static int event_x(XlwScrollBarWidget w, XEvent * event)
{
	return VERT(w) ? event->xbutton.x : event->xbutton.y;
}

static int event_y(XlwScrollBarWidget w, XEvent * event)
{
	return VERT(w) ? event->xbutton.y : event->xbutton.x;
}

/* Safe addition and subtraction */
static void increment_value(XlwScrollBarWidget w, int diff)
{
	w->sb.value = w->sb.maximum - diff < w->sb.value ?
	    w->sb.maximum : w->sb.value + diff;
}

static void decrement_value(XlwScrollBarWidget w, int diff)
{
	w->sb.value = w->sb.minimum + diff > w->sb.value ?
	    w->sb.minimum : w->sb.value - diff;
}

static SliderStyle slider_style(XlwScrollBarWidget w)
{
	return (w->sb.sliderStyle ? w->sb.sliderStyle[0] == 'd' :
		w->sb.knobStyle ? w->sb.knobStyle[0] == 'd' :
		0) ? SLIDER_DIMPLE : SLIDER_PLAIN;
}

static Boolean arrow_same_end(XlwScrollBarWidget w)
{
	return w->sb.arrowPosition
	    && w->sb.arrowPosition[0] == 's' ? True : False;
}

/*-------------------------- GC and Pixel allocation --------------------*/
#ifndef XmUNSPECIFIED_PIXMAP
#define XmUNSPECIFIED_PIXMAP 2
#endif

static GC get_gc(XlwScrollBarWidget w, Pixel fg, Pixel bg, Pixmap pm)
{
	XGCValues values;
	XtGCMask mask;

	if (pm == w->sb.grayPixmap) {
		/* If we're using the gray pixmap, guarantee white on black ...
		 * otherwise, we could end up with something odd like grey on white
		 * when we're on a color display that ran out of color cells
		 */

		fg = WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(w)));
		bg = BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(w)));
	}

	values.foreground = fg;
	values.background = bg;
	values.fill_style = FillOpaqueStippled;
	values.stipple = pm;
/*  mask = GCForeground | GCBackground |
    (pm == None ? 0 : GCStipple | GCFillStyle); gtb */
	if (pm != None && pm != 0 && pm != XmUNSPECIFIED_PIXMAP)
		values.stipple = pm;
	else
		values.stipple = None;
	mask = GCForeground | GCBackground |
	    (values.stipple == None ? 0 : GCStipple | GCFillStyle);

	return XtGetGC((Widget) w, mask, &values);
}

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  From FSF Emacs. */

static int
allocate_nearest_color(Display * display, Colormap screen_colormap,
		       XColor * color_def)
{
	int status = XAllocColor(display, screen_colormap, color_def);
	if (status)
		return status;

	{
		/* If we got to this point, the colormap is full, so we're
		   going to try to get the next closest color.
		   The algorithm used is a least-squares matching, which is
		   what X uses for closest color matching with StaticColor visuals.  */

		int nearest, x;
		unsigned long nearest_delta = ULONG_MAX;

		int no_cells = XDisplayCells(display, XDefaultScreen(display));
		/* Don't use alloca here because lwlib doesn't have the
		   necessary configuration information that src does. */
		XColor *cells = (XColor *) malloc(sizeof(XColor) * no_cells);

		for (x = 0; x < no_cells; x++)
			cells[x].pixel = x;

		XQueryColors(display, screen_colormap, cells, no_cells);

		for (nearest = 0, x = 0; x < no_cells; x++) {
			long dred = (color_def->red >> 8) - (cells[x].red >> 8);
			long dgreen =
			    (color_def->green >> 8) - (cells[x].green >> 8);
			long dblue =
			    (color_def->blue >> 8) - (cells[x].blue >> 8);
			unsigned long delta =
			    dred * dred + dgreen * dgreen + dblue * dblue;

			if (delta < nearest_delta) {
				nearest = x;
				nearest_delta = delta;
			}
		}
		color_def->red = cells[nearest].red;
		color_def->green = cells[nearest].green;
		color_def->blue = cells[nearest].blue;
		free(cells);
		return XAllocColor(display, screen_colormap, color_def);
	}
}

static void make_shadow_pixels(XlwScrollBarWidget w)
{
	Display *dpy = XtDisplay((Widget) w);
	Colormap cmap = w->core.colormap;
	XColor topc, botc;
	int top_frobbed, bottom_frobbed;
	Pixel bg, fg;

	top_frobbed = bottom_frobbed = 0;

	bg = w->core.background_pixel;
	fg = w->sb.foreground;

	if (w->sb.topShadowColor == (Pixel) ~ 0)
		w->sb.topShadowColor = bg;
	if (w->sb.bottomShadowColor == (Pixel) ~ 0)
		w->sb.bottomShadowColor = fg;

	if (w->sb.topShadowColor == bg || w->sb.topShadowColor == fg) {
		topc.pixel = bg;
		XQueryColor(dpy, cmap, &topc);
		/* don't overflow/wrap! */
		topc.red = MINL(65535, topc.red * 1.2);
		topc.green = MINL(65535, topc.green * 1.2);
		topc.blue = MINL(65535, topc.blue * 1.2);
		if (allocate_nearest_color(dpy, cmap, &topc)) {
			if (topc.pixel == bg) {
				XFreeColors(dpy, cmap, &topc.pixel, 1, 0);
				topc.red = MINL(65535, topc.red + 0x8000);
				topc.green = MINL(65535, topc.green + 0x8000);
				topc.blue = MINL(65535, topc.blue + 0x8000);
				if (allocate_nearest_color(dpy, cmap, &topc)) {
					w->sb.topShadowColor = topc.pixel;
				}
			} else {
				w->sb.topShadowColor = topc.pixel;
			}

			top_frobbed = 1;
		}
	}

	if (w->sb.bottomShadowColor == fg || w->sb.bottomShadowColor == bg) {
		botc.pixel = bg;
		XQueryColor(dpy, cmap, &botc);
		botc.red = (botc.red * 3) / 5;
		botc.green = (botc.green * 3) / 5;
		botc.blue = (botc.blue * 3) / 5;
		if (allocate_nearest_color(dpy, cmap, &botc)) {
			if (botc.pixel == bg) {
				XFreeColors(dpy, cmap, &botc.pixel, 1, 0);
				botc.red = MINL(65535, botc.red + 0x4000);
				botc.green = MINL(65535, botc.green + 0x4000);
				botc.blue = MINL(65535, botc.blue + 0x4000);
				if (allocate_nearest_color(dpy, cmap, &botc)) {
					w->sb.bottomShadowColor = botc.pixel;
				}
			} else {
				w->sb.bottomShadowColor = botc.pixel;
			}
			bottom_frobbed = 1;
		}
	}

	if (top_frobbed && bottom_frobbed) {
		int top_avg =
		    ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
		int bot_avg =
		    ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
		if (bot_avg > top_avg) {
			Pixel tmp = w->sb.topShadowColor;
			w->sb.topShadowColor = w->sb.bottomShadowColor;
			w->sb.bottomShadowColor = tmp;
		} else if (topc.pixel == botc.pixel) {
			if (botc.pixel == bg)
				w->sb.topShadowColor = bg;
			else
				w->sb.bottomShadowColor = fg;
		}
	}

	if (w->sb.topShadowColor == w->core.background_pixel ||
	    w->sb.bottomShadowColor == w->core.background_pixel) {
		/* Assume we're in mono. This code should be okay even if we're
		 * really in color but just short on color cells -- We want the
		 * following behavior, which has been empirically determined to
		 * work well for all fg/bg combinations in mono: If the trough
		 * and slider are BOTH black, then use a white top shadow and a
		 * grey bottom shadow, otherwise use a grey top shadow and a
		 * black bottom shadow.
		 */

		Pixel white =
		    WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(w)));
		Pixel black =
		    BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(w)));

		/* Note: core.background_pixel is the color of the slider ... */

		if (w->core.background_pixel == black &&
		    w->sb.troughColor == black) {
			w->sb.topShadowColor = white;
			w->sb.bottomShadowPixmap = w->sb.grayPixmap;
		} else {
			w->sb.topShadowPixmap = w->sb.grayPixmap;
			w->sb.bottomShadowColor = black;
		}
	}
}

static void make_trough_pixel(XlwScrollBarWidget w)
{
	Display *dpy = XtDisplay((Widget) w);
	Colormap cmap = w->core.colormap;
	XColor troughC;

	if (w->sb.troughColor == (Pixel) ~ 0)
		w->sb.troughColor = w->core.background_pixel;

	if (w->sb.troughColor == w->core.background_pixel) {
		troughC.pixel = w->core.background_pixel;
		XQueryColor(dpy, cmap, &troughC);
		troughC.red = (troughC.red * 4) / 5;
		troughC.green = (troughC.green * 4) / 5;
		troughC.blue = (troughC.blue * 4) / 5;
		if (allocate_nearest_color(dpy, cmap, &troughC))
			w->sb.troughColor = troughC.pixel;
	}
}

/*-------------------------- Draw 3D Border -----------------------------*/
static void
draw_shadows(Display * dpy, Drawable d, GC shine_gc, GC shadow_gc,
	     int x, int y, int width, int height, int shadowT)
{
	XSegment shine[10], shadow[10];
	int i;

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT <= 0)
		return;

	for (i = 0; i < shadowT; i++) {
		/*  Top segments  */
		shine[i].x1 = x;
		shine[i].y2 = shine[i].y1 = y + i;
		shine[i].x2 = x + width - i - 1;
		/*  Left segments  */
		shine[i + shadowT].x2 = shine[i + shadowT].x1 = x + i;
		shine[i + shadowT].y1 = y + shadowT;
		shine[i + shadowT].y2 = y + height - i - 1;

		/*  Bottom segments  */
		shadow[i].x1 = x + i;
		shadow[i].y2 = shadow[i].y1 = y + height - i - 1;
		shadow[i].x2 = x + width - 1;
		/*  Right segments  */
		shadow[i + shadowT].x2 = shadow[i + shadowT].x1 =
		    x + width - i - 1;
		shadow[i + shadowT].y1 = y + i + 1;
		shadow[i + shadowT].y2 = y + height - 1;
	}

	XDrawSegments(dpy, d, shine_gc, shine, shadowT * 2);
	XDrawSegments(dpy, d, shadow_gc, shadow, shadowT * 2);
}

/*------------------ Draw 3D Arrows: left, up, down, right --------------*/
static int
make_vert_seg(XSegment * seg, int x1, int y1, int x2, int y2, int shadowT)
{
	int i;

	for (i = 0; i < shadowT; i++, seg++) {
		seg->x1 = x1;
		seg->y1 = y1++;
		seg->x2 = x2;
		seg->y2 = y2++;
	}
	return shadowT;
}

static int
make_hor_seg(XSegment * seg, int x1, int y1, int x2, int y2, int shadowT)
{
	int i;

	for (i = 0; i < shadowT; i++, seg++) {
		seg->x1 = x1++;
		seg->y1 = y1;
		seg->x2 = x2++;
		seg->y2 = y2;
	}
	return shadowT;
}

static void
draw_arrow_up(Display * dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
	      int x, int y, int width, int height, int shadowT)
{
	XSegment shine[10], shadow[10];
	XPoint triangle[3];
	int mid;

	mid = width / 2;

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT < 0)
		shadowT = 0;

	/*  /  */
	make_vert_seg(shine, x, y + height - shadowT - 1, x + mid, y, shadowT);
	/*  _\  */
	make_vert_seg(shadow,
		      x, y + height - shadowT - 1,
		      x + width - 1, y + height - shadowT - 1, shadowT);
	make_vert_seg(shadow + shadowT,
		      x + mid, y,
		      x + width - 1, y + height - shadowT - 1, shadowT);

	triangle[0].x = x;
	triangle[0].y = y + height - 1;
	triangle[1].x = x + mid;
	triangle[1].y = y;
	triangle[2].x = x + width - 1;
	triangle[2].y = y + height - 1;

	XFillPolygon(dpy, win, bgGC, triangle, 3, Convex, ArcChord);

	XDrawSegments(dpy, win, shadowGC, shadow, shadowT * 2);
	XDrawSegments(dpy, win, shineGC, shine, shadowT);
}

static void
draw_arrow_left(Display * dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		int x, int y, int width, int height, int shadowT)
{
	XSegment shine[10], shadow[10];
	XPoint triangle[3];

	int mid = width / 2;

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT < 0)
		shadowT = 0;

	/*  /  */
	make_hor_seg(shine, x, y + mid, x + width - shadowT - 1, y, shadowT);
	/*  \|  */
	make_hor_seg(shadow,
		     x, y + mid,
		     x + width - shadowT - 1, y + height - 1, shadowT);
	make_hor_seg(shadow + shadowT,
		     x + width - shadowT - 1, y,
		     x + width - shadowT - 1, y + height - 1, shadowT);

	triangle[0].x = x + width - 1;
	triangle[0].y = y + height - 1;
	triangle[1].x = x;
	triangle[1].y = y + mid;
	triangle[2].x = x + width - 1;
	triangle[2].y = y;

	XFillPolygon(dpy, win, bgGC, triangle, 3, Convex, ArcChord);

	XDrawSegments(dpy, win, shadowGC, shadow, shadowT * 2);
	XDrawSegments(dpy, win, shineGC, shine, shadowT);
}

static void
draw_arrow_down(Display * dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		int x, int y, int width, int height, int shadowT)
{
	XSegment shine[10], shadow[10];
	XPoint triangle[3];
	int mid;

	mid = width / 2;

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT < 0)
		shadowT = 0;

	/*  \-  */
	make_vert_seg(shine, x, y, x + mid, y + height - shadowT - 1, shadowT);
	make_vert_seg(shine + shadowT, x, y, x + width - 1, y, shadowT);
	/*  /  */
	make_vert_seg(shadow,
		      x + width - 1, y,
		      x + mid, y + height - shadowT - 1, shadowT);

	triangle[0].x = x;
	triangle[0].y = y;
	triangle[1].x = x + mid;
	triangle[1].y = y + height - 1;
	triangle[2].x = x + width - 1;
	triangle[2].y = y;

	XFillPolygon(dpy, win, bgGC, triangle, 3, Convex, ArcChord);

	XDrawSegments(dpy, win, shadowGC, shadow, shadowT);
	XDrawSegments(dpy, win, shineGC, shine, shadowT * 2);
}

static void
draw_arrow_right(Display * dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		 int x, int y, int width, int height, int shadowT)
{
	XSegment shine[10], shadow[10];
	XPoint triangle[3];
	int mid;

	mid = width / 2;

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT < 0)
		shadowT = 0;

	/*  |\  */
	make_hor_seg(shine, x, y, x + width - shadowT - 1, y + mid, shadowT);
	make_hor_seg(shine + shadowT, x, y, x, y + height - 1, shadowT);
	/*  /  */
	make_hor_seg(shadow,
		     x, y + height - 1,
		     x + width - shadowT - 1, y + mid, shadowT);

	triangle[0].x = x + 1;
	triangle[0].y = y + height - 1;
	triangle[1].x = x + width - 1;
	triangle[1].y = y + mid;
	triangle[2].x = x + 1;
	triangle[2].y = y;

	XFillPolygon(dpy, win, bgGC, triangle, 3, Convex, ArcChord);

	XDrawSegments(dpy, win, shadowGC, shadow, shadowT);
	XDrawSegments(dpy, win, shineGC, shine, shadowT * 2);
}

static void
draw_dimple(Display * dpy, Drawable win, GC shine, GC shadow,
	    int x, int y, int width, int height)
{
	XDrawArc(dpy, win, shine, x, y, width, height, 46 * 64, 180 * 64);
	XDrawArc(dpy, win, shadow, x, y, width, height, 45 * 64, -179 * 64);
}

/*------- Scrollbar values -> pixels, pixels -> scrollbar values --------*/

static void
seg_pixel_sizes(XlwScrollBarWidget w, int *above_return,
		int *ss_return, int *below_return)
{
	float total, height, fuz;
	int value, above, ss, below;

	height = widget_h(w);
	if (w->sb.showArrows)
		height -= (2 * arrow_h(w));

	value = w->sb.value - w->sb.minimum;

	total = w->sb.maximum - w->sb.minimum;
	fuz = total / 2;

	ss = (int)((height * w->sb.sliderSize + fuz) / total);
	above = (int)((height * value + fuz) / total);
	below = (int)((height) - (ss + above));

	/* Don't let slider get smaller than SS_MIN */
	if (ss < SS_MIN) {
		/* add a percent amount for integer rounding */
		float tmp =
		    (((float)(SS_MIN - ss) * (float)value) / total) + 0.5;

		above -= (int)tmp;
		ss = SS_MIN;
		below = (int)((height) - (ss + above));

		if (above < 0) {
			above = 0;
			below = (int)(height - ss);
		}
		if (below < 0) {
			above = (int)(height - ss);
			below = 0;
		}
		if (ss > height) {
			above = 0;
			ss = (int)height;
			below = 0;
		}
	}

	*above_return = above;
	*ss_return = ss;
	*below_return = below;

	CHECK(w);
}

static void verify_values(XlwScrollBarWidget w)
{
	int total = w->sb.maximum - w->sb.minimum;

	if (w->sb.sliderSize > total)
		w->sb.sliderSize = total;

	if (w->sb.pageIncrement > total)
		w->sb.pageIncrement = total;

	if (w->sb.increment > total)
		w->sb.increment = total;

	if (w->sb.value < w->sb.minimum)
		w->sb.value = w->sb.minimum;

	if (w->sb.value > w->sb.maximum)
		w->sb.value = w->sb.maximum;

	if (w->sb.sliderSize > w->sb.maximum - w->sb.value)
		w->sb.sliderSize = w->sb.maximum - w->sb.value;
}

static int value_from_pixel(XlwScrollBarWidget w, int above)
{
	float total, height, fuz;
	int value, ss;

	height = widget_h(w);
	if (w->sb.showArrows)
		height -= (2 * arrow_h(w));

	total = w->sb.maximum - w->sb.minimum;
	fuz = height / 2;

	ss = (int)((height * w->sb.sliderSize + (total / 2)) / total);

	if (ss < SS_MIN) {
		/* add a percent amount for integer rounding */
		above += (int)((((SS_MIN - ss) * above) + fuz) / height);
	}

	{
		/* Prevent SIGFPE's that would occur if we don't truncate the value. */
		float floatval =
		    w->sb.minimum + ((float)(above * total + fuz) / height);
		if (floatval >= (float)INT_MAX)
			value = INT_MAX;
		else if (floatval <= (float)INT_MIN)
			value = INT_MIN;
		else
			value = (int)floatval;
	}

	return value;
}

static void
redraw_dimple(XlwScrollBarWidget w, Display * dpy, Window win,
	      int x, int y, int width, int height)
{
	if (SLIDER_DIMPLE == slider_style(w)) {
		int size;
		int slider_p = (w->sb.armed == ARM_SLIDER);
		GC shine = slider_p ? w->sb.bottomShadowGC : w->sb.topShadowGC;
		GC shadow = slider_p ? w->sb.topShadowGC : w->sb.bottomShadowGC;
		int shadowT = w->sb.shadowThickness;

		x += shadowT;
		y += shadowT;
		width -= 2 * shadowT;
		height -= 2 * shadowT;

		size = (width < height ? width : height) * 3 / 4;

		if (size % 2 != (width < height ? width : height) % 2)
			size--;

		DBUG(fprintf(stderr, "%d %d\n",
			     x + (width / 2) - (size / 2) - 2 * shadowT,
			     width - size - shadowT));

		draw_dimple(dpy, win, shine, shadow,
			    x + (width / 2) - (size / 2),
			    y + (height / 2) - (size / 2), size, size);
	}
}

static void draw_slider(XlwScrollBarWidget w, int above, int ss, int below)
{
	Display *dpy = XtDisplay((Widget) w);
	Window win = XtWindow((Widget) w);

	int x = widget_x(w);
	int y = widget_y(w);
	int width = widget_w(w);
	int height = widget_h(w);
	int shadowT = w->sb.shadowThickness;
	int vert_p = VERT(w);

	if (shadowT > (width / 2))
		shadowT = (width / 2);
	if (shadowT > (height / 2))
		shadowT = (height / 2);
	if (shadowT < 0)
		shadowT = 0;

	if (w->sb.showArrows && !arrow_same_end(w))
		y += arrow_h(w);

	/* trough above slider */
	if (above > 0) {
		if (vert_p)
			XClearArea(dpy, win, x, y, width, above, False);
		else
			XClearArea(dpy, win, y, x, above, width, False);
	}

	/* slider */
	if (vert_p) {
		draw_shadows(dpy, win, w->sb.topShadowGC, w->sb.bottomShadowGC,
			     x, y + above, width, ss, shadowT);
		XFillRectangle(dpy, win, w->sb.backgroundGC,
			       x + shadowT, y + above + shadowT,
			       width - 2 * shadowT, ss - 2 * shadowT);
		redraw_dimple(w, dpy, win, x, y + above, width, ss);
	} else {
		draw_shadows(dpy, win, w->sb.topShadowGC, w->sb.bottomShadowGC,
			     y + above, x, ss, width, shadowT);
		XFillRectangle(dpy, win, w->sb.backgroundGC,
			       y + above + shadowT, x + shadowT,
			       ss - 2 * shadowT, width - 2 * shadowT);
		redraw_dimple(w, dpy, win, y + above, x, ss, width);
	}

	/* trough below slider */
	if (below > 0) {
		if (vert_p)
			XClearArea(dpy, win, x, y + above + ss, width, below,
				   False);
		else
			XClearArea(dpy, win, y + above + ss, x, below, width,
				   False);
	}

	CHECK(w);
}

static void
redraw_up_arrow(XlwScrollBarWidget w, Boolean armed, Boolean clear_behind)
{
	Display *dpy = XtDisplay((Widget) w);
	Window win = XtWindow((Widget) w);

	int x = widget_x(w);
	int y = widget_y(w);
	int width = widget_w(w);
	int height = widget_h(w);
	int shadowT = w->sb.shadowThickness;
	int arrow_height = arrow_h(w);

	GC bg = w->sb.backgroundGC;
	GC shine = armed ? w->sb.bottomShadowGC : w->sb.topShadowGC;
	GC shadow = armed ? w->sb.topShadowGC : w->sb.bottomShadowGC;

	if (VERT(w)) {
		if (arrow_same_end(w))
			y += height - 2 * arrow_height;
		if (clear_behind)
			XClearArea(dpy, win, x, y, width, arrow_height + 1,
				   False);
		draw_arrow_up(dpy, win, bg, shine, shadow,
			      x + (width - arrow_height) / 2, y, arrow_height,
			      arrow_height, shadowT);
	} else {
		if (arrow_same_end(w))
			y += height - 2 * arrow_height;
		if (clear_behind)
			XClearArea(dpy, win, y, x, arrow_height + 1, height,
				   False);
		draw_arrow_left(dpy, win, bg, shine, shadow, y,
				x + (width - arrow_height) / 2, arrow_height,
				arrow_height, shadowT);
	}
}

static void
redraw_down_arrow(XlwScrollBarWidget w, Boolean armed, Boolean clear_behind)
{
	Display *dpy = XtDisplay((Widget) w);
	Window win = XtWindow((Widget) w);

	int x = widget_x(w);
	int y = widget_y(w);
	int width = widget_w(w);
	int height = widget_h(w);
	int shadowT = w->sb.shadowThickness;
	int arrow_height = arrow_h(w);

	GC bg = w->sb.backgroundGC;
	GC shine = armed ? w->sb.bottomShadowGC : w->sb.topShadowGC;
	GC shadow = armed ? w->sb.topShadowGC : w->sb.bottomShadowGC;

	if (VERT(w)) {
		if (clear_behind)
			XClearArea(dpy, win, x, y + height - arrow_height,
				   width, arrow_height + 1, False);
		draw_arrow_down(dpy, win, bg, shine, shadow,
				x + (width - arrow_height) / 2,
				y + height - arrow_height + 1, arrow_height,
				arrow_height, shadowT);
	} else {
		if (clear_behind)
			XClearArea(dpy, win, y + height - arrow_height, x,
				   arrow_height + 1, height, False);
		draw_arrow_right(dpy, win, bg, shine, shadow,
				 y + height - arrow_height + 1,
				 x + (width - arrow_height) / 2,
				 arrow_height, arrow_height, shadowT);
	}
}

static void
redraw_everything(XlwScrollBarWidget w, Region region, Boolean behind_arrows)
{
	Display *dpy = XtDisplay((Widget) w);
	Window win = XtWindow((Widget) w);

	if (w->sb.showArrows) {
		if (region == NULL) {
			redraw_up_arrow(w, False, behind_arrows);
			redraw_down_arrow(w, False, behind_arrows);
		} else {
			int x = widget_x(w);
			int y = widget_y(w);
			int width = widget_w(w);
			int height = widget_h(w);
			int arrow_height = arrow_h(w);
			int ax = x, ay = y;

			if (arrow_same_end(w)) {
				if (VERT(w))
					ay = y + height - arrow_height -
					    arrow_height;
				else
					ax = x + height - arrow_height -
					    arrow_height;
			}
			if (XRectInRegion(region, ax, ay, width, width))
				redraw_up_arrow(w, False, behind_arrows);

			if (VERT(w))
				ay = y + height - arrow_height;
			else
				ax = x + height - arrow_height;
			if (XRectInRegion(region, ax, ay, width, width))
				redraw_down_arrow(w, False, behind_arrows);
		}
	}

	draw_shadows(dpy, win, w->sb.bottomShadowGC, w->sb.topShadowGC, 0, 0,
		     w->core.width, w->core.height, w->sb.shadowThickness);

	draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);
}

/*-------------------------- Method Functions ---------------------------*/

static void
Initialize(Widget treq, Widget tnew, ArgList args, Cardinal * num_args)
{
	XlwScrollBarWidget request = (XlwScrollBarWidget) treq;
	XlwScrollBarWidget w = (XlwScrollBarWidget) tnew;
	Display *dpy = XtDisplay((Widget) w);
	Window win = RootWindowOfScreen(DefaultScreenOfDisplay(dpy));

	if (request->core.width == 0)
		w->core.width += (VERT(w) ? 12 : 25);
	if (request->core.height == 0)
		w->core.height += (VERT(w) ? 25 : 12);

	verify_values(w);

	w->sb.lastY = 0;
	w->sb.above = 0;
	w->sb.ss = 0;
	w->sb.below = 0;
	w->sb.armed = ARM_NONE;
	w->sb.forced_scroll = FORCED_SCROLL_NONE;

	if (w->sb.shadowThickness > 5)
		w->sb.shadowThickness = 5;

	w->sb.grayPixmap =
	    XCreatePixmapFromBitmapData(dpy, win, (char *)gray_bits, gray_width,
					gray_height, 1, 0, 1);

	make_trough_pixel(w);

	make_shadow_pixels(w);

	w->sb.backgroundGC =
	    get_gc(w, w->core.background_pixel, w->core.background_pixel, None);
	w->sb.topShadowGC =
	    get_gc(w, w->sb.topShadowColor, w->core.background_pixel,
		   w->sb.topShadowPixmap);
	w->sb.bottomShadowGC =
	    get_gc(w, w->sb.bottomShadowColor, w->core.background_pixel,
		   w->sb.bottomShadowPixmap);

	w->sb.fullRedrawNext = True;

	w->sb.timerActive = False;
}

static void Destroy(Widget widget)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	Display *dpy = XtDisplay((Widget) w);

	XtReleaseGC(widget, w->sb.bottomShadowGC);
	XtReleaseGC(widget, w->sb.topShadowGC);
	XtReleaseGC(widget, w->sb.backgroundGC);

	XFreePixmap(dpy, w->sb.grayPixmap);

	if (w->sb.timerActive) {
		XtRemoveTimeOut(w->sb.timerId);
		w->sb.timerActive = False;	/* Should be a no-op, but you never know */
	}
}

static void
Realize(Widget widget, XtValueMask * valuemask, XSetWindowAttributes * attr)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	Display *dpy = XtDisplay((Widget) w);
	Window win;
	XSetWindowAttributes win_attr;

	(*coreClassRec.core_class.realize) (widget, valuemask, attr);

	win = XtWindow((Widget) w);

	seg_pixel_sizes(w, &w->sb.above, &w->sb.ss, &w->sb.below);

	XSetWindowBackground(dpy, win, w->sb.troughColor);

	/* Change bit gravity so widget is not cleared on resize */
	win_attr.bit_gravity = NorthWestGravity;
	XChangeWindowAttributes(dpy, win, CWBitGravity, &win_attr);

}

static void Resize(Widget widget)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	Display *dpy = XtDisplay((Widget) w);
	Window win = XtWindow((Widget) w);

	if (XtIsRealized(widget)) {
		DBUG(fprintf(stderr, "Resize = %08lx\n", w));

		seg_pixel_sizes(w, &w->sb.above, &w->sb.ss, &w->sb.below);

		/* redraw_everything (w, NULL, True); */

		w->sb.fullRedrawNext = True;
		/* Force expose event */
		XClearArea(dpy, win, widget_x(w), widget_y(w), 1, 1, True);
	}

	if (w->sb.timerActive) {
		XtRemoveTimeOut(w->sb.timerId);
		w->sb.timerActive = False;
	}
}

static void Redisplay(Widget widget, XEvent * event, Region region)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	DBUG(fprintf(stderr, "Redisplay = %08lx\n", w));

	if (XtIsRealized(widget)) {
		if (w->sb.fullRedrawNext)
			redraw_everything(w, NULL, True);
		else
			redraw_everything(w, region, False);
		w->sb.fullRedrawNext = False;
	}
}

static Boolean
SetValues(Widget current, Widget request, Widget neww,
	  ArgList args, Cardinal * num_args)
{
	XlwScrollBarWidget cur = (XlwScrollBarWidget) current;
	XlwScrollBarWidget w = (XlwScrollBarWidget) neww;
	Boolean do_redisplay = False;

	if (cur->sb.troughColor != w->sb.troughColor) {
		if (XtIsRealized((Widget) w)) {
			XSetWindowBackground(XtDisplay((Widget) w),
					     XtWindow((Widget) w),
					     w->sb.troughColor);
			do_redisplay = True;
		}
	}

	if (cur->core.background_pixel != w->core.background_pixel) {
		XtReleaseGC((Widget) cur, cur->sb.backgroundGC);
		w->sb.backgroundGC =
		    get_gc(w, w->core.background_pixel,
			   w->core.background_pixel, None);
		do_redisplay = True;
	}

	if (cur->sb.topShadowColor != w->sb.topShadowColor ||
	    cur->sb.topShadowPixmap != w->sb.topShadowPixmap) {
		XtReleaseGC((Widget) cur, cur->sb.topShadowGC);
		w->sb.topShadowGC =
		    get_gc(w, w->sb.topShadowColor, w->core.background_pixel,
			   w->sb.topShadowPixmap);
		do_redisplay = True;
	}

	if (cur->sb.bottomShadowColor != w->sb.bottomShadowColor ||
	    cur->sb.bottomShadowPixmap != w->sb.bottomShadowPixmap) {
		XtReleaseGC((Widget) cur, cur->sb.bottomShadowGC);
		w->sb.bottomShadowGC =
		    get_gc(w, w->sb.bottomShadowColor, w->core.background_pixel,
			   w->sb.bottomShadowPixmap);
		do_redisplay = True;
	}

	if (cur->sb.orientation != w->sb.orientation)
		do_redisplay = True;

	if (cur->sb.minimum != w->sb.minimum ||
	    cur->sb.maximum != w->sb.maximum ||
	    cur->sb.sliderSize != w->sb.sliderSize ||
	    cur->sb.value != w->sb.value ||
	    cur->sb.pageIncrement != w->sb.pageIncrement ||
	    cur->sb.increment != w->sb.increment) {
		verify_values(w);
		if (XtIsRealized((Widget) w)) {
			seg_pixel_sizes(w, &w->sb.above, &w->sb.ss,
					&w->sb.below);
			draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);
		}
	}

	if (w->sb.shadowThickness > 5)
		w->sb.shadowThickness = 5;

	return do_redisplay;
}

void
XlwScrollBarGetValues(Widget widget, int *value, int *sliderSize,
		      int *increment, int *pageIncrement)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	if (w && XtClass((Widget) w) == xlwScrollBarWidgetClass) {
		if (value)
			*value = w->sb.value;
		if (sliderSize)
			*sliderSize = w->sb.sliderSize;
		if (increment)
			*increment = w->sb.increment;
		if (pageIncrement)
			*pageIncrement = w->sb.pageIncrement;
	}
}

void
XlwScrollBarSetValues(Widget widget, int value, int sliderSize,
		      int increment, int pageIncrement, Boolean notify)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	if (w && XtClass((Widget) w) == xlwScrollBarWidgetClass &&
	    (w->sb.value != value ||
	     w->sb.sliderSize != sliderSize ||
	     w->sb.increment != increment ||
	     w->sb.pageIncrement != pageIncrement)) {
		int last_value = w->sb.value;

		w->sb.value = value;
		w->sb.sliderSize = sliderSize;
		w->sb.increment = increment;
		w->sb.pageIncrement = pageIncrement;

		verify_values(w);

		if (XtIsRealized(widget)) {
			seg_pixel_sizes(w, &w->sb.above, &w->sb.ss,
					&w->sb.below);
			draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

			if (w->sb.value != last_value && notify)
				call_callbacks(w, XmCR_VALUE_CHANGED,
					       w->sb.value, 0, NULL);
		}
	}
}

/*-------------------------- Action Functions ---------------------------*/

static void timer(XtPointer data, XtIntervalId * id)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) data;
	w->sb.timerActive = False;

	if (w->sb.armed != ARM_NONE) {
		int last_value = w->sb.value;
		int reason = XmCR_NONE;

		switch (w->sb.armed) {
		case ARM_PAGEUP:
			decrement_value(w, w->sb.pageIncrement);
			reason = XmCR_PAGE_DECREMENT;
			break;
		case ARM_PAGEDOWN:
			increment_value(w, w->sb.pageIncrement);
			reason = XmCR_PAGE_INCREMENT;
			break;
		case ARM_UP:
			decrement_value(w, w->sb.increment);
			reason = XmCR_DECREMENT;
			break;
		case ARM_DOWN:
			increment_value(w, w->sb.increment);
			reason = XmCR_INCREMENT;
			break;
		case ARM_NONE:
			/* This one should never happen... */
			assert(w->sb.armed != ARM_NONE);
			break;
		case ARM_SLIDER:
			/* Should something be done for slider? */
		default:
			reason = XmCR_NONE;
		}

		verify_values(w);

		if (last_value != w->sb.value) {
			seg_pixel_sizes(w, &w->sb.above, &w->sb.ss,
					&w->sb.below);
			draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

			call_callbacks(w, reason, w->sb.value, 0, NULL);

			w->sb.timerId =
			    XtAppAddTimeOut(XtWidgetToApplicationContext
					    ((Widget) w),
					    (unsigned long)w->sb.repeatDelay,
					    timer, (XtPointer) w);
			w->sb.timerActive = True;
		}
	}
}

static button_where what_button(XlwScrollBarWidget w, int mouse_x, int mouse_y)
{
	int width = widget_w(w);
	int height = widget_h(w);
	int arrow_height = arrow_h(w);

	mouse_x -= widget_x(w);
	mouse_y -= widget_y(w);

	if (mouse_x < 0 || mouse_x >= width || mouse_y < 0 || mouse_y >= height)
		return BUTTON_NONE;

	if (w->sb.showArrows) {
		if (mouse_y >= (height -= arrow_height))
			return BUTTON_DOWN_ARROW;

		if (arrow_same_end(w)) {
			if (mouse_y >= (height -= arrow_height))
				return BUTTON_UP_ARROW;
		} else if ((mouse_y -= arrow_height) < 0)
			return BUTTON_UP_ARROW;
	}

	if ((mouse_y -= w->sb.above) < 0)
		return BUTTON_TROUGH_ABOVE;

	if ((mouse_y -= w->sb.ss) < 0)
		return BUTTON_SLIDER;

	return BUTTON_TROUGH_BELOW;
}

static void
Select(Widget widget, XEvent * event, String * parms, Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	button_where sb_button;

	int mouse_x = event_x(w, event);
	int mouse_y = event_y(w, event);

	int last_value = w->sb.savedValue = w->sb.value;
	int reason = XmCR_NONE;

	XtGrabKeyboard((Widget) w, False, GrabModeAsync, GrabModeAsync,
		       event->xbutton.time);

	sb_button = what_button(w, mouse_x, mouse_y);

	if (w->sb.forced_scroll != FORCED_SCROLL_NONE) {
		switch (sb_button) {
		case BUTTON_TROUGH_ABOVE:
		case BUTTON_TROUGH_BELOW:
		case BUTTON_SLIDER:
			sb_button = BUTTON_NONE;	/* cause next switch to fall through */
			if (w->sb.forced_scroll == FORCED_SCROLL_UPLEFT) {
				decrement_value(w, w->sb.pageIncrement);
				w->sb.armed = ARM_PAGEUP;
				reason = XmCR_PAGE_DECREMENT;
				break;
			} else if (w->sb.forced_scroll ==
				   FORCED_SCROLL_DOWNRIGHT) {
				increment_value(w, w->sb.pageIncrement);
				w->sb.armed = ARM_PAGEDOWN;
				reason = XmCR_PAGE_INCREMENT;
				break;
			}
			abort();
		case BUTTON_NONE:
		default:
			;	/* Do nothing */
		}
	}

	switch (sb_button) {
	case BUTTON_TROUGH_ABOVE:
		decrement_value(w, w->sb.pageIncrement);
		w->sb.armed = ARM_PAGEUP;
		reason = XmCR_PAGE_DECREMENT;
		break;
	case BUTTON_TROUGH_BELOW:
		increment_value(w, w->sb.pageIncrement);
		w->sb.armed = ARM_PAGEDOWN;
		reason = XmCR_PAGE_INCREMENT;
		break;
	case BUTTON_SLIDER:
		w->sb.lastY = mouse_y;
		w->sb.armed = ARM_SLIDER;
		draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);
		break;
	case BUTTON_UP_ARROW:
		if (event->xbutton.state & ControlMask) {
			w->sb.value = w->sb.minimum;
			reason = XmCR_TO_TOP;
		} else {
			decrement_value(w, w->sb.increment);
			reason = XmCR_DECREMENT;
		}
		w->sb.armed = ARM_UP;
		redraw_up_arrow(w, True, False);
		break;
	case BUTTON_DOWN_ARROW:
		if (event->xbutton.state & ControlMask) {
			w->sb.value = w->sb.maximum;
			reason = XmCR_TO_BOTTOM;
		} else {
			increment_value(w, w->sb.increment);
			reason = XmCR_INCREMENT;
		}
		w->sb.armed = ARM_DOWN;
		redraw_down_arrow(w, True, False);
		break;
	case BUTTON_NONE:
	default:
		break;               /* Do nothing */
	}

	verify_values(w);

	if (last_value != w->sb.value) {
		seg_pixel_sizes(w, &w->sb.above, &w->sb.ss, &w->sb.below);
		draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

		call_callbacks(w, reason, w->sb.value, mouse_y, event);

		if (w->sb.timerActive)
			XtRemoveTimeOut(w->sb.timerId);

		w->sb.timerId =
		    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget) w),
				    (unsigned long)w->sb.initialDelay,
				    timer, (XtPointer) w);
		w->sb.timerActive = True;
	}

	CHECK(w);
}

static void
PageDownOrRight(Widget widget, XEvent * event, String * parms,
		Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	w->sb.forced_scroll = FORCED_SCROLL_DOWNRIGHT;
	Select(widget, event, parms, num_parms);
	w->sb.forced_scroll = FORCED_SCROLL_NONE;
}

static void
PageUpOrLeft(Widget widget, XEvent * event, String * parms,
	     Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	w->sb.forced_scroll = FORCED_SCROLL_UPLEFT;
	Select(widget, event, parms, num_parms);
	w->sb.forced_scroll = FORCED_SCROLL_NONE;
}

static void
Drag(Widget widget, XEvent * event, String * parms, Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	if (w->sb.armed == ARM_SLIDER) {
		int mouse_y = event_y(w, event);
		int diff = mouse_y - w->sb.lastY;

		if (diff < -(w->sb.above)) {	/* up */
			mouse_y -= (diff + w->sb.above);
			diff = -(w->sb.above);
		} else if (diff > w->sb.below) {	/* down */
			mouse_y -= (diff - w->sb.below);
			diff = w->sb.below;
		}

		if (diff) {
			w->sb.above += diff;
			w->sb.below -= diff;

			draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

			w->sb.lastY = mouse_y;

			w->sb.value = value_from_pixel(w, w->sb.above);
			verify_values(w);
			CHECK(w);

			call_callbacks(w, XmCR_DRAG, w->sb.value,
				       event_y(w, event), event);
		}
	}
	CHECK(w);
}

static void
Release(Widget widget, XEvent * event, String * parms, Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	switch (w->sb.armed) {
	case ARM_SLIDER:
		call_callbacks(w, XmCR_VALUE_CHANGED, w->sb.value,
			       event_y(w, event), event);
		w->sb.armed = ARM_NONE;
		draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);
		break;
	case ARM_UP:
		redraw_up_arrow(w, False, False);
		break;
	case ARM_DOWN:
		redraw_down_arrow(w, False, False);
		break;
	default:
		;		/* Do nothing */
	}

	XtUngrabKeyboard((Widget) w, event->xbutton.time);

	w->sb.armed = ARM_NONE;
}

static void
Jump(Widget widget, XEvent * event, String * parms, Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
	int last_value;

	int mouse_x = event_x(w, event);
	int mouse_y = event_y(w, event);

	int scroll_region_y = widget_y(w);
	int scroll_region_h = widget_h(w);

	if (w->sb.showArrows) {
		int arrow_height = arrow_h(w);
		scroll_region_h -= 2 * arrow_height;
		if (!arrow_same_end(w))
			scroll_region_y += arrow_height;
	}

	XtGrabKeyboard((Widget) w, False, GrabModeAsync, GrabModeAsync,
		       event->xbutton.time);

	switch (what_button(w, mouse_x, mouse_y)) {
	case BUTTON_TROUGH_ABOVE:
	case BUTTON_TROUGH_BELOW:
	case BUTTON_SLIDER:
		w->sb.savedValue = w->sb.value;

		last_value = w->sb.value;

		w->sb.above = mouse_y - (w->sb.ss / 2) - scroll_region_y;
		if (w->sb.above < 0)
			w->sb.above = 0;
		else if (w->sb.above + w->sb.ss > scroll_region_h)
			w->sb.above = scroll_region_h - w->sb.ss;

		w->sb.below = scroll_region_h - w->sb.ss - w->sb.above;

		w->sb.armed = ARM_SLIDER;
		draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

		w->sb.value = value_from_pixel(w, w->sb.above);
		verify_values(w);
		CHECK(w);

		w->sb.lastY = mouse_y;

		if (w->sb.value != last_value)
			call_callbacks(w, XmCR_DRAG, w->sb.value, mouse_y,
				       event);

		break;
	default:
		;		/* Do nothing */
	}
	CHECK(w);
}

static void
Abort(Widget widget, XEvent * event, String * parms, Cardinal * num_parms)
{
	XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

	if (w->sb.armed != ARM_NONE) {
		if (w->sb.value != w->sb.savedValue) {
			w->sb.value = w->sb.savedValue;

			seg_pixel_sizes(w, &w->sb.above, &w->sb.ss,
					&w->sb.below);
			draw_slider(w, w->sb.above, w->sb.ss, w->sb.below);

			call_callbacks(w, XmCR_VALUE_CHANGED, w->sb.value,
				       event_y(w, event), event);
		}

		switch (w->sb.armed) {
		case ARM_UP:
			redraw_up_arrow(w, False, False);
			break;
		case ARM_DOWN:
			redraw_down_arrow(w, False, False);
			break;
		default:;	/* Do nothing */
		}

		w->sb.armed = ARM_NONE;

		XtUngrabKeyboard((Widget) w, event->xbutton.time);
	}
}
