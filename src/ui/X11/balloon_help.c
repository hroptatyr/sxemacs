/* Balloon Help
   Copyright (c) 1997 Douglas Keller

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

/*
 * Balloon Help
 *
 * Version: 1.337 (Sun Apr 13 04:52:10 1997)
 *
 * Written by Douglas Keller <dkeller@vnet.ibm.com>
 *
 *
 */

#include <config.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>

#include "xintrinsic.h"

#include "balloon_help.h"

#ifndef max
#define max(x,y) (x>y?x:y)
#endif

#undef bool
#define bool int

#define MARGIN_WIDTH      4
#define POINTER_OFFSET    8
#define BORDER_WIDTH      2
#define BORDER_WIDTH_HALF 1

#define CONE_HEIGHT    20
#define CONE_WIDTH     50

#define SHAPE_CONE_TOP          (1<<0)
#define SHAPE_CONE_LEFT         (1<<1)
#define SHAPE_CONE_TOP_LEFT     (SHAPE_CONE_TOP | SHAPE_CONE_LEFT)
#define SHAPE_CONE_TOP_RIGHT    (SHAPE_CONE_TOP)
#define SHAPE_CONE_BOTTOM_LEFT  (SHAPE_CONE_LEFT)
#define SHAPE_CONE_BOTTOM_RIGHT (0)
#define SHAPE_CONE_FREE         (-1)

static Display *b_dpy;

static XFontStruct *b_fontStruct;
static GC b_gc;

static GC b_shineGC;
static GC b_shadowGC;

static Window b_win;
static bool b_winMapped;

static Pixmap b_mask;
static int b_maskWidth, b_maskHeight;
static GC b_maskGC;

static const char *b_text;
static int b_width, b_height;

static XtIntervalId b_timer;
static unsigned long b_delay;

static int b_screenWidth, b_screenHeight;

static int b_lastShape;

/*============================================================================

============================================================================*/

static GC
create_gc(Display * dpy, Window win, unsigned long fg, unsigned long bg,
	  XFontStruct * fontStruct)
{
	XGCValues gcv;
	unsigned long mask;

	gcv.foreground = fg;
	gcv.background = bg;
	gcv.font = fontStruct->fid;
	gcv.join_style = JoinMiter;
	gcv.line_width = BORDER_WIDTH;

	mask = GCFont | GCBackground | GCForeground | GCJoinStyle | GCLineWidth;

	return XCreateGC(dpy, win, mask, &gcv);
}

static void destroy_gc(Display * dpy, GC gc)
{
	if (gc) {
		XFreeGC(dpy, gc);
	}
}

/*============================================================================

============================================================================*/

static Window create_window(Display * dpy, unsigned long bg)
{
	Window win;
	XSetWindowAttributes attr;
	unsigned long attr_mask;

	attr_mask = CWOverrideRedirect | CWBackPixel | CWSaveUnder;
	attr.override_redirect = True;
	attr.background_pixel = bg;
	attr.save_under = True;

	win =
	    XCreateWindow(dpy,
			  DefaultRootWindow(dpy),
			  0, 0, 1, 1,
			  0,
			  CopyFromParent, InputOutput, CopyFromParent,
			  attr_mask, &attr);

	XSelectInput(dpy, win,
		     SubstructureRedirectMask |
		     SubstructureNotifyMask |
		     ExposureMask | EnterWindowMask | LeaveWindowMask);
	return win;
}

static void destroy_window(Display * dpy, Window win)
{
	if (win) {
		XDestroyWindow(dpy, win);
	}
}

/*============================================================================

============================================================================*/

static void get_pointer_xy(Display * dpy, int *x_return, int *y_return)
{
	int dummy;
	unsigned int mask;
	Window dummy_win;

	XQueryPointer(dpy, RootWindow(dpy, DefaultScreen(dpy)), &dummy_win,
		      &dummy_win, x_return, y_return, &dummy, &dummy, &mask);
}

/*============================================================================

============================================================================*/

static void create_pixmap_mask(int width, int height)
{
	b_maskWidth = width;
	b_maskHeight = height;
	b_mask = XCreatePixmap(b_dpy, b_win, width, height, 1);
}

static void destroy_pixmap_mask(void)
{
	XFreePixmap(b_dpy, b_mask);
}

static void grow_pixmap_mask(int width, int height)
{
	if (width > b_maskWidth || height > b_maskHeight) {
		destroy_pixmap_mask();
		create_pixmap_mask(width, height);
	}
}

/*============================================================================

============================================================================*/

static void
text_extent(XFontStruct * fontStruct, const char *text, int len,
	    int *width, int *height)
{
	XCharStruct extent;
	int dummy;

	XTextExtents(fontStruct, text, len, &dummy, &dummy, &dummy, &extent);

	*width = extent.width;
	*height = fontStruct->ascent + fontStruct->descent;
}

static void
get_text_size(Display * dpy, XFontStruct * fontStruct, const char *text,
	      int *max_width, int *max_height)
{
	int width;
	int height;
	const char *start;
	const char *end;

	*max_width = *max_height = 0;

	start = text;
	while ((end = strchr(start, '\n'))) {
		text_extent(fontStruct, start, end - start, &width, &height);
		*max_width = max(width, *max_width);
		*max_height += height;

		start = end + 1;
	}
	text_extent(fontStruct, start, strlen(start), &width, &height);
	*max_width = max(width, *max_width);
	*max_height += height;

	/* Min width */
	*max_width = max(*max_width, CONE_WIDTH / 2 * 3);

}

static void
draw_text(Display * dpy, Window win, GC gc, XFontStruct * fontStruct,
	  int x, int y, const char *text)
{
	const char *start;
	const char *end;
	int font_height;

	y += fontStruct->ascent;

	font_height = fontStruct->ascent + fontStruct->descent;

	start = text;
	while ((end = strchr(start, '\n'))) {
		XDrawString(dpy, win, gc, x, y, start, end - start);

		start = end + 1;
		y += font_height;
	}
	XDrawString(dpy, win, gc, x, y, start, strlen(start));
}

/*============================================================================

============================================================================*/

static int
get_shape(int last_shape, int x, int y, int width, int height,
	  int screen_width, int screen_height)
{
	/* Can we use last_shape? */
	if (((last_shape == SHAPE_CONE_TOP_LEFT) &&
	     (x + width < screen_width) && (y + height < screen_height)) ||
	    ((last_shape == SHAPE_CONE_TOP_RIGHT) &&
	     (x - width > 0) && (y + height < screen_height)) ||
	    ((last_shape == SHAPE_CONE_BOTTOM_LEFT) &&
	     (x + width < screen_width) && (y - height > 0)) ||
	    ((last_shape == SHAPE_CONE_BOTTOM_RIGHT) &&
	     (x - width > 0) && (y - height > 0)))
		return last_shape;

	/* Try to pick a shape that will not get changed,
	   e.g. if top left quadrant, top_left */
	return (x < screen_width / 2) ?
	    (y <
	     screen_height /
	     2 ? SHAPE_CONE_TOP_LEFT : SHAPE_CONE_BOTTOM_LEFT) : (y <
								  screen_height
								  /
								  2 ?
								  SHAPE_CONE_TOP_RIGHT
								  :
								  SHAPE_CONE_BOTTOM_RIGHT);
}

static void make_mask(int shape, int x, int y, int width, int height)
{
	XPoint cone[3];

	grow_pixmap_mask(width, height);

	/* Clear mask */
	XSetForeground(b_dpy, b_maskGC, 0);
	XFillRectangle(b_dpy, b_mask, b_maskGC, 0, 0, width, height);

	/* Enable text area */
	XSetForeground(b_dpy, b_maskGC, 1);
	XFillRectangle(b_dpy, b_mask, b_maskGC, 0,
		       shape & SHAPE_CONE_TOP ? CONE_HEIGHT : 0, width,
		       height - CONE_HEIGHT);

	/* Enable for cone area */
	cone[0].x =
	    (shape & SHAPE_CONE_LEFT) ? CONE_WIDTH / 2 : width -
	    (CONE_WIDTH / 2);
	cone[0].y =
	    (shape & SHAPE_CONE_TOP) ? CONE_HEIGHT : height - CONE_HEIGHT;
	cone[1].x = (shape & SHAPE_CONE_LEFT) ? 0 : width;
	cone[1].y = (shape & SHAPE_CONE_TOP) ? 0 : height;
	cone[2].x = (shape & SHAPE_CONE_LEFT) ? CONE_WIDTH : width - CONE_WIDTH;
	cone[2].y =
	    (shape & SHAPE_CONE_TOP) ? CONE_HEIGHT : height - CONE_HEIGHT;

	XFillPolygon(b_dpy, b_mask, b_maskGC, cone, 3, Nonconvex,
		     CoordModeOrigin);

}

static void show_help(XtPointer data, XtIntervalId * id)
{
	int x, y;
	int shape;
	XPoint border[3];

	if (id == NULL || ((id && b_timer) && b_text)) {
		b_timer = None;

		/* size */
		get_text_size(b_dpy, b_fontStruct, b_text, &b_width, &b_height);
		b_width += 2 * MARGIN_WIDTH + 2 * BORDER_WIDTH;
		b_height += 2 * MARGIN_WIDTH + 2 * BORDER_WIDTH + CONE_HEIGHT;

		/* origin */
		get_pointer_xy(b_dpy, &x, &y);

		/* guess at shape */
		shape = get_shape(b_lastShape, x, y, b_width, b_height,
				  b_screenWidth, b_screenHeight);

		x += (shape & SHAPE_CONE_LEFT) ? POINTER_OFFSET :
		    -POINTER_OFFSET;
		y += (shape & SHAPE_CONE_TOP) ? POINTER_OFFSET :
		    -POINTER_OFFSET;

		/* make sure it is still ok with offset */
		shape =
		    get_shape(shape, x, y, b_width, b_height, b_screenWidth,
			      b_screenHeight);

		b_lastShape = shape;

		make_mask(shape, x, y, b_width, b_height);

		XShapeCombineMask(b_dpy, b_win, ShapeBounding, 0, 0, b_mask,
				  ShapeSet);

		XMoveResizeWindow(b_dpy, b_win,
				  (shape & SHAPE_CONE_LEFT) ? x : x - b_width,
				  (shape & SHAPE_CONE_TOP) ? y : y - b_height,
				  b_width, b_height);

		XClearWindow(b_dpy, b_win);

		XMapRaised(b_dpy, b_win);
		b_winMapped = True;

		draw_text(b_dpy, b_win, b_gc, b_fontStruct,
			  BORDER_WIDTH + MARGIN_WIDTH,
			  BORDER_WIDTH + MARGIN_WIDTH +
			  ((shape & SHAPE_CONE_TOP) ? CONE_HEIGHT : 0), b_text);

		/* 3d border */
		/* shine- top left */
		border[0].x = 0 + BORDER_WIDTH_HALF;
		border[0].y =
		    ((shape & SHAPE_CONE_TOP) ? b_height : b_height -
		     CONE_HEIGHT) - BORDER_WIDTH_HALF;
		border[1].x = 0 + BORDER_WIDTH_HALF;
		border[1].y =
		    ((shape & SHAPE_CONE_TOP) ? CONE_HEIGHT : 0) +
		    BORDER_WIDTH_HALF;
		border[2].x = b_width - BORDER_WIDTH_HALF;
		border[2].y = border[1].y;
		XDrawLines(b_dpy, b_win, b_shineGC, border, 3, CoordModeOrigin);

		/* shadow- bottom right */
		border[0].x = 0 + BORDER_WIDTH_HALF;
		border[0].y =
		    ((shape & SHAPE_CONE_TOP) ? b_height : b_height -
		     CONE_HEIGHT) - BORDER_WIDTH_HALF;
		border[1].x = b_width - BORDER_WIDTH_HALF;
		border[1].y = border[0].y;
		border[2].x = b_width - BORDER_WIDTH_HALF;
		border[2].y =
		    ((shape & SHAPE_CONE_TOP) ? CONE_HEIGHT : 0) +
		    BORDER_WIDTH_HALF;
		XDrawLines(b_dpy, b_win, b_shadowGC, border, 3,
			   CoordModeOrigin);

		/* cone */
		if (SHAPE_CONE_TOP_LEFT == shape) {
			XClearArea(b_dpy, b_win,
				   CONE_WIDTH / 2 + BORDER_WIDTH,
				   CONE_HEIGHT,
				   CONE_WIDTH / 2 - BORDER_WIDTH,
				   BORDER_WIDTH, False);
			XDrawLine(b_dpy, b_win, b_shadowGC,
				  0,
				  0,
				  CONE_WIDTH / 2 + BORDER_WIDTH_HALF,
				  CONE_HEIGHT);
			XDrawLine(b_dpy, b_win, b_shineGC,
				  0,
				  0,
				  CONE_WIDTH - BORDER_WIDTH_HALF, CONE_HEIGHT);
		} else if (SHAPE_CONE_TOP_RIGHT == shape) {
			XClearArea(b_dpy, b_win,
				   b_width - CONE_WIDTH + BORDER_WIDTH,
				   CONE_HEIGHT,
				   CONE_WIDTH / 2 - BORDER_WIDTH,
				   BORDER_WIDTH, False);
			XDrawLine(b_dpy, b_win, b_shadowGC,
				  b_width,
				  0,
				  b_width - CONE_WIDTH / 2 - BORDER_WIDTH_HALF,
				  CONE_HEIGHT);
			XDrawLine(b_dpy, b_win, b_shineGC,
				  b_width,
				  0,
				  b_width - CONE_WIDTH + BORDER_WIDTH_HALF,
				  CONE_HEIGHT);
		} else if (SHAPE_CONE_BOTTOM_LEFT == shape) {
			XClearArea(b_dpy, b_win,
				   CONE_WIDTH / 2 + BORDER_WIDTH,
				   b_height - CONE_HEIGHT - BORDER_WIDTH,
				   CONE_WIDTH / 2 - BORDER_WIDTH,
				   BORDER_WIDTH, False);
			XDrawLine(b_dpy, b_win, b_shadowGC,
				  0,
				  b_height - 1,
				  CONE_WIDTH, b_height - 1 - CONE_HEIGHT);
			XDrawLine(b_dpy, b_win, b_shineGC,
				  0,
				  b_height - 1,
				  CONE_WIDTH / 2 + BORDER_WIDTH,
				  b_height - 1 - CONE_HEIGHT);
		} else if (SHAPE_CONE_BOTTOM_RIGHT == shape) {
			XClearArea(b_dpy, b_win,
				   b_width - 1 - CONE_WIDTH + BORDER_WIDTH,
				   b_height - CONE_HEIGHT - BORDER_WIDTH,
				   CONE_WIDTH / 2 - BORDER_WIDTH - 1,
				   BORDER_WIDTH, False);
			XDrawLine(b_dpy, b_win, b_shadowGC,
				  b_width - 1,
				  b_height - 1,
				  b_width - 1 - CONE_WIDTH,
				  b_height - 1 - CONE_HEIGHT);
			XDrawLine(b_dpy, b_win, b_shineGC,
				  b_width - 1,
				  b_height - 1,
				  b_width - 1 - CONE_WIDTH / 2 - BORDER_WIDTH,
				  b_height - 1 - CONE_HEIGHT);
		}
	}

}

/*============================================================================

============================================================================*/

static void balloon_help_destroy(void)
{
	assert(b_dpy != NULL);
	b_dpy = NULL;

	destroy_window(b_dpy, b_win);
	destroy_gc(b_dpy, b_gc);

	destroy_gc(b_dpy, b_shineGC);
	destroy_gc(b_dpy, b_shadowGC);

	destroy_pixmap_mask();
	destroy_gc(b_dpy, b_maskGC);

	if (b_timer)
		XtRemoveTimeOut(b_timer);
}

void
balloon_help_create(Display * dpy,
		    Pixel fg, Pixel bg, Pixel shine, Pixel shadow,
		    XFontStruct * font)
{
	if (b_dpy)
		balloon_help_destroy();

	b_dpy = dpy;

	b_fontStruct = font;

	b_win = create_window(dpy, bg);
	b_gc = create_gc(dpy, b_win, fg, bg, b_fontStruct);

	b_shineGC = create_gc(dpy, b_win, shine, bg, b_fontStruct);
	b_shadowGC = create_gc(dpy, b_win, shadow, bg, b_fontStruct);

	create_pixmap_mask(1, 1);
	b_maskGC = create_gc(dpy, b_mask, bg, fg, b_fontStruct);

	b_winMapped = False;
	b_timer = None;
	b_delay = 500;

	b_screenWidth = DisplayWidth(b_dpy, DefaultScreen(b_dpy));
	b_screenHeight = DisplayHeight(b_dpy, DefaultScreen(b_dpy));

	b_lastShape = SHAPE_CONE_FREE;
}

void balloon_help_set_delay(unsigned long milliseconds)
{
	b_delay = milliseconds;
}

void balloon_help_show(const char *text)
{
	assert(b_dpy != NULL);

	/* We don't copy the text */
	b_text = text;
	b_lastShape = SHAPE_CONE_FREE;

	if (b_winMapped) {
		/* If help is already being shown, don't delay just update */
		show_help(NULL, NULL);
	} else {
		b_timer =
		    XtAppAddTimeOut(XtDisplayToApplicationContext(b_dpy),
				    b_delay, show_help, NULL);
	}
}

void balloon_help_hide(void)
{
	assert(b_dpy != NULL);

	b_text = NULL;
	XUnmapWindow(b_dpy, b_win);
	b_winMapped = False;
	if (b_timer) {
		XtRemoveTimeOut(b_timer);
		b_timer = None;
	}
}

void balloon_help_move_to_pointer(void)
{
	assert(b_dpy != NULL);

	if (b_winMapped) {
		int x, y;
		int shape = b_lastShape;

		get_pointer_xy(b_dpy, &x, &y);

		x += (shape & SHAPE_CONE_LEFT) ? POINTER_OFFSET :
		    -POINTER_OFFSET;
		y += (shape & SHAPE_CONE_TOP) ? POINTER_OFFSET :
		    -POINTER_OFFSET;

		shape =
		    get_shape(shape, x, y, b_width, b_height, b_screenWidth,
			      b_screenHeight);

		if (shape == b_lastShape) {
			XMoveWindow(b_dpy, b_win,
				    shape & SHAPE_CONE_LEFT ? x : x - b_width,
				    shape & SHAPE_CONE_TOP ? y : y - b_height);
		} else {
			/* text would be off screen, rebuild with new shape */
			b_lastShape = SHAPE_CONE_FREE;
			show_help(NULL, NULL);
		}
	}
}
