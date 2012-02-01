/* Functions for the X window system.
   Copyright (C) 1989, 1992-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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


/* Synched up with: Not synched with FSF. */

/* Substantially rewritten for XEmacs.  */

/* 7-8-00 !!#### This file needs definite Mule review. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* Numerous places access the fields of
				   a core widget directly.  We could
				   use XtGetValues(), but ... */
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include "xmu.h"
#include "EmacsManager.h"
#include "EmacsFrameP.h"
#include "EmacsShell.h"
#ifdef EXTERNAL_WIDGET
#include "ExternalShell.h"
#endif
#include "glyphs-x.h"
#include "objects-x.h"
#include "scrollbar-x.h"

#include "buffer.h"
#include "events/events.h"
#include "extents.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/window.h"
#include "ui/gutter.h"

#if defined (HAVE_OFFIX_DND) || defined (HAVE_CDE)
#include "events/events-mod.h"
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_x_frame_plist;

Lisp_Object Qoverride_redirect;
Lisp_Object Qwindow_id;
Lisp_Object Qx_resource_name;

EXFUN(Fx_window_id, 1);

/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* Return the Emacs frame-object corresponding to an X window */
struct frame *x_window_to_frame(struct device *d, Window wdesc)
{
	Lisp_Object tail, frame;
	struct frame *f;

	/* This function was previously written to accept only a window argument
	   (and to loop over all devices looking for a matching window), but
	   that is incorrect because window ID's are not unique across displays. */

	for (tail = DEVICE_FRAME_LIST(d); CONSP(tail); tail = XCDR(tail)) {
		frame = XCAR(tail);
		if (!FRAMEP(frame))
			continue;
		f = XFRAME(frame);
		if (FRAME_X_P(f) && XtWindow(FRAME_X_TEXT_WIDGET(f)) == wdesc)
			return f;
	}
	return 0;
}

/* Like x_window_to_frame but also compares the window with the widget's
   windows */
struct frame *x_any_window_to_frame(struct device *d, Window wdesc)
{
	Widget w;
	assert(DEVICE_X_P(d));

	w = XtWindowToWidget(DEVICE_X_DISPLAY(d), wdesc);

	if (!w)
		return 0;

	/* We used to map over all frames here and then map over all widgets
	   belonging to that frame. However it turns out that this was very fragile
	   as it requires our display structures to be in sync _and_ that the
	   loop is told about every new widget somebody adds. Therefore we
	   now let Xt find it for us (which does a bottom-up search which
	   could even be faster) */
	return x_any_widget_or_parent_to_frame(d, w);
}

static struct frame *x_find_frame_for_window(struct device *d, Window wdesc)
{
	Lisp_Object tail, frame;
	struct frame *f;
	/* This function was previously written to accept only a window argument
	   (and to loop over all devices looking for a matching window), but
	   that is incorrect because window ID's are not unique across displays. */

	for (tail = DEVICE_FRAME_LIST(d); CONSP(tail); tail = XCDR(tail)) {
		frame = XCAR(tail);
		f = XFRAME(frame);
		/* This frame matches if the window is any of its widgets. */
		if (wdesc == XtWindow(FRAME_X_SHELL_WIDGET(f)) ||
		    wdesc == XtWindow(FRAME_X_CONTAINER_WIDGET(f)) ||
		    wdesc == XtWindow(FRAME_X_TEXT_WIDGET(f)))
			return f;

		/* Match if the window is one of the widgets at the top of the frame
		   (menubar, Energize psheets). */

		/* Note: Jamie once said

		   "Do *not* match if the window is this frame's psheet."

		   But this is wrong and will screw up some functions that expect
		   x_any_window_to_frame() to work as advertised.  I think the reason
		   for this statement is that, in the old (broken) event loop, where
		   not all events went through XtDispatchEvent(), psheet events
		   would incorrectly get sucked away by Emacs if this function matched
		   on psheet widgets. */

		/* Note: that this called only from
		   x_any_widget_or_parent_to_frame it is unnecessary to iterate
		   over the top level widgets. */

		/* Note:  we use to special case scrollbars but this turns out to be a bad idea
		   because
		   1. We sometimes get events for _unmapped_ scrollbars and our
		   callers don't want us to fail.
		   2. Starting with the 21.2 widget stuff there are now loads of
		   widgets to check and it is easy to forget adding them in a loop here.
		   See x_any_window_to_frame
		   3. We pick up all widgets now anyway. */
	}

	return 0;
}

struct frame *x_any_widget_or_parent_to_frame(struct device *d, Widget widget)
{
	while (widget) {
		struct frame *f = x_find_frame_for_window(d, XtWindow(widget));
		if (f)
			return f;
		widget = XtParent(widget);
	}

	return 0;
}

struct frame *decode_x_frame(Lisp_Object frame)
{
	if (NILP(frame))
		XSETFRAME(frame, selected_frame());
	CHECK_LIVE_FRAME(frame);
	/* this will also catch dead frames, but putting in the above check
	   results in a more useful error */
	CHECK_X_FRAME(frame);
	return XFRAME(frame);
}

/************************************************************************/
/*			window-manager interactions			*/
/************************************************************************/

#if 0
/* Not currently used. */

void x_wm_mark_shell_size_user_specified(Widget wmshell)
{
	if (!XtIsWMShell(wmshell))
		abort();
	EmacsShellSetSizeUserSpecified(wmshell);
}

void x_wm_mark_shell_position_user_specified(Widget wmshell)
{
	if (!XtIsWMShell(wmshell))
		abort();
	EmacsShellSetPositionUserSpecified(wmshell);
}

#endif

void x_wm_set_shell_iconic_p(Widget shell, int iconic_p)
{
	if (!XtIsWMShell(shell))
		abort();

	/* Because of questionable logic in Shell.c, this sequence can't work:

	   w = XtCreatePopupShell (...);
	   Xt_SET_VALUE (w, XtNiconic, True);
	   XtRealizeWidget (w);

	   The iconic resource is only consulted at initialization time (when
	   XtCreatePopupShell is called) instead of at realization time (just
	   before the window gets created, which would be more sensible) or
	   at management-time (just before the window gets mapped, which would
	   be most sensible of all).

	   The bug is that Shell's SetValues method doesn't do anything to
	   w->wm.wm_hints.initial_state until after the widget has been realized.
	   Calls to XtSetValues are ignored in the window between creation and
	   realization.  This is true of MIT X11R5 patch level 25, at least.
	   (Apparently some other versions of Xt don't have this bug?)
	 */
	Xt_SET_VALUE(shell, XtNiconic, iconic_p);
	EmacsShellSmashIconicHint(shell, iconic_p);
}

void x_wm_set_cell_size(Widget wmshell, int cw, int ch)
{
	Arg al[2];

	if (!XtIsWMShell(wmshell))
		abort();
	if (cw <= 0 || ch <= 0)
		abort();

	XtSetArg(al[0], XtNwidthInc, cw);
	XtSetArg(al[1], XtNheightInc, ch);
	XtSetValues(wmshell, al, 2);
}

void x_wm_set_variable_size(Widget wmshell, int width, int height)
{
	Arg al[2];

	if (!XtIsWMShell(wmshell))
		abort();
#ifdef DEBUG_GEOMETRY_MANAGEMENT
	/* See comment in EmacsShell.c */
	printf("x_wm_set_variable_size: %d %d\n", width, height);
	fflush(stdout);
#endif

	XtSetArg(al[0], XtNwidthCells, width);
	XtSetArg(al[1], XtNheightCells, height);
	XtSetValues(wmshell, al, 2);
}

/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS
   and WM_DELETE_WINDOW, then add them.  (They may already be present
   because of the toolkit (Motif adds them, for example, but Xt doesn't).
 */
static void x_wm_hack_wm_protocols(Widget widget)
{
	Display *dpy = XtDisplay(widget);
	struct device *d = get_device_from_display(dpy);
	Window w = XtWindow(widget);
	int need_delete = 1;
	int need_focus = 1;

	assert(XtIsWMShell(widget));

	{
		Atom type, *atoms = 0;
		int format = 0;
		unsigned long nitems = 0;
		unsigned long bytes_after;

		if (Success ==
		    XGetWindowProperty(dpy, w, DEVICE_XATOM_WM_PROTOCOLS(d), 0,
				       100, False, XA_ATOM, &type, &format,
				       &nitems, &bytes_after,
				       (unsigned char **)((void*)&atoms))
		    && format == 32 && type == XA_ATOM)
			while (nitems > 0) {
				nitems--;
				if (atoms[nitems] ==
				    DEVICE_XATOM_WM_DELETE_WINDOW(d))
					need_delete = 0;
				else if (atoms[nitems] ==
					 DEVICE_XATOM_WM_TAKE_FOCUS(d))
					need_focus = 0;
			}
		if (atoms)
			XFree((char *)atoms);
	}
	{
		Atom props[10];
		int count = 0;
		if (need_delete)
			props[count++] = DEVICE_XATOM_WM_DELETE_WINDOW(d);
		if (need_focus)
			props[count++] = DEVICE_XATOM_WM_TAKE_FOCUS(d);
		if (count)
			XChangeProperty(dpy, w, DEVICE_XATOM_WM_PROTOCOLS(d),
					XA_ATOM, 32, PropModeAppend,
					(unsigned char *)props, count);
	}
}

static void x_wm_store_class_hints(Widget shell, char *frame_name)
{
	Display *dpy = XtDisplay(shell);
	char *app_name, *app_class;
	XClassHint classhint;

	if (!XtIsWMShell(shell))
		abort();

	XtGetApplicationNameAndClass(dpy, &app_name, &app_class);
	classhint.res_name = frame_name;
	classhint.res_class = app_class;
	XSetClassHint(dpy, XtWindow(shell), &classhint);
}

#ifndef HAVE_WMCOMMAND
static void x_wm_maybe_store_wm_command(struct frame *f)
{
	Widget w = FRAME_X_SHELL_WIDGET(f);
	struct device *d = XDEVICE(FRAME_DEVICE(f));

	if (!XtIsWMShell(w))
		abort();

	if (NILP(DEVICE_X_WM_COMMAND_FRAME(d))) {
		int argc;
		char **argv;
		make_argc_argv(Vcommand_line_args, &argc, &argv);
		XSetCommand(XtDisplay(w), XtWindow(w), argv, argc);
		free_argc_argv(argv);
		XSETFRAME(DEVICE_X_WM_COMMAND_FRAME(d), f);
	}
}

/* If we're deleting the frame on which the WM_COMMAND property has been
   set, then move that property to another frame so that there is exactly
   one frame that has that property set.
 */
static void x_wm_maybe_move_wm_command(struct frame *f)
{
	struct device *d = XDEVICE(FRAME_DEVICE(f));

	/* There may not be a frame in DEVICE_X_WM_COMMAND_FRAME()
	   if we C-c'ed at startup at the right time. */
	if (FRAMEP(DEVICE_X_WM_COMMAND_FRAME(d))
	    && f == XFRAME(DEVICE_X_WM_COMMAND_FRAME(d))) {
		Lisp_Object rest = DEVICE_FRAME_LIST(d);
		DEVICE_X_WM_COMMAND_FRAME(d) = Qnil;
		/* find some random other X frame that is not this one, or give up */
		/* skip non-top-level (ExternalClient) frames */
		while (!NILP(rest) &&
		       (f == XFRAME(XCAR(rest)) ||
			!FRAME_X_TOP_LEVEL_FRAME_P(XFRAME(XCAR(rest)))))
			rest = XCDR(rest);
		if (NILP(rest))
			return;
		f = XFRAME(XCAR(rest));

		x_wm_maybe_store_wm_command(f);

	}
}
#endif				/* !HAVE_WMCOMMAND */

static int x_frame_iconified_p(struct frame *f)
{
	Atom actual_type;
	int actual_format;
	unsigned long nitems, bytesafter;
	unsigned long *datap = 0;
	Widget widget;
	int result = 0;
	struct device *d = XDEVICE(FRAME_DEVICE(f));

	widget = FRAME_X_SHELL_WIDGET(f);
	if (Success == XGetWindowProperty(XtDisplay(widget), XtWindow(widget),
					  DEVICE_XATOM_WM_STATE(d), 0, 2, False,
					  DEVICE_XATOM_WM_STATE(d),
					  &actual_type, &actual_format, &nitems,
					  &bytesafter,
					  (unsigned char **)((void*)&datap))
	    && datap) {
		if (nitems <= 2	/* "suggested" by ICCCM version 1 */
		    && datap[0] == IconicState)
			result = 1;
		XFree((char *)datap);
	}
	return result;
}

/************************************************************************/
/*                          frame properties                            */
/************************************************************************/

/* Connect the frame-property names (symbols) to the corresponding
   X Resource Manager names.  The name of a property, as a Lisp symbol,
   has an `x-resource-name' property which is a Lisp_String. */

static void init_x_prop_symbols(void)
{
#define def(sym, rsrc) \
   Fput (sym, Qx_resource_name, build_string (rsrc))
#define defi(sym,rsrc) \
   def (sym, rsrc); Fput (sym, Qintegerp, Qt)

#if 0				/* this interferes with things. #### fix this right */
	def(Qminibuffer, XtNminibuffer);
	def(Qunsplittable, XtNunsplittable);
#endif
	defi(Qinternal_border_width, XtNinternalBorderWidth);
#ifdef HAVE_TOOLBARS
	def(Qtop_toolbar_shadow_color, XtNtopToolBarShadowColor);
	def(Qbottom_toolbar_shadow_color, XtNbottomToolBarShadowColor);
	def(Qbackground_toolbar_color, XtNbackgroundToolBarColor);
	def(Qtop_toolbar_shadow_pixmap, XtNtopToolBarShadowPixmap);
	def(Qbottom_toolbar_shadow_pixmap, XtNbottomToolBarShadowPixmap);
	defi(Qtoolbar_shadow_thickness, XtNtoolBarShadowThickness);
#endif
	def(Qscrollbar_placement, XtNscrollBarPlacement);
	defi(Qinter_line_space, XtNinterline);
	/* font, foreground */
	def(Qiconic, XtNiconic);
	def(Qbar_cursor, XtNbarCursor);
	def(Qvisual_bell, XtNvisualBell);
	defi(Qbell_volume, XtNbellVolume);
	def(Qpointer_background, XtNpointerBackground);
	def(Qpointer_color, XtNpointerColor);
	def(Qtext_pointer, XtNtextPointer);
	def(Qspace_pointer, XtNspacePointer);
	def(Qmodeline_pointer, XtNmodeLinePointer);
	def(Qgc_pointer, XtNgcPointer);
	/* geometry, initial_geometry */
	def(Qinitially_unmapped, XtNinitiallyUnmapped);
	/* preferred_width, preferred_height */
	def(Quse_backing_store, XtNuseBackingStore);

	/* inherited: */

	def(Qborder_color, XtNborderColor);
	defi(Qborder_width, XtNborderWidth);
	defi(Qwidth, XtNwidth);
	defi(Qheight, XtNheight);
	defi(Qleft, XtNx);
	defi(Qtop, XtNy);

#undef def
}

static Lisp_Object color_to_string(Widget w, unsigned long pixel)
{
	char buf[255];
	int sz;

	XColor color;
	color.pixel = pixel;
	XQueryColor(XtDisplay(w), w->core.colormap, &color);
	sz = snprintf(buf, sizeof(buf), "#%04x%04x%04x", color.red, color.green, color.blue);
	assert(sz>=0 && (size_t)sz < sizeof(buf));
	return build_string(buf);
}

static void
x_get_top_level_position(Display * d, Window w, Position * x, Position * y)
{
	Window root, parent = w, *children;
	unsigned int nchildren;
	XWindowAttributes xwa;

	do {
		w = parent;
		if (!XQueryTree(d, w, &root, &parent, &children, &nchildren)) {
			*x = 0;
			*y = 0;
			return;
		}
		XFree(children);
	}
	while (root != parent);
	XGetWindowAttributes(d, w, &xwa);
	*x = xwa.x;
	*y = xwa.y;
}

#if 0
static void x_smash_bastardly_shell_position(Widget shell)
{
	/* Naturally those bastards who wrote Xt couldn't be bothered
	   to learn about race conditions and such.  We can't trust
	   the X and Y values to have any semblance of correctness,
	   so we smash the right values in place. */

	/* We might be called before we've actually realized the window (if
	   we're checking for the minibuffer resource).  This will bomb in
	   that case so we don't bother calling it. */
	if (XtWindow(shell))
		x_get_top_level_position(XtDisplay(shell), XtWindow(shell),
					 &shell->core.x, &shell->core.y);
}
#endif				/* 0 */

static Lisp_Object x_frame_property(struct frame *f, Lisp_Object property)
{
	Widget shell = FRAME_X_SHELL_WIDGET(f);
	EmacsFrame w = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	Widget gw = (Widget) w;

	if (EQ(Qleft, property) || EQ(Qtop, property)) {
		Position x, y;
		if (!XtWindow(shell))
			return Qzero;
		x_get_top_level_position(XtDisplay(shell), XtWindow(shell), &x,
					 &y);
		if (EQ(Qleft, property))
			return make_int(x);
		if (EQ(Qtop, property))
			return make_int(y);
	}
	if (EQ(Qborder_width, property))
		return make_int(w->core.border_width);
	if (EQ(Qinternal_border_width, property))
		return make_int(w->emacs_frame.internal_border_width);
	if (EQ(Qborder_color, property))
		return color_to_string(gw, w->core.border_pixel);
#ifdef HAVE_TOOLBARS
	if (EQ(Qtop_toolbar_shadow_color, property))
		return color_to_string(gw,
				       w->emacs_frame.top_toolbar_shadow_pixel);
	if (EQ(Qbottom_toolbar_shadow_color, property))
		return color_to_string(gw,
				       w->emacs_frame.
				       bottom_toolbar_shadow_pixel);
	if (EQ(Qbackground_toolbar_color, property))
		return color_to_string(gw,
				       w->emacs_frame.background_toolbar_pixel);
	if (EQ(Qtoolbar_shadow_thickness, property))
		return make_int(w->emacs_frame.toolbar_shadow_thickness);
#endif				/* HAVE_TOOLBARS */
	if (EQ(Qinter_line_space, property))
		return make_int(w->emacs_frame.interline);
	if (EQ(Qwindow_id, property))
		return Fx_window_id(make_frame(f));

	return Qunbound;
}

static int x_internal_frame_property_p(struct frame *f, Lisp_Object property)
{
	return EQ(property, Qleft)
	    || EQ(property, Qtop)
	    || EQ(property, Qborder_width)
	    || EQ(property, Qinternal_border_width)
	    || EQ(property, Qborder_color)
#ifdef HAVE_TOOLBARS
	    || EQ(property, Qtop_toolbar_shadow_color)
	    || EQ(property, Qbottom_toolbar_shadow_color)
	    || EQ(property, Qbackground_toolbar_color)
	    || EQ(property, Qtoolbar_shadow_thickness)
#endif				/* HAVE_TOOLBARS */
	    || EQ(property, Qinter_line_space)
	    || EQ(property, Qwindow_id)
	    || STRINGP(property);
}

static Lisp_Object x_frame_properties(struct frame *f)
{
	Lisp_Object props = Qnil;
	Widget shell = FRAME_X_SHELL_WIDGET(f);
	EmacsFrame w = (EmacsFrame) FRAME_X_TEXT_WIDGET(f);
	Widget gw = (Widget) w;
	Position x, y;

	props = cons3(Qwindow_id, Fx_window_id(make_frame(f)), props);
	props =
	    cons3(Qinter_line_space, make_int(w->emacs_frame.interline), props);

#ifdef HAVE_TOOLBARS
	props = cons3(Qtoolbar_shadow_thickness,
		      make_int(w->emacs_frame.toolbar_shadow_thickness), props);
	props = cons3(Qbackground_toolbar_color,
		      color_to_string(gw,
				      w->emacs_frame.background_toolbar_pixel),
		      props);
	props =
	    cons3(Qbottom_toolbar_shadow_color,
		  color_to_string(gw,
				  w->emacs_frame.bottom_toolbar_shadow_pixel),
		  props);
	props =
	    cons3(Qtop_toolbar_shadow_color,
		  color_to_string(gw, w->emacs_frame.top_toolbar_shadow_pixel),
		  props);
#endif				/* HAVE_TOOLBARS */

	props = cons3(Qborder_color,
		      color_to_string(gw, w->core.border_pixel), props);
	props = cons3(Qinternal_border_width,
		      make_int(w->emacs_frame.internal_border_width), props);
	props = cons3(Qborder_width, make_int(w->core.border_width), props);

	if (!XtWindow(shell))
		x = y = 0;
	else
		x_get_top_level_position(XtDisplay(shell), XtWindow(shell), &x,
					 &y);

	props = cons3(Qtop, make_int(y), props);
	props = cons3(Qleft, make_int(x), props);

	return props;
}

/* Functions called only from `x_set_frame_properties' to set
   individual properties. */

static void
x_set_frame_text_value(struct frame *f, Bufbyte * value,
		       String Xt_resource_name,
		       String Xt_resource_encoding_name)
{
	Atom encoding = XA_STRING;
	String new_XtValue = (String) value;
	String old_XtValue = NULL;

#ifdef MULE
	Bufbyte *ptr;
	/* Optimize for common ASCII case */
	for (ptr = value; *ptr; ptr++)
		if (!BYTE_ASCII_P(*ptr)) {
			char *tmp;
			encoding = DEVICE_XATOM_COMPOUND_TEXT(
				XDEVICE(FRAME_DEVICE(f)));
			C_STRING_TO_EXTERNAL(value, tmp, Qctext);
			new_XtValue = (String) tmp;
			break;
		}
#endif				/* MULE */

	/* #### Caching is device-independent - belongs in update_frame_title. */
	Xt_GET_VALUE(FRAME_X_SHELL_WIDGET(f), Xt_resource_name, &old_XtValue);
	if (!old_XtValue || strcmp(new_XtValue, old_XtValue)) {
		Arg al[2];
		XtSetArg(al[0], Xt_resource_name, new_XtValue);
		XtSetArg(al[1], Xt_resource_encoding_name, encoding);
		XtSetValues(FRAME_X_SHELL_WIDGET(f), al, 2);
	}
}

static void x_set_title_from_bufbyte(struct frame *f, Bufbyte * name)
{
	x_set_frame_text_value(f, name, XtNtitle, XtNtitleEncoding);
}

static void x_set_icon_name_from_bufbyte(struct frame *f, Bufbyte * name)
{
	x_set_frame_text_value(f, name, XtNiconName, XtNiconNameEncoding);
}

/* Set the initial frame size as specified.  This function is used
   when the frame's widgets have not yet been realized.  In this
   case, it is not sufficient just to set the width and height of
   the EmacsFrame widget, because they will be ignored when the
   widget is realized (instead, the shell's geometry resource is
   used). */

static void
x_set_initial_frame_size(struct frame *f, int flags, int x, int y,
			 unsigned int w, unsigned int h)
{
	char  shell_geom[255];
	int   xval, yval;
	char  xsign, ysign;
	char  uspos = !!(flags & (XValue | YValue));
	char  ussize = !!(flags & (WidthValue | HeightValue));
	char *temp;
	int   sz = 0;

	/* assign the correct size to the EmacsFrame widget ... */
	EmacsFrameSetCharSize(FRAME_X_TEXT_WIDGET(f), w, h);

	/* and also set the WMShell's geometry */
	(flags & XNegative) ? (xval = -x, xsign = '-') : (xval = x, xsign =
							  '+');
	(flags & YNegative) ? (yval = -y, ysign = '-') : (yval = y, ysign =
							  '+');

	if (uspos && ussize)
		sz = snprintf(shell_geom, sizeof(shell_geom),
			      "=%dx%d%c%d%c%d", w, h, xsign, xval, ysign,
			      yval);
	else if (uspos)
		sz = snprintf(shell_geom, sizeof(shell_geom),
			      "=%c%d%c%d", xsign, xval, ysign, yval);
	else if (ussize)
		sz = snprintf(shell_geom, sizeof(shell_geom),
			      "=%dx%d", w, h);
	assert(sz >=0 && (size_t)sz < sizeof(shell_geom));
	if (uspos || ussize) {
		temp = xnew_atomic_array(char, 1 + strlen(shell_geom));
		strcpy(temp, shell_geom);
		FRAME_X_GEOM_FREE_ME_PLEASE(f) = temp;
	} else {
		temp = NULL;
	}
	Xt_SET_VALUE(FRAME_X_SHELL_WIDGET(f), XtNgeometry, temp);
}

/* Report to X that a frame property of frame S is being set or changed.
   If the property is not specially recognized, do nothing.
 */

static void x_set_frame_properties(struct frame *f, Lisp_Object plist)
{
	Position x, y;
	Dimension width = 0, height = 0;
	Bool width_specified_p = False;
	Bool height_specified_p = False;
	Bool x_position_specified_p = False;
	Bool y_position_specified_p = False;
	Bool internal_border_width_specified = False;
	Lisp_Object tail;
	Widget w;

	/* We can be called after the X IO error handler has seen a
	   broken pipe on the relevant display. Don't do anything in
	   that case.  */
	if (!FRAME_LIVE_P (f) || DEVICE_X_BEING_DELETED (XDEVICE (FRAME_DEVICE (f))))
	  return;

	w = FRAME_X_TEXT_WIDGET (f);

	for (tail = plist; !NILP(tail); tail = Fcdr(Fcdr(tail))) {
		Lisp_Object prop = Fcar(tail);
		Lisp_Object val = Fcar(Fcdr(tail));

		if (STRINGP(prop)) {
			const char *extprop;

			if (XSTRING_LENGTH(prop) == 0)
				continue;

			LISP_STRING_TO_EXTERNAL(prop, extprop, Qctext);
			if (STRINGP(val)) {
				const Extbyte *extval = NULL;
				Extcount extvallen;

				TO_EXTERNAL_FORMAT(LISP_STRING, val,
						   ALLOCA, (extval, extvallen),
						   Qctext);
				if ( extval != NULL ) {
					XtVaSetValues(w, XtVaTypedArg, extprop,
						      XtRString, extval, extvallen + 1,
						      NULL);
				} else {
					message("x_set_frame_properties: Could not transcode property");
				}
			} else
				XtVaSetValues(w, XtVaTypedArg, extprop, XtRInt,
					      XINT(val), sizeof(int),
					      NULL);
		} else if (SYMBOLP(prop)) {
			Lisp_Object str = Fget(prop, Qx_resource_name, Qnil);
			int int_p = !NILP(Fget(prop, Qintegerp, Qnil));

			if (NILP(prop) || NILP(str)) {
				/* Kludge to handle the font property. */
				if (EQ(prop, Qfont)) {
					/* If the value is not a string we silently ignore it. */
					if (STRINGP(val)) {
						Lisp_Object frm, font_spec;

						XSETFRAME(frm, f);
						font_spec =
						    Fget(Fget_face(Qdefault),
							 Qfont, Qnil);

						Fadd_spec_to_specifier
						    (font_spec, val, frm, Qnil,
						     Qnil);
						update_frame_face_values(f);
					}

					continue;
				} else
					continue;
			}
			CHECK_STRING(str);

			/* Kludge the width/height so that we interpret them in characters
			   instead of pixels.  Yuck yuck yuck. */
			if (!strcmp((char *)XSTRING_DATA(str), "width")) {
				CHECK_INT(val);
				width = XINT(val);
				width_specified_p = True;
				continue;
			}
			if (!strcmp((char *)XSTRING_DATA(str), "height")) {
				CHECK_INT(val);
				height = XINT(val);
				height_specified_p = True;
				continue;
			}
			/* Further kludge the x/y. */
			if (!strcmp((char *)XSTRING_DATA(str), "x")) {
				CHECK_INT(val);
				x = (Position) XINT(val);
				x_position_specified_p = True;
				continue;
			}
			if (!strcmp((char *)XSTRING_DATA(str), "y")) {
				CHECK_INT(val);
				y = (Position) XINT(val);
				y_position_specified_p = True;
				continue;
			}
			/* Have you figured out by now that this entire function is
			   one gigantic kludge? */
			if (!strcmp((char *)XSTRING_DATA(str),
				    "internalBorderWidth")) {
				internal_border_width_specified = True;
			}

			if (int_p) {
				CHECK_INT(val);
				Xt_SET_VALUE(w, (char *)XSTRING_DATA(str),
					     XINT(val));
			} else if (EQ(val, Qt)) {
				Xt_SET_VALUE(w, (char *)XSTRING_DATA(str), True);	/* XtN... */
			} else if (EQ(val, Qnil)) {
				Xt_SET_VALUE(w, (char *)XSTRING_DATA(str), False);	/* XtN... */
			} else {
				CHECK_STRING(val);
				XtVaSetValues(w, XtVaTypedArg,
					      /* XtN... */
					      (char *)XSTRING_DATA(str),
					      XtRString,
					      XSTRING_DATA(val),
					      XSTRING_LENGTH(val) + 1,
					      NULL);
			}

#ifdef HAVE_SCROLLBARS
			if (!strcmp((char *)XSTRING_DATA(str), "scrollBarWidth")
			    || !strcmp((char *)XSTRING_DATA(str),
				       "scrollBarHeight")) {
				x_update_frame_scrollbars(f);
			}
#endif				/* HAVE_SCROLLBARS */
		}
	}

	/* Kludge kludge kludge.   We need to deal with the size and position
	   specially. */
	{
		int size_specified_p = width_specified_p || height_specified_p;
		int position_specified_p = x_position_specified_p ||
		    y_position_specified_p;

		if (!width_specified_p)
			width = FRAME_WIDTH(f);
		if (!height_specified_p)
			height = FRAME_HEIGHT(f);

		/* Kludge kludge kludge kludge. */
		if (position_specified_p &&
		    (!x_position_specified_p || !y_position_specified_p)) {
			Position dummy;
			Widget shell = FRAME_X_SHELL_WIDGET(f);
			x_get_top_level_position(XtDisplay(shell),
						 XtWindow(shell),
						 (x_position_specified_p ?
						  &dummy : &x),
						 (y_position_specified_p ?
						  &dummy : &y));
#if 0
			x = (int)(FRAME_X_SHELL_WIDGET(f)->core.x);
			y = (int)(FRAME_X_SHELL_WIDGET(f)->core.y);
#endif
		}

		if (!f->init_finished) {
			int flags =
			    (size_specified_p ? WidthValue | HeightValue : 0) |
			    (position_specified_p ? XValue | YValue |
			     (x < 0 ? XNegative : 0) | (y < 0 ? YNegative : 0)
			     : 0);
			if (size_specified_p
			    || position_specified_p
			    || internal_border_width_specified)
				x_set_initial_frame_size(f, flags, x, y, width,
							 height);
		} else {
			if (size_specified_p || internal_border_width_specified) {
				Lisp_Object frame;
				XSETFRAME(frame, f);
				Fset_frame_size(frame, make_int(width),
						make_int(height), Qnil);
			}
			if (position_specified_p) {
				Lisp_Object frame;
				XSETFRAME(frame, f);
				Fset_frame_position(frame, make_int(x),
						    make_int(y));
			}
		}
	}
}

static int frame_title_format_already_set;

static void maybe_set_frame_title_format(Widget shell)
{

	/* Only do this if this is the first X frame we're creating.

	   If the *title resource (or -title option) was specified, then
	   set frame-title-format to its value.
	 */

	if (!frame_title_format_already_set) {
		/* No doubt there's a less stupid way to do this. */
		char *results[2];
		XtResource resources[2];
		results[0] = results[1] = 0;
		resources[0].resource_name = XtNtitle;
		resources[0].resource_class = XtCTitle;
		resources[0].resource_type = XtRString;
		resources[0].resource_size = sizeof(String);
		resources[0].resource_offset = 0;
		resources[0].default_type = XtRString;
		resources[0].default_addr = 0;
		resources[1].resource_name = XtNiconName;
		resources[1].resource_class = XtCIconName;
		resources[1].resource_type = XtRString;
		resources[1].resource_size = sizeof(String);
		resources[1].resource_offset = sizeof(char *);
		resources[1].default_type = XtRString;
		resources[1].default_addr = 0;
		XtGetSubresources(XtParent(shell), (XtPointer) results,
				  shell->core.name,
				  shell->core.widget_class->core_class.
				  class_name, resources, XtNumber(resources), 0,
				  0);
		if (results[0])
			Vframe_title_format = build_string(results[0]);
		if (results[1])
			Vframe_icon_title_format = build_string(results[1]);
	}

	frame_title_format_already_set = 1;
}

#ifdef HAVE_CDE
#include <Dt/Dt.h>
#include <Dt/Dnd.h>

static Widget CurrentDragWidget = NULL;
static XtCallbackRec dnd_convert_cb_rec[2];
static XtCallbackRec dnd_destroy_cb_rec[2];
static int drag_not_done = 0;

static void
x_cde_destroy_callback(Widget widget, XtPointer clientData, XtPointer callData)
{
	DtDndDragFinishCallbackStruct *dragFinishInfo =
	    (DtDndDragFinishCallbackStruct *) callData;
	DtDndContext *dragData = dragFinishInfo->dragData;
	int i;

	/* free the items */
	if (callData != NULL && dragData != NULL) {
		if (dragData->protocol == DtDND_BUFFER_TRANSFER) {
			for (i = 0; i < dragData->numItems; i++) {
				XtFree((char *)dragData->data.buffers[i].bp);
				if (dragData->data.buffers[i].name)
					XtFree(dragData->data.buffers[i].name);
			}
		} else {
			for (i = 0; i < dragData->numItems; i++)
				XtFree(dragData->data.files[i]);
		}
	}

	/* free the data string */
	xfree(clientData);

	CurrentDragWidget = NULL;
}

static void
x_cde_convert_callback(Widget widget, XtPointer clientData, XtPointer callData)
{
	DtDndConvertCallbackStruct *convertInfo =
	    (DtDndConvertCallbackStruct *) callData;
	char *textdata = (char *)clientData;
	char *textptr = NULL;
	int i;

	if (convertInfo == NULL) {
		return;
	}

	if ((convertInfo->dragData->protocol != DtDND_BUFFER_TRANSFER
	     && convertInfo->dragData->protocol != DtDND_FILENAME_TRANSFER) ||
	    (convertInfo->reason != DtCR_DND_CONVERT_DATA)) {
		return;
	}

	for (textptr = textdata, i = 0;
	     i < convertInfo->dragData->numItems;
	     textptr += strlen(textptr) + 1, i++) {
		if (convertInfo->dragData->protocol == DtDND_BUFFER_TRANSFER) {
			convertInfo->dragData->data.buffers[i].bp =
			    XtNewString(textptr);
			convertInfo->dragData->data.buffers[i].size =
			    strlen(textptr);
			convertInfo->dragData->data.buffers[i].name = NULL;
		} else {
			convertInfo->dragData->data.files[i] =
			    XtNewString(textptr);
		}
	}

	convertInfo->status = DtDND_SUCCESS;
}

static Lisp_Object abort_current_drag(Lisp_Object arg)
{
	if (CurrentDragWidget && drag_not_done) {
		XmDragCancel(CurrentDragWidget);
		CurrentDragWidget = NULL;
	}
	return arg;
}

DEFUN("cde-start-drag-internal", Fcde_start_drag_internal, 3, 3, 0,	/*
Start a CDE drag from a buffer.
First argument is the event that started the drag (must be a
button-press-event),
second arg defines if the data should be treated as a buffer or
a filename transfer (set to nil for buffer transfer),
and the third argument is a list of data strings.
WARNING: can only handle plain/text and file: transfers!
*/
      (event, dragtype, dragdata))
{
	if (EVENTP(event)) {
		struct frame *f = decode_x_frame(Fselected_frame(Qnil));
		XEvent x_event;
		Widget wid = FRAME_X_TEXT_WIDGET(f);
		Display *display = XtDisplayOfObject(wid);
		struct device *d = get_device_from_display(display);
		struct x_device *xd = DEVICE_X_DATA(d);
		XWindowAttributes win_attrib;
		unsigned int modifier = 0, state = 0;
		char *Ctext;
		int numItems = 0, textlen = 0, pos = 0;
		Lisp_Event *lisp_event = XEVENT(event);
		Lisp_Object item = Qnil;
		struct gcpro gcpro1;

		/* only drag if this is really a press */
		if (EVENT_TYPE(lisp_event) != button_press_event
		    || !LISTP(dragdata))
			return Qnil;

		GCPRO1(item);

		/*
		 * not so cross hack that converts a emacs event back to a XEvent
		 */

		x_event.xbutton.type = ButtonPress;
		x_event.xbutton.send_event = False;
		x_event.xbutton.display = XtDisplayOfObject(wid);
		x_event.xbutton.window = XtWindowOfObject(wid);
		x_event.xbutton.root = XRootWindow(x_event.xbutton.display, 0);
		x_event.xbutton.subwindow = 0;
		x_event.xbutton.time = lisp_event->timestamp;
		x_event.xbutton.x = lisp_event->event.button.x;
		x_event.xbutton.y = lisp_event->event.button.y;
		if (Success == XGetWindowAttributes(x_event.xbutton.display,
						    x_event.xbutton.window,
						    &win_attrib)) {
			x_event.xbutton.x_root =
			    win_attrib.x + lisp_event->event.button.x;
			x_event.xbutton.y_root =
			    win_attrib.y + lisp_event->event.button.y;
		} else {
			x_event.xbutton.x_root = lisp_event->event.button.x;	/* this is wrong */
			x_event.xbutton.y_root = lisp_event->event.button.y;
		}
		modifier = lisp_event->event.button.modifiers;
		if (modifier & XEMACS_MOD_SHIFT)
			state |= ShiftMask;
		if (modifier & XEMACS_MOD_CONTROL)
			state |= ControlMask;
		if (modifier & XEMACS_MOD_META)
			state |= xd->MetaMask;
		if (modifier & XEMACS_MOD_SUPER)
			state |= xd->SuperMask;
		if (modifier & XEMACS_MOD_HYPER)
			state |= xd->HyperMask;
		if (modifier & XEMACS_MOD_ALT)
			state |= xd->AltMask;
		state |= Button1Mask << (lisp_event->event.button.button - 1);

		x_event.xbutton.state = state;
		x_event.xbutton.button = lisp_event->event.button.button;
		x_event.xkey.same_screen = True;

		/* convert data strings into a big string */
		item = dragdata;
		while (!NILP(item)) {
			if (!STRINGP(XCAR(item))) {
				numItems = 0;
				break;
			}
			textlen += XSTRING_LENGTH(XCAR(item)) + 1;
			numItems++;
			item = XCDR(item);
		}

		if (numItems) {
			/*
			 * concatenate all strings given to one large string, with
			 * \0 as separator. List is ended by \0.
			 */
			Ctext = (char*)xmalloc_atomic(textlen + 1);
			Ctext[0] = 0;

			item = dragdata;
			while (!NILP(item)) {
				if (!STRINGP(XCAR(item))) {
					numItems = 0;
					xfree(Ctext);
					Ctext = NULL;
					break;
				}
				strcpy(Ctext + pos,
				       (const char *)XSTRING_DATA(XCAR(item)));
				pos += XSTRING_LENGTH(XCAR(item)) + 1;
				item = XCDR(item);
			}
			Ctext[pos] = 0;

			dnd_convert_cb_rec[0].callback = x_cde_convert_callback;
			dnd_convert_cb_rec[0].closure = (XtPointer) Ctext;
			dnd_convert_cb_rec[1].callback = NULL;
			dnd_convert_cb_rec[1].closure = NULL;

			dnd_destroy_cb_rec[0].callback = x_cde_destroy_callback;
			dnd_destroy_cb_rec[0].closure = (XtPointer) Ctext;
			dnd_destroy_cb_rec[1].callback = NULL;
			dnd_destroy_cb_rec[1].closure = NULL;

			CurrentDragWidget =
			    DtDndDragStart(wid, &x_event,
					   (NILP(dragtype) ?
					    DtDND_BUFFER_TRANSFER :
					    DtDND_FILENAME_TRANSFER), numItems,
					   XmDROP_COPY, dnd_convert_cb_rec,
					   dnd_destroy_cb_rec, NULL, 0);
		}

		UNGCPRO;

		return numItems ? Qt : Qnil;
	}

	return Qnil;
}

static void
x_cde_transfer_callback(Widget widget, XtPointer clientData, XtPointer callData)
{
	char *filePath, *hurl;
	int ii, enqueue = 1;
	Lisp_Object frame = Qnil;
	Lisp_Object l_type = Qnil;
	Lisp_Object l_data = Qnil;
	DtDndTransferCallbackStruct *transferInfo = NULL;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/*
	   this needs to be changed to the new protocol:
	   - we need the button, modifier and pointer states to create a
	   correct misc_user_event
	   - the data must be converted to the new format (URL/MIME)
	 */
	/* return; */

	transferInfo = (DtDndTransferCallbackStruct *) callData;
	if (transferInfo == NULL)
		return;

	GCPRO3(frame, l_type, l_data);

	frame = make_frame((struct frame *)clientData);

	if (transferInfo->dropData->protocol == DtDND_FILENAME_TRANSFER) {
		l_type = Qdragdrop_URL;

		for (ii = 0; ii < transferInfo->dropData->numItems; ii++) {
			filePath = transferInfo->dropData->data.files[ii];
			hurl = dnd_url_hexify_string((char *)filePath, "file:");
			/* #### Mule-izing required */
			l_data = Fcons(make_string((Bufbyte *) hurl,
						   strlen(hurl)), l_data);
			xfree(hurl);
		}
	} else if (transferInfo->dropData->protocol == DtDND_BUFFER_TRANSFER) {
		int speccount = specpdl_depth();

		/* Problem: all buffers a treated as text/plain!!!
		   Solution: Also support DtDND_TEXT_TRANSFER
		   perhaps implementation of the Motif protocol
		   (which is the base of CDE) will clear this */
		l_type = Qdragdrop_MIME;
		record_unwind_protect(abort_current_drag, Qnil);
		drag_not_done = 1;
		for (ii = 0; ii < transferInfo->dropData->numItems; ii++) {
			/* let us forget this name thing for now... */
			/* filePath = transferInfo->dropData->data.buffers[ii].name;
			   path = (filePath == NULL) ? Qnil
			   : make_string ((Bufbyte *)filePath, strlen (filePath)); */
			/* what, if the data is no text, and how can I tell it? */
			l_data =
			    Fcons(list3
				  (list1
				   (make_string((Bufbyte *) "text/plain", 10)),
				   make_string((Bufbyte *) "8bit", 4),
				   make_string((Bufbyte *) transferInfo->
					       dropData->data.buffers[ii].bp,
					       transferInfo->dropData->data.
					       buffers[ii].size)), l_data);
		}
		drag_not_done = 0;
		unbind_to(speccount, Qnil);
	} else			/* the other cases: NOOP_TRANSFER */
		enqueue = 0;

	/* The Problem: no button and mods from CDE... */
	if (enqueue)
		enqueue_misc_user_event_pos(frame, Qdragdrop_drop_dispatch,
					    Fcons(l_type, l_data),
					    0 /* this is the button */ ,
					    0 /* these are the mods */ ,
					    transferInfo->x, transferInfo->y);

	UNGCPRO;
	return;
}
#endif				/* HAVE_CDE */


/************************************************************************/
/*				widget creation				*/
/************************************************************************/

/* The widget hierarchy is

	argv[0]			shell		container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame

   We accept geometry specs in this order:

	*FRAME-NAME.geometry
	*EmacsFrame.geometry
	Emacs.geometry

   Other possibilities for widget hierarchies might be

	argv[0]			frame		container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame
   or
	argv[0]			FRAME-NAME	container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame
   or
	argv[0]			FRAME-NAME	container	emacsTextPane
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame

#ifdef EXTERNAL_WIDGET
   The ExternalShell widget is simply a replacement for the Shell widget
   which is able to deal with using an externally-supplied window instead
   of always creating its own.
#endif

*/

#ifdef EXTERNAL_WIDGET

static int is_valid_window(Window w, struct device *d)
{
	XWindowAttributes xwa;
	Display *dpy = DEVICE_X_DISPLAY(d);

	expect_x_error(dpy);
	XGetWindowAttributes(dpy, w, &xwa);
	return !x_error_occurred_p(dpy);
}

#endif				/* EXTERNAL_WIDGET */

/* This sends a synthetic mouse-motion event to the frame, if the mouse
   is over the frame.  This ensures that the cursor gets set properly
   before the user moves the mouse for the first time. */

static void x_send_synthetic_mouse_event(struct frame *f)
{
	/* #### write this function. */
}

static int first_x_frame_p(struct frame *f)
{
	Lisp_Object rest = DEVICE_FRAME_LIST(XDEVICE(f->device));
	while (!NILP(rest) &&
	       (f == XFRAME(XCAR(rest)) || !FRAME_X_P(XFRAME(XCAR(rest)))))
		rest = XCDR(rest);
	return NILP(rest);
}

/* Figure out what size the EmacsFrame widget should initially be,
   and set it.  Should be called after the default font has been
   determined but before the widget has been realized. */

static void x_initialize_frame_size(struct frame *f)
{
	/* Geometry of the AppShell */
	int app_flags = 0;
	int app_x = 0;
	int app_y = 0;
	unsigned int app_w = 0;
	unsigned int app_h = 0;

	/* Geometry of the EmacsFrame */
	int frame_flags = 0;
	int frame_x = 0;
	int frame_y = 0;
	unsigned int frame_w = 0;
	unsigned int frame_h = 0;

	/* Hairily merged geometry */
	int x = 0;
	int y = 0;
	unsigned int w = 80;
	unsigned int h = 40;
	int flags = 0;

	char *geom = 0, *ew_geom = 0;
	Boolean iconic_p = False, ew_iconic_p = False;

	Widget wmshell = FRAME_X_SHELL_WIDGET(f);
	/* #### This may not be an ApplicationShell any more, with the 'popup
	   frame property. */
	Widget app_shell = XtParent(wmshell);
	Widget ew = FRAME_X_TEXT_WIDGET(f);

/* set the position of the frame's root window now.  When the
   frame was created, the position was initialized to (0,0). */
	{
		struct window *win = XWINDOW(f->root_window);

		WINDOW_LEFT(win) = FRAME_LEFT_BORDER_END(f)
		    + FRAME_LEFT_GUTTER_BOUNDS(f);
		WINDOW_TOP(win) = FRAME_TOP_BORDER_END(f)
		    + FRAME_TOP_GUTTER_BOUNDS(f);

		if (!NILP(f->minibuffer_window)) {
			win = XWINDOW(f->minibuffer_window);
			WINDOW_LEFT(win) = FRAME_LEFT_BORDER_END(f)
			    + FRAME_LEFT_GUTTER_BOUNDS(f);
		}
	}

#ifdef EXTERNAL_WIDGET
	/* If we're an external widget, then the size of the frame is predetermined
	   (by the client) and is not our decision to make. */
	if (FRAME_X_EXTERNAL_WINDOW_P(f))
		return;
#endif

#if 0
	/* #### this junk has not been tested; therefore it's
	   probably wrong.  Doesn't really matter at this point because
	   currently all frames are either top-level or external widgets. */

	/* If we're not our own top-level window, then we shouldn't go messing around
	   with top-level shells or "Emacs.geometry" or any such stuff.  Therefore,
	   we do as follows to determine the size of the frame:

	   1) If a value for the frame's "geometry" resource was specified, then
	   use it.  (This specifies a size in characters.)
	   2) Else, if the "width" and "height" resources were specified, then
	   leave them alone.  (This is a value in pixels.  Sorry, we can't break
	   Xt conventions here.)
	   3) Else, assume a size of 64x12.  (This is somewhat arbitrary, but
	   it's unlikely that a size of 80x40 is desirable because we're probably
	   inside of a dialog box.)

	   Set the widget's x, y, height, and width as determined.  Don't set the
	   top-level container widget, because we don't necessarily know what it
	   is. (Assume it is smart and pays attention to our values.)
	 */

	if (!FRAME_X_TOP_LEVEL_FRAME_P(f)) {
		Xt_GET_VALUE(ew, XtNgeometry, &ew_geom);
		if (ew_geom)
			frame_flags = XParseGeometry(ew_geom,
						     &frame_x, &frame_y,
						     &frame_w, &frame_h);
		if (!(frame_flags & (WidthValue | HeightValue))) {
			Arg al[2];
			XtSetArg(al[0], XtNwidth, &frame_w);
			XtSetArg(al[1], XtNheight, &frame_h);
			XtGetValues(ew, al, 2);
			if (!frame_w && !frame_h) {
				frame_w = 64;
				frame_h = 12;
				frame_flags |= WidthValue | HeightValue;
			}
		}
		if (frame_flags & (WidthValue | HeightValue))
			EmacsFrameSetCharSize(ew, frame_w, frame_h);
		if (frame_flags & (XValue | YValue)) {
			Arg al[2];
			XtSetArg(al[0], XtNwidth, &frame_w);
			XtSetArg(al[1], XtNheight, &frame_h);
			XtGetValues(ew, al, 2);

			if (frame_flags & XNegative)
				frame_x += frame_w;
			if (frame_flags & YNegative)
				frame_y += frame_h;

			XtSetArg(al[0], XtNx, frame_x);
			XtSetArg(al[1], XtNy, frame_y);
			XtSetValues(ew, al, 2);
		}
		return;
	}
#endif

	/* OK, we're a top-level shell. */

	if (!XtIsWMShell(wmshell))
		abort();

	/* If the EmacsFrame doesn't have a geometry but the shell does,
	   treat that as the geometry of the frame.
	   (Is this bogus? I'm not sure.) */

	Xt_GET_VALUE(ew, XtNgeometry, &ew_geom);
	if (!ew_geom) {
		Xt_GET_VALUE(wmshell, XtNgeometry, &geom);
		if (geom) {
			ew_geom = geom;
			Xt_SET_VALUE(ew, XtNgeometry, ew_geom);
		}
	}

	/* If the Shell is iconic, then the EmacsFrame is iconic.
	   (Is this bogus? I'm not sure.) */
	Xt_GET_VALUE(ew, XtNiconic, &ew_iconic_p);
	if (!ew_iconic_p) {
		Xt_GET_VALUE(wmshell, XtNiconic, &iconic_p);
		if (iconic_p) {
			ew_iconic_p = iconic_p;
			Xt_SET_VALUE(ew, XtNiconic, iconic_p);
		}
	}

	Xt_GET_VALUE(app_shell, XtNgeometry, &geom);
	if (geom)
		app_flags =
		    XParseGeometry(geom, &app_x, &app_y, &app_w, &app_h);

	if (ew_geom)
		frame_flags = XParseGeometry(ew_geom,
					     &frame_x, &frame_y,
					     &frame_w, &frame_h);

	if (first_x_frame_p(f)) {
		/* If this is the first frame created:
		   ====================================

		   - Use the ApplicationShell's size/position, if specified.
		   (This is "Emacs.geometry", or the "-geometry" command line arg.)
		   - Else use the EmacsFrame's size/position.
		   (This is "*FRAME-NAME.geometry")

		   - If the AppShell is iconic, the frame should be iconic.

		   AppShell comes first so that -geometry always applies to the first
		   frame created, even if there is an "every frame" entry in the
		   resource database.
		 */
		if (app_flags & (XValue | YValue)) {
			x = app_x;
			y = app_y;
			flags |=
			    (app_flags &
			     (XValue | YValue | XNegative | YNegative));
		} else if (frame_flags & (XValue | YValue)) {
			x = frame_x;
			y = frame_y;
			flags |=
			    (frame_flags &
			     (XValue | YValue | XNegative | YNegative));
		}

		if (app_flags & (WidthValue | HeightValue)) {
			w = app_w;
			h = app_h;
			flags |= (app_flags & (WidthValue | HeightValue));
		} else if (frame_flags & (WidthValue | HeightValue)) {
			w = frame_w;
			h = frame_h;
			flags |= (frame_flags & (WidthValue | HeightValue));
		}

		/* If the AppShell is iconic, then the EmacsFrame is iconic. */
		if (!ew_iconic_p) {
			Xt_GET_VALUE(app_shell, XtNiconic, &iconic_p);
			if (iconic_p) {
				ew_iconic_p = iconic_p;
				Xt_SET_VALUE(ew, XtNiconic, iconic_p);
			}
		}
	} else {
		/* If this is not the first frame created:
		   ========================================

		   - use the EmacsFrame's size/position if specified
		   - Otherwise, use the ApplicationShell's size, but not position.

		   So that means that one can specify the position of the first frame
		   with "Emacs.geometry" or `-geometry'; but can only specify the
		   position of subsequent frames with "*FRAME-NAME.geometry".

		   AppShell comes second so that -geometry does not apply to subsequent
		   frames when there is an "every frame" entry in the resource db,
		   but does apply to the first frame.
		 */
		if (frame_flags & (XValue | YValue)) {
			x = frame_x;
			y = frame_y;
			flags |=
			    (frame_flags &
			     (XValue | YValue | XNegative | YNegative));
		}

		if (frame_flags & (WidthValue | HeightValue)) {
			w = frame_w;
			h = frame_h;
			flags |= (frame_flags & (WidthValue | HeightValue));
		} else if (app_flags & (WidthValue | HeightValue)) {
			w = app_w;
			h = app_h;
			flags |= (app_flags & (WidthValue | HeightValue));
		}
	}

	x_set_initial_frame_size(f, flags, x, y, w, h);
}

static void x_get_layout_sizes(struct frame *f, Dimension * topbreadth)
{
	int i;

	/* compute height of all top-area widgets */
	for (i = 0, *topbreadth = 0; i < FRAME_X_NUM_TOP_WIDGETS(f); i++) {
		Widget wid = FRAME_X_TOP_WIDGETS(f)[i];
		if (wid && XtIsManaged(wid))
			*topbreadth +=
			    wid->core.height + 2 * wid->core.border_width;
	}
}

static void
x_layout_widgets(Widget w, XtPointer client_data, XtPointer call_data)
{
	struct frame *f = (struct frame *)client_data;
	EmacsManagerResizeStruct *emst = (EmacsManagerResizeStruct *) call_data;
	Dimension width = emst->width;
	Dimension height = emst->height;
	Widget text = FRAME_X_TEXT_WIDGET(f);
	Dimension textbord = text->core.border_width;
	Dimension topbreadth;
	Position text_x = 0, text_y = 0;
	int i;

	x_get_layout_sizes(f, &topbreadth);

	/* first the menubar and psheets ... */
	for (i = 0; i < FRAME_X_NUM_TOP_WIDGETS(f); i++) {
		Widget wid = FRAME_X_TOP_WIDGETS(f)[i];
		if (wid && XtIsManaged(wid)) {
			Dimension bord = wid->core.border_width;
			XtConfigureWidget(wid, 0, text_y,
					  width - 2 * bord, wid->core.height,
					  bord);
			text_y += wid->core.height + 2 * bord;
		}
	}

#ifdef HAVE_SCROLLBARS
	f->scrollbar_y_offset = topbreadth + textbord;
#endif

	/* finally the text area */
	{
		Dimension nw = width - 2 * textbord;
		Dimension nh = height - text_y - 2 * textbord;

		if (nh != f->pixheight || nw != f->pixwidth)
			MARK_FRAME_SIZE_SLIPPED(f);
		XtConfigureWidget(text, text_x, text_y, nw, nh, textbord);
	}
}

static void
x_do_query_geometry(Widget w, XtPointer client_data, XtPointer call_data)
{
	struct frame *f = (struct frame *)client_data;
	EmacsManagerQueryGeometryStruct *emst =
	    (EmacsManagerQueryGeometryStruct *) call_data;
	Widget text = FRAME_X_TEXT_WIDGET(f);
	Dimension textbord = text->core.border_width;
	Dimension topbreadth;
	XtWidgetGeometry req, repl;
	int mask = emst->request_mode & (CWWidth | CWHeight);

	x_get_layout_sizes(f, &topbreadth);

	/* Strip away menubar from suggested size, and ask the text widget
	   what size it wants to be.  */
	req.request_mode = mask;
	if (mask & CWWidth)
		req.width = emst->proposed_width - 2 * textbord;
	if (mask & CWHeight)
		req.height = emst->proposed_height - topbreadth - 2 * textbord;
	XtQueryGeometry(text, &req, &repl);

	/* Now add the menubar back again */
	emst->proposed_width = repl.width + 2 * textbord;
	emst->proposed_height = repl.height + topbreadth + 2 * textbord;
}

/* Creates the widgets for a frame.
   lisp_window_id is a Lisp description of an X window or Xt
   widget to parse.
   parent is a frame to use as the parent.
   overridep if non-nil says to set the override-redirect setting.

   This function does not create or map the windows.  (That is
   done by x_popup_frame().)
 */
static void
x_create_widgets(struct frame *f, Lisp_Object lisp_window_id,
		 Lisp_Object parent, Lisp_Object overridep)
{
	struct device *d = XDEVICE(f->device);
	Visual *visual = DEVICE_X_VISUAL(d);
	int depth = DEVICE_X_DEPTH(d);
	Colormap cmap = DEVICE_X_COLORMAP(d);
#ifdef EXTERNAL_WIDGET
	Window window_id = 0;
#endif
	const char *name;
	Arg al[25];
	int ac = 0;
	Widget text, container, shell;
	Widget parentwid = 0;
#ifdef HAVE_MENUBARS
	int menubar_visible;
	Widget menubar;
#endif

	if (STRINGP(f->name))
		LISP_STRING_TO_EXTERNAL(f->name, name, Qctext);
	else
		name = "emacs";

	/* The widget hierarchy is

	   argv[0]                    shell           pane            FRAME-NAME
	   ApplicationShell   EmacsShell      EmacsManager    EmacsFrame

	   (the type of the shell is ExternalShell if this frame is running
	   in another client's window)

	   However the EmacsShell widget has WM_CLASS of FRAME-NAME/Emacs.
	   Normally such shells have name/class shellname/appclass, which in this
	   case would be "shell/Emacs" instead of "frame-name/Emacs".  We could
	   also get around this by naming the shell "frame-name", but that would
	   be confusing because the text area (the EmacsFrame widget inferior of
	   the shell) is also called that.  So we just set the WM_CLASS property.
	 */

#ifndef EXTERNAL_WIDGET
	if (!NILP(lisp_window_id))
		error
		    ("support for external widgets was not enabled at compile-time");
#else
	if (!NILP(lisp_window_id)) {
		char *string;

		CHECK_STRING(lisp_window_id);
		string = (char *)XSTRING_DATA(lisp_window_id);
		if (string[0] == '0' && (string[1] == 'x' || string[1] == 'X'))
			sscanf(string + 2, "%lxu", &window_id);
#if 0
		else if (string[0] == 'w') {
			sscanf(string + 1, "%x", &parent_widget);
			if (parent_widget)
				window_id = XtWindow(parent_widget);
		}
#endif
		else
			sscanf(string, "%lu", &window_id);
		if (!is_valid_window(window_id, d))
			error("Invalid window %lu", (unsigned long)window_id);
		FRAME_X_EXTERNAL_WINDOW_P(f) = 1;
	} else
#endif				/* EXTERNAL_WIDGET */
		FRAME_X_TOP_LEVEL_FRAME_P(f) = 1;

	ac = 0;
	XtSetArg(al[ac], XtNallowShellResize, True);
	ac++;
#ifdef LWLIB_USES_MOTIF
	/* Motif sucks beans.  Without this in here, it will delete the window
	   out from under us when it receives a WM_DESTROY_WINDOW message
	   from the WM. */
	XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING);
	ac++;
#endif

#ifdef EXTERNAL_WIDGET
	if (window_id) {
		XtSetArg(al[ac], XtNwindow, window_id);
		ac++;
	} else
#endif				/* EXTERNAL_WIDGET */
	{
		XtSetArg(al[ac], XtNinput, True);
		ac++;
		XtSetArg(al[ac], XtNminWidthCells, 10);
		ac++;
		XtSetArg(al[ac], XtNminHeightCells, 1);
		ac++;
		XtSetArg(al[ac], XtNvisual, visual);
		ac++;
		XtSetArg(al[ac], XtNdepth, depth);
		ac++;
		XtSetArg(al[ac], XtNcolormap, cmap);
		ac++;
	}

	if (!NILP(overridep)) {
		XtSetArg(al[ac], XtNoverrideRedirect, True);
		ac++;
	}

	/* #### maybe we should check for FRAMEP instead? */
	if (!NILP(parent)) {
		parentwid = FRAME_X_SHELL_WIDGET(XFRAME(parent));
		XtSetArg(al[ac], XtNtransientFor, parentwid);
		ac++;
	}

	shell = XtCreatePopupShell("shell", (
#ifdef EXTERNAL_WIDGET
						    window_id ?
						    externalShellWidgetClass :
#endif
						    parentwid ?
						    transientEmacsShellWidgetClass
						    :
						    topLevelEmacsShellWidgetClass),
				   parentwid ? parentwid :
				   DEVICE_XT_APP_SHELL(d), al, ac);
	FRAME_X_SHELL_WIDGET(f) = shell;
	maybe_set_frame_title_format(shell);

	/* Create the manager widget */
	ac = 0;
	XtSetArg(al[ac], XtNvisual, visual);
	ac++;
	XtSetArg(al[ac], XtNdepth, depth);
	ac++;
	XtSetArg(al[ac], XtNcolormap, cmap);
	ac++;

	container = XtCreateWidget("container",
				   emacsManagerWidgetClass, shell, al, ac);
	FRAME_X_CONTAINER_WIDGET(f) = container;
	XtAddCallback(container, XtNresizeCallback, x_layout_widgets,
		      (XtPointer) f);
	XtAddCallback(container, XtNqueryGeometryCallback, x_do_query_geometry,
		      (XtPointer) f);

	/* Create the text area */
	ac = 0;
	XtSetArg(al[ac], XtNvisual, visual);
	ac++;
	XtSetArg(al[ac], XtNdepth, depth);
	ac++;
	XtSetArg(al[ac], XtNcolormap, cmap);
	ac++;
	XtSetArg(al[ac], XtNborderWidth, 0);
	ac++;			/* should this be settable? */
	XtSetArg(al[ac], XtNemacsFrame, f);
	ac++;
	text = XtCreateWidget(name, emacsFrameClass, container, al, ac);
	FRAME_X_TEXT_WIDGET(f) = text;

#ifdef HAVE_MENUBARS
	/* Create the initial menubar widget. */
	menubar_visible = x_initialize_frame_menubar(f);
	FRAME_X_TOP_WIDGETS(f)[0] = menubar = FRAME_X_MENUBAR_WIDGET(f);
	FRAME_X_NUM_TOP_WIDGETS(f) = 1;

	if (menubar_visible)
		XtManageChild(menubar);
#endif				/* HAVE_MENUBARS */
	XtManageChild(text);
	XtManageChild(container);
}

/* We used to call XtPopup() in x_popup_frame, but that doesn't give
   you control over whether the widget is initially mapped or not
   because XtPopup() makes an unconditional call to XMapRaised().
   Boy, those Xt designers were clever.

   When we first removed it we only kept the XtRealizeWidget call in
   XtPopup.  For everything except HP's that was enough.  For HP's,
   though, the failure to call the popup callbacks resulted in XEmacs
   not accepting any input.  Bizarre but true.  Stupid but true.

   So, in case there are any other gotchas floating out there along
   the same lines I've duplicated the majority of XtPopup here.  It
   assumes no grabs and that the widget is not already popped up, both
   valid assumptions for the one place this is called from. */
static void xemacs_XtPopup(Widget widget)
{
	ShellWidget shell_widget = (ShellWidget) widget;
	XtGrabKind call_data = XtGrabNone;

	XtCallCallbacks(widget, XtNpopupCallback, (XtPointer) & call_data);

	shell_widget->shell.popped_up = TRUE;
	shell_widget->shell.grab_kind = XtGrabNone;
	shell_widget->shell.spring_loaded = False;

	if (shell_widget->shell.create_popup_child_proc != NULL)
		(*(shell_widget->shell.create_popup_child_proc)) (widget);

	/* The XtSetValues below are not in XtPopup menu.  We just want to
	   make absolutely sure... */
	Xt_SET_VALUE(widget, XtNmappedWhenManaged, False);
	XtRealizeWidget(widget);
	Xt_SET_VALUE(widget, XtNmappedWhenManaged, True);
}

/* create the windows for the specified frame and display them.
   Note that the widgets have already been created, and any
   necessary geometry calculations have already been done. */
static void x_popup_frame(struct frame *f)
{
	Widget shell_widget = FRAME_X_SHELL_WIDGET(f);
	Widget frame_widget = FRAME_X_TEXT_WIDGET(f);
	struct device *d = XDEVICE(FRAME_DEVICE(f));

	/* Before mapping the window, make sure that the WMShell's notion of
	   whether it should be iconified is synchronized with the EmacsFrame's
	   notion.
	 */
	if (FRAME_X_TOP_LEVEL_FRAME_P(f))
		x_wm_set_shell_iconic_p(shell_widget,
					((EmacsFrame) frame_widget)
					->emacs_frame.iconic);

	xemacs_XtPopup(shell_widget);

	if (!((EmacsFrame) frame_widget)->emacs_frame.initially_unmapped)
		XtMapWidget(shell_widget);
	else {
		/* We may have set f->visible to 1 in x_init_frame(), so undo
		   that now. */
		FRAME_X_TOTALLY_VISIBLE_P(f) = 0;
		f->visible = 0;
	}

#ifdef EXTERNAL_WIDGET
	if (FRAME_X_EXTERNAL_WINDOW_P(f))
		ExternalShellReady(shell_widget, XtWindow(frame_widget),
				   KeyPressMask);
	else
#endif
	if (FRAME_X_TOP_LEVEL_FRAME_P(f)) {
		/* tell the window manager about us. */
		x_wm_store_class_hints(shell_widget, XtName(frame_widget));

#ifndef HAVE_WMCOMMAND
		x_wm_maybe_store_wm_command(f);
#endif				/* HAVE_WMCOMMAND */

		x_wm_hack_wm_protocols(shell_widget);
	}
#ifdef HAVE_XIM
	XIM_init_frame(f);
#endif				/* HAVE_XIM */

#ifdef HACK_EDITRES
	/* Allow XEmacs to respond to EditRes requests.  See the O'Reilly Xt */
	/* Intrinsics Programming Manual, Motif Edition, Aug 1993, Sect 14.14, */
	/* pp. 483-493. */
	XtAddEventHandler(shell_widget,	/* the shell widget in question */
			  (EventMask) NoEventMask,	/* OR with existing mask */
			  True,	/* called on non-maskable events? */
			  (XtEventHandler) _XEditResCheckMessages,	/* the handler */
			  NULL);
#endif				/* HACK_EDITRES */

#ifdef HAVE_CDE
	{
		XtCallbackRec dnd_transfer_cb_rec[2];

		dnd_transfer_cb_rec[0].callback = x_cde_transfer_callback;
		dnd_transfer_cb_rec[0].closure = (XtPointer) f;
		dnd_transfer_cb_rec[1].callback = NULL;
		dnd_transfer_cb_rec[1].closure = NULL;

		DtDndVaDropRegister(FRAME_X_TEXT_WIDGET(f),
				    DtDND_FILENAME_TRANSFER |
				    DtDND_BUFFER_TRANSFER, XmDROP_COPY,
				    dnd_transfer_cb_rec, DtNtextIsBuffer, True,
				    DtNregisterChildren, True,
				    DtNpreserveRegistration, False, NULL);
	}
#endif				/* HAVE_CDE */

	/* Do a stupid property change to force the server to generate a
	   propertyNotify event so that the event_stream server timestamp will
	   be initialized to something relevant to the time we created the window.
	 */
	XChangeProperty(XtDisplay(frame_widget), XtWindow(frame_widget),
			DEVICE_XATOM_WM_PROTOCOLS(d), XA_ATOM, 32,
			PropModeAppend, (unsigned char *)NULL, 0);

	x_send_synthetic_mouse_event(f);
}

static void allocate_x_frame_struct(struct frame *f)
{
	/* zero out all slots. */
	f->frame_data = xnew_and_zero(struct x_frame);

	/* yeah, except the lisp ones */
	FRAME_X_ICON_PIXMAP(f) = Qnil;
	FRAME_X_ICON_PIXMAP_MASK(f) = Qnil;
}

/************************************************************************/
/*				Lisp functions				*/
/************************************************************************/

static void x_init_frame_1(struct frame *f, Lisp_Object props)
{
	/* This function can GC */
	Lisp_Object device = FRAME_DEVICE(f);
	Lisp_Object lisp_window_id = Fplist_get(props, Qwindow_id, Qnil);
	Lisp_Object popup = Fplist_get(props, Qpopup, Qnil);
	Lisp_Object overridep = Fplist_get (props, Qoverride_redirect, Qnil);

	if (!NILP(popup)) {
		if (EQ(popup, Qt))
			popup = Fselected_frame(device);
		CHECK_LIVE_FRAME(popup);
		if (!EQ(device, FRAME_DEVICE(XFRAME(popup))))
			signal_simple_error_2
			    ("Parent must be on same device as frame", device,
			     popup);
	}

	/*
	 * Previously we set this only if NILP (DEVICE_SELECTED_FRAME (d))
	 * to make sure that messages were displayed as soon as possible
	 * if we're creating the first frame on a device.  But it is
	 * better to just set this all the time, so that when a new frame
	 * is created that covers the selected frame, echo area status
	 * messages can still be seen.  f->visible is reset later if the
	 * initially-unmapped property is found to be non-nil in the
	 * frame properties.
	 */
	f->visible = 1;

	allocate_x_frame_struct(f);
	x_create_widgets(f, lisp_window_id, popup, overridep);
}

static void x_init_frame_2(struct frame *f, Lisp_Object props)
{
	/* Set up the values of the widget/frame.  A case could be made for putting
	   this inside of the widget's initialize method. */

	update_frame_face_values(f);
	x_initialize_frame_size(f);
	/* Kyle:
	 *   update_frame_title() can't be done here, because some of the
	 *   modeline specs depend on the frame's device having a selected
	 *   frame, and that may not have been set up yet.  The redisplay
	 *   will update the frame title anyway, so nothing is lost.
	 * JV:
	 *   It turns out it gives problems with FVWMs name based mapping.
	 *   We'll just  need to be careful in the modeline specs.
	 */
	update_frame_title(f);
}

static void x_init_frame_3(struct frame *f)
{
	/* Pop up the frame. */

	x_popup_frame(f);
}

static void x_mark_frame(struct frame *f)
{
	mark_object(FRAME_X_ICON_PIXMAP(f));
	mark_object(FRAME_X_ICON_PIXMAP_MASK(f));
}

static void x_set_frame_icon(struct frame *f)
{
	Pixmap x_pixmap, x_mask;

	if (IMAGE_INSTANCEP(f->icon)
	    && IMAGE_INSTANCE_PIXMAP_TYPE_P(XIMAGE_INSTANCE(f->icon))) {
		x_pixmap = XIMAGE_INSTANCE_X_PIXMAP(f->icon);
		x_mask = XIMAGE_INSTANCE_X_MASK(f->icon);
	} else {
		x_pixmap = 0;
		x_mask = 0;
	}

	/* Store the X data into the widget. */
	{
		Arg al[2];
		XtSetArg(al[0], XtNiconPixmap, x_pixmap);
		XtSetArg(al[1], XtNiconMask, x_mask);
		XtSetValues(FRAME_X_SHELL_WIDGET(f), al, 2);
	}
}

static void x_set_frame_pointer(struct frame *f)
{
	XDefineCursor(XtDisplay(FRAME_X_TEXT_WIDGET(f)),
		      XtWindow(FRAME_X_TEXT_WIDGET(f)),
		      XIMAGE_INSTANCE_X_CURSOR(f->pointer));
	XSync(XtDisplay(FRAME_X_TEXT_WIDGET(f)), 0);
}

static Lisp_Object x_get_frame_parent(struct frame *f)
{
	Widget parentwid = 0;

	Xt_GET_VALUE(FRAME_X_SHELL_WIDGET(f), XtNtransientFor, &parentwid);
	/* find the frame whose wid is parentwid */
	if (parentwid) {
		Lisp_Object frmcons;
		DEVICE_FRAME_LOOP(frmcons, XDEVICE(FRAME_DEVICE(f))) {
			Lisp_Object frame = XCAR(frmcons);
			if (FRAME_X_SHELL_WIDGET(XFRAME(frame)) == parentwid)
				return frame;
		}
	}
	return Qnil;
}

DEFUN("x-window-id", Fx_window_id, 0, 1, 0,	/*
Get the ID of the X11 window.
This gives us a chance to manipulate the Emacs window from within a
different program.  Since the ID is an unsigned long, we return it as
a string.
*/
      (frame))
{
	char str[255];
	struct frame *f = decode_x_frame(frame);

	int sz = snprintf(str, sizeof(str), "%lu", XtWindow(FRAME_X_TEXT_WIDGET(f)));
	assert(sz >= 0 && (size_t)sz < sizeof(str));
	return build_string(str);
}

/************************************************************************/
/*			manipulating the X window			*/
/************************************************************************/

static void x_set_frame_position(struct frame *f, int xoff, int yoff)
{
	Widget w = FRAME_X_SHELL_WIDGET(f);
	Display *dpy = XtDisplay(w);
	Dimension frame_w = DisplayWidth(dpy, DefaultScreen(dpy));
	Dimension frame_h = DisplayHeight(dpy, DefaultScreen(dpy));
	Dimension shell_w, shell_h, shell_bord;
	int win_gravity;
	Arg al[3];

	XtSetArg(al[0], XtNwidth, &shell_w);
	XtSetArg(al[1], XtNheight, &shell_h);
	XtSetArg(al[2], XtNborderWidth, &shell_bord);
	XtGetValues(w, al, 3);

	win_gravity =
	    xoff >= 0 && yoff >= 0 ? NorthWestGravity :
	    xoff >= 0 ? SouthWestGravity :
	    yoff >= 0 ? NorthEastGravity : SouthEastGravity;
	if (xoff < 0)
		xoff += frame_w - shell_w - 2 * shell_bord;
	if (yoff < 0)
		yoff += frame_h - shell_h - 2 * shell_bord;

	/* Update the hints so that, if this window is currently iconified, it will
	   come back at the right place.  We can't look at s->visible to determine
	   whether it is iconified because it might not be up-to-date yet (the queue
	   might not be processed). */
	XtSetArg(al[0], XtNwinGravity, win_gravity);
	XtSetArg(al[1], XtNx, xoff);
	XtSetArg(al[2], XtNy, yoff);
	XtSetValues(w, al, 3);

	/* Sometimes you will find that

	   (set-frame-position (selected-frame) -50 -50)

	   doesn't put the frame where you expect it to: i.e. it's closer to
	   the lower-right corner than it should be, and it appears that the
	   size of the WM decorations was not taken into account.  This is
	   *not* a problem with this function.  Both mwm and twm have bugs
	   in handling this situation. (mwm ignores the window gravity and
	   always assumes NorthWest, except the first time you map the
	   window; twm gets things almost right, but forgets to account for
	   the border width of the top-level window.) This function does
	   what it's supposed to according to the ICCCM, and I'm not about
	   to hack around window-manager bugs. */

#if 0
	/* This is not necessary under either mwm or twm */
	x_wm_mark_shell_position_user_specified(w);
#endif
}

/* Call this to change the size of frame S's x-window. */

static void x_set_frame_size(struct frame *f, int cols, int rows)
{
	EmacsFrameSetCharSize(FRAME_X_TEXT_WIDGET(f), cols, rows);
#if 0
	/* this is not correct.  x_set_frame_size() is called from
	   Fset_frame_size(), which may or may not have been called
	   by the user (e.g. update_EmacsFrame() calls it when the font
	   changes).  For now, don't bother with getting this right. */
	x_wm_mark_shell_size_user_specified(FRAME_X_SHELL_WIDGET(f));
#endif
}

static void x_set_mouse_position(struct window *w, int x, int y)
{
	struct frame *f = XFRAME(w->frame);

	Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));
	XWarpPointer(display, None, XtWindow(FRAME_X_TEXT_WIDGET(f)),
		     0, 0, 0, 0, w->pixel_left + x, w->pixel_top + y);
}

static int
x_get_mouse_position(struct device *d, Lisp_Object * frame, int *x, int *y)
{
	Display *display = DEVICE_X_DISPLAY(d);
	Window child_window;
	Window root_window;
	Window win;
	int root_x, root_y;
	int win_x, win_y;
	unsigned int keys_and_buttons;
	struct frame *f;

	if (XQueryPointer(display, RootWindow(display, DefaultScreen(display)),
			  &root_window, &child_window, &root_x, &root_y,
			  &win_x, &win_y, &keys_and_buttons) == False)
		return 0;

	if (child_window == None)
		return 0;	/* not over any window. */

	while (1) {
		win = child_window;
		if (XTranslateCoordinates
		    (display, root_window, win, root_x, root_y, &win_x, &win_y,
		     &child_window) == False)
			/* Huh? */
			return 0;

		if (child_window == None)
			break;
	}

	/* At this point, win is the innermost window containing the pointer
	   and win_x and win_y are the coordinates of that window. */
	f = x_any_window_to_frame(d, win);
	if (!f)
		return 0;
	XSETFRAME(*frame, f);

	if (XTranslateCoordinates(display, win,
				  XtWindow(FRAME_X_TEXT_WIDGET(f)),
				  win_x, win_y, x, y, &child_window) == False)
		/* Huh? */
		return 0;

	return 1;
}

static void x_cant_notify_wm_error(void)
{
	error("Can't notify window manager of iconification.");
}

/* Raise frame F.  */
static void x_raise_frame_1(struct frame *f, int force)
{
	if (FRAME_VISIBLE_P(f) || force) {
		Widget bottom_dialog;
		XWindowChanges xwc;
		unsigned int flags;
		Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));
		Window emacs_window = XtWindow(FRAME_X_SHELL_WIDGET(f));

		/* first raises all the dialog boxes, then put emacs just below the
		 * bottom most dialog box */
		bottom_dialog = lw_raise_all_pop_up_widgets();
		if (bottom_dialog && XtWindow(bottom_dialog)) {
			xwc.sibling = XtWindow(bottom_dialog);
			xwc.stack_mode = Below;
			flags = CWSibling | CWStackMode;
		} else {
			xwc.stack_mode = Above;
			flags = CWStackMode;
		}

		if (!XReconfigureWMWindow(display, emacs_window,
					  DefaultScreen(display), flags, &xwc))
			x_cant_notify_wm_error();
	}
}

static void x_raise_frame(struct frame *f)
{
	x_raise_frame_1(f, 1);
}

/* Lower frame F.  */
static void x_lower_frame(struct frame *f)
{
	if (FRAME_VISIBLE_P(f)) {
		Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));
		XWindowChanges xwc;
		unsigned int flags = CWStackMode;

		xwc.stack_mode = Below;
		if (!XReconfigureWMWindow
		    (display, XtWindow(FRAME_X_SHELL_WIDGET(f)),
		     DefaultScreen(display), flags, &xwc))
			x_cant_notify_wm_error();
	}
}

static void x_enable_frame(struct frame *f)
{
	XtSetSensitive(FRAME_X_SHELL_WIDGET(f), True);
}

static void x_disable_frame(struct frame *f)
{
	XtSetSensitive(FRAME_X_SHELL_WIDGET(f), False);
}

/* Change from withdrawn state to mapped state. */
static void x_make_frame_visible(struct frame *f)
{
	Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));

	if (!FRAME_VISIBLE_P(f))
		XMapRaised(display, XtWindow(FRAME_X_SHELL_WIDGET(f)));
	else
		x_raise_frame_1(f, 0);
}

/* Change from mapped state to withdrawn state. */
static void x_make_frame_invisible(struct frame *f)
{
	Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));

	if (!FRAME_VISIBLE_P(f))
		return;

	if (!XWithdrawWindow(display,
			     XtWindow(FRAME_X_SHELL_WIDGET(f)),
			     DefaultScreen(display)))
		x_cant_notify_wm_error();
}

static int x_frame_visible_p(struct frame *f)
{
#if 0
	Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));
	XWindowAttributes xwa;
	int result;

	/* JV:
	   This is bad, very bad :-(
	   It is not compatible with our tristate visible and
	   it should never ever change the visibility for us, this leads to
	   the frame-freeze problem under fvwm because with the pager

	   Mappedness != Viewability != Visibility != Emacs f->visible

	   This first unequalness is the reason for the frame freezing problem
	   under fvwm (it happens when the frame is another fvwm-page)

	   The second unequalness happen when it is on the same fvwm-page
	   but in an invisible part of the visible screen.

	   For now we just return the XEmacs internal value --- which might not be up
	   to date. Is that a problem? ---. Otherwise we should
	   use async visibility like in standard Emacs.
	 */

	if (!XGetWindowAttributes(display,
				  XtWindow(FRAME_X_SHELL_WIDGET(f)), &xwa))
		result = 0;
	else
		result = xwa.map_state == IsViewable;
	/* In this implementation it should at least be != IsUnmapped
	   JV */

	f->visible = result;
	return result;
#endif				/* 0 */

	return f->visible;
}

static int x_frame_totally_visible_p(struct frame *f)
{
	return FRAME_X_TOTALLY_VISIBLE_P(f);
}

/* Change window state from mapped to iconified. */
static void x_iconify_frame(struct frame *f)
{
	Display *display = DEVICE_X_DISPLAY(XDEVICE(f->device));

	if (!XIconifyWindow(display,
			    XtWindow(FRAME_X_SHELL_WIDGET(f)),
			    DefaultScreen(display)))
		x_cant_notify_wm_error();

	f->iconified = 1;
}

/* Sets the X focus to frame f. */
static void x_focus_on_frame(struct frame *f)
{
	XWindowAttributes xwa;
	Widget shell_widget;
	int viewable = 0;

	assert(FRAME_X_P(f));

	shell_widget = FRAME_X_SHELL_WIDGET(f);
	if (!XtWindow(shell_widget))
		return;

#ifdef EXTERNAL_WIDGET
	if (FRAME_X_EXTERNAL_WINDOW_P(f))
		ExternalShellSetFocus(shell_widget);
#endif				/* EXTERNAL_WIDGET */

	/* Do the ICCCM focus change if the window is still visible.
	   The s->visible flag might not be up-to-date, because we might
	   not have processed magic events recently.  So make a server
	   round-trip to find out whether it's really mapped right now.
	   We grab the server to do this, because that's the only way to
	   eliminate the race condition.
	 */
	XGrabServer(XtDisplay(shell_widget));
	if (XGetWindowAttributes(XtDisplay(shell_widget),
				 XtWindow(shell_widget), &xwa))
		/* JV: it is bad to change the visibility like this, so we don't for the
		   moment, at least change_frame_visibility should be called
		   Note also that under fvwm a frame can be Viewable (and thus Mapped)
		   but still X-invisible
		   f->visible = xwa.map_state == IsViewable; */
		viewable = xwa.map_state == IsViewable;

	if (viewable) {
		Window focus;
		int revert_to;
		XGetInputFocus(XtDisplay(shell_widget), &focus, &revert_to);
		/* Don't explicitly set the focus on this window unless the focus
		   was on some other window (not PointerRoot).  Note that, even when
		   running a point-to-type window manager like *twm, there is always
		   a focus window; the window manager maintains that based on the
		   mouse position.  If you set the "NoTitleFocus" option in these
		   window managers, then the server itself maintains the focus via
		   PointerRoot, and changing that to focus on the window would make
		   the window grab the focus.  Very bad.
		 */
		if (focus != PointerRoot) {
			XSetInputFocus(XtDisplay(shell_widget),
				       XtWindow(shell_widget),
				       RevertToParent,
				       DEVICE_X_MOUSE_TIMESTAMP
				       (XDEVICE(FRAME_DEVICE(f))));
			XFlush(XtDisplay(shell_widget));
		}
	}
	XUngrabServer(XtDisplay(shell_widget));
	XFlush(XtDisplay(shell_widget));	/* hey, I'd like to DEBUG this... */
}

/* Destroy the X window of frame F.  */
static void x_delete_frame(struct frame *f)
{
	Display *dpy;

#ifndef HAVE_WMCOMMAND
	if (FRAME_X_TOP_LEVEL_FRAME_P(f))
		x_wm_maybe_move_wm_command(f);
#endif				/* HAVE_WMCOMMAND */

#ifdef HAVE_CDE
	DtDndDropUnregister(FRAME_X_TEXT_WIDGET(f));
#endif				/* HAVE_CDE */

	assert(FRAME_X_SHELL_WIDGET(f) != 0);
	dpy = XtDisplay(FRAME_X_SHELL_WIDGET(f));

#ifdef EXTERNAL_WIDGET
	expect_x_error(dpy);
	/* for obscure reasons having (I think) to do with the internal
	   window-to-widget hierarchy maintained by Xt, we have to call
	   XtUnrealizeWidget() here.  Xt can really suck. */
	if (f->being_deleted)
		XtUnrealizeWidget(FRAME_X_SHELL_WIDGET(f));
	XtDestroyWidget(FRAME_X_SHELL_WIDGET(f));
	x_error_occurred_p(dpy);
#else
	XtDestroyWidget(FRAME_X_SHELL_WIDGET(f));
	/* make sure the windows are really gone! */
	/* #### Is this REALLY necessary? */
	XFlush(dpy);
#endif				/* EXTERNAL_WIDGET */

	FRAME_X_SHELL_WIDGET(f) = 0;

	if (FRAME_X_GEOM_FREE_ME_PLEASE(f)) {
		xfree(FRAME_X_GEOM_FREE_ME_PLEASE(f));
		FRAME_X_GEOM_FREE_ME_PLEASE(f) = 0;
	}

	if (f->frame_data) {
		xfree(f->frame_data);
		f->frame_data = 0;
	}
}

static void x_update_frame_external_traits(struct frame *frm, Lisp_Object name)
{
	Arg al[10];
	int ac = 0;
	Lisp_Object frame;

	XSETFRAME(frame, frm);

	if (EQ(name, Qforeground)) {
		Lisp_Object color = FACE_FOREGROUND(Vdefault_face, frame);
		XColor fgc;

		if (!EQ(color, Vthe_null_color_instance)) {
			fgc = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(color));
			XtSetArg(al[ac], XtNforeground, (void *)fgc.pixel);
			ac++;
		}
	} else if (EQ(name, Qbackground)) {
		Lisp_Object color = FACE_BACKGROUND(Vdefault_face, frame);
		XColor bgc;

		if (!EQ(color, Vthe_null_color_instance)) {
			bgc = COLOR_INSTANCE_X_COLOR(XCOLOR_INSTANCE(color));
			XtSetArg(al[ac], XtNbackground, (void *)bgc.pixel);
			ac++;
		}

		/* Really crappy way to force the modeline shadows to be
		   redrawn.  But effective. */
		MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(frm);
		MARK_FRAME_CHANGED(frm);
	} else if (EQ(name, Qfont)) {
		Lisp_Object font =
		    FACE_FONT(Vdefault_face, frame, Vcharset_ascii);

		if (!EQ(font, Vthe_null_font_instance)) {
			XtSetArg(al[ac], XtNfont,
				 (void *)
				 FONT_INSTANCE_X_FONT(XFONT_INSTANCE(font)));
			ac++;
		}
	} else
		abort();

	XtSetValues(FRAME_X_TEXT_WIDGET(frm), al, ac);

#ifdef HAVE_TOOLBARS
	/* Setting the background clears the entire frame area
	   including the toolbar so we force an immediate redraw of
	   it. */
	if (EQ(name, Qbackground))
		MAYBE_DEVMETH(XDEVICE(frm->device), redraw_frame_toolbars,
			      (frm));
#endif				/* HAVE_TOOLBARS */

	/* Set window manager resize increment hints according to
	   the new character size */
	if (EQ(name, Qfont))
		EmacsFrameRecomputeCellSize(FRAME_X_TEXT_WIDGET(frm));
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_frame_x(void)
{
	defsymbol(&Qwindow_id, "window-id");
	defsymbol(&Qoverride_redirect, "override-redirect");
	defsymbol(&Qx_resource_name, "x-resource-name");

	DEFSUBR(Fx_window_id);
#ifdef HAVE_CDE
	DEFSUBR(Fcde_start_drag_internal);
#endif
}

void console_type_create_frame_x(void)
{
	/* frame methods */
	CONSOLE_HAS_METHOD(x, init_frame_1);
	CONSOLE_HAS_METHOD(x, init_frame_2);
	CONSOLE_HAS_METHOD(x, init_frame_3);
	CONSOLE_HAS_METHOD(x, mark_frame);
	CONSOLE_HAS_METHOD(x, focus_on_frame);
	CONSOLE_HAS_METHOD(x, delete_frame);
	CONSOLE_HAS_METHOD(x, get_mouse_position);
	CONSOLE_HAS_METHOD(x, set_mouse_position);
	CONSOLE_HAS_METHOD(x, raise_frame);
	CONSOLE_HAS_METHOD(x, lower_frame);
	CONSOLE_HAS_METHOD(x, enable_frame);
	CONSOLE_HAS_METHOD(x, disable_frame);
	CONSOLE_HAS_METHOD(x, make_frame_visible);
	CONSOLE_HAS_METHOD(x, make_frame_invisible);
	CONSOLE_HAS_METHOD(x, iconify_frame);
	CONSOLE_HAS_METHOD(x, set_frame_size);
	CONSOLE_HAS_METHOD(x, set_frame_position);
	CONSOLE_HAS_METHOD(x, frame_property);
	CONSOLE_HAS_METHOD(x, internal_frame_property_p);
	CONSOLE_HAS_METHOD(x, frame_properties);
	CONSOLE_HAS_METHOD(x, set_frame_properties);
	CONSOLE_HAS_METHOD(x, set_title_from_bufbyte);
	CONSOLE_HAS_METHOD(x, set_icon_name_from_bufbyte);
	CONSOLE_HAS_METHOD(x, frame_visible_p);
	CONSOLE_HAS_METHOD(x, frame_totally_visible_p);
	CONSOLE_HAS_METHOD(x, frame_iconified_p);
	CONSOLE_HAS_METHOD(x, set_frame_pointer);
	CONSOLE_HAS_METHOD(x, set_frame_icon);
	CONSOLE_HAS_METHOD(x, get_frame_parent);
	CONSOLE_HAS_METHOD(x, update_frame_external_traits);
}

void vars_of_frame_x(void)
{
#ifdef EXTERNAL_WIDGET
	Fprovide(intern("external-widget"));
#endif

	/* this call uses only safe functions from emacs.c */
	init_x_prop_symbols();

	DEFVAR_LISP("default-x-frame-plist", &Vdefault_x_frame_plist	/*
Plist of default frame-creation properties for X frames.
These override what is specified in the resource database and in
`default-frame-plist', but are overridden by the arguments to the
particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

window-id                    The X window ID corresponding to the
			     frame.  May be set only at startup, and
			     only if external widget support was
			     compiled in; doing so causes the frame
			     to be created as an "external widget"
			     in another program that uses an existing
			     window in the program rather than creating
			     a new one.
initially-unmapped           If non-nil, the frame will not be visible
			     when it is created.  In this case, you
			     need to call `make-frame-visible' to make
			     the frame appear.
popup                        If non-nil, it should be a frame, and this
			     frame will be created as a "popup" frame
			     whose parent is the given frame.  This
			     will make the window manager treat the
			     frame as a dialog box, which may entail
			     doing different things (e.g. not asking
			     for positioning, and not iconifying
			     separate from its parent).
override-redirect	     If non-nil, the frame will not be subject to
			     window-manager control.  In particular, it
			     will lack decorations, for more attractive
			     appearance of balloon help, aka tooltips.
inter-line-space             Not currently implemented.
toolbar-shadow-thickness     Thickness of toolbar shadows.
background-toolbar-color     Color of toolbar background.
bottom-toolbar-shadow-color  Color of bottom shadows on toolbars.
			     (*Not* specific to the bottom-toolbar.)
top-toolbar-shadow-color     Color of top shadows on toolbars.
			     (*Not* specific to the top-toolbar.)
internal-border-width        Width of internal border around text area.
border-width                 Width of external border around text area.
top                          Y position (in pixels) of the upper-left
			     outermost corner of the frame (i.e. the
			     upper-left of the window-manager
			     decorations).
left                         X position (in pixels) of the upper-left
			     outermost corner of the frame (i.e. the
			     upper-left of the window-manager
			     decorations).
border-color                 Color of external border around text area.
cursor-color                 Color of text cursor.

See also `default-frame-plist', which specifies properties which apply
to all frames, not just X frames.
									 */ );
	Vdefault_x_frame_plist = Qnil;

	x_console_methods->device_specific_frame_props =
	    &Vdefault_x_frame_plist;
}
