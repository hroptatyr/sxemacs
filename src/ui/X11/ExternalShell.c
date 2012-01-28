/* External shell widget.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.

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

/* Written by Ben Wing, September 1993. */

/* This is a special Shell that is designed to use an externally-
   provided window created by someone else (possibly another process).
   That other window should have an associated widget of class
   ExternalClient.  The two widgets communicate with each other using
   ClientMessage events and properties on the external window.

   Ideally this feature should be independent of Emacs.  Unfortunately
   there are lots and lots of specifics that need to be dealt with
   for this to work properly, and some of them can't conveniently
   be handled within the widget's methods.  Some day the code may
   be rewritten so that the embedded-widget feature can be used by
   any application, with appropriate entry points that are called
   at specific points within the application.

   This feature is similar to the OLE (Object Linking & Embedding)
   feature provided by MS Windows.
 */

#ifdef emacs

#include <config.h>

#ifndef EXTERNAL_WIDGET
ERROR ! This ought not be getting compiled if EXTERNAL_WIDGET
	is undefined
#endif
#endif				/* emacs */
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include "ExternalShellP.h"
#include "ui/X11/extw-Xt.h"
#ifdef emacs
	extern void emacs_Xt_handle_focus_event(XEvent * event);
#endif

/* Communication between this shell and the client widget:

   Communication is through ClientMessage events with message_type
   EXTW_NOTIFY and format 32.  Both the shell and the client widget
   communicate with each other by sending the message to the same
   window (the "external window" below), and the data.l[0] value is
   used to determine who sent the message.

   The data is formatted as follows:

   data.l[0] = who sent this message: external_shell_send (0) or
	       external_client_send (1)
   data.l[1] = message type (see enum en_extw_notify below)
   data.l[2-4] = data associated with this message

   EventHandler() handles messages from the other side.

   extw_send_notify_3() sends a message to the other side.

   extw_send_geometry_value() is used when an XtWidgetGeometry structure
      needs to be sent.  This is too much data to fit into a
      ClientMessage, so the data is stored in a property and then
      extw_send_notify_3() is called.

   extw_get_geometry_value() receives an XtWidgetGeometry structure from a
      property.

   extw_wait_for_response() is used when a response to a sent message
      is expected.  It looks for a matching event within a
      particular timeout.

   The particular message types are as follows:

1) extw_notify_init (event_window, event_mask)

   This is sent from the shell to the client after the shell realizes
   its EmacsFrame widget on the client's "external window".  This
   tells the client that it should start passing along events of the
   types specified in event_mask.  event_window specifies the window
   of the EmacsFrame widget, which is a child of the client's
   external window.

   extw_notify_init (client_type)

   When the client receives an extw_notify_init message from the
   shell, it sends back a message of the same sort specifying the type
   of the toolkit used by the client (Motif, generic Xt, or Xlib).

2) extw_notify_end ()

   This is sent from the shell to the client when the shell's
   EmacsFrame widget is destroyed, and tells the client to stop
   passing events along.

3) extw_notify_qg (result)

   This is sent from the client to the shell when a QueryGeometry
   request is received on the client.  The XtWidgetGeometry structure
   specified in the QueryGeometry request is passed on in the
   EXTW_QUERY_GEOMETRY property (of type EXTW_WIDGET_GEOMETRY) on the
   external window.  result is unused.

   In response, the shell passes the QueryGeometry request down the
   widget tree, and when a response is received, sends a message of
   type extw_notify_qg back to the client, with result specifying the
   GeometryResult value.  If this value is XtGeometryAlmost, the
   returned XtWidgetGeometry structure is stored into the same property
   as above. [BPW is there a possible race condition here?]

4) extw_notify_gm (result)

   A very similar procedure to that for extw_notify_qg is followed
   when the shell's RootGeometryManager method is called, indicating
   that a child widget wishes to change the shell's geometry.  The
   XtWidgetGeometry structure is stored in the EXTW_GEOMETRY_MANAGER
   property.

5) extw_notify_focus_in (), extw_notify_focus_out ()

   These are sent from the client to the shell when the client gains
   or loses the keyboard focus.  It is done this way because Xt
   maintains its own concept of keyboard focus and only the client
   knows this information.
*/

#define NOTIFY(w, type, l0, l1, l2) \
  extw_send_notify_3(XtDisplay((Widget)(w)),\
   (w)->externalShell.external_window, type, l0, l1, l2)

static void ExternalShellInitialize(Widget req, Widget new, ArgList args,
				    Cardinal * num_args);
static void ExternalShellRealize(Widget wid, Mask * vmask, XSetWindowAttributes
				 * attr);
static void ExternalShellDestroy(Widget w);
static void ChangeManaged(Widget wid);
static XtGeometryResult ExternalShellRootGeometryManager(Widget gw,
							 XtWidgetGeometry *
							 request,
							 XtWidgetGeometry *
							 reply);
static void EventHandler(Widget wid, XtPointer closure, XEvent * event,
			 Boolean * continue_to_dispatch);

#ifndef DEFAULT_WM_TIMEOUT
# define DEFAULT_WM_TIMEOUT 5000
#endif

void ExternalShellUnrealize(Widget w);

static XtResource resources[] = {
#define offset(field) XtOffset(ExternalShellWidget, externalShell.field)
	{XtNwindow, XtCWindow,
	 XtRWindow, sizeof(Window),
	 offset(external_window), XtRImmediate, (XtPointer) 0}
	,
	{XtNclientTimeout, XtCClientTimeout,
	 XtRInt, sizeof(int),
	 offset(client_timeout), XtRImmediate, (XtPointer) DEFAULT_WM_TIMEOUT},
	{XtNdeadClient, XtCDeadClient,
	 XtRBoolean, sizeof(Boolean),
	 offset(dead_client), XtRImmediate, (XtPointer) False}
	,
};

static CompositeClassExtensionRec compositeClassExtRec = {
	NULL,
	NULLQUARK,
	XtCompositeExtensionVersion,
	sizeof(CompositeClassExtensionRec),
	TRUE,
};

static ShellClassExtensionRec shellClassExtRec = {
	NULL,
	NULLQUARK,
	XtShellExtensionVersion,
	sizeof(ShellClassExtensionRec),
	ExternalShellRootGeometryManager
};

ExternalShellClassRec externalShellClassRec = {
	{			/*
				 *        core_class fields
				 */
	 /* superclass         */ (WidgetClass) & shellClassRec,
	 /* class_name         */ "ExternalShell",
	 /* size               */ sizeof(ExternalShellRec),
	 /* Class Initializer  */ NULL,
						/* class_part_initialize */ NULL,
						/* XtInheritClassPartInitialize, */
	 /* Class init'ed ?    */ FALSE,
	 /* initialize         */ ExternalShellInitialize,
	 /* initialize_notify  */ NULL,
	 /* realize            */ ExternalShellRealize,
	 /* actions            */ NULL,
	 /* num_actions        */ 0,
	 /* resources          */ resources,
	 /* resource_count     */ XtNumber(resources),
	 /* xrm_class          */ NULLQUARK,
	 /* compress_motion    */ FALSE,
	 /* compress_exposure  */ TRUE,
	 /* compress_enterleave */ FALSE,
	 /* visible_interest   */ TRUE,
							/* destroy            */ ExternalShellDestroy,
							/* XtInheritDestroy, */
	 /* resize             */ XtInheritResize,
	 /* expose             */ NULL,
					/* set_values         */ NULL,
					/* XtInheritSetValues, */
	 /* set_values_hook    */ NULL,
	 /* set_values_almost  */ XtInheritSetValuesAlmost,
	 /* get_values_hook    */ NULL,
	 /* accept_focus       */ NULL,
	 /* intrinsics version */ XtVersion,
	 /* callback offsets   */ NULL,
	 /* tm_table           */ NULL,
	 /* query_geometry     */ NULL,
	 /* display_accelerator */ NULL,
	 /* extension          */ NULL
	 }
	, {			/* Composite */
	   /* geometry_manager   */ XtInheritGeometryManager,
							/* change_managed     */ ChangeManaged,
							/* XtInheritChangeManaged */
	   /* insert_child       */ XtInheritInsertChild,
	   /* delete_child       */ XtInheritDeleteChild,
	   /* extension          */ (XtPointer) & compositeClassExtRec
	   }
	, {			/* Shell */
	   /* extension          */ (XtPointer) & shellClassExtRec
	   }
	, {			/* ExternalShell */
	   0}
};

WidgetClass externalShellWidgetClass = (WidgetClass) & externalShellClassRec;

static void
ExternalShellInitialize(Widget req, Widget new, ArgList args,
			Cardinal * num_args)
{
	XtAddEventHandler(new, 0, TRUE, EventHandler, (XtPointer) NULL);
	extw_initialize_atoms(XtDisplay(req));
	extw_which_side = extw_shell_send;
}

static Widget find_managed_child(CompositeWidget w)
{
	int i;
	Widget *childP = w->composite.children;

	for (i = w->composite.num_children; i; i--, childP++)
		if (XtIsWidget(*childP) && XtIsManaged(*childP))
			return *childP;
	return NULL;
}

#ifndef XtCXtToolkitError
# define XtCXtToolkitError "XtToolkitError"
#endif

static void EventHandler(wid, closure, event, continue_to_dispatch)
Widget wid;
XtPointer closure;		/* unused */
XEvent *event;
Boolean *continue_to_dispatch;	/* unused */
{
	ExternalShellWidget w = (ExternalShellWidget) wid;

	if (w->core.window != event->xany.window) {
		XtAppErrorMsg(XtWidgetToApplicationContext(wid),
			      "invalidWindow", "eventHandler",
			      XtCXtToolkitError, "Event with wrong window",
			      (String *) NULL, (Cardinal *) NULL);
		return;
	}

	if (event->type == ClientMessage &&
	    event->xclient.data.l[0] == extw_client_send &&
	    event->xclient.message_type == a_EXTW_NOTIFY)
		switch (event->xclient.data.l[1]) {

		case extw_notify_gm:
			/* client is alive again. */
			w->externalShell.dead_client = False;
			break;

		case extw_notify_qg:{
				XtWidgetGeometry xwg, xwg_return;
				XtGeometryResult result;
				Widget child =
				    find_managed_child((CompositeWidget) w);

				if (child) {
					extw_get_geometry_value(XtDisplay(wid),
								XtWindow(wid),
								a_EXTW_QUERY_GEOMETRY,
								&xwg);
					result =
					    XtQueryGeometry(child, &xwg,
							    &xwg_return);
				} else
					result = XtGeometryYes;

				extw_send_geometry_value(XtDisplay(wid),
							 XtWindow(wid),
							 a_EXTW_QUERY_GEOMETRY,
							 extw_notify_qg,
							 result ==
							 XtGeometryAlmost ?
							 &xwg_return : NULL,
							 result);
				break;
			}

		case extw_notify_focus_in:{
				XFocusChangeEvent evnt;

				evnt.type = FocusIn;
				evnt.serial =
				    LastKnownRequestProcessed(XtDisplay(wid));
				evnt.send_event = True;
				evnt.display = XtDisplay(wid);
				evnt.window = XtWindow(wid);
				evnt.mode = NotifyNormal;
				evnt.detail = NotifyAncestor;
#ifdef emacs
				emacs_Xt_handle_focus_event((XEvent *) & evnt);
#else
				XtDispatchEvent((XEvent *) & evnt);
#endif
				break;
			}

		case extw_notify_focus_out:{
				XFocusChangeEvent evnt;

				evnt.type = FocusOut;
				evnt.serial =
				    LastKnownRequestProcessed(XtDisplay(wid));
				evnt.send_event = True;
				evnt.display = XtDisplay(wid);
				evnt.window = XtWindow(wid);
				evnt.mode = NotifyNormal;
				evnt.detail = NotifyAncestor;
#ifdef emacs
				emacs_Xt_handle_focus_event((XEvent *) & evnt);
#else
				XtDispatchEvent((XEvent *) & evnt);
#endif
				break;
			}

		case extw_notify_end:
			/* frame should be destroyed. */
			break;
		}
}

/* Lifted almost entirely from GetGeometry() in Shell.c
 */
static void GetGeometry(Widget W, Widget child)
{
	ExternalShellWidget w = (ExternalShellWidget) W;
	int x, y, win_gravity = -1, flag;
	XSizeHints hints;
	Window win = w->externalShell.external_window;

	{
		Window dummy_root;
		unsigned int dummy_bd_width, dummy_depth, width, height;

		/* determine the existing size of the window. */
		XGetGeometry(XtDisplay(W), win, &dummy_root, &x, &y, &width,
			     &height, &dummy_bd_width, &dummy_depth);
		w->core.width = width;
		w->core.height = height;
	}

	if (w->shell.geometry != NULL) {
		char def_geom[128];
		int width, height;
		int sz;

		x = w->core.x;
		y = w->core.y;
		width = w->core.width;
		height = w->core.height;
		hints.flags = 0;

		sz = snprintf(def_geom, sizeof(def_geom), "%dx%d+%d+%d", width, height, x, y);
		assert(sz >= 0 && sz < sizeof(def_geom))
		flag = XWMGeometry(XtDisplay(W),
				   XScreenNumberOfScreen(XtScreen(W)),
				   w->shell.geometry, def_geom,
				   (unsigned int)w->core.border_width,
				   &hints, &x, &y, &width, &height,
				   &win_gravity);
		if (flag) {
			if (flag & XValue)
				w->core.x = (Position) x;
			if (flag & YValue)
				w->core.y = (Position) y;
			if (flag & WidthValue)
				w->core.width = (Dimension) width;
			if (flag & HeightValue)
				w->core.height = (Dimension) height;
		} else {
			String params[2];
			Cardinal num_params = 2;
			params[0] = XtName(W);
			params[1] = w->shell.geometry;
			XtAppWarningMsg(XtWidgetToApplicationContext(W),
					"badGeometry", "shellRealize",
					XtCXtToolkitError,
					"Shell widget \"%s\" has an invalid geometry specification: \"%s\"",
					params, &num_params);
		}
	} else
		flag = 0;

	w->shell.client_specified |= _XtShellGeometryParsed;
}

/* Lifted almost entirely from Realize() in Shell.c
 */
static void ExternalShellRealize(Widget wid, Mask * vmask,
				 XSetWindowAttributes * attr)
{
	ExternalShellWidget w = (ExternalShellWidget) wid;
	Mask mask = *vmask;
	Window win = w->externalShell.external_window;

	if (!win) {
		Cardinal count = 1;
		XtErrorMsg("invalidWindow", "shellRealize", XtCXtToolkitError,
			   "No external window specified for ExternalShell widget %s",
			   &wid->core.name, &count);
	}

	if (!(w->shell.client_specified & _XtShellGeometryParsed)) {
		/* we'll get here only if there was no child the first
		   time we were realized.  If the shell was Unrealized
		   and then re-Realized, we probably don't want to
		   re-evaluate the defaults anyway.
		 */
		GetGeometry(wid, (Widget) NULL);
	} else if (w->core.background_pixmap == XtUnspecifiedPixmap) {
		/* I attempt to inherit my child's background to avoid screen flash
		 * if there is latency between when I get resized and when my child
		 * is resized.  Background=None is not satisfactory, as I want the
		 * user to get immediate feedback on the new dimensions (most
		 * particularly in the case of a non-reparenting wm).  It is
		 * especially important to have the server clear any old cruft
		 * from the display when I am resized larger.
		 */
		Widget *childP = w->composite.children;
		int i;
		for (i = w->composite.num_children; i; i--, childP++) {
			if (XtIsWidget(*childP) && XtIsManaged(*childP)) {
				if ((*childP)->core.background_pixmap
				    != XtUnspecifiedPixmap) {
					mask &= ~(CWBackPixel);
					mask |= CWBackPixmap;
					attr->background_pixmap =
					    w->core.background_pixmap =
					    (*childP)->core.background_pixmap;
				} else {
					attr->background_pixel =
					    w->core.background_pixel =
					    (*childP)->core.background_pixel;
				}
				break;
			}
		}
	}

	if (w->shell.save_under) {
		mask |= CWSaveUnder;
		attr->save_under = TRUE;
	}
	if (w->shell.override_redirect) {
		mask |= CWOverrideRedirect;
		attr->override_redirect = TRUE;
	}
	if (wid->core.width == 0 || wid->core.height == 0) {
		Cardinal count = 1;
		XtErrorMsg("invalidDimension", "shellRealize",
			   XtCXtToolkitError,
			   "Shell widget %s has zero width and/or height",
			   &wid->core.name, &count);
	}
	wid->core.window = win;
	XChangeWindowAttributes(XtDisplay(wid), wid->core.window, mask, attr);

}

static void ExternalShellDestroy(wid)
Widget wid;
{
	ExternalShellWidget w = (ExternalShellWidget) wid;

	if (XtIsRealized(wid))
		ExternalShellUnrealize(wid);

	NOTIFY(w, extw_notify_end, 0, 0, 0);
}

/* Invoke matching routine from superclass, but first override its
   geometry opinions with our own routine */

static void ChangeManaged(wid)
Widget wid;
{
	if (!XtIsRealized(wid))
		GetGeometry(wid, (Widget) NULL);
	(*((ShellClassRec *) externalShellClassRec.core_class.superclass)->
	 composite_class.change_managed) (wid);
}

/* Based on RootGeometryManager() in Shell.c */

static XtGeometryResult ExternalShellRootGeometryManager(gw, request, reply)
Widget gw;
XtWidgetGeometry *request, *reply;
{
	ExternalShellWidget w = (ExternalShellWidget) gw;
	unsigned int mask = request->request_mode;
	XEvent event;
	int oldx, oldy, oldwidth, oldheight, oldborder_width;
	unsigned long request_num;
	XtWidgetGeometry req = *request;	/* don't modify caller's structure */

	oldx = w->core.x;
	oldy = w->core.y;
	oldwidth = w->core.width;
	oldheight = w->core.height;
	oldborder_width = w->core.border_width;

#define PutBackGeometry() \
	{ w->core.x = oldx; \
	  w->core.y = oldy; \
	  w->core.width = oldwidth; \
	  w->core.height = oldheight; \
	  w->core.border_width = oldborder_width; }

	if (mask & CWX) {
		if (w->core.x == request->x)
			mask &= ~CWX;
		else
			w->core.x = request->x;
	}
	if (mask & CWY) {
		if (w->core.y == request->y)
			mask &= ~CWY;
		else
			w->core.y = request->y;
	}
	if (mask & CWBorderWidth) {
		if (w->core.border_width == request->border_width)
			mask &= ~CWBorderWidth;
		else
			w->core.border_width = request->border_width;
	}
	if (mask & CWWidth) {
		if (w->core.width == request->width)
			mask &= ~CWWidth;
		else
			w->core.width = request->width;
	}
	if (mask & CWHeight) {
		if (w->core.height == request->height)
			mask &= ~CWHeight;
		else
			w->core.height = request->height;
	}

	if (!XtIsRealized((Widget) w))
		return XtGeometryYes;

	req.sibling = None;
	req.request_mode = mask & ~CWSibling;
	request_num = NextRequest(XtDisplay(w));
	extw_send_geometry_value(XtDisplay(w), XtWindow(w),
				 a_EXTW_GEOMETRY_MANAGER,
				 extw_notify_gm, &req, 0);

	if (w->externalShell.dead_client == TRUE) {
		/* The client is sick.  Refuse the request.
		 * If the client recovers and decides to honor the
		 * request, it will be handled by Shell's EventHandler().
		 */
		PutBackGeometry();
		return XtGeometryNo;
	}

	if (extw_wait_for_response(gw, &event, request_num, extw_notify_gm,
				   w->externalShell.client_timeout)) {
		XtGeometryResult result =
		    (XtGeometryResult) event.xclient.data.l[2];

		if (result != XtGeometryYes)
			PutBackGeometry();
		if (result == XtGeometryAlmost) {
			extw_get_geometry_value(XtDisplay(w), XtWindow(w),
						a_EXTW_GEOMETRY_MANAGER, reply);
		}
		return result;
	} else {
		w->externalShell.dead_client = TRUE;	/* timed out; must be broken */
		PutBackGeometry();
		return XtGeometryNo;
	}
#undef PutBackGeometry
}

static void
hack_event_masks_1(Display * display, Window w, int this_window_propagate)
{
	Window root, parent, *children;
	unsigned int nchildren;
	unsigned int i;

	if (!XQueryTree(display, w, &root, &parent, &children, &nchildren))
		return;
	for (i = 0; i < nchildren; i++)
		hack_event_masks_1(display, children[i], 1);
	if (children)
		XFree(children);
	{
		XWindowAttributes xwa;
		XSetWindowAttributes xswa;
		if (XGetWindowAttributes(display, w, &xwa)) {
			xswa.event_mask = xwa.your_event_mask & ~KeyPressMask;
			if (this_window_propagate)
				xswa.do_not_propagate_mask =
				    xwa.do_not_propagate_mask & ~KeyPressMask;
			XChangeWindowAttributes(display, w, CWEventMask, &xswa);
		}
	}
}

/* fix all event masks on all subwindows of the specified window so that
   all key presses in any subwindow filter up to the specified window.

   We have to do this cruftiness with external widgets so that we don't
   step on Motif's concept of keyboard focus.  (Due to the nature of
   Xt and Motif, X's idea of who gets the keyboard events may not jive
   with Xt's idea of same, and Xt redirects the events to the proper
   window.  This occurs on the client side and we have no knowledge
   of it, so we have to rely on a SendEvent from the client side to
   receive our keyboard events.)
*/

static void hack_event_masks(Display * display, Window w)
{
	hack_event_masks_1(display, w, 0);
}

/* external entry points */

Bool ExternalShellReady(Widget w, Window win, long event_mask)
{
	ExternalShellWidget ew = (ExternalShellWidget) w;
	XEvent event;
	unsigned long request_num;

	request_num = NextRequest(XtDisplay(w));
	NOTIFY(ew, extw_notify_init, (long)win, event_mask, 0);
	if (extw_wait_for_response(w, &event, request_num, extw_notify_init,
				   ew->externalShell.client_timeout)) {
		/* Xt/Xm extw's have more elaborate focus needs than mere
		   Xlib ones.

		   Rather independently, they *don't* need the
		   ConfigureNotify event, having fixed up the window size in
		   ChangeManaged, above, but Xlib extw's do need this.
		 */
		ew->externalShell.client_type = event.xclient.data.l[2];
		if (ew->externalShell.client_type != EXTW_TYPE_XLIB) {
			hack_event_masks(XtDisplay(w), XtWindow(w));
		} else {
			XConfigureEvent ev;
			XWindowAttributes xwa;
			ev.type = ConfigureNotify;
			ev.display = XtDisplay(w);
			ev.event = ev.window = XtWindow(w);
			XGetWindowAttributes(ev.display, ev.window, &xwa);
			ev.x = xwa.x;
			ev.y = xwa.y;
			ev.width = xwa.width;
			ev.height = xwa.height;
			ev.border_width = xwa.border_width;
			ev.above = None;
			ev.override_redirect = xwa.override_redirect;
			XtDispatchEvent((XEvent *) & ev);
		}
		return TRUE;
	} else
		return FALSE;
}

void ExternalShellSetFocus(Widget wid)
{
	ExternalShellWidget w = (ExternalShellWidget) wid;

	NOTIFY(w, extw_notify_set_focus, 0, 0, 0);
}

extern void _XtUnregisterWindow(Window, Widget);

void ExternalShellUnrealize(Widget w)
{
#if (XT_REVISION > 5)
	XtUnregisterDrawable(XtDisplay(w), w->core.window);
#else
	extern void _XtUnregisterWindow(Window, Widget);
	_XtUnregisterWindow(w->core.window, w);
#endif
	w->core.window = 0;
}
