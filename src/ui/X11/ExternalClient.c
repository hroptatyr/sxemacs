/* External client widget.
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

#ifdef emacs

#include <config.h>

#ifndef EXTERNAL_WIDGET
ERROR ! This ought not be getting compiled if EXTERNAL_WIDGET
	is undefined
#endif
#endif				/* emacs */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef EXTW_USES_MOTIF
# include <Xm/XmP.h>
# include <Xm/PrimitiveP.h>
# include <X11/keysym.h>
#else
# include "xintrinsicp.h"
# include <X11/StringDefs.h>
#endif
#include "ExternalClientP.h"
#include "ui/X11/extw-Xt.h"
/* This is the client widget, used to communicate with an ExternalShell
   widget. */
#define NOTIFY(w, type, l0, l1, l2) \
  extw_send_notify_3(XtDisplay((Widget)(w)), XtWindow((Widget)(w)),\
		     type, l0, l1, l2)
	static void externalClientInitialize(Widget req, Widget new,
					     ArgList args, Cardinal * num_args);
static void externalClientRealize(Widget widget, XtValueMask * mask,
				  XSetWindowAttributes * attrs);
static void Destroy(Widget w);
static void EventHandler(Widget wid, XtPointer closure, XEvent * event,
			 Boolean * continue_to_dispatch);
static void MaskableEventHandler(Widget wid, XtPointer closure, XEvent * event,
				 Boolean * continue_to_dispatch);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry *,
				      XtWidgetGeometry *);
static void ExternalClientFocusIn(Widget, XEvent *, String *, Cardinal *);
static void ExternalClientFocusOut(Widget, XEvent *, String *, Cardinal *);
static void ExternalClientEnter(Widget, XEvent *, String *, Cardinal *);
static void ExternalClientLeave(Widget, XEvent *, String *, Cardinal *);

static int my_error_handler(Display * display, XErrorEvent * xev);
static int (*error_old_handler) (Display *, XErrorEvent *);

static XtResource resources[] = {
#define offset(field) XtOffset(ExternalClientWidget, externalClient.field)
	{XtNshellTimeout, XtCShellTimeout,
	 XtRInt, sizeof(int),
	 offset(shell_timeout), XtRImmediate, (XtPointer) DEFAULT_WM_TIMEOUT},
	{XtNdeadShell, XtCDeadShell,
	 XtRBoolean, sizeof(Boolean),
	 offset(dead_shell), XtRImmediate, (XtPointer) False}
	,
#ifdef EXTW_USES_MOTIF
	{XmNnavigationType, XmCNavigationType,
	 XmRNavigationType, sizeof(XmNavigationType),
	 XtOffset(ExternalClientWidget, primitive.navigation_type),
	 XtRImmediate, (XtPointer) XmTAB_GROUP}
	,
#endif
	{XtNemacsProcID, XtCEmacsProcID,
	 XtRString, sizeof(String),
	 offset(emacs_procid), XtRImmediate, (XtPointer) NULL}
	,
	{XtNshellReadyCallback, XtCCallback,
	 XtRCallback, sizeof(XtCallbackList),
	 offset(shell_ready_callback), XtRImmediate, (XtPointer) NULL}
	,
	{XtNshellName, XtCShellName,
	 XtRString, sizeof(String),
	 offset(shell_name), XtRImmediate, (XtPointer) NULL}
};

static XtActionsRec actions[] = {
	{"focusIn", ExternalClientFocusIn},
	{"focusOut", ExternalClientFocusOut},
	{"enter", ExternalClientEnter},
	{"leave", ExternalClientLeave},
};

ExternalClientClassRec externalClientClassRec = {
	{			/*
				 *        core_class fields
				 */
#ifdef EXTW_USES_MOTIF
	 /* superclass         */ (WidgetClass) & xmPrimitiveClassRec,
#else
	 /* superclass         */ (WidgetClass) & coreClassRec,
#endif
	 /* class_name         */ "ExternalClient",
	 /* size               */ sizeof(ExternalClientRec),
	 /* Class Initializer  */ NULL,
						/* class_part_initialize */ NULL,
						/* XtInheritClassPartInitialize, */
	 /* Class init'ed ?    */ FALSE,
	 /* initialize         */ externalClientInitialize,
	 /* initialize_notify  */ NULL,
	 /* realize            */ externalClientRealize,
	 /* actions            */ actions,
	 /* num_actions        */ XtNumber(actions),
	 /* resources          */ resources,
	 /* resource_count     */ XtNumber(resources),
	 /* xrm_class          */ NULLQUARK,
	 /* compress_motion    */ FALSE,
	 /* compress_exposure  */ TRUE,
	 /* compress_enterleave */ FALSE,
	 /* visible_interest   */ TRUE,
						/* destroy            */ Destroy,
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
					/* tm_table           */ "",
					/* MUST NOT BE NULL or
					   XtInheritTranslations in Motif!!!!!
					   Otherwise keyboard focus translations
					   will not work. */
	 /* query_geometry     */ QueryGeometry,
	 /* display_accelerator */ NULL,
	 /* extension          */ NULL
	 }
	,
#ifdef EXTW_USES_MOTIF
	{
	 XmInheritBorderHighlight,	/* Primitive border_highlight */
	 XmInheritBorderHighlight,	/* Primitive border_unhighlight */
	 XtInheritTranslations,	/* translations */
	 NULL,			/* arm_and_activate */
	 NULL,			/* get resources */
	 0,			/* num get_resources */
	 NULL,			/* extension */
	 }
	,
#endif
	{
	 0}
};

WidgetClass externalClientWidgetClass = (WidgetClass) & externalClientClassRec;

static void
externalClientInitialize(Widget req, Widget new, ArgList args,
			 Cardinal * num_args)
{
	ExternalClientWidget ecw = (ExternalClientWidget) new;
	static int error_handler_added = 0;

	extw_initialize_atoms(XtDisplay(new));
	extw_which_side = extw_client_send;

#ifdef EXTW_USES_MOTIF

	/* yes I know this is horrible.  However, the XmPrimitive class adds
	   the Tab translation in its initialization routine, so we have to
	   override it here.  This is all the fault of Xt, which doesn't
	   provide a proper inheritance mechanism for translations.

	   -- BPW

	 */

	XtOverrideTranslations(new,
			       XtParseTranslationTable("None<Key>Tab:\n"
						       "<FocusIn>:focusIn()\n"
						       "<FocusOut>:focusOut()\n"
						       "<Enter>:enter()\n"
						       "<Leave>:leave()\n"));

#endif

	XtAddEventHandler(new, 0, TRUE, EventHandler, (XtPointer) NULL);

	ecw->externalClient.shell_ready = False;
	ecw->externalClient.has_focus = False;

	if (!error_handler_added) {
		error_handler_added = 1;
		error_old_handler = XSetErrorHandler(my_error_handler);
	}
}

static void
externalClientRealize(Widget w, XtValueMask * vm, XSetWindowAttributes * attrs)
{
	ExternalClientWidget ecw = (ExternalClientWidget) w;

#ifdef EXTW_USES_MOTIF
	(*xmPrimitiveWidgetClass->core_class.realize) (w, vm, attrs);
#else
	(*coreWidgetClass->core_class.realize) (w, vm, attrs);
#endif

}

/***********************************************************************/

/* window-to-widget list. */

struct ww_list {
	Window win;
	Widget wid;
	struct ww_list *next;
};

struct ww_list ww_list[1];

static int add_ww(Window win, Widget wid)
{
	struct ww_list *ww = (struct ww_list *)malloc(sizeof(struct ww_list));
	if (!ww)
		return 0;
	ww->win = win;
	ww->wid = wid;
	ww->next = ww_list->next;
	ww_list->next = ww;
	return 1;
}

static Widget remove_ww(Window win)
{
	struct ww_list *w1, *w2;
	Widget wid = 0;

	for (w1 = ww_list, w2 = w1->next; w2; w1 = w2, w2 = w2->next)
		if (w2->win == win) {
			w1->next = w2->next;
			wid = w2->wid;
			free(w2);
			break;
		}
	return wid;
}

/***********************************************************************/

/* stolen outright from Intrinsic.c */

static void ComputeWindowAttributes(widget, value_mask, values)
Widget widget;
XtValueMask *value_mask;
XSetWindowAttributes *values;
{
	*value_mask = CWEventMask | CWColormap;
	(*values).event_mask = XtBuildEventMask(widget);
	(*values).colormap = widget->core.colormap;
	if (widget->core.background_pixmap != XtUnspecifiedPixmap) {
		*value_mask |= CWBackPixmap;
		(*values).background_pixmap = widget->core.background_pixmap;
	} else {
		*value_mask |= CWBackPixel;
		(*values).background_pixel = widget->core.background_pixel;
	}
	if (widget->core.border_pixmap != XtUnspecifiedPixmap) {
		*value_mask |= CWBorderPixmap;
		(*values).border_pixmap = widget->core.border_pixmap;
	} else {
		*value_mask |= CWBorderPixel;
		(*values).border_pixel = widget->core.border_pixel;
	}
	if (widget->core.widget_class->core_class.expose == (XtExposeProc) NULL) {
		/* Try to avoid redisplay upon resize by making bit_gravity the same
		   as the default win_gravity */
		*value_mask |= CWBitGravity;
		(*values).bit_gravity = NorthWestGravity;
	}
}				/* ComputeWindowAttributes */

static void end_connection(ExternalClientWidget w)
{
	XSetWindowAttributes xswa;
	XtValueMask mask;
	Widget wid = (Widget) w;

	w->externalClient.shell_ready = False;
	XtRemoveEventHandler(wid, w->externalClient.event_mask,
			     FALSE, MaskableEventHandler, (XtPointer) NULL);
	ComputeWindowAttributes(wid, &mask, &xswa);
	XChangeWindowAttributes(XtDisplay(wid), XtWindow(wid), mask, &xswa);
	XClearArea(XtDisplay(wid), XtWindow(wid), 0, 0, 0, 0, True);
}

static int my_error_handler(Display * display, XErrorEvent * xev)
{
	Widget wid;

	if (xev->error_code != BadWindow)
		goto call_old;
	wid = remove_ww(xev->resourceid);
	if (wid) {
		end_connection((ExternalClientWidget) wid);
		return 0;
	}

      call_old:
	return error_old_handler(display, xev);
}

static void
MaskableEventHandler(Widget wid, XtPointer closure, XEvent * event,
		     Boolean * continue_to_dispatch)
     /* closure and continue_to_dispatch unused */
{
	ExternalClientWidget w = (ExternalClientWidget) wid;

	if (w->externalClient.shell_ready) {
		if (event->type == KeyPress || event->type == KeyRelease ||
		    event->type == ButtonPress || event->type == ButtonRelease
		    || event->type == MotionNotify)
			event->xkey.subwindow = 0;
#ifdef EXTW_USES_MOTIF
		/* hackkkkkkkkkkkkkk!  Suppress CTRL-TAB, SHIFT-TAB, etc. so that
		   Emacs doesn't attempt to interpret focus-change keystrokes. */
		if (event->type == KeyPress &&
		    XLookupKeysym((XKeyEvent *) event, 0) == XK_Tab &&
		    (event->xkey.state & ControlMask ||
		     event->xkey.state & ShiftMask))
			return;
#endif
		event->xany.window = w->core.window;
		XSendEvent(XtDisplay(wid), w->externalClient.event_window,
			   FALSE, 0, event);
		XSync(XtDisplay(wid), 0);	/* make sure that any BadWindow errors
						   (meaning the server died) get handled
						   before XSendEvent is called again. */

	}
}

static void
EventHandler(Widget wid, XtPointer closure, XEvent * event,
	     Boolean * continue_to_dispatch)
     /* closure and continue_to_dispatch unused */
{
	ExternalClientWidget w = (ExternalClientWidget) wid;

	if (w->core.window != event->xany.window) {
		XtAppErrorMsg(XtWidgetToApplicationContext(wid),
			      "invalidWindow", "eventHandler",
			      XtCXtToolkitError, "Event with wrong window",
			      (String *) NULL, (Cardinal *) NULL);
		return;
	}

	if (event->type == ClientMessage &&
	    event->xclient.message_type == a_EXTW_NOTIFY &&
	    event->xclient.data.l[0] == extw_shell_send)
		switch (event->xclient.data.l[1]) {

		case extw_notify_qg:
			/* shell is alive again. */

			w->externalClient.dead_shell = False;
			break;

		case extw_notify_gm:
			{
				XtWidgetGeometry xwg, xwg_return;
				XtGeometryResult result;

				extw_get_geometry_value(XtDisplay(wid),
							XtWindow(wid),
							a_EXTW_GEOMETRY_MANAGER,
							&xwg);
				result =
				    XtMakeGeometryRequest(wid, &xwg,
							  &xwg_return);

				extw_send_geometry_value(XtDisplay(wid),
							 XtWindow(wid),
							 a_EXTW_GEOMETRY_MANAGER,
							 extw_notify_gm,
							 result ==
							 XtGeometryAlmost ?
							 &xwg_return : NULL,
							 result);
				break;
			}

		case extw_notify_init:
			w->externalClient.shell_ready = True;
			w->externalClient.event_window =
			    event->xclient.data.l[2];
			w->externalClient.event_mask = event->xclient.data.l[3];
			add_ww(w->externalClient.event_window, (Widget) w);

			XtAddEventHandler(wid, w->externalClient.event_mask,
					  FALSE, MaskableEventHandler,
					  (XtPointer) NULL);
#ifdef EXTW_USES_MOTIF
			NOTIFY(w, extw_notify_init, EXTW_TYPE_MOTIF, 0, 0);
#else
			NOTIFY(w, extw_notify_init, EXTW_TYPE_XT, 0, 0);
#endif
			break;

		case extw_notify_end:
			end_connection(w);
			remove_ww(w->externalClient.event_window);
			break;

		case extw_notify_set_focus:
#ifdef EXTW_USES_MOTIF
			XmProcessTraversal(wid, XmTRAVERSE_CURRENT);
#else
			XtSetKeyboardFocus(wid, None);
#endif
			break;

		}
}

static void Destroy(wid)
Widget wid;
{
	ExternalClientWidget w = (ExternalClientWidget) wid;

	NOTIFY(w, extw_notify_end, 0, 0, 0);
}

static XtGeometryResult QueryGeometry(gw, request, reply)
Widget gw;
XtWidgetGeometry *request, *reply;
{
	ExternalClientWidget w = (ExternalClientWidget) gw;
	XEvent event;
	unsigned long request_num;
	Display *display = XtDisplay(gw);
	XtWidgetGeometry req = *request;	/* don't modify caller's structure */

	if (!XtIsRealized((Widget) w) || !w->externalClient.shell_ready)
		return XtGeometryYes;

	if (w->externalClient.dead_shell == TRUE)
		/* The shell is sick. */
		return XtGeometryNo;

	req.sibling = None;
	req.request_mode &= ~CWSibling;
	request_num = NextRequest(display);
	extw_send_geometry_value(XtDisplay(gw), XtWindow(gw),
				 a_EXTW_QUERY_GEOMETRY, extw_notify_qg, &req,
				 0);

	if (extw_wait_for_response(gw, &event, request_num, extw_notify_qg,
				   w->externalClient.shell_timeout)) {
		XtGeometryResult result =
		    (XtGeometryResult) event.xclient.data.l[0];

		if (result == XtGeometryAlmost) {
			extw_get_geometry_value(XtDisplay(gw), XtWindow(gw),
						a_EXTW_QUERY_GEOMETRY, reply);
		}
		return result;
	} else {
		w->externalClient.dead_shell = TRUE;	/* timed out; must be broken */
		return XtGeometryNo;
	}
}

static void ExternalClientFocusIn(Widget w, XEvent * event, String * params,
				  Cardinal * num_params)
{
	ExternalClientWidget ecw = (ExternalClientWidget) w;

	if (event->xfocus.send_event && !ecw->externalClient.has_focus) {
		ecw->externalClient.has_focus = True;
		NOTIFY(ecw, extw_notify_focus_in, 0, 0, 0);
	}
#ifdef EXTW_USES_MOTIF
	_XmPrimitiveFocusIn(w, event, params, num_params);
#endif
}

static void ExternalClientFocusOut(Widget w, XEvent * event, String * params,
				   Cardinal * num_params)
{
	ExternalClientWidget ecw = (ExternalClientWidget) w;

	if (event->xfocus.send_event && ecw->externalClient.has_focus) {
		ecw->externalClient.has_focus = False;
		NOTIFY(ecw, extw_notify_focus_out, 0, 0, 0);
	}
#ifdef EXTW_USES_MOTIF
	_XmPrimitiveFocusOut(w, event, params, num_params);
#endif
}

static void ExternalClientEnter(Widget w, XEvent * event, String * params,
				Cardinal * num_params)
{
	ExternalClientWidget ecw = (ExternalClientWidget) w;

	if (
#ifdef EXTW_USES_MOTIF
		   _XmGetFocusPolicy(w) != XmEXPLICIT &&
#endif
		   !ecw->externalClient.has_focus &&
		   event->xcrossing.focus
		   && event->xcrossing.detail != NotifyInferior) {
		ecw->externalClient.has_focus = True;
		NOTIFY(ecw, extw_notify_focus_in, 0, 0, 0);
	}
#ifdef EXTW_USES_MOTIF
	_XmPrimitiveEnter(w, event, params, num_params);
#endif
}

static void ExternalClientLeave(Widget w, XEvent * event, String * params,
				Cardinal * num_params)
{
	ExternalClientWidget ecw = (ExternalClientWidget) w;

	if (
#ifdef EXTW_USES_MOTIF
		   _XmGetFocusPolicy(w) != XmEXPLICIT &&
#endif
		   ecw->externalClient.has_focus &&
		   event->xcrossing.focus
		   && event->xcrossing.detail != NotifyInferior) {
		ecw->externalClient.has_focus = False;
		NOTIFY(ecw, extw_notify_focus_out, 0, 0, 0);
	}
#ifdef EXTW_USES_MOTIF
	_XmPrimitiveLeave(w, event, params, num_params);
#endif
}
