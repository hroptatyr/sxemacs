/* External client, raw Xlib version.
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

/* Written by Ben Wing, February 1994. */

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#include "ui/X11/extw-Xlib.h"

/* this is not a perfect solution, but otherwise we have to include all
   of the Xt junk */

#define XtGeometryNo 1

#if (XlibSpecificationRelease < 5)
# define XPointer char *
#endif

static int context_inited;
static XContext focus_context;

/* does the specified window have the focus, given that the pointer just
   entered (or left) the window (according to enter_p)?  This question
   does not have an obvious answer in X.  (Basically, X sucks.) */

static int window_has_focus_p(Display * display, Window win, int enter_p)
{
	Window focuswin;
	int dummy;

	XGetInputFocus(display, &focuswin, &dummy);
	if (focuswin == PointerRoot)
		return enter_p;
	if (focuswin == win)
		return True;
	if (!enter_p)
		return False;
	do {
		Status st;
		Window root_win, parent_win;
		Window *child_win;
		int nchild;

		st = XQueryTree(display, win, &root_win, &parent_win,
				&child_win, &nchild);
		if (!st)
			return False;
		XFree((XPointer) child_win);
		if (parent_win == focuswin)
			return True;
		if (parent_win == root_win)
			return False;
		win = parent_win;
	}
	while (1);
}

/* External entry points when using XLib directly */

void ExternalClientInitialize(Display * display, Window win);
void ExternalClientInitialize(Display * display, Window win)
{
	extw_initialize_atoms(display);
	extw_which_side = extw_client_send;
	if (!context_inited) {
		focus_context = XUniqueContext();
		context_inited = 1;
	}
	XSaveContext(display, win, focus_context, 0);
	XSelectInput(display, win, EnterWindowMask | LeaveWindowMask |
		     FocusChangeMask);
}

void ExternalClientEventHandler(Display * display, Window win, XEvent * event);
void ExternalClientEventHandler(Display * display, Window win, XEvent * event)
{
	if (win != event->xany.window)
		return;

	if (event->type == ClientMessage &&
	    event->xclient.message_type == a_EXTW_NOTIFY &&
	    event->xclient.data.l[0] == extw_shell_send)
		switch (event->xclient.data.l[1]) {
		case extw_notify_gm:
			/* for the moment, just refuse geometry requests. */
			extw_send_notify_3(display, win, extw_notify_gm,
					   XtGeometryNo, 0, 0);
			break;

		case extw_notify_init:
			extw_send_notify_3(display, win, extw_notify_init,
					   EXTW_TYPE_XLIB, 0, 0);
			break;

		case extw_notify_end:
			XClearArea(display, win, 0, 0, 0, 0, True);
			break;
	} else {
		int focus_status;
		XPointer current_focus;

		if (event->type == FocusIn)
			focus_status = 1;
		else if (event->type == FocusOut)
			focus_status = 0;
		else if (event->type == EnterNotify &&
			 event->xcrossing.detail != NotifyInferior)
			focus_status = window_has_focus_p(display, win, 1);
		else if (event->type == LeaveNotify &&
			 event->xcrossing.detail != NotifyInferior)
			focus_status = window_has_focus_p(display, win, 0);
		else
			return;
		XFindContext(display, win, focus_context, &current_focus);
		if (focus_status != (int)current_focus) {
			XSaveContext(display, win, focus_context,
				     (XPointer) focus_status);
			extw_send_notify_3(display, win,
					   focus_status ? extw_notify_focus_in :
					   extw_notify_focus_out, 0, 0, 0);
		}
	}
}
