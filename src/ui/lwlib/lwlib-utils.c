/* Defines some widget utility functions.
   Copyright (C) 1992 Lucid, Inc.

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

#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include "lwlib-utils.h"

void destroy_all_children(Widget widget)
{
	Widget *children;
	unsigned int number;
	int i;

	children = XtCompositeChildren(widget, &number);
	if (children) {
		/* Unmanage all children and destroy them.  They will only be
		 * really destroyed when we get out of DispatchEvent. */
		for (i = 0; i < (int)number; i++) {
			Widget child = children[i];
			if (!child->core.being_destroyed) {
				XtUnmanageChild(child);
				XtDestroyWidget(child);
			}
		}
		XtFree((char *)children);
	}
}

/* Redisplay the contents of the widget, without first clearing it. */
void XtNoClearRefreshWidget(Widget widget)
{
	XEvent event;
	XExposeEvent *ev = &event.xexpose;

	ev->type = Expose;
	ev->serial = 0;
	ev->send_event = 0;
	ev->display = XtDisplay(widget);
	ev->window = XtWindow(widget);
	ev->x = 0;
	ev->y = 0;
	ev->width = widget->core.width;
	ev->height = widget->core.height;
	ev->count = 0;

	(*widget->core.widget_class->core_class.expose)
	    (widget, &event, (Region) NULL);
}

/*
 * Apply a function to all the subwidgets of a given widget recursively.
*/
void XtApplyToWidgets(Widget w, XtApplyToWidgetsProc proc, XtPointer arg)
{
	if (XtIsComposite(w)) {
		CompositeWidget cw = (CompositeWidget) w;
		/* We have to copy the children list before mapping over it, because
		   the procedure might add/delete elements, which would lose badly. */
		int nkids = cw->composite.num_children;
		Widget *kids = (Widget *) malloc(sizeof(Widget) * nkids);
		int i;
		memcpy(kids, cw->composite.children, sizeof(Widget) * nkids);
		for (i = 0; i < nkids; i++)
/* This prevent us from using gadgets, why is it here? */
/*	if (XtIsWidget (kids [i])) */
		{
			/* do the kiddies first in case we're destroying */
			XtApplyToWidgets(kids[i], proc, arg);
			proc(kids[i], arg);
		}
		free(kids);
	}
}

/*
 * Apply a function to all the subwidgets of a given widget recursively.
 * Stop as soon as the function returns non NULL and returns this as a value.
 */
void *XtApplyUntilToWidgets(Widget w, XtApplyUntilToWidgetsProc proc,
			    XtPointer arg)
{
	void *result;
	if (XtIsComposite(w)) {
		CompositeWidget cw = (CompositeWidget) w;
		unsigned int i;
		for (i = 0; i < cw->composite.num_children; i++)
			if (XtIsWidget(cw->composite.children[i])) {
				result = proc(cw->composite.children[i], arg);
				if (result)
					return result;
				result =
				    XtApplyUntilToWidgets(cw->composite.
							  children[i], proc,
							  arg);
				if (result)
					return result;
			}
	}
	return NULL;
}

/*
 * Returns a copy of the list of all children of a composite widget
 */
Widget *XtCompositeChildren(Widget widget, unsigned int *number)
{
	CompositeWidget cw = (CompositeWidget) widget;
	Widget *result;
	int n;
	int i;

	if (!XtIsComposite(widget)) {
		*number = 0;
		return NULL;
	}
	n = cw->composite.num_children;
	result = (Widget *) XtMalloc(n * sizeof(Widget));
	*number = n;
	for (i = 0; i < n; i++)
		result[i] = cw->composite.children[i];
	return result;
}

Boolean XtWidgetBeingDestroyedP(Widget widget)
{
	return widget->core.being_destroyed;
}

void XtSafelyDestroyWidget(Widget widget)
{
#if 0

	/* this requires IntrinsicI.h (actually, InitialI.h) */

	XtAppContext app = XtWidgetToApplicationContext(widget);

	if (app->dispatch_level == 0) {
		app->dispatch_level = 1;
		XtDestroyWidget(widget);
		/* generates an event so that the event loop will be called */
		XChangeProperty(XtDisplay(widget), XtWindow(widget),
				XA_STRING, XA_STRING, 32, PropModeAppend, NULL,
				0);
		app->dispatch_level = 0;
	} else
		XtDestroyWidget(widget);

#else
	abort();
#endif
}
