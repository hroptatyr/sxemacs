/* Emacs shell widget -- glue.
   Copyright (C) 1994, 1995 Sun Microsystems, Inc.

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

/* Written by Ben Wing, May, 1994. */

#include <config.h>

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include "EmacsShell.h"
#include "ExternalShell.h"

#if 0				/* Not currently used */

/* The root_geometry_manager() method in Shell.c is fucked up with regard
   to the user-specified-position vs. program-specified-position and
   user-specified-size vs. program-specified-size flag. (It always
   sets program-specified whenever the program requests a change
   in its size or position, even when this came from direct user
   request.) So we provide external entry points to fix this after
   the program requested a size or position change.  If it turns
   out that the user-specified-position flag needs to be set at the
   *same* time that the geometry change request is made, then we
   will have to duplicate the entire root_geometry_manager() method;
   but I don't think there are any WM's that require this. */

/* junk stolen from IntrinsicI.h */

extern void _XtAllocError(String /* alloc_type */ );

/* junk ungraciously copied from Shell.c */

static void ComputeWMSizeHints(w, hints)
WMShellWidget w;
XSizeHints *hints;
{
	long flags;
	hints->flags = flags = w->wm.size_hints.flags;
#define copy(field) hints->field = w->wm.size_hints.field
	if (flags & (USPosition | PPosition)) {
		copy(x);
		copy(y);
	}
	if (flags & (USSize | PSize)) {
		copy(width);
		copy(height);
	}
	if (flags & PMinSize) {
		copy(min_width);
		copy(min_height);
	}
	if (flags & PMaxSize) {
		copy(max_width);
		copy(max_height);
	}
	if (flags & PResizeInc) {
		copy(width_inc);
		copy(height_inc);
	}
	if (flags & PAspect) {
		copy(min_aspect.x);
		copy(min_aspect.y);
		copy(max_aspect.x);
		copy(max_aspect.y);
	}
#undef copy
#define copy(field) hints->field = w->wm.field
	if (flags & PBaseSize) {
		copy(base_width);
		copy(base_height);
	}
	if (flags & PWinGravity)
		copy(win_gravity);
#undef copy
}

static void _SetWMSizeHints(w)
WMShellWidget w;
{
	XSizeHints *size_hints = XAllocSizeHints();

	if (size_hints == NULL)
		_XtAllocError("XAllocSizeHints");
	ComputeWMSizeHints(w, size_hints);
	XSetWMNormalHints(XtDisplay((Widget) w), XtWindow((Widget) w),
			  size_hints);
	XFree((char *)size_hints);
}

/* end of junk ungraciously copied from Shell.c */

#endif				/* 0 */
#if 0				/* Not currently used */

void EmacsShellSetSizeUserSpecified(Widget gw)
{
	WMShellWidget w = (WMShellWidget) gw;
	w->wm.size_hints.flags |= USSize;
	w->wm.size_hints.flags &= ~PSize;
	if (!w->shell.override_redirect && XtIsRealized(gw))
		_SetWMSizeHints(w);
}

void EmacsShellSetPositionUserSpecified(Widget gw)
{
	WMShellWidget w = (WMShellWidget) gw;
	w->wm.size_hints.flags |= USPosition;
	w->wm.size_hints.flags &= ~PPosition;
	if (!w->shell.override_redirect && XtIsRealized(gw))
		_SetWMSizeHints(w);
}

#endif				/* 0 */

void EmacsShellSmashIconicHint(Widget shell, int iconic_p)
{
	/* See comment in frame-x.c about this */
	WMShellWidget wmshell = (WMShellWidget) shell;
	assert(XtIsSubclass(shell, wmShellWidgetClass));
	/* old_state = (wmshell->wm.wm_hints.flags & StateHint
	   ? wmshell->wm.wm_hints.initial_state
	   : NormalState); */
	wmshell->wm.wm_hints.flags |= StateHint;
	wmshell->wm.wm_hints.initial_state =
	    iconic_p ? IconicState : NormalState;
}

void EmacsShellUpdateSizeHints(Widget gw)
{
	if (XtIsSubclass(gw, topLevelEmacsShellWidgetClass))
		TopLevelEmacsShellUpdateSizeHints(gw);
#ifdef EXTERNAL_WIDGET
	else if (XtIsSubclass(gw, externalShellWidgetClass))
		/* do what ??? Don't abort! */ ;
#endif
	else if (XtIsSubclass(gw, transientEmacsShellWidgetClass))
		TransientEmacsShellUpdateSizeHints(gw);
	else
		abort();
}
