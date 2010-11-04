/* Common code between client and shell widgets; not Xt-specific.
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
#endif
#include <X11/Xlib.h>
#include "ui/X11/extw-Xlib.h"
	int extw_which_side;

static int atoms_initialized;
Atom a_EXTW_QUERY_GEOMETRY, a_EXTW_GEOMETRY_MANAGER, a_EXTW_WIDGET_GEOMETRY,
    a_EXTW_NOTIFY;

void extw_initialize_atoms(Display * display)
{
	if (!atoms_initialized) {
		a_EXTW_QUERY_GEOMETRY =
		    XInternAtom(display, "EXTW_QUERY_GEOMETRY", False);
		a_EXTW_GEOMETRY_MANAGER =
		    XInternAtom(display, "EXTW_GEOMETRY_MANAGER", False);
		a_EXTW_WIDGET_GEOMETRY =
		    XInternAtom(display, "EXTW_WIDGET_GEOMETRY", False);
		a_EXTW_NOTIFY = XInternAtom(display, "EXTW_NOTIFY", False);
		atoms_initialized = 1;
	}

}

/* send a notification to the other-side widget. */

void
extw_send_notify_3(Display * display, Window win, en_extw_notify type,
		   long data0, long data1, long data2)
{
	XClientMessageEvent xev;

	xev.type = ClientMessage;
	xev.message_type = a_EXTW_NOTIFY;
	xev.format = 32;
	xev.display = display;
	xev.window = win;
	xev.data.l[0] = extw_which_side;
	xev.data.l[1] = type;
	xev.data.l[2] = data0;
	xev.data.l[3] = data1;
	xev.data.l[4] = data2;

	/* UGGGHHHH!  All I want to do is ensure that the ClientMessage gets
	   received.  Unfortunately X doesn't provide any simple way to do
	   that but instead has this event_mask bogosity in XSendEvent. */

	XSendEvent(display, win, False,
		   extw_which_side == extw_shell_send ? 0 : StructureNotifyMask,
		   (XEvent *) & xev);
}
