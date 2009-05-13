/* Copyright (C) 1993, 1994 Sun Microsystems, Inc.

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

#ifndef INCLUDED_extw_Xlib_h_
#define INCLUDED_extw_Xlib_h_

#define extw_shell_send 0
#define extw_client_send 1

typedef enum {
	extw_notify_init,
	extw_notify_end,
	extw_notify_qg,
	extw_notify_gm,
	extw_notify_set_focus,
	extw_notify_focus_in,
	extw_notify_focus_out
} en_extw_notify;

extern Atom a_EXTW_QUERY_GEOMETRY, a_EXTW_GEOMETRY_MANAGER,
    a_EXTW_WIDGET_GEOMETRY, a_EXTW_NOTIFY;
extern int extw_which_side;

typedef enum {
	EXTW_TYPE_NONE,
	EXTW_TYPE_XLIB,
	EXTW_TYPE_XT,
	EXTW_TYPE_MOTIF
} en_extw_type;

void extw_initialize_atoms(Display * display);
void extw_send_notify_3(Display * display, Window win, en_extw_notify type,
			long data0, long data1, long data2);

#endif				/* INCLUDED_extw_Xlib_h_ */
