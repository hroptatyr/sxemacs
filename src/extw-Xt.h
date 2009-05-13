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

#ifndef INCLUDED_extw_Xt_h_
#define INCLUDED_extw_Xt_h_

#include "extw-Xlib.h"

#ifndef XtCXtToolkitError
#define XtCXtToolkitError "XtToolkitError"
#endif

#ifndef DEFAULT_WM_TIMEOUT
#define DEFAULT_WM_TIMEOUT 5000
#endif

void extw_send_geometry_value(Display * display, Window win, Atom property,
			      en_extw_notify type, XtWidgetGeometry * xwg,
			      long data0);
void extw_get_geometry_value(Display * display, Window win, Atom property,
			     XtWidgetGeometry * xwg);
Bool extw_wait_for_response(Widget w, XEvent * event, unsigned long request_num,
			    en_extw_notify type, unsigned long timeout);

#endif				/* INCLUDED_extw_Xt_h_ */
