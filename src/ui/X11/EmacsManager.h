/* Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

/* Written by Ben Wing. */

#ifndef INCLUDED_EmacsManager_h_
#define INCLUDED_EmacsManager_h_

#ifndef XtNresizeCallback
#define XtNresizeCallback "resizeCallback"
#endif

#ifndef XtNqueryGeometryCallback
#define XtNqueryGeometryCallback "queryGeometryCallback"
#endif

#ifndef XtNuserData
#define XtNuserData "userData"
#endif
#ifndef XtCUserData
#define XtCUserData "UserData"
#endif

typedef struct _EmacsManagerClassRec *EmacsManagerWidgetClass;
typedef struct _EmacsManagerRec *EmacsManagerWidget;
extern WidgetClass emacsManagerWidgetClass;

/* External entry points */
typedef struct {
	Dimension width, height;
} EmacsManagerResizeStruct;

typedef struct {
	Dimension proposed_width, proposed_height;
	XtGeometryMask request_mode;
} EmacsManagerQueryGeometryStruct;

void EmacsManagerChangeSize(Widget w, Dimension width, Dimension height);

#endif				/* INCLUDED_EmacsManager_h_ */
