/* Emacs shell widget internal header file.
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

#ifndef INCLUDED_EmacsShellP_h_
#define INCLUDED_EmacsShellP_h_

#include "xintrinsic.h"
#include <X11/ShellP.h>
#include "EmacsShell.h"

/****** TopLevelEmacsShell ******/

typedef struct {		/* new fields for TopLevelEmacsShell class */
	int dummy;
} TopLevelEmacsShellClassPart;

/* full class record declaration */
typedef struct _TopLevelEmacsShellClassRec {
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ShellClassPart shell_class;
	WMShellClassPart wm_shell_class;
	VendorShellClassPart vendor_shell_class;
	TopLevelShellClassPart top_level_shell_class;
	TopLevelEmacsShellClassPart emacs_shell_class;
} TopLevelEmacsShellClassRec;

typedef struct {		/* new fields for TopLevelEmacsShell widget */
	int width_cells, height_cells;
	int min_width_cells, min_height_cells;
} TopLevelEmacsShellPart;

typedef struct _TopLevelEmacsShellRec {	/* full instance record */
	CorePart core;
	CompositePart composite;
	ShellPart shell;
	WMShellPart wm;
	VendorShellPart vendor;
	TopLevelShellPart top_level;
	TopLevelEmacsShellPart emacs_shell;
} TopLevelEmacsShellRec;

/* class pointer */
extern TopLevelEmacsShellClassRec topLevelEmacsShellClassRec;

/****** TransientEmacsShell ******/

typedef struct {		/* new fields for TransientEmacsShell class */
	int dummy;
} TransientEmacsShellClassPart;

/* full class record declaration */
typedef struct _TransientEmacsShellClassRec {
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ShellClassPart shell_class;
	WMShellClassPart wm_shell_class;
	VendorShellClassPart vendor_shell_class;
	TransientShellClassPart transient_shell_class;
	TransientEmacsShellClassPart emacs_shell_class;
} TransientEmacsShellClassRec;

typedef struct {		/* new fields for TransientEmacsShell widget */
	int width_cells, height_cells;
	int min_width_cells, min_height_cells;
} TransientEmacsShellPart;

typedef struct _TransientEmacsShellRec {	/* full instance record */
	CorePart core;
	CompositePart composite;
	ShellPart shell;
	WMShellPart wm;
	VendorShellPart vendor;
	TransientShellPart transient;
	TransientEmacsShellPart emacs_shell;
} TransientEmacsShellRec;

/* class pointer */
extern TransientEmacsShellClassRec transientEmacsShellClassRec;

#endif				/* INCLUDED_EmacsShellP_h_ */
