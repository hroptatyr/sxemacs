/* Emacs shell widget external header file.
   Copyright (C) 1994 Sun Microsystems, Inc.

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

#ifndef INCLUDED_EmacsShell_h_
#define INCLUDED_EmacsShell_h_

#ifndef XtNwidthCells
#define XtNwidthCells "widthCells"
#endif
#ifndef XtCWidthCells
#define XtCWidthCells "WidthCells"
#endif

#ifndef XtNheightCells
#define XtNheightCells "heightCells"
#endif
#ifndef XtCHeightCells
#define XtCHeightCells "HeightCells"
#endif

#ifndef XtNminWidthCells
#define XtNminWidthCells "minWidthCells"
#endif
#ifndef XtCMinWidthCells
#define XtCMinWidthCells "MinWidthCells"
#endif

#ifndef XtNminHeightCells
#define XtNminHeightCells "minHeightCells"
#endif
#ifndef XtCMinHeightCells
#define XtCMinHeightCells "MinHeightCells"
#endif

typedef struct _TopLevelEmacsShellClassRec *TopLevelEmacsShellWidgetClass;
typedef struct _TopLevelEmacsShellRec *TopLevelEmacsShellWidget;
extern WidgetClass topLevelEmacsShellWidgetClass;

typedef struct _TransientEmacsShellClassRec *TransientEmacsShellWidgetClass;
typedef struct _TransientEmacsShellRec *TransientEmacsShellWidget;
extern WidgetClass transientEmacsShellWidgetClass;

void EmacsShellUpdateSizeHints(Widget gw);
void TopLevelEmacsShellUpdateSizeHints(Widget gw);
void TransientEmacsShellUpdateSizeHints(Widget gw);
void EmacsShellSetSizeUserSpecified(Widget gw);
void EmacsShellSetPositionUserSpecified(Widget gw);
void EmacsShellSmashIconicHint(Widget shell, int iconic_p);

#endif				/* INCLUDED_EmacsShell_h_ */
