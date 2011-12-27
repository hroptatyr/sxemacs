/* External shell widget external header file.
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

#ifndef INCLUDED_ExternalShell_h_
#define INCLUDED_ExternalShell_h_

#ifndef XtNwindow
#define XtNwindow "window"
#endif
#ifndef XtCWindow
#define XtCWindow "Window"
#endif

#ifndef XtNclientTimeout
#define XtNclientTimeout "clientTimeout"
#endif
#ifndef XtCClientTimeout
#define XtCClientTimeout "ClientTimeout"
#endif

#ifndef XtNdeadClient
#define XtNdeadClient "deadClient"
#endif
#ifndef XtCDeadClient
#define XtCDeadClient "DeadClient"
#endif

typedef struct _ExternalShellClassRec *ExternalShellWidgetClass;
typedef struct _ExternalShellRec *ExternalShellWidget;
extern WidgetClass externalShellWidgetClass;

Bool ExternalShellReady(Widget w, Window win, long event_mask);
void ExternalShellSetFocus(Widget w);
void ExternalShellUnrealize(Widget w);

#define is_external_shell(w) (XtClass (w) == externalShellWidgetClass)

#endif				/* INCLUDED_ExternalShell_h_ */
