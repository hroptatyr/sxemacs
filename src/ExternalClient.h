/* External client widget external header file.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing. */

#ifndef INCLUDED_ExternalClient_h_
#define INCLUDED_ExternalClient_h_

#ifndef XtNshellTimeout
#define XtNshellTimeout "shellTimeout"
#endif
#ifndef XtCShellTimeout
#define XtCShellTimeout "ShellTimeout"
#endif

#ifndef XtNdeadShell
#define XtNdeadShell "deadShell"
#endif
#ifndef XtCDeadShell
#define XtCDeadShell "DeadShell"
#endif

#ifndef XtNemacsProcID
#define XtNemacsProcID "emacsProcID"
#endif
#ifndef XtCEmacsProcID
#define XtCEmacsProcID "EmacsProcID"
#endif

#ifndef XtNshellReadyCallback
#define XtNshellReadyCallback "shellReadyCallback"
#endif

#ifndef XtNshellName
#define XtNshellName "shellName"
#endif
#ifndef XtCShellName
#define XtCShellName "ShellName"
#endif

#ifndef XtNuseToolTalk
#define XtNuseToolTalk "useToolTalk"
#endif
#ifndef XtCUseToolTalk
#define XtCUseToolTalk "UseToolTalk"
#endif

typedef struct _ExternalClientClassRec *ExternalClientWidgetClass;
typedef struct _ExternalClientRec *ExternalClientWidget;
extern WidgetClass externalClientWidgetClass;

/* External entry points when using direct Xlib */

void ExternalClientInitialize   (Display *display, Window win);
void ExternalClientEventHandler (Display *display, Window win, XEvent *event);

#endif /* INCLUDED_ExternalClient_h_ */
