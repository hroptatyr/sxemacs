/* External shell widget internal header file.
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

/* Written by Ben Wing, September 1993. */

#ifndef INCLUDED_ExternalShellP_h_
#define INCLUDED_ExternalShellP_h_

#include "xintrinsic.h"
#include <X11/ShellP.h>
#include "ExternalShell.h"

typedef struct {		/* new fields for ExternalShell class */
   int dummy;
} ExternalShellClassPart;

typedef struct _ExternalShellClassRec {	/* full class record declaration */
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ShellClassPart shell_class;
    ExternalShellClassPart externalShell_class;
} ExternalShellClassRec;

typedef struct {		/* new fields for ExternalShell widget */
    Window external_window;	/* an already-created window to run on */
    Bool dead_client;		/* is the client dead? */
    unsigned long client_timeout;/* how long to wait for client's response */

    /* private */
    unsigned char client_type;
} ExternalShellPart;

typedef struct _ExternalShellRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
    ShellPart shell;
    ExternalShellPart externalShell;
} ExternalShellRec;

extern ExternalShellClassRec externalShellClassRec;	 /* class pointer */

#endif /* INCLUDED_ExternalShellP_h_ */
