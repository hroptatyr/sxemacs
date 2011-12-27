/* External client widget internal header file.
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

/* Written by Ben Wing. */

#ifndef INCLUDED_ExternalClientP_h_
#define INCLUDED_ExternalClientP_h_

#include "ExternalClient.h"
#ifdef EXTW_USES_MOTIF
#include <Xm/PrimitiveP.h>
#endif

typedef struct {		/* new fields for ExternalClient class */
	int dummy;
} ExternalClientClassPart;

typedef struct _ExternalClientClassRec {	/* full class record declaration */
	CoreClassPart core_class;
#ifdef EXTW_USES_MOTIF
	XmPrimitiveClassPart primitive_class;
#endif
	ExternalClientClassPart externalClient_class;
} ExternalClientClassRec;

typedef struct {		/* new fields for ExternalClient widget */
	Bool dead_shell;	/* is the shell dead? */
	unsigned long shell_timeout;	/* how long to wait for shell's response */
	int shell_ready;	/* is the shell ready? */
	Window event_window;
	long event_mask;
	Bool has_focus;
	char *emacs_procid;
	XtCallbackList shell_ready_callback;
	String shell_name;
	/* Probably need a `Bool use_dbus' here one day */
} ExternalClientPart;

typedef struct _ExternalClientRec {	/* full instance record */
	CorePart core;
#ifdef EXTW_USES_MOTIF
	XmPrimitivePart primitive;
#endif
	ExternalClientPart externalClient;
} ExternalClientRec;

extern ExternalClientClassRec externalClientClassRec;	/* class pointer */

#endif				/* INCLUDED_ExternalClientP_h_ */
