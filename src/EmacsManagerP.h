/* Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing. */

#ifndef INCLUDED_EmacsManagerP_h_
#define INCLUDED_EmacsManagerP_h_


#include "xintrinsicp.h"
#ifdef LWLIB_USES_MOTIF
#include "xmmanagerp.h"
#endif
#include "EmacsManager.h"

typedef struct {		/* new fields for EmacsManager class */
  int dummy;
} EmacsManagerClassPart;

typedef struct _EmacsManagerClassRec {	/* full class record declaration */
  CoreClassPart core_class;
  CompositeClassPart composite_class;
#ifdef LWLIB_USES_MOTIF
  ConstraintClassPart constraint_class;
  XmManagerClassPart manager_class;
#endif
  EmacsManagerClassPart emacs_manager_class;
} EmacsManagerClassRec;

typedef struct {		/* new fields for EmacsManager widget */
  XtCallbackList resize_callback;
  XtCallbackList query_geometry_callback;
  XtPointer user_data;
} EmacsManagerPart;

typedef struct _EmacsManagerRec {	/* full instance record */
    CorePart core;
    CompositePart composite;
#ifdef LWLIB_USES_MOTIF
    ConstraintPart constraint;
    XmManagerPart manager;
#endif
    EmacsManagerPart emacs_manager;
} EmacsManagerRec;

extern EmacsManagerClassRec emacsManagerClassRec;	 /* class pointer */

#endif /* INCLUDED_EmacsManagerP_h_ */
