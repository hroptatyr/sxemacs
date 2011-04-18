/* Emacs manager widget.
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

/* Written by Ben Wing, May, 1994. */

#include <config.h>

#include <X11/StringDefs.h>
#include "EmacsManagerP.h"
#ifdef LWLIB_MENUBARS_MOTIF
#include <Xm/RowColumn.h>
#endif				/* LWLIB_MENUBARS_MOTIF */

/* For I, Emacs, am a kind god.  Unlike the goddess Athena and the
   Titan Motif, I require no ritual sacrifices to placate the lesser
   daemons of geometry management. */

static XtResource resources[] = {
#define offset(field) XtOffset(EmacsManagerWidget, emacs_manager.field)
	{XtNresizeCallback, XtCCallback,
	 XtRCallback, sizeof(XtCallbackList),
	 offset(resize_callback), XtRImmediate, (XtPointer) 0}
	,
	{XtNqueryGeometryCallback, XtCCallback,
	 XtRCallback, sizeof(XtCallbackList),
	 offset(query_geometry_callback), XtRImmediate, (XtPointer) 0}
	,
	{XtNuserData, XtCUserData,
	 XtRPointer, sizeof(XtPointer),
	 offset(user_data), XtRImmediate, (XtPointer) 0}
	,
};

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

static XtGeometryResult QueryGeometry(Widget wid,
				      XtWidgetGeometry * request,
				      XtWidgetGeometry * reply);
static void Resize(Widget w);
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry * request,
					XtWidgetGeometry * reply);
static void ChangeManaged(Widget w);
static void Realize(Widget w, Mask * valueMask,
		    XSetWindowAttributes * attributes);
static void ClassInitialize(void);

EmacsManagerClassRec emacsManagerClassRec = {
	{
/* core_class fields      */
#ifdef LWLIB_USES_MOTIF
	 /* superclass         */ (WidgetClass) & xmManagerClassRec,
#else
	 /* superclass         */ (WidgetClass) & compositeClassRec,
#endif
	 /* class_name         */ "EmacsManager",
	 /* widget_size        */ sizeof(EmacsManagerRec),
	 /* class_initialize   */ ClassInitialize,
	 /* class_part_init    */ NULL,
	 /* class_inited       */ FALSE,
	 /* initialize         */ NULL,
	 /* initialize_hook    */ NULL,
	 /* realize            */ Realize,
	 /* actions            */ NULL,
	 /* num_actions        */ 0,
	 /* resources          */ resources,
	 /* num_resources      */ XtNumber(resources),
	 /* xrm_class          */ NULLQUARK,
	 /* compress_motion    */ TRUE,
	 /* compress_exposure  */ XtExposeCompressMaximal | XtExposeNoRegion,
	 /* compress_enterleave */ TRUE,
	 /* visible_interest   */ FALSE,
	 /* destroy            */ NULL,
	 /* resize             */ Resize,
	 /* expose             */ NULL,
	 /* set_values         */ NULL,
	 /* set_values_hook    */ NULL,
	 /* set_values_almost  */ XtInheritSetValuesAlmost,
	 /* get_values_hook    */ NULL,
	 /* accept_focus       */ NULL,
	 /* version            */ XtVersion,
	 /* callback_private   */ NULL,
	 /* tm_table           */ XtInheritTranslations,
	 /* query_geometry     */ QueryGeometry,
	 /* display_accelerator */ XtInheritDisplayAccelerator,
	 /* extension          */ NULL
	 }
	,
	{
/* composite_class fields */
	 /* geometry_manager   */ GeometryManager,
	 /* change_managed     */ ChangeManaged,
	 /* insert_child       */ XtInheritInsertChild,
	 /* delete_child       */ XtInheritDeleteChild,
	 /* extension          */ NULL
	 }
	,
#ifdef LWLIB_USES_MOTIF
	{
	 /* constraint_class fields */
	 NULL,			/* resource list        */
	 0,			/* num resources        */
	 0,			/* constraint size      */
	 (XtInitProc) NULL,	/* init proc            */
	 (XtWidgetProc) NULL,	/* destroy proc         */
	 (XtSetValuesFunc) NULL,	/* set values proc      */
	 NULL,			/* extension            */
	 }
	,
	{
/* manager_class fields */
	 XtInheritTranslations,	/* translations           */
	 NULL,			/* syn_resources          */
	 0,			/* num_syn_resources      */
	 NULL,			/* syn_cont_resources     */
	 0,			/* num_syn_cont_resources */
	 XmInheritParentProcess,	/* parent_process         */
	 NULL,			/* extension              */
	 }
	,
#endif
	{
/* emacs_manager_class fields */
	 /* empty              */ 0,
	 }
};

WidgetClass emacsManagerWidgetClass = (WidgetClass) & emacsManagerClassRec;

/* What is my preferred size?  A suggested size may be given. */

static XtGeometryResult
QueryGeometry(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
	EmacsManagerWidget emw = (EmacsManagerWidget) w;
	EmacsManagerQueryGeometryStruct struc;
	int request_mode = request->request_mode;

	struc.request_mode = request_mode;
	struc.proposed_width = (request_mode & CWWidth) ? request->width : 0;
	struc.proposed_height = (request_mode & CWHeight) ? request->height : 0;
	XtCallCallbackList(w, emw->emacs_manager.query_geometry_callback,
			   &struc);
	reply->request_mode = CWWidth | CWHeight;
	reply->width = struc.proposed_width;
	reply->height = struc.proposed_height;
	if (((request_mode & CWWidth) && (request->width != reply->width)) ||
	    ((request_mode & CWHeight) && (request->height != reply->height)))
		return XtGeometryAlmost;
	return XtGeometryYes;
}

static void Resize(Widget w)
{
	EmacsManagerWidget emw = (EmacsManagerWidget) w;
	EmacsManagerResizeStruct struc;

	struc.width = w->core.width;
	struc.height = w->core.height;
	XtCallCallbackList(w, emw->emacs_manager.resize_callback, &struc);
}

static XtGeometryResult
GeometryManager(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
	/* Sure, any changes are fine. */

#ifdef LWLIB_MENUBARS_MOTIF
	/* The Motif menubar will merrily request a new size every time a
	   child is added or deleted.  Blow it off because it doesn't know
	   what it's talking about. */
	if (XtClass(w) != xmRowColumnWidgetClass)
#endif				/* LWLIB_MENUBARS_MOTIF */
	{
		if (request->request_mode & CWWidth)
			w->core.width = request->width;
		if (request->request_mode & CWHeight)
			w->core.height = request->height;
	}
	if (request->request_mode & CWBorderWidth)
		w->core.border_width = request->border_width;
	if (request->request_mode & CWX)
		w->core.x = request->x;
	if (request->request_mode & CWY)
		w->core.y = request->y;

	return XtGeometryYes;
}

static void ChangeManaged(Widget w)
{
	if (!XtIsRealized(w)) {
		XtWidgetGeometry request, reply;

		/* find out how big we'd like to be ... */

		request.request_mode = 0;
		XtQueryGeometry(w, &request, &reply);
		EmacsManagerChangeSize(w, reply.width, reply.height);
	}
}

static void
Realize(Widget w, Mask * valueMask, XSetWindowAttributes * attributes)
{
	attributes->bit_gravity = NorthWestGravity;
	*valueMask |= CWBitGravity;

	XtCreateWindow(w, (unsigned)InputOutput, (Visual *) CopyFromParent,
		       *valueMask, attributes);
}

static void ClassInitialize(void)
{
	return;
}

void EmacsManagerChangeSize(Widget w, Dimension width, Dimension height)
{
	if (width == 0)
		width = w->core.width;
	if (height == 0)
		height = w->core.height;

	/* do nothing if we're already that size */
	if (w->core.width != width || w->core.height != height) {
		XtGeometryResult result =
		    XtMakeResizeRequest(w, width, height, &w->core.width,
					&w->core.height);
		if (result == XtGeometryNo)
			return;
		if (result == XtGeometryAlmost)
			XtMakeResizeRequest(w, w->core.width, w->core.height,
					    NULL, NULL);
		Resize(w);
	}
}
