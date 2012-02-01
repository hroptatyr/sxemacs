/* Emacs shell widget -- define the two widgets.
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

/*
   It is rather unfortunate that we have to do this.  Blame those
   short-sighted people who designed the monstrosities known as
   Xt and ICCCM.
*/

/*
   This widget is not actually Emacs-specific; perhaps there could
   be a better name than "EmacsShell".  What it does is work around
   a limitation in Xt in correctly dealing with the window-manager
   size hints with applications that

   (a) dynamically change their window size
   (b) have a cell size (width-inc and height-inc) other than 1

   and

   (c) cannot predict in advance exactly what size their shell will be
       (This is the more common situation, when you have a number
       of widgets, each with their own size ideas)

   This widget assumes that your program contains a fixed "base size"
   plus some number of cells (e.g. character cells).  The WMShell
   resources "widthInc" and "heightInc" specify the size of a
   character cell, and the window manager will report the app's
   size in cells rather than in pixels.

   If you use this widget, do not use the WMShell resources
   "baseWidth", "baseHeight", "minWidth", or "minHeight".
   Instead, use "widthCells" and "heightCells" to specify the
   current size in cells (you must keep this up-to-date),
   and "minWidthCells" and "minHeightCells" to specify the
   minimum size in cells.

   Every time that the program issues a size command, the
   "baseWidth", "baseHeight", "minWidth", and "minHeight" fields
   of the WM_NORMAL_HINTS property will be updated to stay in
   line with the resource values specified above.  The calculations
   are done once the desired shell size is known but before the
   window-manager size-change request is issued. (We must do it
   at this time because before then we don't know what size we
   will request, and after the request the deed has already
   been done.)

   After you change the "baseWidth", "baseHeight", "minWidth",
   or "minHeight" resources, you need to call
   EmacsShellUpdateSizeHints() to manually update the size
   hints, except in the following two circumstances:

   (a) you are about to make a geometry request.
   (b) you are changing only "baseWidth" and "baseHeight"
       from within a resize procedure.  (In this case,
       the size hints are already correct.)

*/

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <X11/StringDefs.h>
#include "xintrinsicp.h"
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>
#include "EmacsShellP.h"

#if defined (DEFINE_TOP_LEVEL_EMACS_SHELL)
#define EMACS_SHELL_WIDGET TopLevelEmacsShellWidget
#define SUPERCLASS_WIDGET_CLASS topLevelShellWidgetClass
#define SUPERCLASS_CLASS_REC topLevelShellClassRec
#define EMACS_SHELL_REC TopLevelEmacsShellRec
#define EMACS_SHELL_CLASS_REC topLevelEmacsShellClassRec
#define EMACS_SHELL_CLASS_REC_TYPE TopLevelEmacsShellClassRec
#define EMACS_SHELL_CLASS_NAME "TopLevelEmacsShell"
#define EMACS_SHELL_WIDGET_CLASS topLevelEmacsShellWidgetClass
#define EMACS_SHELL_UPDATE_SIZE_HINTS TopLevelEmacsShellUpdateSizeHints
#elif defined (DEFINE_TRANSIENT_EMACS_SHELL)
#define EMACS_SHELL_WIDGET TransientEmacsShellWidget
#define SUPERCLASS_WIDGET_CLASS transientShellWidgetClass
#define SUPERCLASS_CLASS_REC transientShellClassRec
#define EMACS_SHELL_REC TransientEmacsShellRec
#define EMACS_SHELL_CLASS_REC transientEmacsShellClassRec
#define EMACS_SHELL_CLASS_REC_TYPE TransientEmacsShellClassRec
#define EMACS_SHELL_CLASS_NAME "TransientEmacsShell"
#define EMACS_SHELL_WIDGET_CLASS transientEmacsShellWidgetClass
#define EMACS_SHELL_UPDATE_SIZE_HINTS TransientEmacsShellUpdateSizeHints
#else
Error.Must define either DEFINE_TOP_LEVEL_EMACS_SHELL or
    DEFINE_TRANSIENT_EMACS_SHELL.
#endif
    typedef struct {
	XtPointer next_extension;
	XrmQuark record_type;
	long version;
	Cardinal record_size;
} GenericClassExtRec;

static XtGeometryResult RootGeometryManager(Widget gw,
					    XtWidgetGeometry * request,
					    XtWidgetGeometry * reply);
static void ChangeManaged(Widget w);

/* snarfed from Shell.c */
#define BIGSIZE ((Dimension)32767)

static XtResource resources[] = {
#define offset(field) XtOffset(EMACS_SHELL_WIDGET, emacs_shell.field)
#define coreoffset(field) XtOffset(EMACS_SHELL_WIDGET, core.field)
#ifdef LWLIB_USES_MOTIF
	/* *** BOGOSITY^10! *** The Motif VendorShell fucks around with
	   the default values for X and Y, for no obvious reason.  This
	   causes Shell to indicate that the defaults of (0,0) were
	   program-specified, instead of letting the WM do what it wants. */
	{XtNx, XtCPosition,
	 XtRPosition, sizeof(Position),
	 coreoffset(x), XtRImmediate, (XtPointer) BIGSIZE}
	,
	{XtNy, XtCPosition,
	 XtRPosition, sizeof(Position),
	 coreoffset(y), XtRImmediate, (XtPointer) BIGSIZE}
	,
#endif
	{XtNwidthCells, XtCWidthCells,
	 XtRInt, sizeof(int),
	 offset(width_cells), XtRImmediate, (XtPointer) 0},
	{XtNheightCells, XtCHeightCells,
	 XtRInt, sizeof(int),
	 offset(height_cells), XtRImmediate, (XtPointer) 0},
	{XtNminWidthCells, XtCMinWidthCells,
	 XtRInt, sizeof(int),
	 offset(min_width_cells), XtRImmediate, (XtPointer) 0},
	{XtNminHeightCells, XtCMinHeightCells,
	 XtRInt, sizeof(int),
	 offset(min_height_cells), XtRImmediate, (XtPointer) 0},
};

static CompositeClassExtensionRec compositeClassExtRec = {
	NULL,
	NULLQUARK,
	XtCompositeExtensionVersion,
	sizeof(CompositeClassExtensionRec),
	TRUE,
};

static ShellClassExtensionRec shellClassExtRec = {
	NULL,
	NULLQUARK,
	XtShellExtensionVersion,
	sizeof(ShellClassExtensionRec),
	RootGeometryManager
};

EMACS_SHELL_CLASS_REC_TYPE EMACS_SHELL_CLASS_REC = {
	{			/*
				 *        core_class fields
				 */
	 /* superclass         */ (WidgetClass) & SUPERCLASS_CLASS_REC,
	 /* class_name         */ (String) EMACS_SHELL_CLASS_NAME,
	 /* size               */ sizeof(EMACS_SHELL_REC),
	 /* Class Initializer  */ NULL,
						/* class_part_initialize */ NULL,
						/* XtInheritClassPartInitialize, */
	 /* Class init'ed ?    */ FALSE,
	 /* initialize         */ NULL,
	 /* initialize_notify  */ NULL,
	 /* realize            */ XtInheritRealize,
	 /* actions            */ NULL,
	 /* num_actions        */ 0,
	 /* resources          */ resources,
	 /* resource_count     */ XtNumber(resources),
	 /* xrm_class          */ NULLQUARK,
	 /* compress_motion    */ TRUE,
	 /* compress_exposure  */ XtExposeCompressMaximal | XtExposeNoRegion,
	 /* compress_enterleave */ TRUE,
	 /* visible_interest   */ TRUE,
	 /* destroy            */ NULL,
	 /* resize             */ XtInheritResize,
	 /* expose             */ NULL,
					/* set_values         */ NULL,
					/* XtInheritSetValues, */
	 /* set_values_hook    */ NULL,
	 /* set_values_almost  */ XtInheritSetValuesAlmost,
	 /* get_values_hook    */ NULL,
	 /* accept_focus       */ NULL,
	 /* intrinsics version */ XtVersion,
	 /* callback offsets   */ NULL,
	 /* tm_table           */ NULL,
	 /* query_geometry     */ NULL,
	 /* display_accelerator */ NULL,
	 /* extension          */ NULL
	 }
	, {			/* Composite */
	   /* geometry_manager   */ XtInheritGeometryManager,
	   /* change_managed     */ ChangeManaged,
	   /* insert_child       */ XtInheritInsertChild,
	   /* delete_child       */ XtInheritDeleteChild,
	   /* extension          */ (XtPointer) & compositeClassExtRec
	   }
	, {			/* Shell */
	   /* extension          */ (XtPointer) & shellClassExtRec
	   }
	, {			/* WMShell */
	   /* extension          */ NULL
	   }
	, {			/* VendorShell */
	   /* extension          */ NULL
	   }
	, {			/* TopLevelShell or TransientShell */
	   /* both have exactly one XtPointer here. */
	   /* extension          */ NULL
	   }
	, {			/* EmacsShell */
	   0}
};

WidgetClass EMACS_SHELL_WIDGET_CLASS = (WidgetClass) & EMACS_SHELL_CLASS_REC;

static void
update_size_hints_internal(EMACS_SHELL_WIDGET w, int width, int height)
{
	int base_width, base_height;
	int cell_width, cell_height;
	Arg al[10];

	/* time to update them thar size hints */
	cell_width = w->wm.size_hints.width_inc;
	cell_height = w->wm.size_hints.height_inc;
	base_width = width - cell_width * w->emacs_shell.width_cells;
	base_height = height - cell_height * w->emacs_shell.height_cells;
#ifdef DEBUG_GEOMETRY_MANAGEMENT
	/* Very useful info when debugging geometry management problems.
	   When it's guaranteed that no more such problems exist, take
	   this stuff out. */
	printf("update_size_hints_internal:\n");
	printf("  actual pixel size: %d %d\n", width, height);
	printf("  cell size in pixels: %d %d\n", cell_width, cell_height);
	printf("  text area size in cells: %d %d\n", w->emacs_shell.width_cells,
	       w->emacs_shell.height_cells);
	printf("  base size set to: %d %d\n", base_width, base_height);
	fflush(stdout);
#endif
	XtSetArg(al[0], XtNbaseWidth, base_width);
	XtSetArg(al[1], XtNbaseHeight, base_height);
	XtSetArg(al[2], XtNminWidth, base_width +
		 cell_width * w->emacs_shell.min_width_cells);
	XtSetArg(al[3], XtNminHeight, base_height +
		 cell_height * w->emacs_shell.min_height_cells);
	XtSetValues((Widget) w, al, 4);
}

static XtGeometryResult
SuperClassRootGeometryManager(Widget gw,
			      XtWidgetGeometry * request,
			      XtWidgetGeometry * reply)
{
	ShellWidgetClass swc = (ShellWidgetClass) SUPERCLASS_WIDGET_CLASS;
	ShellClassExtensionRec *scer;
	GenericClassExtRec *gcer;

	/* find the shell extension record that specifies the
	   root geometry manager method */
	for (gcer = (GenericClassExtRec *) swc->shell_class.extension;
	     gcer; gcer = (GenericClassExtRec *) gcer->next_extension) {
		if (gcer->record_type == NULLQUARK)
			break;
	}

	if (!gcer)
		abort();

	/* call it to actually make the geometry request */
	scer = (ShellClassExtensionRec *) gcer;
	return (scer->root_geometry_manager) (gw, request, reply);
}

static XtGeometryResult
RootGeometryManager(Widget gw,
		    XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
	EMACS_SHELL_WIDGET w = (EMACS_SHELL_WIDGET) gw;
	/* OK since this file is not dumped */
	static int reentrant = 0;
	XtGeometryResult result;

	if (reentrant)
		abort();
	reentrant++;

#ifdef DEBUG_GEOMETRY_MANAGEMENT
	printf("root_geometry_manager:\n");
	printf("  current shell size: %d %d\n", w->core.width, w->core.height);
	if (request->request_mode & CWWidth)
		printf("width requested;");
	if (request->request_mode & CWHeight)
		printf("height requested;");
	printf("\n");
	printf("  requested shell size: %d %d\n", request->width,
	       request->height);
#endif
	/* update the size hints */
	update_size_hints_internal(w,
				   request->request_mode & CWWidth ?
				   request->width : w->core.width,
				   request->request_mode & CWHeight ?
				   request->height : w->core.height);

	result = SuperClassRootGeometryManager(gw, request, reply);

#ifdef DEBUG_GEOMETRY_MANAGEMENT
	printf("  result: %s\n",
	       result == XtGeometryYes ? "XtGeometryYes" :
	       result == XtGeometryNo ? "XtGeometryNo" :
	       result == XtGeometryAlmost ? "XtGeometryAlmost" :
	       "XtGeometryDone");
	if (reply->request_mode & CWWidth)
		printf("width returned;");
	if (reply->request_mode & CWHeight)
		printf("height returned;");
	printf("\n");
	printf("  resulting shell size: %d %d\n", reply->width, reply->height);
	printf("----------\n");
	fflush(stdout);
#endif
	reentrant--;
	return result;
}

static void ChangeManaged(Widget wid)
{
	EMACS_SHELL_WIDGET w = (EMACS_SHELL_WIDGET) wid;

	/* If not realized, then we're being called from XtRealizeWidget().
	   RootGeometryManager() has not yet been called, and thus our
	   base size is incorrect.  We need to set it now or the Shell
	   will mess up geometry specifications with negative positional
	   offsets. */
	if (!XtIsRealized(wid)) {
		Widget child = NULL;
		Cardinal i;

		/* the managed child indicates what our size is */
		for (i = 0; i < w->composite.num_children; i++) {
			if (XtIsManaged(w->composite.children[i])) {
				child = w->composite.children[i];
				update_size_hints_internal(w, child->core.width,
							   child->core.height);
			break;
			}
		}

	}

	/* call the real ChangeManaged */
	(((ShellWidgetClass) SUPERCLASS_WIDGET_CLASS)->
	 composite_class.change_managed) (wid);
}

/******************* external entry points *********************/

void EMACS_SHELL_UPDATE_SIZE_HINTS(Widget gw)
{
	EMACS_SHELL_WIDGET w = (EMACS_SHELL_WIDGET) gw;
	update_size_hints_internal(w, w->core.width, w->core.height);
}
