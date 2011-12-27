/* The emacs frame widget.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* #### Note to potential hackers: Don't mess with this unless you're
   sure you know what you're doing!  Xt is a lot more subtle than
   you may think. */

#include <config.h>
#include "lisp.h"

#include "ui/X11/console-x.h"
#include "ui/X11/glyphs-x.h"
#include "ui/X11/objects-x.h"
#include <X11/Shell.h>
#include "EmacsFrameP.h"
#include "EmacsManager.h"	/* for EmacsManagerChangeSize */
#include "ui/X11/xmu.h"

#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/toolbar.h"
#include "ui/window.h"

static void EmacsFrameClassInitialize(void);
static void EmacsFrameInitialize(Widget, Widget, ArgList, Cardinal *);
static void EmacsFrameRealize(Widget, XtValueMask *, XSetWindowAttributes *);
static void EmacsFrameResize(Widget widget);
static Boolean EmacsFrameSetValues(Widget, Widget, Widget, ArgList, Cardinal *);
static XtGeometryResult EmacsFrameQueryGeometry(Widget, XtWidgetGeometry *,
						XtWidgetGeometry *);

extern void emacs_Xt_mapping_action(Widget w, XEvent * event);

#undef XtOffset
#define XtOffset(p_type,field) \
	((Cardinal) (((char *) (&(((p_type)0)->field))) - ((char *)0)))
#define offset(field) XtOffset (EmacsFrame, emacs_frame.field)

static XtResource resources[] = {
	{XtNgeometry, XtCGeometry,
	 XtRString, sizeof(String),
	 offset(geometry), XtRString, (XtPointer) 0}
	,
	{XtNiconic, XtCIconic,
	 XtRBoolean, sizeof(Boolean),
	 offset(iconic), XtRImmediate, (XtPointer) False}
	,

	{XtNemacsFrame, XtCEmacsFrame,
	 XtRPointer, sizeof(XtPointer),
	 offset(frame), XtRImmediate, 0}
	,
	{XtNmenubar, XtCMenubar,
	 XtRBoolean, sizeof(Boolean),
	 offset(menubar_p), XtRImmediate, (XtPointer) True}
	,
	{XtNinitiallyUnmapped, XtCInitiallyUnmapped,
	 XtRBoolean, sizeof(Boolean),
	 offset(initially_unmapped), XtRImmediate, (XtPointer) False}
	,
	{XtNminibuffer, XtCMinibuffer,
	 XtRBoolean, sizeof(Boolean),
	 offset(minibuffer), XtRImmediate, (XtPointer) True}
	,
	{XtNunsplittable, XtCUnsplittable,
	 XtRBoolean, sizeof(Boolean),
	 offset(unsplittable), XtRImmediate, (XtPointer) False}
	,
	{XtNinternalBorderWidth, XtCInternalBorderWidth,
	 XtRInt, sizeof(int),
	 offset(internal_border_width), XtRImmediate, (XtPointer) 4},
#ifdef HAVE_SCROLLBARS
	{XtNscrollBarWidth, XtCScrollBarWidth,
	 XtRInt, sizeof(int),
	 offset(scrollbar_width), XtRImmediate, (XtPointer) - 1},
	{XtNscrollBarHeight, XtCScrollBarHeight,
	 XtRInt, sizeof(int),
	 offset(scrollbar_height), XtRImmediate, (XtPointer) - 1},
	{XtNscrollBarPlacement, XtCScrollBarPlacement,
	 XtRScrollBarPlacement, sizeof(unsigned char),
	 offset(scrollbar_placement), XtRImmediate,
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID) || \
    defined (LWLIB_SCROLLBARS_ATHENA3D)
	 (XtPointer) XtBOTTOM_RIGHT
#else
	 (XtPointer) XtBOTTOM_LEFT
#endif
	 },
#endif				/* HAVE_SCROLLBARS */

#ifdef HAVE_TOOLBARS
	{XtNtopToolBarHeight, XtCTopToolBarHeight,
	 XtRInt, sizeof(int),
	 offset(top_toolbar_height), XtRImmediate, (XtPointer) - 1},
	{XtNbottomToolBarHeight, XtCBottomToolBarHeight,
	 XtRInt, sizeof(int),
	 offset(bottom_toolbar_height), XtRImmediate, (XtPointer) - 1},
	{XtNleftToolBarWidth, XtCLeftToolBarWidth,
	 XtRInt, sizeof(int),
	 offset(left_toolbar_width), XtRImmediate, (XtPointer) - 1},
	{XtNrightToolBarWidth, XtCRightToolBarWidth,
	 XtRInt, sizeof(int),
	 offset(right_toolbar_width), XtRImmediate, (XtPointer) - 1},
	{XtNtopToolBarBorderWidth, XtCTopToolBarBorderWidth,
	 XtRInt, sizeof(int),
	 offset(top_toolbar_border_width), XtRImmediate, (XtPointer) - 1},
	{XtNbottomToolBarBorderWidth, XtCBottomToolBarBorderWidth,
	 XtRInt, sizeof(int),
	 offset(bottom_toolbar_border_width), XtRImmediate, (XtPointer) - 1},
	{XtNleftToolBarBorderWidth, XtCLeftToolBarBorderWidth,
	 XtRInt, sizeof(int),
	 offset(left_toolbar_border_width), XtRImmediate, (XtPointer) - 1},
	{XtNrightToolBarBorderWidth, XtCRightToolBarBorderWidth,
	 XtRInt, sizeof(int),
	 offset(right_toolbar_border_width), XtRImmediate, (XtPointer) - 1},
	{XtNtopToolBarShadowColor, XtCTopToolBarShadowColor,
	 XtRPixel, sizeof(Pixel),
	 offset(top_toolbar_shadow_pixel), XtRString, (XtPointer) "#000000"}
	,
	{XtNbottomToolBarShadowColor, XtCBottomToolBarShadowColor,
	 XtRPixel, sizeof(Pixel),
	 offset(bottom_toolbar_shadow_pixel), XtRString, (XtPointer) "#000000"}
	,
	{XtNbackgroundToolBarColor, XtCBackgroundToolBarColor,
	 XtRPixel, sizeof(Pixel),
	 offset(background_toolbar_pixel), XtRImmediate, (XtPointer) - 1}
	,
	{XtNforegroundToolBarColor, XtCForegroundToolBarColor,
	 XtRPixel, sizeof(Pixel),
	 offset(foreground_toolbar_pixel), XtRImmediate, (XtPointer) - 1}
	,
	{XtNtopToolBarShadowPixmap, XtCTopToolBarShadowPixmap,
	 XtRPixmap, sizeof(Pixmap),
	 offset(top_toolbar_shadow_pixmap), XtRImmediate, (XtPointer) None}
	,
	{XtNbottomToolBarShadowPixmap, XtCBottomToolBarShadowPixmap,
	 XtRPixmap, sizeof(Pixmap),
	 offset(bottom_toolbar_shadow_pixmap), XtRImmediate, (XtPointer) None}
	,
	{XtNtoolBarShadowThickness, XtCToolBarShadowThickness,
	 XtRDimension, sizeof(Dimension),
	 offset(toolbar_shadow_thickness), XtRImmediate, (XtPointer) 2}
	,
#endif				/* HAVE_TOOLBARS */

	{XtNinterline, XtCInterline,
	 XtRInt, sizeof(int),
	 offset(interline), XtRImmediate, (XtPointer) 0},
	{
#ifdef I18N4
	 XtNfontSet, XtCFontSet,
	 XtRFontSet, sizeof(XFontSet),
#else
	 XtNfont, XtCFont,
	 XtRFontStruct, sizeof(XFontStruct *),
#endif
	 offset(font), XtRImmediate, (XtPointer) 0}
	,
	{XtNforeground, XtCForeground,
	 XtRPixel, sizeof(Pixel),
	 offset(foreground_pixel), XtRString, (XtPointer) "Black"}
	,
	{XtNbackground, XtCBackground,
	 XtRPixel, sizeof(Pixel),
	 offset(background_pixel), XtRString, (XtPointer) "Gray80"}
	,
	{XtNcursorColor, XtCForeground,
	 XtRPixel, sizeof(Pixel),
	 offset(cursor_color), XtRString, (XtPointer) "XtDefaultForeground"}
	,
	{XtNbarCursor, XtCBarCursor,
	 XtRBoolean, sizeof(Boolean),
	 offset(bar_cursor), XtRImmediate, (XtPointer) 0}
	,
	{XtNvisualBell, XtCVisualBell,
	 XtRBoolean, sizeof(Boolean),
	 offset(visual_bell), XtRImmediate, (XtPointer) 0}
	,
	{XtNbellVolume, XtCBellVolume,
	 XtRInt, sizeof(int),
	 offset(bell_volume), XtRImmediate, (XtPointer) 0},
	{XtNuseBackingStore, XtCUseBackingStore,
	 XtRBoolean, sizeof(Boolean),
	 offset(use_backing_store), XtRImmediate, (XtPointer) NotUseful}
	,
	{XtNpreferredWidth, XtCPreferredWidth,
	 XtRDimension, sizeof(Dimension),
	 offset(preferred_width), XtRImmediate, (XtPointer) 0}
	,
	{XtNpreferredHeight, XtCPreferredHeight,
	 XtRDimension, sizeof(Dimension),
	 offset(preferred_height), XtRImmediate, (XtPointer) 0}
	,
};

#undef offset

/* Xt is stupid and dumb.
   Xt is stupid and dumb.
   Xt is stupid and dumb. */

static XtActionsRec emacsFrameActionsTable[] = {
	{"mapping", (XtActionProc) emacs_Xt_mapping_action},
};

static char emacsFrameTranslations[] = "\
<Mapping>: mapping()\n\
";

/* If we're running under Motif, make this widget a subclass
   of XmPrimitive.  It's not clear this is necessary, but it
   may make focus behavior work better. */

EmacsFrameClassRec emacsFrameClassRec = {
	{			/* core fields */
#ifdef LWLIB_USES_MOTIF
	 /* superclass               */ (WidgetClass) & xmPrimitiveClassRec,
#else
	 /* superclass               */ &widgetClassRec,
#endif
	 /* class_name               */ "EmacsFrame",
	 /* widget_size              */ sizeof(EmacsFrameRec),
	 /* class_initialize         */ EmacsFrameClassInitialize,
	 /* class_part_initialize    */ 0,
	 /* class_inited             */ FALSE,
	 /* initialize               */ EmacsFrameInitialize,
	 /* initialize_hook          */ 0,
	 /* realize                  */ EmacsFrameRealize,
	 /* actions                  */ emacsFrameActionsTable,
	 /* num_actions              */ XtNumber(emacsFrameActionsTable),
	 /* resources                */ resources,
	 /* resource_count           */ XtNumber(resources),
	 /* xrm_class                */ NULLQUARK,
	 /* compress_motion          */ TRUE,
#ifdef LWLIB_USES_MOTIF
	 /* compress_exposure        */ TRUE,
#else
	 /* compress_exposure        */
	 XtExposeCompressMaximal | XtExposeNoRegion,
#endif
	 /* compress_enterleave      */ TRUE,
	 /* visible_interest         */ FALSE,
	 /* destroy                  */ NULL,
	 /* resize                   */ EmacsFrameResize,
	 /* expose                   */ XtInheritExpose,
	 /* set_values               */ EmacsFrameSetValues,
	 /* set_values_hook          */ 0,
	 /* set_values_almost        */ XtInheritSetValuesAlmost,
	 /* get_values_hook          */ 0,
	 /* accept_focus             */ XtInheritAcceptFocus,
	 /* version                  */ XtVersion,
	 /* callback_private         */ 0,
	 /* tm_table                 */ emacsFrameTranslations,
	 /* query_geometry           */ EmacsFrameQueryGeometry,
	 /* display_accelerator      */ XtInheritDisplayAccelerator,
	 /* extension                */ 0
	 }
	,
#ifdef LWLIB_USES_MOTIF
	{			/* XmPrimitiveClassPart
				 */
	 (XtWidgetProc) _XtInherit,	/* border_highlight */
	 (XtWidgetProc) _XtInherit,	/* border_unhighlight */
	 /* Setting the following to NULL causes PrimitiveInitialize()
	    not to add traversal (TAB etc. to switch focus) and
	    focus-in/out (border highlight/unhighlight) translations.
	    If you want those translations, use the value XtInheritTranslations
	    instead.  Doing this, however, will interfere with Emacs
	    focus handling (which highlights/unhighlights the text cursor),
	    and will lead to strange display results around the border of the
	    widget. */
	 NULL,			/* translations */
	 NULL,			/* arm_and_activate */
	 NULL,			/* get resources */
	 0,			/* num get_resources */
	 NULL,			/* extension */
	 }
	,
#endif				/* LWLIB_USES_MOTIF */
	{
	 0}
};
WidgetClass emacsFrameClass = (WidgetClass) & emacsFrameClassRec;

static void update_various_frame_slots(EmacsFrame ew)
{
	ew->emacs_frame.frame->pixheight = ew->core.height;
	ew->emacs_frame.frame->pixwidth = ew->core.width;
}

static void
EmacsFrameInitialize(Widget request, Widget new, ArgList dum1, Cardinal * dum2)
{
	EmacsFrame ew = (EmacsFrame) new;
	struct frame *f = ew->emacs_frame.frame;

	if (!f)
		fatal("can't create an emacs frame widget without a frame.");

	ew->emacs_frame.frame->internal_border_width =
	    ew->emacs_frame.internal_border_width;
}

void emacs_Xt_event_handler(Widget wid /* unused */ ,
			    XtPointer closure /* unused */ ,
			    XEvent * event,
			    Boolean * continue_to_dispatch /* unused */ );

static void
EmacsFrameRealize(Widget widget, XtValueMask * mask,
		  XSetWindowAttributes * attrs)
{
	EmacsFrame ew = (EmacsFrame) widget;
	struct frame *f = ew->emacs_frame.frame;
	Widget shell_widget = FRAME_X_SHELL_WIDGET(f);

	attrs->event_mask =
	    ExposureMask |
	    VisibilityChangeMask |
	    PropertyChangeMask | StructureNotifyMask | SubstructureNotifyMask |
	    /*SubstructureRedirectMask | *//* Only for WMs! */
	    KeyPressMask |
	    KeyReleaseMask |
	    ButtonPressMask |
	    ButtonReleaseMask |
	    FocusChangeMask |
	    PointerMotionHintMask |
	    PointerMotionMask | LeaveWindowMask | EnterWindowMask;

#ifdef I18N4
	/* Make sure that events wanted by the input method are selected. */
	attrs->event_mask |= input_method_event_mask;
#endif

	*mask |= CWEventMask;

	if (ew->emacs_frame.use_backing_store) {
		attrs->backing_store = Always;
		*mask |= CWBackingStore;
	}
	XtCreateWindow(widget, InputOutput, (Visual *) CopyFromParent, *mask,
		       attrs);

	/* snarf the events we want. */
	XtInsertEventHandler(widget, attrs->event_mask, TRUE,
			     emacs_Xt_event_handler, NULL, XtListHead);
	/* some events (e.g. map-notify and WM_DELETE_WINDOW) get sent
	   directly to the shell, and the above event handler won't see
	   them.  So add a handler to get them.  These events don't
	   propagate, so there's no danger of them being seen twice. */
	XtInsertEventHandler(shell_widget,
			     EnterWindowMask | LeaveWindowMask |
			     VisibilityChangeMask | StructureNotifyMask |
			     KeyPressMask,
			     TRUE, emacs_Xt_event_handler, NULL, XtListHead);

#ifdef EXTERNAL_WIDGET
	/* #### Not sure if this special case is necessary */
	if (!FRAME_X_EXTERNAL_WINDOW_P(f))
#endif
		/* This is necessary under Motif in order to make it possible to click in
		   a buffer and move focus out of a dialog box or control panel and back
		   into emacs-land; also necessary so that you can still type chars
		   if the cursor is over the menubar or scrollbar. */
		lw_set_keyboard_focus(shell_widget, FRAME_X_TEXT_WIDGET(f));
}

/* DO NOT CALL THIS FUNCTION!  Only Xt is supposed to do this. */

static void EmacsFrameResize(Widget widget)
{
	EmacsFrame ew = (EmacsFrame) widget;
	struct frame *f = ew->emacs_frame.frame;
	int columns;
	int rows;
	XtWidgetGeometry req, repl;

	update_various_frame_slots(ew);

	pixel_to_char_size(f, ew->core.width, ew->core.height, &columns, &rows);
	change_frame_size(f, rows, columns, 0);

	/* Now we tell the EmacsShell that we've changed the size of the non-fixed
	   portion of the frame.  Note that, if we the resize occurred as a result
	   of EmacsFrameSetCharSize(), this information will be stored twice.
	   This is not a big deal, as storing this information doesn't actually
	   do anything until the next resize. */
	if (FRAME_X_TOP_LEVEL_FRAME_P(f))
		x_wm_set_variable_size(FRAME_X_SHELL_WIDGET(f), columns, rows);

	/* Kick the manager so that it knows we've changed size. */
	req.request_mode = 0;
	XtQueryGeometry(FRAME_X_CONTAINER_WIDGET(f), &req, &repl);
	EmacsManagerChangeSize(FRAME_X_CONTAINER_WIDGET(f), repl.width,
			       repl.height);
}

static Boolean
EmacsFrameSetValues(Widget cur_widget, Widget req_widget, Widget new_widget,
		    ArgList argv, Cardinal * argc)
{
	EmacsFrame cur = (EmacsFrame) cur_widget;
	EmacsFrame new = (EmacsFrame) new_widget;
	struct frame *f = new->emacs_frame.frame;
	Lisp_Object frame;

	XSETFRAME(frame, f);
	in_resource_setting++;
	/* This function does not need to do much.  Pretty much everything
	   interesting will get done in the resize method, which will
	   (if necessary) get called by Xt when this function returns
	   (see below).
	 */

	/* #### This function will not work if it is not called from
	   update_EmacsFrame(), called from SET_FACE_PROPERTY().
	   The code located there should be moved inside of here instead,
	   so that things work if either SET_FACE_PROPERTY() is
	   called or XtSetValues() is called.
	 */

	if (cur->emacs_frame.iconic != new->emacs_frame.iconic &&
	    FRAME_X_TOP_LEVEL_FRAME_P(new->emacs_frame.frame))
		x_wm_set_shell_iconic_p(FRAME_X_SHELL_WIDGET
					(new->emacs_frame.frame),
					new->emacs_frame.iconic);

	/* If we got here, then we were likely called as a result of
	   the EditRes protocol, so go ahead and change scrollbar-width
	   and scrollbar-height.  Otherwise, we're merely mirroring
	   a change made to scrollbar-width etc. so don't do anything
	   special. */
	if (cur->emacs_frame.internal_border_width !=
	    new->emacs_frame.internal_border_width) {
		f->internal_border_width =
		    new->emacs_frame.internal_border_width;
		MARK_FRAME_SIZE_SLIPPED(f);
	}
#ifdef HAVE_SCROLLBARS
	if (cur->emacs_frame.scrollbar_width !=
	    new->emacs_frame.scrollbar_width)
		Fadd_spec_to_specifier
		    (Vscrollbar_width,
		     make_int(new->emacs_frame.scrollbar_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.scrollbar_height !=
	    new->emacs_frame.scrollbar_height)
		Fadd_spec_to_specifier
		    (Vscrollbar_height,
		     make_int(new->emacs_frame.scrollbar_height),
		     frame, Qnil, Qnil);
#endif				/* HAVE_SCROLLBARS */
#ifdef HAVE_TOOLBARS
	if (cur->emacs_frame.top_toolbar_height !=
	    new->emacs_frame.top_toolbar_height)
		Fadd_spec_to_specifier
		    (Vtoolbar_size[TOP_TOOLBAR],
		     make_int(new->emacs_frame.top_toolbar_height),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.bottom_toolbar_height !=
	    new->emacs_frame.bottom_toolbar_height)
		Fadd_spec_to_specifier
		    (Vtoolbar_size[BOTTOM_TOOLBAR],
		     make_int(new->emacs_frame.bottom_toolbar_height),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.left_toolbar_width !=
	    new->emacs_frame.left_toolbar_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_size[LEFT_TOOLBAR],
		     make_int(new->emacs_frame.left_toolbar_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.right_toolbar_width !=
	    new->emacs_frame.right_toolbar_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_size[RIGHT_TOOLBAR],
		     make_int(new->emacs_frame.right_toolbar_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.top_toolbar_border_width !=
	    new->emacs_frame.top_toolbar_border_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_border_width[TOP_TOOLBAR],
		     make_int(new->emacs_frame.top_toolbar_border_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.bottom_toolbar_border_width !=
	    new->emacs_frame.bottom_toolbar_border_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_border_width[BOTTOM_TOOLBAR],
		     make_int(new->emacs_frame.bottom_toolbar_border_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.left_toolbar_border_width !=
	    new->emacs_frame.left_toolbar_border_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_border_width[LEFT_TOOLBAR],
		     make_int(new->emacs_frame.left_toolbar_border_width),
		     frame, Qnil, Qnil);
	if (cur->emacs_frame.right_toolbar_border_width !=
	    new->emacs_frame.right_toolbar_border_width)
		Fadd_spec_to_specifier
		    (Vtoolbar_border_width[RIGHT_TOOLBAR],
		     make_int(new->emacs_frame.right_toolbar_border_width),
		     frame, Qnil, Qnil);
#endif				/* HAVE_TOOLBARS */

	in_resource_setting--;

	/* If the request was to resize us, but the size has not changed, Xt
	   will do nothing, and won't call our resize callback. Since such a
	   request might be issued as a result of hiding/showing menubar or
	   changing toolbar placement, where we rely on relayout made by the
	   callback, we go ahead and simulate such a call */
	if (cur->core.width == new->core.width
	    && cur->core.height == new->core.height) {
		Cardinal i;
		for (i = 0; i < *argc; i++)
			if (strcmp(argv[i].name, XtNwidth) == 0
			    || strcmp(argv[i].name, XtNheight) == 0) {
				EmacsFrameResize(new_widget);
				break;
			}
	}

	return False;

	/* Note that if either (a) we return True, or (b) the width or
	   height has changed, an Expose event will be generated.  The Xt
	   manual says you should not return True if the width or height has
	   changed, because then two Expose events will be generated.

	   In any case, there is no need to return True because
	   SET_FACE_PROPERTY(), which does the resource
	   setting, automatically forces a redisplay as necessary. */
}

static XtGeometryResult
EmacsFrameQueryGeometry(Widget widget, XtWidgetGeometry * request,
			XtWidgetGeometry * result)
{
	EmacsFrame ew = (EmacsFrame) widget;
	int mask = request->request_mode;
	Dimension width, height;
	int ok_width_int, ok_height_int;
	Dimension ok_width, ok_height;

	/* We have a definite preference for what size we would like
	   to be.

	   1) If a preferred size was specified for us, use it.
	   (This is not currently used)
	   2) If a proposed size was given, round it to the nearest
	   multiple of the default char size and return it.
	   3) Otherwise, take our current size and round it to the
	   nearest multiple of the default char size. */

	width = mask & CWWidth ? request->width : ew->core.width;
	height = mask & CWHeight ? request->height : ew->core.height;
	round_size_to_char(ew->emacs_frame.frame, width, height,
			   &ok_width_int, &ok_height_int);
	ok_width = (Dimension) ok_width_int;
	ok_height = (Dimension) ok_height_int;
	if (ew->emacs_frame.preferred_width)
		ok_width = ew->emacs_frame.preferred_width;
	if (ew->emacs_frame.preferred_height)
		ok_height = ew->emacs_frame.preferred_height;
	result->request_mode |= CWWidth | CWHeight;
	result->width = ok_width;
	result->height = ok_height;
	if (((mask & CWWidth) && ok_width != request->width)
	    || ((mask & CWHeight) && ok_height != request->height))
		return XtGeometryAlmost;
	else
		return XtGeometryYes;
}

/* Xt string-to-scrollbar-placement converter */
/* #### Convert this to a `new-style' converter (See XtAddTypeConverter) */

/* This variable cannot be a stack variable. */
static unsigned char cvt_string_scrollbar_placement;

/* ARGSUSED */
static void Xt_StringToScrollBarPlacement(XrmValuePtr args,	/* unused */
					  Cardinal * num_args,	/* unused */
					  XrmValuePtr fromVal,
					  XrmValuePtr toVal)
{
	XrmQuark q;
	char *lowerName = (char *)alloca(strlen((char *)fromVal->addr) + 1);

	XmuCopyISOLatin1Lowered(lowerName, (char *)fromVal->addr);
	q = XrmStringToQuark(lowerName);

	toVal->size = sizeof(cvt_string_scrollbar_placement);
	toVal->addr = (XPointer) & cvt_string_scrollbar_placement;

	if (q == XrmStringToQuark("top-left")
	    || q == XrmStringToQuark("top_left"))
		cvt_string_scrollbar_placement = XtTOP_LEFT;
	else if (q == XrmStringToQuark("bottom-left")
		 || q == XrmStringToQuark("bottom_left"))
		cvt_string_scrollbar_placement = XtBOTTOM_LEFT;
	else if (q == XrmStringToQuark("top-right")
		 || q == XrmStringToQuark("top_right"))
		cvt_string_scrollbar_placement = XtTOP_RIGHT;
	else if (q == XrmStringToQuark("bottom-right")
		 || q == XrmStringToQuark("bottom_right"))
		cvt_string_scrollbar_placement = XtBOTTOM_RIGHT;
	else {
		XtStringConversionWarning(fromVal->addr, "scrollBarPlacement");
		toVal->addr = NULL;
		toVal->size = 0;
	}
}

static void EmacsFrameClassInitialize(void)
{
	XtAddConverter(XtRString, XtRScrollBarPlacement,
		       Xt_StringToScrollBarPlacement, NULL, 0);
}

/********************* Special entrypoints *******************/

void EmacsFrameRecomputeCellSize(Widget w)
{
	EmacsFrame ew = (EmacsFrame) w;
	int cw, ch;
	struct frame *f = ew->emacs_frame.frame;

	if (!XtIsSubclass(w, emacsFrameClass))
		abort();

	default_face_height_and_width(make_frame(f), &ch, &cw);
	if (FRAME_X_TOP_LEVEL_FRAME_P(f))
		x_wm_set_cell_size(FRAME_X_SHELL_WIDGET(f), cw, ch);
}

/* Set the size of the widget to have the number of rows and columns
   specified.  This both causes the X window to change and the
   internal frame structures to get modified to match. */

void EmacsFrameSetCharSize(Widget widget, int columns, int rows)
{
	EmacsFrame ew = (EmacsFrame) widget;
	int pixel_width, pixel_height;
	struct frame *f = ew->emacs_frame.frame;

	if (columns < 3)
		columns = 3;	/* no way buddy */
	if (rows < 1)
		rows = 1;

	char_to_pixel_size(f, columns, rows, &pixel_width, &pixel_height);

	if (FRAME_X_TOP_LEVEL_FRAME_P(f))
		x_wm_set_variable_size(FRAME_X_SHELL_WIDGET(f), columns, rows);

	{
		Arg al[2];
		XtSetArg(al[0], XtNwidth, pixel_width);
		XtSetArg(al[1], XtNheight, pixel_height);
		XtSetValues((Widget) ew, al, countof(al));
	}
}
