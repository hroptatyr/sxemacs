/* scrollbar implementation -- X interface.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Amdahl Corporation.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Darrell Kindred <dkindred+@cmu.edu>.

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

/* This file Mule-ized (more like Mule-verified) by Ben Wing, 7-8-00. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "EmacsFrame.h"
#include "glyphs-x.h"
#include "gui-x.h"
#include "scrollbar-x.h"

#include "ui/frame.h"
#include "ui/window.h"

static void x_update_vertical_scrollbar_callback(Widget widget, LWLIB_ID id,
						 XtPointer client_data);
static void x_update_horizontal_scrollbar_callback(Widget widget, LWLIB_ID id,
						   XtPointer client_data);

/* Used to prevent changing the size of the slider while drag
   scrolling, under Motif.  This is necessary because the Motif
   scrollbar is incredibly stupid about updating the slider and causes
   lots of flicker if it is done too often.  */
static int inhibit_slider_size_change;
int stupid_vertical_scrollbar_drag_hack;

/* Doesn't work with athena */
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
static int vertical_drag_in_progress;
#endif

/* A device method. */
static int x_inhibit_scrollbar_slider_size_change(void)
{
	/* Doesn't work with Athena */
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
	return inhibit_slider_size_change;
#else
	return 0;
#endif
}

/* A device method. */
static void x_free_scrollbar_instance(struct scrollbar_instance *instance)
{
	if ( instance && instance->scrollbar_data ) {

		if (SCROLLBAR_X_NAME(instance))
			xfree(SCROLLBAR_X_NAME(instance));

		if (SCROLLBAR_X_WIDGET(instance)) {
			if (XtIsManaged(SCROLLBAR_X_WIDGET(instance)))
				XtUnmanageChild(SCROLLBAR_X_WIDGET(instance));

			lw_destroy_all_widgets(SCROLLBAR_X_ID(instance));
		}
		xfree(instance->scrollbar_data);
	}
}

/* A device method. */
static void x_release_scrollbar_instance(struct scrollbar_instance *instance)
{
	if (XtIsManaged(SCROLLBAR_X_WIDGET(instance)))
		XtUnmanageChild(SCROLLBAR_X_WIDGET(instance));
}

/* A device method. */
static void
x_create_scrollbar_instance(struct frame *f, int vertical,
			    struct scrollbar_instance *instance)
{
	char buffer[32];
	int sz;

	/* initialize the X specific data section. */
	instance->scrollbar_data = xnew_and_zero(struct x_scrollbar_data);

	SCROLLBAR_X_ID(instance) = new_lwlib_id();
	sz = snprintf(buffer, sizeof(buffer), "scrollbar_%d", SCROLLBAR_X_ID(instance));
	assert(sz >= 0 && (size_t)sz < sizeof(buffer));
	SCROLLBAR_X_NAME(instance) = xstrdup(buffer);
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID) || \
    defined (LWLIB_SCROLLBARS_ATHENA3D)
	SCROLLBAR_X_VDRAG_ORIG_VALUE(instance) = -1;
#endif

	if (vertical) {
		SCROLLBAR_X_WIDGET(instance) =
		    lw_create_widget("vertical-scrollbar",
				     SCROLLBAR_X_NAME(instance),
				     SCROLLBAR_X_ID(instance), NULL,
				     FRAME_X_CONTAINER_WIDGET(f), 0,
				     x_update_vertical_scrollbar_callback, NULL,
				     NULL);
	} else {
		SCROLLBAR_X_WIDGET(instance) =
		    lw_create_widget("horizontal-scrollbar",
				     SCROLLBAR_X_NAME(instance),
				     SCROLLBAR_X_ID(instance), NULL,
				     FRAME_X_CONTAINER_WIDGET(f), 0,
				     x_update_horizontal_scrollbar_callback,
				     NULL, NULL);
	}
}

#define UPDATE_DATA_FIELD(field)				\
  if (new_##field >= 0 &&					\
      SCROLLBAR_X_POS_DATA (inst).field != new_##field) {	\
    SCROLLBAR_X_POS_DATA (inst).field = new_##field;		\
    inst->scrollbar_instance_changed = 1;			\
  }

/* A device method. */
/* #### The -1 check is such a hack. */
static void
x_update_scrollbar_instance_values(struct window *w,
				   struct scrollbar_instance *inst,
				   int new_line_increment,
				   int new_page_increment,
				   int new_minimum, int new_maximum,
				   int new_slider_size,
				   int new_slider_position,
				   int new_scrollbar_width,
				   int new_scrollbar_height,
				   int new_scrollbar_x, int new_scrollbar_y)
{
	UPDATE_DATA_FIELD(line_increment);
	UPDATE_DATA_FIELD(page_increment);
	UPDATE_DATA_FIELD(minimum);
	UPDATE_DATA_FIELD(maximum);
	UPDATE_DATA_FIELD(slider_size);
	UPDATE_DATA_FIELD(slider_position);
	UPDATE_DATA_FIELD(scrollbar_width);
	UPDATE_DATA_FIELD(scrollbar_height);
	UPDATE_DATA_FIELD(scrollbar_x);
	UPDATE_DATA_FIELD(scrollbar_y);

	/* This doesn't work with Athena, why? */
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
	if (w && !vertical_drag_in_progress) {
		int new_vov = SCROLLBAR_X_POS_DATA(inst).slider_position;
		int new_vows = marker_position(w->start[CURRENT_DISP]);

		if (SCROLLBAR_X_VDRAG_ORIG_VALUE(inst) != new_vov) {
			SCROLLBAR_X_VDRAG_ORIG_VALUE(inst) = new_vov;
			inst->scrollbar_instance_changed = 1;
		}
		if (SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(inst) != new_vows) {
			SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(inst) = new_vows;
			inst->scrollbar_instance_changed = 1;
		}
	}
#endif
}

/* Used by x_update_scrollbar_instance_status. */
static void update_one_scrollbar_bs(struct frame *f, Widget sb_widget)
{
	Boolean use_backing_store;

	Xt_GET_VALUE(FRAME_X_TEXT_WIDGET(f), XtNuseBackingStore,
		     &use_backing_store);

	if (use_backing_store && sb_widget) {
		unsigned long mask = CWBackingStore;
		XSetWindowAttributes attrs;

		attrs.backing_store = Always;
		XChangeWindowAttributes(XtDisplay(sb_widget),
					XtWindow(sb_widget), mask, &attrs);
	}
}

/* Create a widget value structure for passing down to lwlib so that
   it can update the scrollbar widgets.  Used by
   x_update_scrollbar_instance_status. */
static widget_value *scrollbar_instance_to_widget_value(struct
							scrollbar_instance
							*instance)
{
	widget_value *wv;

	wv = xmalloc_widget_value();
	/* #### maybe should add malloc_scrollbar_values to resource these? */
	wv->scrollbar_data = xnew(scrollbar_values);

	wv->name = SCROLLBAR_X_NAME(instance);
	wv->name = xstrdup(wv->name);
	wv->value = 0;
	wv->key = 0;
	wv->enabled = instance->scrollbar_is_active;
	wv->selected = 0;
	wv->call_data = NULL;

	*wv->scrollbar_data = SCROLLBAR_X_POS_DATA(instance);

	wv->next = NULL;

	return wv;
}

/* Used by x_update_scrollbar_instance_status. */
static void update_one_widget_scrollbar_pointer(struct window *w, Widget wid)
{
	if (POINTER_IMAGE_INSTANCEP(w->scrollbar_pointer)) {
		XDefineCursor(XtDisplay(wid), XtWindow(wid),
			      XIMAGE_INSTANCE_X_CURSOR(w->scrollbar_pointer));
		XSync(XtDisplay(wid), False);
	}
}

/* A device method. */
static void
x_update_scrollbar_instance_status(struct window *w, int active, int size,
				   struct scrollbar_instance *instance)
{
	struct frame *f = XFRAME(w->frame);
	Boolean managed = XtIsManaged(SCROLLBAR_X_WIDGET(instance));

	if (active && size) {
		widget_value *wv = scrollbar_instance_to_widget_value(instance);

		if (instance->scrollbar_instance_changed) {
			lw_modify_all_widgets(SCROLLBAR_X_ID(instance), wv, 0);
			instance->scrollbar_instance_changed = 0;
		}

		if (!managed) {
			XtManageChild(SCROLLBAR_X_WIDGET(instance));
			if (XtWindow(SCROLLBAR_X_WIDGET(instance))) {
				/* Raise this window so that it's visible on top of the
				   text window below it. */
				XRaiseWindow(XtDisplay
					     (SCROLLBAR_X_WIDGET(instance)),
					     XtWindow(SCROLLBAR_X_WIDGET
						      (instance)));
				update_one_widget_scrollbar_pointer(w,
								    SCROLLBAR_X_WIDGET
								    (instance));
				if (!SCROLLBAR_X_BACKING_STORE_INITIALIZED
				    (instance)) {
					update_one_scrollbar_bs(f,
								SCROLLBAR_X_WIDGET
								(instance));
					SCROLLBAR_X_BACKING_STORE_INITIALIZED
					    (instance) = 1;
				}
			}
		}

		if (!wv->scrollbar_data)
			abort();
		free_widget_value_tree(wv);
	} else if (managed) {
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
		/* This isn't needed with Athena Scrollbars.  It might not be needed */
		/* with Motif scrollbars (it is apparently needed with Lesstif). */
		XtUngrabKeyboard(SCROLLBAR_X_WIDGET(instance), CurrentTime);
#endif
		XtUnmanageChild(SCROLLBAR_X_WIDGET(instance));
	}
}

enum x_scrollbar_loop {
	X_FIND_SCROLLBAR_WINDOW_MIRROR,
	X_SET_SCROLLBAR_POINTER,
	X_WINDOW_IS_SCROLLBAR,
	X_UPDATE_FRAME_SCROLLBARS
};

static struct window_mirror *x_scrollbar_loop(enum x_scrollbar_loop type,
					      Lisp_Object window,
					      struct window_mirror *mir,
					      LWLIB_ID id, Window x_win)
{
	struct window_mirror *retval = NULL;

	while (mir) {
		struct scrollbar_instance *vinstance =
		    mir->scrollbar_vertical_instance;
		struct scrollbar_instance *hinstance =
		    mir->scrollbar_horizontal_instance;
		struct window *w = XWINDOW(window);

		if (mir->vchild)
			retval =
			    x_scrollbar_loop(type, w->vchild, mir->vchild, id,
					     x_win);
		else if (mir->hchild)
			retval =
			    x_scrollbar_loop(type, w->hchild, mir->hchild, id,
					     x_win);
		if (retval)
			return retval;

		if (hinstance || vinstance) {
			switch (type) {
			case X_FIND_SCROLLBAR_WINDOW_MIRROR:
				if ((vinstance
				     && SCROLLBAR_X_ID(vinstance) == id)
				    || (hinstance
					&& SCROLLBAR_X_ID(hinstance) == id))
					return mir;
				break;
			case X_UPDATE_FRAME_SCROLLBARS:
				if (!mir->vchild && !mir->hchild)
					update_window_scrollbars(w, mir, 1, 0);
				break;
			case X_SET_SCROLLBAR_POINTER:
				if (!mir->vchild && !mir->hchild) {
					Widget widget;

					widget = SCROLLBAR_X_WIDGET(hinstance);
					if (widget && XtIsManaged(widget))
						update_one_widget_scrollbar_pointer
						    (w, widget);

					widget = SCROLLBAR_X_WIDGET(vinstance);
					if (widget && XtIsManaged(widget))
						update_one_widget_scrollbar_pointer
						    (w, widget);
				}
				break;
			case X_WINDOW_IS_SCROLLBAR:
				if (!mir->vchild && !mir->hchild) {
					Widget widget;

					widget = SCROLLBAR_X_WIDGET(hinstance);
					if (widget && XtIsManaged(widget) &&
					    XtWindow(widget) == x_win)
						return (struct window_mirror *)
						    1;

					widget = SCROLLBAR_X_WIDGET(vinstance);
					if (widget && XtIsManaged(widget) &&
					    XtWindow(widget) == x_win)
						return (struct window_mirror *)
						    1;
				}
				break;
			default:
				abort();
			}
		}

		mir = mir->next;
		window = w->next;
	}

	return NULL;
}

/* Used by callbacks. */
static struct window_mirror *find_scrollbar_window_mirror(struct frame *f,
							  LWLIB_ID id)
{
	if (f->mirror_dirty)
		update_frame_window_mirror(f);
	return x_scrollbar_loop(X_FIND_SCROLLBAR_WINDOW_MIRROR, f->root_window,
				f->root_mirror, id, (Window) NULL);
}

/*
 * This is the only callback provided for vertical scrollbars.  It
 * should be able to handle all of the scrollbar events in
 * scroll_action (see lwlib.h).  The client data will be of type
 * scroll_event (see lwlib.h). */
static void
x_update_vertical_scrollbar_callback(Widget widget, LWLIB_ID id,
				     XtPointer client_data)
{
	/* This function can GC */
	scroll_event *data = (scroll_event *) client_data;
	struct device *d = get_device_from_display(XtDisplay(widget));
	struct frame *f = x_any_window_to_frame(d, XtWindow(widget));
	Lisp_Object win, frame;
	struct scrollbar_instance *instance;
	struct window_mirror *mirror;

	if (!f)
		return;

	mirror = find_scrollbar_window_mirror(f, id);
	if (!mirror)
		return;

	win = real_window(mirror, 1);

	if (NILP(win))
		return;
	instance = mirror->scrollbar_vertical_instance;
	frame = WINDOW_FRAME(XWINDOW(win));

	/* It seems that this is necessary whenever signal_special_Xt_user_event()
	   is called.  #### Why??? */
	DEVICE_X_MOUSE_TIMESTAMP(d) = DEVICE_X_GLOBAL_MOUSE_TIMESTAMP(d);

	switch (data->action) {
	case SCROLLBAR_LINE_UP:
		signal_special_Xt_user_event(frame, Qscrollbar_line_up, win);
		break;

	case SCROLLBAR_LINE_DOWN:
		signal_special_Xt_user_event(frame, Qscrollbar_line_down, win);
		break;

		/* The Athena scrollbar paging behavior is that of xterms.
		   Depending on where you click the size of the page varies.
		   Motif always does a standard Emacs page. */
	case SCROLLBAR_PAGE_UP: {
#if !defined (LWLIB_SCROLLBARS_MOTIF) && !defined (LWLIB_SCROLLBARS_LUCID) && \
	!defined (LWLIB_SCROLLBARS_ATHENA3D)
		double tmp = ((double)data->slider_value /
			      (double)SCROLLBAR_X_POS_DATA(instance).
			      scrollbar_height);
		double line =
			tmp * (double)window_displayed_height(XWINDOW(win));

		if (line > -1.0) {
			line = -1.0;
		}
		signal_special_Xt_user_event(frame, Qscrollbar_page_up,
					     Fcons(win, make_int((int)line)));
#else
		signal_special_Xt_user_event(frame, Qscrollbar_page_up,
					     Fcons(win, Qnil));
#endif
	}
		break;

	case SCROLLBAR_PAGE_DOWN: {
#if !defined (LWLIB_SCROLLBARS_MOTIF) && !defined (LWLIB_SCROLLBARS_LUCID) && \
	!defined (LWLIB_SCROLLBARS_ATHENA3D)
		double tmp = ((double)data->slider_value /
			      (double)SCROLLBAR_X_POS_DATA(instance).
			      scrollbar_height);
		double line =
			tmp * (double)window_displayed_height(XWINDOW(win));

		if (SCROLLBAR_X_POS_DATA(instance).maximum >
		    (SCROLLBAR_X_POS_DATA(instance).slider_size +
		     SCROLLBAR_X_POS_DATA(instance).slider_position)) {
			if (line < 1.0) {
				line = 1.0;
			}
			signal_special_Xt_user_event(
				frame,
				Qscrollbar_page_down,
				Fcons(win, make_int((int)line)));
		}
#else
		signal_special_Xt_user_event(frame, Qscrollbar_page_down,
					     Fcons(win, Qnil));
#endif
	}
		break;

	case SCROLLBAR_TOP:
		signal_special_Xt_user_event(frame, Qscrollbar_to_top, win);
		break;

	case SCROLLBAR_BOTTOM:
		signal_special_Xt_user_event(frame, Qscrollbar_to_bottom, win);
		break;

	case SCROLLBAR_CHANGE:
		inhibit_slider_size_change = 0;
#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
		vertical_drag_in_progress = 0;
		SCROLLBAR_X_VDRAG_ORIG_VALUE(instance) = data->slider_value;
		SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(instance) =
		    XINT(Fwindow_start(win));
#else
		stupid_vertical_scrollbar_drag_hack = 0;
#endif
		break;

	case SCROLLBAR_DRAG: {
		int value;

		inhibit_slider_size_change = 1;

#if defined (LWLIB_SCROLLBARS_MOTIF) || defined (LWLIB_SCROLLBARS_LUCID)
		/* Doing drags with Motif-like scrollbars is a mess, since we
		   want to avoid having the window position jump when you
		   first grab the scrollbar, but we also want to ensure that
		   you can scroll all the way to the top or bottom of the
		   buffer.  This can all be replaced with something sane when
		   we get line-based scrolling. */

		vertical_drag_in_progress = 1;

		if (SCROLLBAR_X_VDRAG_ORIG_VALUE(instance) < 0) {
			SCROLLBAR_X_VDRAG_ORIG_VALUE(instance) =
				data->slider_value;
			SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(instance) =
				XINT(Fwindow_start(win));
		}

		/* Could replace this piecewise linear scrolling with a
		   quadratic through the three points, but I'm not sure that
		   would feel any nicer in practice. */
		if (data->slider_value <
		    SCROLLBAR_X_VDRAG_ORIG_VALUE(instance)) {
			/* We've dragged up; slide linearly from original position to
			   window-start=data.minimum, slider-value=data.minimum. */

			if (SCROLLBAR_X_VDRAG_ORIG_VALUE(instance)
			    <= SCROLLBAR_X_POS_DATA(instance).minimum) {
				/* shouldn't get here, but just in case */
				value =
					SCROLLBAR_X_POS_DATA(instance).
					minimum;
			} else {
				value = (int)
					(SCROLLBAR_X_POS_DATA(instance).
					 minimum + (((double)
						     (SCROLLBAR_X_VDRAG_ORIG_WINDOW_START(instance)
						      -
						      SCROLLBAR_X_POS_DATA
						      (instance).minimum)
						     * (data->slider_value -
							SCROLLBAR_X_POS_DATA
							(instance).minimum))
						    /
						    (SCROLLBAR_X_VDRAG_ORIG_VALUE
						     (instance)
						     -
						     SCROLLBAR_X_POS_DATA
						     (instance).minimum)));
			}
		} else {
			/* We've dragged down; slide linearly from original position to
			   window-start=data.maximum, slider-value=data.maximum. */

			if (SCROLLBAR_X_VDRAG_ORIG_VALUE(instance)
			    >= (SCROLLBAR_X_POS_DATA(instance).maximum -
				SCROLLBAR_X_POS_DATA(instance).
				slider_size)) {
				/* avoid divide by zero */
				value =
					SCROLLBAR_X_VDRAG_ORIG_WINDOW_START
					(instance);
			} else {
				value = (int)
					(SCROLLBAR_X_VDRAG_ORIG_WINDOW_START
					 (instance)
					 + (((double)
					     (SCROLLBAR_X_POS_DATA
					      (instance).maximum -
					      SCROLLBAR_X_VDRAG_ORIG_WINDOW_START
					      (instance))
					     * (data->slider_value -
						SCROLLBAR_X_VDRAG_ORIG_VALUE
						(instance)))
					    /
					    (SCROLLBAR_X_POS_DATA(instance).
					     maximum -
					     SCROLLBAR_X_POS_DATA(instance).
					     slider_size -
					     SCROLLBAR_X_VDRAG_ORIG_VALUE
					     (instance))));
			}
		}
#else
		stupid_vertical_scrollbar_drag_hack = 0;
		value = data->slider_value;
#endif

		if (value >= SCROLLBAR_X_POS_DATA(instance).maximum)
			value =
				SCROLLBAR_X_POS_DATA(instance).maximum - 1;
		if (value < SCROLLBAR_X_POS_DATA(instance).minimum)
			value = SCROLLBAR_X_POS_DATA(instance).minimum;

		signal_special_Xt_user_event(frame,
					     Qscrollbar_vertical_drag,
					     Fcons(win,
						   make_int(value)));
	}
		break;
	default:
		/* maybe punish the user here? */
		/* abort(); */
		break;
	}
}

/*
 * This is the only callback provided for horizontal scrollbars.  It
 * should be able to handle all of the scrollbar events in
 * scroll_action (see lwlib.h).  The client data will be of type
 * scroll_event (see lwlib.h). */
static void
x_update_horizontal_scrollbar_callback(Widget widget, LWLIB_ID id,
				       XtPointer client_data)
{
	scroll_event *data = (scroll_event *) client_data;
	struct device *d = get_device_from_display(XtDisplay(widget));
	struct frame *f = x_any_window_to_frame(d, XtWindow(widget));
	Lisp_Object win, frame;
	struct window_mirror *mirror;

	if (!f)
		return;

	mirror = find_scrollbar_window_mirror(f, id);
	if (!mirror)
		return;

	win = real_window(mirror, 1);

	if (NILP(win))
		return;
	frame = WINDOW_FRAME(XWINDOW(win));

	/* It seems that this is necessary whenever signal_special_Xt_user_event()
	   is called.  #### Why??? */
	DEVICE_X_MOUSE_TIMESTAMP(d) = DEVICE_X_GLOBAL_MOUSE_TIMESTAMP(d);

	switch (data->action) {
	case SCROLLBAR_LINE_UP:
		signal_special_Xt_user_event(frame, Qscrollbar_char_left, win);
		break;
	case SCROLLBAR_LINE_DOWN:
		signal_special_Xt_user_event(frame, Qscrollbar_char_right, win);
		break;
	case SCROLLBAR_PAGE_UP:
		signal_special_Xt_user_event(frame, Qscrollbar_page_left, win);
		break;
	case SCROLLBAR_PAGE_DOWN:
		signal_special_Xt_user_event(frame, Qscrollbar_page_right, win);
		break;
	case SCROLLBAR_TOP:
		signal_special_Xt_user_event(frame, Qscrollbar_to_left, win);
		break;
	case SCROLLBAR_BOTTOM:
		signal_special_Xt_user_event(frame, Qscrollbar_to_right, win);
		break;
	case SCROLLBAR_CHANGE:
		inhibit_slider_size_change = 0;
		break;
	case SCROLLBAR_DRAG:
		inhibit_slider_size_change = 1;
		/* #### Fix the damn toolkit code so they all work the same way.
		   Lucid is the one mostly wrong. */
#if defined (LWLIB_SCROLLBARS_LUCID) || defined (LWLIB_SCROLLBARS_ATHENA3D)
		signal_special_Xt_user_event(frame, Qscrollbar_horizontal_drag,
					     (Fcons
					      (win,
					       make_int(data->slider_value))));
#else
		signal_special_Xt_user_event(frame, Qscrollbar_horizontal_drag,
					     (Fcons
					      (win,
					       make_int(data->slider_value -
							1))));
#endif
		break;
	default:
		break;
	}
}

static void x_scrollbar_pointer_changed_in_window(struct window *w)
{
	Lisp_Object window;

	XSETWINDOW(window, w);
	x_scrollbar_loop(X_SET_SCROLLBAR_POINTER, window, find_window_mirror(w),
			 0, (Window) NULL);
}

/* Make sure that all scrollbars on frame are up-to-date.  Called
   directly from x_set_frame_properties in frame-x.c*/
void x_update_frame_scrollbars(struct frame *f)
{
	/* Consider this code to be "in_display" so that we abort() if Fsignal()
	   gets called. */
	in_display++;
	x_scrollbar_loop(X_UPDATE_FRAME_SCROLLBARS, f->root_window,
			 f->root_mirror, 0, (Window) NULL);
	in_display--;
	if (in_display < 0)
		abort();
}

#if defined MEMORY_USAGE_STATS && !(defined HAVE_BDWGC && defined EF_USE_BDWGC)

static int
x_compute_scrollbar_instance_usage(struct device *d,
				   struct scrollbar_instance *inst,
				   struct overhead_stats *ovstats)
{
	int total = 0;

	while (inst) {
		struct x_scrollbar_data *data =
		    (struct x_scrollbar_data *)inst->scrollbar_data;

		total += malloced_storage_size(data, sizeof(*data), ovstats);
		total +=
		    malloced_storage_size(data->name, 1 + strlen(data->name),
					  ovstats);
		inst = inst->next;
	}

	return total;
}

#endif				/* MEMORY_USAGE_STATS */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_scrollbar_x(void)
{
	CONSOLE_HAS_METHOD(x, inhibit_scrollbar_slider_size_change);
	CONSOLE_HAS_METHOD(x, free_scrollbar_instance);
	CONSOLE_HAS_METHOD(x, release_scrollbar_instance);
	CONSOLE_HAS_METHOD(x, create_scrollbar_instance);
	CONSOLE_HAS_METHOD(x, update_scrollbar_instance_values);
	CONSOLE_HAS_METHOD(x, update_scrollbar_instance_status);
	CONSOLE_HAS_METHOD(x, scrollbar_pointer_changed_in_window);
#if defined MEMORY_USAGE_STATS && !(defined HAVE_BDWGC && defined EF_USE_BDWGC)
	CONSOLE_HAS_METHOD(x, compute_scrollbar_instance_usage);
#endif				/* MEMORY_USAGE_STATS */
}

void reinit_vars_of_scrollbar_x(void)
{
	stupid_vertical_scrollbar_drag_hack = 1;
}

void vars_of_scrollbar_x(void)
{
	reinit_vars_of_scrollbar_x();

#if defined (LWLIB_SCROLLBARS_LUCID)
	Fprovide(intern("lucid-scrollbars"));
#elif defined (LWLIB_SCROLLBARS_MOTIF)
	Fprovide(intern("motif-scrollbars"));
#elif defined (LWLIB_SCROLLBARS_ATHENA)
	Fprovide(intern("athena-scrollbars"));
#endif
}
