/* scrollbar implementation -- GTK interface.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Amdhal Corporation.
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
/* Gtk version by William M. Perry */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "glyphs-gtk.h"
#include "gui-gtk.h"
#include "scrollbar-gtk.h"

#include "frame.h"
#include "window.h"

static gboolean scrollbar_cb(GtkAdjustment * adj, gpointer user_data);

/* Used to prevent changing the size of the slider while drag
   scrolling, under Motif.  This is necessary because the Motif
   scrollbar is incredibly stupid about updating the slider and causes
   lots of flicker if it is done too often.  */
static int inhibit_slider_size_change;

static int vertical_drag_in_progress;

/* A device method. */
static int gtk_inhibit_scrollbar_slider_size_change(void)
{
	return inhibit_slider_size_change;
}

/* A device method. */
static void gtk_free_scrollbar_instance(struct scrollbar_instance *instance)
{
	if (SCROLLBAR_GTK_WIDGET(instance)) {
		gtk_widget_hide_all(SCROLLBAR_GTK_WIDGET(instance));
		gtk_widget_destroy(SCROLLBAR_GTK_WIDGET(instance));
	}

	if (instance->scrollbar_data)
		xfree(instance->scrollbar_data);
}

/* A device method. */
static void gtk_release_scrollbar_instance(struct scrollbar_instance *instance)
{
	/* It won't hurt to hide it all the time, will it? */
	gtk_widget_hide_all(SCROLLBAR_GTK_WIDGET(instance));
}

static gboolean
scrollbar_drag_hack_cb(GtkWidget * w, GdkEventButton * ev, gpointer v)
{
	vertical_drag_in_progress = (int)v;
	inhibit_slider_size_change = (int)v;
	return (FALSE);
}

/* A device method. */
static void
gtk_create_scrollbar_instance(struct frame *f, int vertical,
			      struct scrollbar_instance *instance)
{
	GtkAdjustment *adj =
	    GTK_ADJUSTMENT(gtk_adjustment_new(0, 0, 0, 0, 0, 0));
	GtkScrollbar *sb = NULL;

	/* initialize the X specific data section. */
	instance->scrollbar_data = xnew_and_zero(struct gtk_scrollbar_data);

	SCROLLBAR_GTK_ID(instance) = new_gui_id();
	SCROLLBAR_GTK_VDRAG_ORIG_VALUE(instance) = -1;
	SCROLLBAR_GTK_LAST_VALUE(instance) = adj->value;

	gtk_object_set_data(GTK_OBJECT(adj), GTK_DATA_GUI_IDENTIFIER,
			    (void *)SCROLLBAR_GTK_ID(instance));
	gtk_object_set_data(GTK_OBJECT(adj), GTK_DATA_FRAME_IDENTIFIER, f);
	gtk_object_set_data(GTK_OBJECT(adj), "xemacs::sb_instance", instance);

	sb = GTK_SCROLLBAR(vertical ? gtk_vscrollbar_new(adj) :
			   gtk_hscrollbar_new(adj));
	/* With gtk version > 1.2.8 the gtk code does not call
	   gtk_widget_request_size() on the newly created scrollbars
	   anymore (catering to theme engines).
	   #### Maybe it is better to postpone this call to just before
	   gtk_widget_show() is called on the scrollbar? */
#if GTK_MAJOR_VERSION == 1 && GTK_MINOR_VERSION == 2 && GTK_BINARY_AGE > 8
	gtk_widget_size_request(GTK_WIDGET(sb), &(GTK_WIDGET(sb)->requisition));
#endif
	SCROLLBAR_GTK_WIDGET(instance) = GTK_WIDGET(sb);

	gtk_signal_connect(GTK_OBJECT(adj), "value-changed",
			   GTK_SIGNAL_FUNC(scrollbar_cb), (gpointer) vertical);

	gtk_signal_connect(GTK_OBJECT(sb), "button-press-event",
			   GTK_SIGNAL_FUNC(scrollbar_drag_hack_cb),
			   (gpointer) 1);
	gtk_signal_connect(GTK_OBJECT(sb), "button-release-event",
			   GTK_SIGNAL_FUNC(scrollbar_drag_hack_cb),
			   (gpointer) 0);

	gtk_fixed_put(GTK_FIXED(FRAME_GTK_TEXT_WIDGET(f)),
		      SCROLLBAR_GTK_WIDGET(instance), 0, 0);
	gtk_widget_hide(SCROLLBAR_GTK_WIDGET(instance));
}

#define UPDATE_DATA_FIELD(field)				\
  if (new_##field >= 0 &&					\
      SCROLLBAR_GTK_POS_DATA (inst).field != new_##field) {	\
    SCROLLBAR_GTK_POS_DATA (inst).field = new_##field;		\
    inst->scrollbar_instance_changed = 1;			\
  }

/* A device method. */
/* #### The -1 check is such a hack. */
static void
gtk_update_scrollbar_instance_values(struct window *w,
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

	if (w && !vertical_drag_in_progress) {
		int new_vov = SCROLLBAR_GTK_POS_DATA(inst).slider_position;
		int new_vows = marker_position(w->start[CURRENT_DISP]);

		if (SCROLLBAR_GTK_VDRAG_ORIG_VALUE(inst) != new_vov) {
			SCROLLBAR_GTK_VDRAG_ORIG_VALUE(inst) = new_vov;
			inst->scrollbar_instance_changed = 1;
		}
		if (SCROLLBAR_GTK_VDRAG_ORIG_WINDOW_START(inst) != new_vows) {
			SCROLLBAR_GTK_VDRAG_ORIG_WINDOW_START(inst) = new_vows;
			inst->scrollbar_instance_changed = 1;
		}
	}
}

/* Used by gtk_update_scrollbar_instance_status. */
static void
update_one_widget_scrollbar_pointer(struct window *w, GtkWidget * wid)
{
	if (!wid->window)
		gtk_widget_realize(wid);

	if (POINTER_IMAGE_INSTANCEP(w->scrollbar_pointer)) {
		gdk_window_set_cursor(GET_GTK_WIDGET_WINDOW(wid),
				      XIMAGE_INSTANCE_GTK_CURSOR(w->
								 scrollbar_pointer));
		gdk_flush();
	}
}

/* A device method. */
static void
gtk_update_scrollbar_instance_status(struct window *w, int active, int size,
				     struct scrollbar_instance *instance)
{
	struct frame *f = XFRAME(w->frame);
	GtkWidget *wid = SCROLLBAR_GTK_WIDGET(instance);
	gboolean managed = GTK_WIDGET_MAPPED(wid);

	if (active && size) {
		if (instance->scrollbar_instance_changed) {
			/* Need to set the height, width, and position of the widget */
			GtkAdjustment *adj =
			    gtk_range_get_adjustment(GTK_RANGE(wid));
			scrollbar_values *pos_data =
			    &SCROLLBAR_GTK_POS_DATA(instance);
			int modified_p = 0;

			/* We do not want to update the size all the time if we can
			   help it.  This cuts down on annoying flicker.
			 */
			if ((wid->allocation.width != pos_data->scrollbar_width)
			    || (wid->allocation.height !=
				pos_data->scrollbar_height)) {
				gtk_widget_set_usize(wid,
						     pos_data->scrollbar_width,
						     pos_data->
						     scrollbar_height);

				/*
				   UGLY! UGLY! UGLY!  Changes to wid->allocation are queued and
				   not performed until the GTK event loop.  However, when the
				   fontlock progress bar is run, the vertical scrollbar's height
				   is change and then changed back before events are again
				   processed.  This means that the change back is not seen and
				   the scrollbar is left too short.  Fix this by making the
				   change manually so the test above sees the change.  This does
				   not seem to cause problems in other cases.
				 */

				wid->allocation.width =
				    pos_data->scrollbar_width;
				wid->allocation.height =
				    pos_data->scrollbar_height;

				modified_p = 1;
			}

			/* Ditto for the x/y position. */
			if ((wid->allocation.x != pos_data->scrollbar_x) ||
			    (wid->allocation.y != pos_data->scrollbar_y)) {
				gtk_fixed_move(GTK_FIXED
					       (FRAME_GTK_TEXT_WIDGET(f)), wid,
					       pos_data->scrollbar_x,
					       pos_data->scrollbar_y);

				/*
				   UGLY! UGLY! UGLY!  Changes to wid->allocation are queued and
				   not performed until the GTK event loop.  However, when the
				   fontlock progress bar is run, the horizontal scrollbar's
				   position is change and then changed back before events are
				   again processed.  This means that the change back is not seen
				   and the scrollbar is left in the wrong position.  Fix this by
				   making the change manually so the test above sees the change.
				   This does not seem to cause problems in other cases.
				 */

				wid->allocation.x = pos_data->scrollbar_x;
				wid->allocation.y = pos_data->scrollbar_y;

				modified_p = 1;
			}

			adj->lower = pos_data->minimum;
			adj->upper = pos_data->maximum;
			adj->page_increment = pos_data->slider_size + 1;
			adj->step_increment = w->max_line_len - 1;
			adj->page_size = pos_data->slider_size + 1;
			adj->value = pos_data->slider_position;

			/* But, if we didn't resize or move the scrollbar, the
			   widget will not get redrawn correctly when the user
			   scrolls around in the XEmacs frame manually.  So we
			   update the slider manually here.
			 */
			if (!modified_p)
				gtk_range_slider_update(GTK_RANGE(wid));

			instance->scrollbar_instance_changed = 0;
		}

		if (!managed) {
			gtk_widget_show(wid);
			update_one_widget_scrollbar_pointer(w, wid);
		}
	} else if (managed) {
		gtk_widget_hide(wid);
	}
}

enum gtk_scrollbar_loop {
	GTK_FIND_SCROLLBAR_WINDOW_MIRROR,
	GTK_SET_SCROLLBAR_POINTER,
	GTK_WINDOW_IS_SCROLLBAR,
	GTK_UPDATE_FRAME_SCROLLBARS
};

static struct window_mirror *gtk_scrollbar_loop(enum gtk_scrollbar_loop type,
						Lisp_Object window,
						struct window_mirror *mir,
						GUI_ID id, GdkWindow * x_win)
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
			    gtk_scrollbar_loop(type, w->vchild, mir->vchild, id,
					       x_win);
		else if (mir->hchild)
			retval =
			    gtk_scrollbar_loop(type, w->hchild, mir->hchild, id,
					       x_win);
		if (retval)
			return retval;

		if (hinstance || vinstance) {
			switch (type) {
			case GTK_FIND_SCROLLBAR_WINDOW_MIRROR:
				if ((vinstance
				     && SCROLLBAR_GTK_ID(vinstance) == id)
				    || (hinstance
					&& SCROLLBAR_GTK_ID(hinstance) == id))
					return mir;
				break;
			case GTK_UPDATE_FRAME_SCROLLBARS:
				if (!mir->vchild && !mir->hchild)
					update_window_scrollbars(w, mir, 1, 0);
				break;
			case GTK_SET_SCROLLBAR_POINTER:
				if (!mir->vchild && !mir->hchild) {
					GtkWidget *widget;

					widget =
					    SCROLLBAR_GTK_WIDGET(hinstance);
					if (widget && GTK_WIDGET_MAPPED(widget))
						update_one_widget_scrollbar_pointer
						    (w, widget);

					widget =
					    SCROLLBAR_GTK_WIDGET(vinstance);
					if (widget && GTK_WIDGET_MAPPED(widget))
						update_one_widget_scrollbar_pointer
						    (w, widget);
				}
				break;
			case GTK_WINDOW_IS_SCROLLBAR:
				if (!mir->vchild && !mir->hchild) {
					GtkWidget *widget;

					widget =
					    SCROLLBAR_GTK_WIDGET(hinstance);
					if (widget && GTK_WIDGET_MAPPED(widget)
					    && GET_GTK_WIDGET_WINDOW(widget) ==
					    x_win)
						return (struct window_mirror *)
						    1;

					widget =
					    SCROLLBAR_GTK_WIDGET(vinstance);
					if (widget && GTK_WIDGET_MAPPED(widget)
					    && GET_GTK_WIDGET_WINDOW(widget) ==
					    x_win)
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
							  GUI_ID id)
{
	if (f->mirror_dirty)
		update_frame_window_mirror(f);
	return gtk_scrollbar_loop(GTK_FIND_SCROLLBAR_WINDOW_MIRROR,
				  f->root_window, f->root_mirror, id,
				  (GdkWindow *) NULL);
}

static gboolean scrollbar_cb(GtkAdjustment * adj, gpointer user_data)
{
	/* This function can GC */
	int vertical = (int)user_data;
	struct frame *f =
	    gtk_object_get_data(GTK_OBJECT(adj), GTK_DATA_FRAME_IDENTIFIER);
	struct scrollbar_instance *instance =
	    gtk_object_get_data(GTK_OBJECT(adj), "xemacs::sb_instance");
	GUI_ID id =
	    (GUI_ID) gtk_object_get_data(GTK_OBJECT(adj),
					 GTK_DATA_GUI_IDENTIFIER);
	Lisp_Object win, frame;
	struct window_mirror *mirror;
	Lisp_Object event_type = Qnil;
	Lisp_Object event_data = Qnil;

	if (!f)
		return (FALSE);

	mirror = find_scrollbar_window_mirror(f, id);
	if (!mirror)
		return (FALSE);

	win = real_window(mirror, 1);

	if (NILP(win))
		return (FALSE);
	instance =
	    vertical ? mirror->scrollbar_vertical_instance : mirror->
	    scrollbar_horizontal_instance;
	frame = WINDOW_FRAME(XWINDOW(win));

	inhibit_slider_size_change = 0;
	switch (GTK_RANGE(SCROLLBAR_GTK_WIDGET(instance))->scroll_type) {
	case GTK_SCROLL_PAGE_BACKWARD:
		event_type =
		    vertical ? Qscrollbar_page_up : Qscrollbar_page_left;
		event_data = Fcons(win, Qnil);
		break;
	case GTK_SCROLL_PAGE_FORWARD:
		event_type =
		    vertical ? Qscrollbar_page_down : Qscrollbar_page_right;
		event_data = Fcons(win, Qnil);
		break;
	case GTK_SCROLL_STEP_FORWARD:
		event_type =
		    vertical ? Qscrollbar_line_down : Qscrollbar_char_right;
		event_data = win;
		break;
	case GTK_SCROLL_STEP_BACKWARD:
		event_type =
		    vertical ? Qscrollbar_line_up : Qscrollbar_char_left;
		event_data = win;
		break;
	case GTK_SCROLL_NONE:
	case GTK_SCROLL_JUMP:
		inhibit_slider_size_change = 1;
		event_type =
		    vertical ? Qscrollbar_vertical_drag :
		    Qscrollbar_horizontal_drag;
		event_data = Fcons(win, make_int((int)adj->value));
		break;
	default:
		abort();
	}

	signal_special_gtk_user_event(frame, event_type, event_data);

	return (TRUE);
}

static void gtk_scrollbar_pointer_changed_in_window(struct window *w)
{
	Lisp_Object window;

	XSETWINDOW(window, w);
	gtk_scrollbar_loop(GTK_SET_SCROLLBAR_POINTER, window,
			   find_window_mirror(w), 0, (GdkWindow *) NULL);
}

/* #### BILL!!! This comment is not true for Gtk - should it be? */
/* Make sure that all scrollbars on frame are up-to-date.  Called
   directly from gtk_set_frame_properties in frame-gtk.c*/
void gtk_update_frame_scrollbars(struct frame *f)
{
	/* Consider this code to be "in_display" so that we abort() if Fsignal()
	   gets called. */
	in_display++;
	gtk_scrollbar_loop(GTK_UPDATE_FRAME_SCROLLBARS, f->root_window,
			   f->root_mirror, 0, (GdkWindow *) NULL);
	in_display--;
	if (in_display < 0)
		abort();
}

#ifdef MEMORY_USAGE_STATS
static int
gtk_compute_scrollbar_instance_usage(struct device *d,
				     struct scrollbar_instance *inst,
				     struct overhead_stats *ovstats)
{
	int total = 0;

	while (inst) {
		struct gtk_scrollbar_data *data =
		    (struct gtk_scrollbar_data *)inst->scrollbar_data;

		total += malloced_storage_size(data, sizeof(*data), ovstats);
		inst = inst->next;
	}

	return total;
}

#endif				/* MEMORY_USAGE_STATS */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_scrollbar_gtk(void)
{
	CONSOLE_HAS_METHOD(gtk, inhibit_scrollbar_slider_size_change);
	CONSOLE_HAS_METHOD(gtk, free_scrollbar_instance);
	CONSOLE_HAS_METHOD(gtk, release_scrollbar_instance);
	CONSOLE_HAS_METHOD(gtk, create_scrollbar_instance);
	CONSOLE_HAS_METHOD(gtk, update_scrollbar_instance_values);
	CONSOLE_HAS_METHOD(gtk, update_scrollbar_instance_status);
	CONSOLE_HAS_METHOD(gtk, scrollbar_pointer_changed_in_window);
#ifdef MEMORY_USAGE_STATS
	CONSOLE_HAS_METHOD(gtk, compute_scrollbar_instance_usage);
#endif				/* MEMORY_USAGE_STATS */
}

void vars_of_scrollbar_gtk(void)
{
	Fprovide(intern("gtk-scrollbars"));
}
