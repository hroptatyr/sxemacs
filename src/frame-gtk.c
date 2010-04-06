/* Functions for the X window system.
   Copyright (C) 1989, 1992-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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


/* Synched up with: Not synched with FSF. */

/* Substantially rewritten for SXEmacs.  */
/* Revamped to use Gdk/Gtk by William Perry */

#include <config.h>
#include "lisp.h"

#include "elhash.h"
#include "console-gtk.h"
#include "ui-gtk.h"
#include "glyphs-gtk.h"
#include "objects-gtk.h"
#include "scrollbar-gtk.h"

#include "gtk-xemacs.h"

#include "buffer.h"
#include "events.h"
#include "extents.h"
#include "faces.h"
#include "frame.h"
#include "window.h"

#ifdef HAVE_GNOME
#include <libgnomeui/libgnomeui.h>
#endif

#ifdef HAVE_DRAGNDROP
#include "dragdrop.h"
#endif

#define BORDER_WIDTH 0
#define INTERNAL_BORDER_WIDTH 0

#define TRANSIENT_DATA_IDENTIFIER "sxemacs::transient_for"
#define UNMAPPED_DATA_IDENTIFIER "sxemacs::initially_unmapped"

#define STUPID_X_SPECIFIC_GTK_STUFF

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
#include <gdk/gdkx.h>
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_gtk_frame_plist;

Lisp_Object Qwindow_id;
Lisp_Object Qdetachable_menubar;
Lisp_Object Qtext_widget;
Lisp_Object Qcontainer_widget;
Lisp_Object Qshell_widget;

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
EXFUN(Fgtk_window_id, 1);
#endif

#ifdef HAVE_DRAGNDROP
enum {
	TARGET_TYPE_STRING,
	TARGET_TYPE_URI_LIST,
};

static GtkTargetEntry dnd_target_table[] = {
	{"STRING", 0, TARGET_TYPE_STRING},
	{"text/plain", 0, TARGET_TYPE_STRING},
	{"text/uri-list", 0, TARGET_TYPE_URI_LIST},
	{"_NETSCAPE_URL", 0, TARGET_TYPE_STRING}
};

static guint dnd_n_targets =
    sizeof(dnd_target_table) / sizeof(dnd_target_table[0]);

#endif

/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* Return the Emacs frame-object which contains the given widget. */
struct frame *gtk_widget_to_frame(GtkWidget * w)
{
	struct frame *f = NULL;

	for (; w; w = w->parent) {
		if ((f = (struct frame *)gtk_object_get_data(GTK_OBJECT(w),
							     GTK_DATA_FRAME_IDENTIFIER)))
			return (f);
	}

	return (selected_frame());
}

/* Return the Emacs frame-object corresponding to an X window */
struct frame *gtk_window_to_frame(struct device *d, GdkWindow * wdesc)
{
	Lisp_Object tail, frame;
	struct frame *f;

	/* This function was previously written to accept only a window argument
	   (and to loop over all devices looking for a matching window), but
	   that is incorrect because window ID's are not unique across displays. */

	for (tail = DEVICE_FRAME_LIST(d); CONSP(tail); tail = XCDR(tail)) {
		frame = XCAR(tail);
		if (!FRAMEP(frame))
			continue;
		f = XFRAME(frame);
		if (FRAME_GTK_P(f)
		    && GET_GTK_WIDGET_WINDOW(FRAME_GTK_TEXT_WIDGET(f)) == wdesc)
			return f;
	}
	return 0;
}

/* Like gtk_window_to_frame but also compares the window with the widget's
   windows */
struct frame *gtk_any_window_to_frame(struct device *d, GdkWindow * w)
{
	do {
		Lisp_Object frmcons;

		DEVICE_FRAME_LOOP(frmcons, d) {
			struct frame *fr = XFRAME(XCAR(frmcons));
			if ((w ==
			     GET_GTK_WIDGET_WINDOW(FRAME_GTK_SHELL_WIDGET(fr)))
			    || (w ==
				GET_GTK_WIDGET_WINDOW(FRAME_GTK_CONTAINER_WIDGET
						      (fr))) ||
#ifdef HAVE_MENUBARS
			    (w ==
			     GET_GTK_WIDGET_WINDOW(FRAME_GTK_MENUBAR_WIDGET
						   (fr))) ||
#endif
			    (w ==
			     GET_GTK_WIDGET_WINDOW(FRAME_GTK_TEXT_WIDGET(fr))))
			{
				return (fr);
			}
		}
		w = gdk_window_get_parent(w);
	} while (w);

	return (0);
}

struct frame *gtk_any_widget_or_parent_to_frame(struct device *d,
						GtkWidget * widget)
{
	return (gtk_any_window_to_frame(d, GET_GTK_WIDGET_WINDOW(widget)));
}

struct device *gtk_any_window_to_device(GdkWindow * w)
{
	struct device *d = NULL;
	Lisp_Object devcons, concons;

	DEVICE_LOOP_NO_BREAK(devcons, concons) {
		d = XDEVICE(XCAR(devcons));
		if (!DEVICE_GTK_P(d))
			continue;
		if (gtk_any_window_to_frame(d, w))
			return (d);
	}
	return (NULL);
}

struct frame *decode_gtk_frame(Lisp_Object frame)
{
	if (NILP(frame))
		XSETFRAME(frame, selected_frame());
	CHECK_LIVE_FRAME(frame);
	/* this will also catch dead frames, but putting in the above check
	   results in a more useful error */
	CHECK_GTK_FRAME(frame);
	return XFRAME(frame);
}

/************************************************************************/
/*			window-manager interactions			*/
/************************************************************************/
static int gtk_frame_iconified_p(struct frame *f)
{
	return (f->iconified);
}

/************************************************************************/
/*                          frame properties                            */
/************************************************************************/

static Lisp_Object gtk_frame_property(struct frame *f, Lisp_Object property)
{
	GtkWidget *shell = FRAME_GTK_SHELL_WIDGET(f);

	if (EQ(Qleft, property) || EQ(Qtop, property)) {
		gint x, y;
		if (!GET_GTK_WIDGET_WINDOW(shell))
			return Qzero;
		gdk_window_get_deskrelative_origin(GET_GTK_WIDGET_WINDOW(shell),
						   &x, &y);
		if (EQ(Qleft, property))
			return make_int(x);
		if (EQ(Qtop, property))
			return make_int(y);
	}
	if (EQ(Qshell_widget, property)) {
		return (FRAME_GTK_LISP_WIDGETS(f)[0]);
	}
	if (EQ(Qcontainer_widget, property)) {
		return (FRAME_GTK_LISP_WIDGETS(f)[1]);
	}
	if (EQ(Qtext_widget, property)) {
		return (FRAME_GTK_LISP_WIDGETS(f)[2]);
	}
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
	if (EQ(Qwindow_id, property))
		return Fgtk_window_id(make_frame(f));
#endif

	return Qunbound;
}

static int gtk_internal_frame_property_p(struct frame *f, Lisp_Object property)
{
	return EQ(property, Qleft)
	    || EQ(property, Qtop)
	    || EQ(Qshell_widget, property)
	    || EQ(Qcontainer_widget, property)
	    || EQ(Qtext_widget, property)
	    || EQ(property, Qwindow_id)
	    || STRINGP(property);
}

static Lisp_Object gtk_frame_properties(struct frame *f)
{
	Lisp_Object props = Qnil;
	GtkWidget *shell = FRAME_GTK_SHELL_WIDGET(f);
	gint x, y;

	props = cons3(Qshell_widget, FRAME_GTK_LISP_WIDGETS(f)[0], props);
	props = cons3(Qcontainer_widget, FRAME_GTK_LISP_WIDGETS(f)[1], props);
	props = cons3(Qtext_widget, FRAME_GTK_LISP_WIDGETS(f)[2], props);

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
	props = cons3(Qwindow_id, Fgtk_window_id(make_frame(f)), props);
#endif

	if (!GET_GTK_WIDGET_WINDOW(shell))
		x = y = 0;
	else
		gdk_window_get_deskrelative_origin(GET_GTK_WIDGET_WINDOW(shell),
						   &x, &y);

	props = cons3(Qtop, make_int(y), props);
	props = cons3(Qleft, make_int(x), props);

	return props;
}

/* Functions called only from `gtk_set_frame_properties' to set
   individual properties. */

static void
gtk_set_frame_text_value(struct frame *f, Bufbyte * value,
			 void (*func) (gpointer, gchar *), gpointer arg)
{
	gchar *the_text = (gchar *) value;

	/* Programmer fuckup or window is not realized yet. */
	if (!func || !arg)
		return;

#ifdef MULE
	{
		Bufbyte *ptr;

		/* Optimize for common ASCII case */
		for (ptr = value; *ptr; ptr++)
			if (!BYTE_ASCII_P(*ptr)) {
				char *tmp;
				C_STRING_TO_EXTERNAL(value, tmp, Qctext);
				the_text = tmp;
				break;
			}
	}
#endif				/* MULE */

	(*func) (arg, (gchar *) the_text);
}

static void gtk_set_title_from_bufbyte(struct frame *f, Bufbyte * name)
{
	if (GTK_IS_WINDOW(FRAME_GTK_SHELL_WIDGET(f)))
		gtk_set_frame_text_value(f, name, (void (*)(gpointer, gchar *))
					 gtk_window_set_title,
					 FRAME_GTK_SHELL_WIDGET(f));
}

static void gtk_set_icon_name_from_bufbyte(struct frame *f, Bufbyte * name)
{
	gtk_set_frame_text_value(f, name, (void (*)(gpointer, gchar *))
				 gdk_window_set_icon_name,
				 FRAME_GTK_SHELL_WIDGET(f)->window);
}

/* Set the initial frame size as specified.  This function is used
   when the frame's widgets have not yet been realized.
*/
static void
gtk_set_initial_frame_size(struct frame *f, int x, int y,
			   unsigned int w, unsigned int h)
{
	GtkWidget *shell = FRAME_GTK_SHELL_WIDGET(f);
	GdkGeometry geometry;
	GdkWindowHints geometry_mask = 0x00;

	if (GTK_IS_WINDOW(shell)) {
		/* Deal with the cell size */
		default_face_height_and_width(make_frame(f),
					      &geometry.height_inc,
					      &geometry.width_inc);
		geometry_mask |= GDK_HINT_RESIZE_INC;

		gtk_window_set_geometry_hints(GTK_WINDOW(shell),
					      FRAME_GTK_TEXT_WIDGET(f),
					      &geometry, geometry_mask);
		gdk_window_set_hints(GET_GTK_WIDGET_WINDOW(shell), x, y, 0, 0,
				     0, 0, GDK_HINT_POS);
		gtk_window_set_policy(GTK_WINDOW(shell), TRUE, TRUE, FALSE);
	}

	FRAME_HEIGHT(f) = h;
	FRAME_WIDTH(f) = w;

	change_frame_size(f, h, w, 0);
	{
		GtkRequisition req;

		gtk_widget_size_request(FRAME_GTK_SHELL_WIDGET(f), &req);
		gtk_widget_set_usize(FRAME_GTK_SHELL_WIDGET(f), req.width,
				     req.height);
	}
}

/* Report that a frame property of frame S is being set or changed.
   If the property is not specially recognized, do nothing.
 */

static void gtk_set_frame_properties(struct frame *f, Lisp_Object plist)
{
	gint x, y;
	gint width = 0, height = 0;
	gboolean width_specified_p = FALSE;
	gboolean height_specified_p = FALSE;
	gboolean x_position_specified_p = FALSE;
	gboolean y_position_specified_p = FALSE;
	Lisp_Object tail;

	for (tail = plist; !NILP(tail); tail = Fcdr(Fcdr(tail))) {
		Lisp_Object prop = Fcar(tail);
		Lisp_Object val = Fcar(Fcdr(tail));

		if (SYMBOLP(prop)) {
			if (EQ(prop, Qfont)) {
				/* If the value is not a string we silently ignore it. */
				if (STRINGP(val)) {
					Lisp_Object frm, font_spec;

					XSETFRAME(frm, f);
					font_spec =
					    Fget(Fget_face(Qdefault), Qfont,
						 Qnil);

					Fadd_spec_to_specifier(font_spec, val,
							       frm, Qnil, Qnil);
					update_frame_face_values(f);
				}
				continue;
			} else if (EQ(prop, Qwidth)) {
				CHECK_INT(val);
				width = XINT(val);
				width_specified_p = TRUE;
				continue;
			} else if (EQ(prop, Qheight)) {
				CHECK_INT(val);
				height = XINT(val);
				height_specified_p = TRUE;
				continue;
			}
			/* Further kludge the x/y. */
			else if (EQ(prop, Qx)) {
				CHECK_INT(val);
				x = (gint) XINT(val);
				x_position_specified_p = TRUE;
				continue;
			} else if (EQ(prop, Qy)) {
				CHECK_INT(val);
				y = (gint) XINT(val);
				y_position_specified_p = TRUE;
				continue;
			}
		}
	}

	/* Kludge kludge kludge.   We need to deal with the size and position
	   specially. */
	{
		int size_specified_p = width_specified_p || height_specified_p;
		int position_specified_p = x_position_specified_p
		    || y_position_specified_p;

		if (!width_specified_p)
			width = 80;
		if (!height_specified_p)
			height = 30;

		/* Kludge kludge kludge kludge. */
		if (position_specified_p &&
		    (!x_position_specified_p || !y_position_specified_p)) {
			gint dummy;
			GtkWidget *shell = FRAME_GTK_SHELL_WIDGET(f);
			gdk_window_get_deskrelative_origin(GET_GTK_WIDGET_WINDOW
							   (shell),
							   (x_position_specified_p
							    ? &dummy : &x),
							   (y_position_specified_p
							    ? &dummy : &y));
		}

		if (!f->init_finished) {
			if (size_specified_p || position_specified_p)
				gtk_set_initial_frame_size(f, x, y, width,
							   height);
		} else {
			if (size_specified_p) {
				Lisp_Object frame;
				XSETFRAME(frame, f);
				Fset_frame_size(frame, make_int(width),
						make_int(height), Qnil);
			}
			if (position_specified_p) {
				Lisp_Object frame;
				XSETFRAME(frame, f);
				Fset_frame_position(frame, make_int(x),
						    make_int(y));
			}
		}
	}
}

/************************************************************************/
/*				widget creation				*/
/************************************************************************/
/* Figure out what size the shell widget should initially be,
   and set it.  Should be called after the default font has been
   determined but before the widget has been realized. */

extern Lisp_Object Vgtk_initial_geometry;

#ifndef HAVE_GNOME
static int get_number(const char **geometry)
{
	int value = 0;
	int mult = 1;

	if (**geometry == '-') {
		mult = -1;
		(*geometry)++;
	}
	while (**geometry && isdigit(**geometry)) {
		value = value * 10 + (**geometry - '0');
		(*geometry)++;
	}
	return value * mult;
}

/*
 */

/**
 * gnome_parse_geometry
 * @geometry: geometry string to be parsed
 * @xpos: X position geometry component
 * @ypos: Y position geometry component
 * @width: pixel width geometry component
 * @height: pixel height geometry component
 *
 * Description:
 * Parses the geometry string passed in @geometry, and fills
 * @xpos, @ypos, @width, and @height with
 * the corresponding values upon completion of the parse.
 * If the parse fails, it should be assumed that @xpos, @ypos, @width,
 * and @height contain undefined values.
 *
 * Returns:
 * %TRUE if the geometry was successfully parsed, %FALSE otherwise.
 **/

static gboolean
gnome_parse_geometry(const gchar * geometry, gint * xpos,
		     gint * ypos, gint * width, gint * height)
{
	int subtract;

	g_return_val_if_fail(xpos != NULL, FALSE);
	g_return_val_if_fail(ypos != NULL, FALSE);
	g_return_val_if_fail(width != NULL, FALSE);
	g_return_val_if_fail(height != NULL, FALSE);

	*xpos = *ypos = *width = *height = -1;

	if (!geometry)
		return FALSE;

	if (*geometry == '=')
		geometry++;
	if (!*geometry)
		return FALSE;
	if (isdigit(*geometry))
		*width = get_number(&geometry);
	if (!*geometry)
		return TRUE;
	if (*geometry == 'x' || *geometry == 'X') {
		geometry++;
		*height = get_number(&geometry);
	}
	if (!*geometry)
		return 1;
	if (*geometry == '+') {
		subtract = 0;
		geometry++;
	} else if (*geometry == '-') {
		subtract = gdk_screen_width();
		geometry++;
	} else
		return FALSE;
	*xpos = get_number(&geometry);
	if (subtract)
		*xpos = subtract - *xpos;
	if (!*geometry)
		return TRUE;
	if (*geometry == '+') {
		subtract = 0;
		geometry++;
	} else if (*geometry == '-') {
		subtract = gdk_screen_height();
		geometry++;
	} else
		return FALSE;
	*ypos = get_number(&geometry);
	if (subtract)
		*ypos = subtract - *ypos;
	return TRUE;
}
#endif

static void gtk_initialize_frame_size(struct frame *f)
{
	gint x = 10, y = 10, w = 80, h = 30;

	if (STRINGP(Vgtk_initial_geometry)) {
		if (!gnome_parse_geometry
		    (XSTRING_DATA(Vgtk_initial_geometry), &x, &y, &w, &h)) {
			x = y = 10;
			w = 80;
			h = 30;
		}
	}

	/* set the position of the frame's root window now.  When the
	   frame was created, the position was initialized to (0,0). */
	{
		struct window *win = XWINDOW(f->root_window);

		WINDOW_LEFT(win) = FRAME_LEFT_BORDER_END(f);
		WINDOW_TOP(win) = FRAME_TOP_BORDER_END(f);

		if (!NILP(f->minibuffer_window)) {
			win = XWINDOW(f->minibuffer_window);
			WINDOW_LEFT(win) = FRAME_LEFT_BORDER_END(f);
		}
	}

	gtk_set_initial_frame_size(f, x, y, w, h);
}

static gboolean
resize_event_cb(GtkWidget * w, GtkAllocation * allocation, gpointer user_data)
{
	struct frame *f = (struct frame *)user_data;

	f->pixwidth = allocation->width;
	f->pixheight = allocation->height;

	if (FRAME_GTK_TEXT_WIDGET(f)->window) {
		Lisp_Object frame;
		XSETFRAME(frame, f);
		Fredraw_frame(frame, Qt);
	}

	return (FALSE);
}

static gboolean
delete_event_cb(GtkWidget * w, GdkEvent * ev, gpointer user_data)
{
	struct frame *f = (struct frame *)user_data;
	Lisp_Object frame;

	XSETFRAME(frame, f);
	enqueue_misc_user_event(frame, Qeval, list3(Qdelete_frame, frame, Qt));

	/* See if tickling the event queue helps us with our delays when
	   clicking 'close' */
	signal_fake_event();

	return (TRUE);
}

extern gboolean emacs_shell_event_handler(GtkWidget * wid, GdkEvent * event,
					  gpointer closure);
extern Lisp_Object build_gtk_object(GtkObject * obj);

#ifndef GNOME_IS_APP
#define GNOME_IS_APP(x) 0
#define gnome_app_set_contents(x,y) 0
#endif

static void cleanup_deleted_frame(gpointer data)
{
	struct frame *f = (struct frame *)data;
	Lisp_Object frame;

	XSETFRAME(frame, f);
	Fdelete_frame(frame, Qt);
}

#ifdef HAVE_DRAGNDROP
extern void
dragndrop_data_received(GtkWidget * widget,
			GdkDragContext * context,
			gint x,
			gint y,
			GtkSelectionData * data, guint info, guint time);

extern gboolean
dragndrop_dropped(GtkWidget * widget,
		  GdkDragContext * drag_context,
		  gint x, gint y, guint time, gpointer user_data);

Lisp_Object Vcurrent_drag_object;

#define DRAG_SELECTION_DATA_ERROR "Error converting drag data to external format"
static void
dragndrop_get_drag(GtkWidget * widget,
		   GdkDragContext * drag_context,
		   GtkSelectionData * data,
		   guint info, guint time, gpointer user_data)
{
	gtk_selection_data_set(data, GDK_SELECTION_TYPE_STRING, 8,
			       DRAG_SELECTION_DATA_ERROR,
			       strlen(DRAG_SELECTION_DATA_ERROR));

	switch (info) {
	case TARGET_TYPE_STRING:
		{
			Lisp_Object string = Vcurrent_drag_object;

			if (!STRINGP(Vcurrent_drag_object)) {
				string = Fprin1_to_string(string, Qnil);
				/* Convert to a string */
			}

			gtk_selection_data_set(data, GDK_SELECTION_TYPE_STRING,
					       8, XSTRING_DATA(string),
					       XSTRING_LENGTH(string));
		}
		break;
	case TARGET_TYPE_URI_LIST:
		break;
	default:
		break;
	}
	Vcurrent_drag_object = Qnil;
}

DEFUN("gtk-start-drag-internal", Fgtk_start_drag_internal, 2, 3, 0,	/*
Start a GTK drag from a buffer.
First arg is the event that started the drag,
second arg should be some string, and the third
is the type of the data (this should be a MIME type as a string (ie: text/plain)).
The type defaults to text/plain.
*/
      (event, data, dtyp))
{
	if (EVENTP(event)) {
		struct frame *f = decode_gtk_frame(Fselected_frame(Qnil));
		GtkWidget *wid = FRAME_GTK_TEXT_WIDGET(f);
		struct Lisp_Event *lisp_event = XEVENT(event);
		GdkAtom dnd_typ;
		GtkTargetList *tl =
		    gtk_target_list_new(dnd_target_table, dnd_n_targets);

		/* only drag if this is really a press */
		if (EVENT_TYPE(lisp_event) != button_press_event)
			return Qnil;

		/* get the desired type */
		if (!NILP(dtyp) && STRINGP(dtyp))
			dnd_typ = gdk_atom_intern(XSTRING_DATA(dtyp), FALSE);

		gtk_drag_begin(wid, tl, GDK_ACTION_COPY,
			       lisp_event->event.button.button, NULL);

		Vcurrent_drag_object = data;

		gtk_target_list_unref(tl);
	}
	return Qnil;
}
#endif

/* Creates the widgets for a frame.
   lisp_window_id is a Lisp description of an X window or Xt
   widget to parse.

   This function does not map the windows.  (That is
   done by gtk_popup_frame().)
*/
static void
gtk_create_widgets(struct frame *f, Lisp_Object lisp_window_id,
		   Lisp_Object parent)
{
	const char *name;
	GtkWidget *text, *container, *shell;
	gboolean embedded_p = !NILP(lisp_window_id);
#ifdef HAVE_MENUBARS
	int menubar_visible;
#endif

	if (STRINGP(f->name))
		TO_EXTERNAL_FORMAT(LISP_STRING, f->name, C_STRING_ALLOCA, name,
				   Qctext);
	else
		name = "emacs";

	FRAME_GTK_TOP_LEVEL_FRAME_P(f) = 1;

	if (embedded_p) {
		CHECK_GTK_OBJECT(lisp_window_id);

		if (!GTK_IS_CONTAINER(XGTK_OBJECT(lisp_window_id)->object)) {
			signal_simple_error
			    ("Window ID must be a GtkContainer subclass",
			     lisp_window_id);
		}

		shell = gtk_vbox_new(FALSE, 0);

		gtk_object_weakref(GTK_OBJECT(shell), cleanup_deleted_frame, f);
		gtk_container_add(GTK_CONTAINER
				  (XGTK_OBJECT(lisp_window_id)->object), shell);
	} else {
#ifdef HAVE_GNOME
		shell = GTK_WIDGET(gnome_app_new("SXEmacs", "SXEmacs/GNOME"));
#else
		shell = GTK_WIDGET(gtk_window_new(GTK_WINDOW_TOPLEVEL));
#endif
	}

	if (!NILP(parent)) {
		/* If this is a transient window, keep the parent info around */
		GtkWidget *parentwid = FRAME_GTK_SHELL_WIDGET(XFRAME(parent));
		gtk_object_set_data(GTK_OBJECT(shell),
				    TRANSIENT_DATA_IDENTIFIER, parentwid);
		gtk_window_set_transient_for(GTK_WINDOW(shell),
					     GTK_WINDOW(parentwid));
	}

	gtk_container_set_border_width(GTK_CONTAINER(shell), 0);

	gtk_object_set_data(GTK_OBJECT(shell), GTK_DATA_FRAME_IDENTIFIER, f);

	FRAME_GTK_SHELL_WIDGET(f) = shell;

	text = GTK_WIDGET(gtk_xemacs_new(f));

	if (!GNOME_IS_APP(shell))
		container =
		    GTK_WIDGET(gtk_vbox_new(FALSE, INTERNAL_BORDER_WIDTH));
	else
		container = shell;

	FRAME_GTK_CONTAINER_WIDGET(f) = container;
	FRAME_GTK_TEXT_WIDGET(f) = text;

#ifdef HAVE_DRAGNDROP
	gtk_drag_dest_set(text,
			  GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT,
			  dnd_target_table, dnd_n_targets,
			  GDK_ACTION_COPY | GDK_ACTION_LINK | GDK_ACTION_ASK);
	gtk_signal_connect(GTK_OBJECT(text), "drag_drop",
			   GTK_SIGNAL_FUNC(dragndrop_dropped), text);
	gtk_signal_connect(GTK_OBJECT(text), "drag_data_received",
			   GTK_SIGNAL_FUNC(dragndrop_data_received), text);
	gtk_signal_connect(GTK_OBJECT(text), "drag_data_get",
			   GTK_SIGNAL_FUNC(dragndrop_get_drag), NULL);
#endif

#ifdef HAVE_MENUBARS
	/* Create the initial menubar widget. */
	menubar_visible = gtk_initialize_frame_menubar(f);

	if (menubar_visible) {
		gtk_widget_show_all(FRAME_GTK_MENUBAR_WIDGET(f));
	}
#endif				/* HAVE_MENUBARS */

#ifdef HAVE_GNOME
	if (GNOME_IS_APP(shell))
		gnome_app_set_contents(GNOME_APP(shell), text);
	else
#endif
		/* Now comes the drawing area, which should fill the rest of the
		 ** frame completely.
		 */
		gtk_box_pack_end(GTK_BOX(container), text, TRUE, TRUE, 0);

	/* Connect main event handler */
	gtk_signal_connect(GTK_OBJECT(shell), "delete-event",
			   GTK_SIGNAL_FUNC(delete_event_cb), f);

	{
		static char *events_to_frob[] = { "focus-in-event",
			"focus-out-event",
			"enter-notify-event",
			"leave-notify-event",
			"map-event",
			"unmap-event",
			"property-notify-event",
			"selection-clear-event",
			"selection-request-event",
			"selection-notify-event",
			"client-event",
			/* "configure-event", */
			"visibility-notify-event",
			NULL
		};
		int i;

		for (i = 0; events_to_frob[i]; i++) {
			gtk_signal_connect(GTK_OBJECT(shell), events_to_frob[i],
					   GTK_SIGNAL_FUNC
					   (emacs_shell_event_handler), f);
		}
	}

	gtk_signal_connect(GTK_OBJECT(shell), "size-allocate",
			   GTK_SIGNAL_FUNC(resize_event_cb), f);

	/* This might be safe to call now... */
	/* gtk_signal_connect (GTK_OBJECT (shell), "event", GTK_SIGNAL_FUNC (emacs_shell_event_handler), f); */

	/* Let's make sure we get all the events we can */
	gtk_widget_set_events(text, GDK_ALL_EVENTS_MASK);

	if (shell != container)
		gtk_container_add(GTK_CONTAINER(shell), container);

	gtk_widget_set_name(shell, "SXEmacs::shell");
	gtk_widget_set_name(container, "SXEmacs::container");
	gtk_widget_set_name(text, "SXEmacs::text");

	FRAME_GTK_LISP_WIDGETS(f)[0] = build_gtk_object(GTK_OBJECT(shell));
	FRAME_GTK_LISP_WIDGETS(f)[1] = build_gtk_object(GTK_OBJECT(container));
	FRAME_GTK_LISP_WIDGETS(f)[2] = build_gtk_object(GTK_OBJECT(text));

	gtk_widget_realize(shell);
}

/* create the windows for the specified frame and display them.
   Note that the widgets have already been created, and any
   necessary geometry calculations have already been done. */
static void gtk_popup_frame(struct frame *f)
{
	/* */

	if (gtk_object_get_data
	    (GTK_OBJECT(FRAME_GTK_SHELL_WIDGET(f)), UNMAPPED_DATA_IDENTIFIER)) {
		FRAME_GTK_TOTALLY_VISIBLE_P(f) = 0;
		f->visible = 0;
		gtk_widget_realize(FRAME_GTK_SHELL_WIDGET(f));
		gtk_widget_realize(FRAME_GTK_TEXT_WIDGET(f));
		gtk_widget_hide_all(FRAME_GTK_SHELL_WIDGET(f));
	} else {
		gtk_widget_show_all(FRAME_GTK_SHELL_WIDGET(f));
	}
}

static void allocate_gtk_frame_struct(struct frame *f)
{
	/* zero out all slots. */
	f->frame_data = xnew_and_zero(struct gtk_frame);

	/* yeah, except the lisp ones */
	FRAME_GTK_ICON_PIXMAP(f) = Qnil;
	FRAME_GTK_ICON_PIXMAP_MASK(f) = Qnil;

	/*
	   Hashtables of callback data for glyphs on the frame.  Make them EQ because
	   we only use ints as keys.  Otherwise we run into stickiness in redisplay
	   because internal_equal() can QUIT.  See enter_redisplay_critical_section().
	 */
	FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE(f) =
	    make_lisp_hash_table(50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQ);
	FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f) =
	    make_lisp_hash_table(50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQ);
	FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE(f) =
	    make_lisp_hash_table(50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQ);
}

/************************************************************************/
/*				Lisp functions				*/
/************************************************************************/

static void gtk_init_frame_1(struct frame *f, Lisp_Object props)
{
	/* This function can GC */
	Lisp_Object initially_unmapped;
	Lisp_Object device = FRAME_DEVICE(f);
	Lisp_Object lisp_window_id = Fplist_get(props, Qwindow_id, Qnil);
	Lisp_Object popup = Fplist_get(props, Qpopup, Qnil);

	if (!NILP(popup)) {
		if (EQ(popup, Qt))
			popup = Fselected_frame(device);
		CHECK_LIVE_FRAME(popup);
		if (!EQ(device, FRAME_DEVICE(XFRAME(popup))))
			signal_simple_error_2
			    ("Parent must be on same device as frame", device,
			     popup);
	}

	initially_unmapped = Fplist_get(props, Qinitially_unmapped, Qnil);

	/*
	 * Previously we set this only if NILP (DEVICE_SELECTED_FRAME (d))
	 * to make sure that messages were displayed as soon as possible
	 * if we're creating the first frame on a device.  But it is
	 * better to just set this all the time, so that when a new frame
	 * is created that covers the selected frame, echo area status
	 * messages can still be seen.  f->visible is reset later if the
	 * initially-unmapped property is found to be non-nil in the
	 * frame properties.
	 */
	f->visible = 1;

	allocate_gtk_frame_struct(f);
	gtk_create_widgets(f, lisp_window_id, popup);

	if (!NILP(initially_unmapped)) {
		gtk_object_set_data(GTK_OBJECT(FRAME_GTK_SHELL_WIDGET(f)),
				    UNMAPPED_DATA_IDENTIFIER, (gpointer) 1);
	}
}

static void gtk_init_frame_2(struct frame *f, Lisp_Object props)
{
	/* Set up the values of the widget/frame.  A case could be made for putting
	   this inside of the widget's initialize method. */

	update_frame_face_values(f);
	gtk_initialize_frame_size(f);
	/* Kyle:
	 *   update_frame_title() can't be done here, because some of the
	 *   modeline specs depend on the frame's device having a selected
	 *   frame, and that may not have been set up yet.  The redisplay
	 *   will update the frame title anyway, so nothing is lost.
	 * JV:
	 *   It turns out it gives problems with FVWMs name based mapping.
	 *   We'll just  need to be carefull in the modeline specs.
	 */
	update_frame_title(f);
}

static void gtk_init_frame_3(struct frame *f)
{
	/* Pop up the frame. */
	gtk_popup_frame(f);
}

static void gtk_mark_frame(struct frame *f)
{
	mark_object(FRAME_GTK_ICON_PIXMAP(f));
	mark_object(FRAME_GTK_ICON_PIXMAP_MASK(f));
	mark_object(FRAME_GTK_LISP_WIDGETS(f)[0]);
	mark_object(FRAME_GTK_LISP_WIDGETS(f)[1]);
	mark_object(FRAME_GTK_LISP_WIDGETS(f)[2]);
	mark_object(FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE(f));
	mark_object(FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f));
	mark_object(FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE(f));
}

static void gtk_set_frame_icon(struct frame *f)
{
	GdkPixmap *gtk_pixmap = NULL, *gtk_mask = NULL;

	if (IMAGE_INSTANCEP(f->icon)
	    && IMAGE_INSTANCE_PIXMAP_TYPE_P(XIMAGE_INSTANCE(f->icon))) {
		gtk_pixmap = XIMAGE_INSTANCE_GTK_PIXMAP(f->icon);
		gtk_mask = XIMAGE_INSTANCE_GTK_MASK(f->icon);
	} else {
		gtk_pixmap = 0;
		gtk_mask = 0;
	}

	gdk_window_set_icon(GET_GTK_WIDGET_WINDOW(FRAME_GTK_SHELL_WIDGET(f)),
			    NULL, gtk_pixmap, gtk_mask);
}

static void gtk_set_frame_pointer(struct frame *f)
{
	GtkWidget *w = FRAME_GTK_TEXT_WIDGET(f);
	GdkCursor *c = XIMAGE_INSTANCE_GTK_CURSOR(f->pointer);

	if (POINTER_IMAGE_INSTANCEP(f->pointer)) {
		gdk_window_set_cursor(GET_GTK_WIDGET_WINDOW(w), c);
		gdk_flush();
	} else {
		/* abort()? */
		stderr_out("POINTER_IMAGE_INSTANCEP (f->pointer) failed!\n");
	}
}

static Lisp_Object gtk_get_frame_parent(struct frame *f)
{
	GtkWidget *parentwid =
	    gtk_object_get_data(GTK_OBJECT(FRAME_GTK_SHELL_WIDGET(f)),
				TRANSIENT_DATA_IDENTIFIER);

	/* find the frame whose wid is parentwid */
	if (parentwid) {
		Lisp_Object frmcons;
		DEVICE_FRAME_LOOP(frmcons, XDEVICE(FRAME_DEVICE(f))) {
			Lisp_Object frame = XCAR(frmcons);
			if (FRAME_GTK_SHELL_WIDGET(XFRAME(frame)) == parentwid)
				return frame;
		}
	}
	return Qnil;
}

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
DEFUN("gtk-window-id", Fgtk_window_id, 0, 1, 0,	/*
Get the ID of the Gtk window.
This gives us a chance to manipulate the Emacs window from within a
different program.  Since the ID is an unsigned long, we return it as
a string.
*/
      (frame))
{
	char str[255];
	struct frame *f = decode_gtk_frame(frame);

	/* Arrrrggghhh... this defeats the whole purpose of using Gdk... do we really need this? */
	sprintf(str, "%lu",
		GDK_WINDOW_XWINDOW(GET_GTK_WIDGET_WINDOW
				   (FRAME_GTK_TEXT_WIDGET(f))));
	return build_string(str);
}
#endif

/************************************************************************/
/*			manipulating the X window			*/
/************************************************************************/

static void gtk_set_frame_position(struct frame *f, int xoff, int yoff)
{
	gtk_widget_set_uposition(FRAME_GTK_SHELL_WIDGET(f), xoff, yoff);
}

/* Call this to change the size of frame S's x-window. */

static void gtk_set_frame_size(struct frame *f, int cols, int rows)
{
	GtkWidget *shell = FRAME_GTK_SHELL_WIDGET(f);
	GdkGeometry geometry;
	GdkWindowHints geometry_mask = 0x00;

	if (GTK_IS_WINDOW(shell)) {
		/* Update the cell size */
		default_face_height_and_width(make_frame(f),
					      &geometry.height_inc,
					      &geometry.width_inc);
		geometry_mask |= GDK_HINT_RESIZE_INC;

		gtk_window_set_geometry_hints(GTK_WINDOW(shell),
					      FRAME_GTK_TEXT_WIDGET(f),
					      &geometry, geometry_mask);
	}

	change_frame_size(f, rows, cols, 0);

	{
		GtkRequisition req;

		gtk_widget_size_request(FRAME_GTK_SHELL_WIDGET(f), &req);
		gtk_widget_set_usize(FRAME_GTK_SHELL_WIDGET(f), req.width,
				     req.height);
	}
}

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
/* There is NO equivalent to XWarpPointer under Gtk */
static void gtk_set_mouse_position(struct window *w, int x, int y)
{
	struct frame *f = XFRAME(w->frame);
	Display *display = GDK_DISPLAY();
	XWarpPointer(display, None,
		     GDK_WINDOW_XWINDOW(GET_GTK_WIDGET_WINDOW
					(FRAME_GTK_TEXT_WIDGET(f))), 0, 0, 0, 0,
		     w->pixel_left + x, w->pixel_top + y);
}
#endif				/* STUPID_X_SPECIFIC_GTK_STUFF */

static int
gtk_get_mouse_position(struct device *d, Lisp_Object * frame, int *x, int *y)
{
	/* Returns the pixel position within the editor text widget */
	gint win_x, win_y;
	GdkWindow *w = gdk_window_at_pointer(&win_x, &win_y);
	struct frame *f = NULL;

	if (!w)
		return (0);

	/* At this point, w is the innermost GdkWindow containing the
	 ** pointer and win_x and win_y are the coordinates of that window.
	 */
	f = gtk_any_window_to_frame(d, w);

	if (!f)
		return (0);

	XSETFRAME(*frame, f);

	gdk_window_get_pointer(GET_GTK_WIDGET_WINDOW(FRAME_GTK_TEXT_WIDGET(f)),
			       &win_x, &win_y, NULL);

	*x = win_x;
	*y = win_y;

	return (1);
}

static void gtk_cant_notify_wm_error(void)
{
	error("Can't notify window manager of iconification.");
}

/* Raise frame F.  */
static void gtk_raise_frame_1(struct frame *f, int force)
{
	if (FRAME_VISIBLE_P(f) || force) {
		GdkWindow *emacs_window =
		    GET_GTK_WIDGET_WINDOW(FRAME_GTK_SHELL_WIDGET(f));

		gdk_window_raise(emacs_window);
	}
}

static void gtk_raise_frame(struct frame *f)
{
	gtk_raise_frame_1(f, 1);
}

/* Lower frame F.  */
static void gtk_lower_frame(struct frame *f)
{
	if (FRAME_VISIBLE_P(f)) {
		gdk_window_lower(GET_GTK_WIDGET_WINDOW
				 (FRAME_GTK_SHELL_WIDGET(f)));
	}
}

/* Change from withdrawn state to mapped state. */
static void gtk_make_frame_visible(struct frame *f)
{
	gtk_widget_map(FRAME_GTK_SHELL_WIDGET(f));
	gtk_raise_frame_1(f, 0);
}

/* Change from mapped state to withdrawn state. */
static void gtk_make_frame_invisible(struct frame *f)
{
	gtk_widget_unmap(FRAME_GTK_SHELL_WIDGET(f));
}

static int gtk_frame_visible_p(struct frame *f)
{
	GtkWidget *w = FRAME_GTK_SHELL_WIDGET(f);

	f->visible = (GTK_OBJECT_FLAGS(w) & GTK_VISIBLE);

	return f->visible;
}

static int gtk_frame_totally_visible_p(struct frame *f)
{
	return FRAME_GTK_TOTALLY_VISIBLE_P(f);
}

/* Change window state from mapped to iconified. */
static void gtk_iconify_frame(struct frame *f)
{
	GdkWindow *w = GET_GTK_WIDGET_WINDOW(FRAME_GTK_SHELL_WIDGET(f));

	/* There is no equivalent to XIconifyWindow in Gtk/Gdk. */
	if (!XIconifyWindow(GDK_WINDOW_XDISPLAY(w),
			    GDK_WINDOW_XWINDOW(w),
			    DefaultScreen(GDK_WINDOW_XDISPLAY(w))))
		gtk_cant_notify_wm_error();

	f->iconified = 1;
}

/* Sets the X focus to frame f. */
static void gtk_focus_on_frame(struct frame *f)
{
	GtkWidget *shell_widget;

	assert(FRAME_GTK_P(f));

	shell_widget = FRAME_GTK_SHELL_WIDGET(f);
	if (!GET_GTK_WIDGET_WINDOW(shell_widget))
		return;

	gtk_widget_grab_focus(shell_widget);
}

/* Destroy the window of frame S.  */
static void gtk_delete_frame(struct frame *f)
{
	GtkWidget *w = FRAME_GTK_SHELL_WIDGET(f);

	gtk_widget_destroy(w);

	if (FRAME_GTK_GEOM_FREE_ME_PLEASE(f))
		xfree(FRAME_GTK_GEOM_FREE_ME_PLEASE(f));
	xfree(f->frame_data);
	f->frame_data = 0;
}

static void gtk_recompute_cell_sizes(struct frame *frm)
{
	if (GTK_IS_WINDOW(FRAME_GTK_SHELL_WIDGET(frm))) {
		GtkWindow *w = GTK_WINDOW(FRAME_GTK_SHELL_WIDGET(frm));
		GdkGeometry geometry;
		GdkWindowHints geometry_mask;
		gint width_inc = 10;
		gint height_inc = 10;

		default_face_height_and_width(make_frame(frm), &height_inc,
					      &width_inc);
		geometry_mask = GDK_HINT_RESIZE_INC;
		geometry.width_inc = width_inc;
		geometry.height_inc = height_inc;

		gtk_window_set_geometry_hints(w, FRAME_GTK_TEXT_WIDGET(frm),
					      &geometry, geometry_mask);
	}
}

static void
gtk_update_frame_external_traits(struct frame *frm, Lisp_Object name)
{
	Lisp_Object frame = Qnil;

	XSETFRAME(frame, frm);

	if (EQ(name, Qforeground)) {
		Lisp_Object color = FACE_FOREGROUND(Vdefault_face, frame);
		GdkColor *fgc;

		if (!EQ(color, Vthe_null_color_instance)) {
			fgc = COLOR_INSTANCE_GTK_COLOR(XCOLOR_INSTANCE(color));
			/* #### BILL!!! The X code set the XtNforeground property of
			   the text widget here.  Why did they bother?  All that type
			   of thing is done down in the guts of the redisplay code,
			   not in the Emacs* widgets. */
		}
	} else if (EQ(name, Qbackground)) {
		Lisp_Object color = FACE_BACKGROUND(Vdefault_face, frame);
		GdkColor *bgc;

		if (!EQ(color, Vthe_null_color_instance)) {
			bgc = COLOR_INSTANCE_GTK_COLOR(XCOLOR_INSTANCE(color));
			if (FRAME_GTK_SHELL_WIDGET(frm)->window) {
				gdk_window_set_background(FRAME_GTK_SHELL_WIDGET
							  (frm)->window, bgc);
			}
			if (FRAME_GTK_TEXT_WIDGET(frm)->window) {
				gdk_window_set_background(FRAME_GTK_TEXT_WIDGET
							  (frm)->window, bgc);
			}
		}

		/* Really crappy way to force the modeline shadows to be
		   redrawn.  But effective. */
		MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(frm);
		MARK_FRAME_CHANGED(frm);
	} else if (EQ(name, Qfont)) {
		Lisp_Object font =
		    FACE_FONT(Vdefault_face, frame, Vcharset_ascii);

		if (!EQ(font, Vthe_null_font_instance)) {
			/* #### BILL!!! The X code set the XtNfont property of the
			   text widget here.  Why did they bother?  All that type of
			   thing is done down in the guts of the redisplay code, not
			   in the Emacs* widgets. */
		}
	} else
		abort();

#ifdef HAVE_TOOLBARS
	/* Setting the background clears the entire frame area
	   including the toolbar so we force an immediate redraw of
	   it. */
	if (EQ(name, Qbackground))
		MAYBE_DEVMETH(XDEVICE(frm->device), redraw_frame_toolbars,
			      (frm));
#endif				/* HAVE_TOOLBARS */

	/* Set window manager resize increment hints according to
	   the new character size */
	if (EQ(name, Qfont) && FRAME_GTK_TOP_LEVEL_FRAME_P(frm))
		gtk_recompute_cell_sizes(frm);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_frame_gtk(void)
{
	defsymbol(&Qwindow_id, "window-id");
	defsymbol(&Qtext_widget, "text-widget");
	defsymbol(&Qcontainer_widget, "container-widget");
	defsymbol(&Qshell_widget, "shell-widget");
	defsymbol(&Qdetachable_menubar, "detachable-menubar");

#ifdef HAVE_DRAGNDROP
	staticpro(&Vcurrent_drag_object);
	Vcurrent_drag_object = Qnil;
	DEFSUBR(Fgtk_start_drag_internal);
#endif
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
	DEFSUBR(Fgtk_window_id);
#endif
}

void console_type_create_frame_gtk(void)
{
	/* frame methods */
	CONSOLE_HAS_METHOD(gtk, init_frame_1);
	CONSOLE_HAS_METHOD(gtk, init_frame_2);
	CONSOLE_HAS_METHOD(gtk, init_frame_3);
	CONSOLE_HAS_METHOD(gtk, mark_frame);
	CONSOLE_HAS_METHOD(gtk, focus_on_frame);
	CONSOLE_HAS_METHOD(gtk, delete_frame);
	CONSOLE_HAS_METHOD(gtk, get_mouse_position);
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
	CONSOLE_HAS_METHOD(gtk, set_mouse_position);
#endif
	CONSOLE_HAS_METHOD(gtk, raise_frame);
	CONSOLE_HAS_METHOD(gtk, lower_frame);
	CONSOLE_HAS_METHOD(gtk, make_frame_visible);
	CONSOLE_HAS_METHOD(gtk, make_frame_invisible);
	CONSOLE_HAS_METHOD(gtk, iconify_frame);
	CONSOLE_HAS_METHOD(gtk, set_frame_size);
	CONSOLE_HAS_METHOD(gtk, set_frame_position);
	CONSOLE_HAS_METHOD(gtk, frame_property);
	CONSOLE_HAS_METHOD(gtk, internal_frame_property_p);
	CONSOLE_HAS_METHOD(gtk, frame_properties);
	CONSOLE_HAS_METHOD(gtk, set_frame_properties);
	CONSOLE_HAS_METHOD(gtk, set_title_from_bufbyte);
	CONSOLE_HAS_METHOD(gtk, set_icon_name_from_bufbyte);
	CONSOLE_HAS_METHOD(gtk, frame_visible_p);
	CONSOLE_HAS_METHOD(gtk, frame_totally_visible_p);
	CONSOLE_HAS_METHOD(gtk, frame_iconified_p);
	CONSOLE_HAS_METHOD(gtk, set_frame_pointer);
	CONSOLE_HAS_METHOD(gtk, set_frame_icon);
	CONSOLE_HAS_METHOD(gtk, get_frame_parent);
	CONSOLE_HAS_METHOD(gtk, update_frame_external_traits);
}

void vars_of_frame_gtk(void)
{
	DEFVAR_LISP("default-gtk-frame-plist", &Vdefault_gtk_frame_plist	/*
Plist of default frame-creation properties for Gtk frames.
These override what is specified in the resource database and in
`default-frame-plist', but are overridden by the arguments to the
particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

initially-unmapped               If non-nil, the frame will not be visible
when it is created.  In this case, you
need to call `make-frame-visible' to make
the frame appear.
popup                            If non-nil, it should be a frame, and this
frame will be created as a "popup" frame
whose parent is the given frame.  This
will make the window manager treat the
frame as a dialog box, which may entail
doing different things (e.g. not asking
for positioning, and not iconifying
separate from its parent).
inter-line-space         Not currently implemented.
toolbar-shadow-thickness Thickness of toolbar shadows.
background-toolbar-color Color of toolbar background.
bottom-toolbar-shadow-color      Color of bottom shadows on toolbars.
(*Not* specific to the bottom-toolbar.)
top-toolbar-shadow-color Color of top shadows on toolbars.
(*Not* specific to the top-toolbar.)
internal-border-width            Width of internal border around text area.
border-width                     Width of external border around text area.
top                              Y position (in pixels) of the upper-left
outermost corner of the frame (i.e. the
upper-left of the window-manager
decorations).
left                             X position (in pixels) of the upper-left
outermost corner of the frame (i.e. the
upper-left of the window-manager
decorations).
border-color                     Color of external border around text area.
cursor-color                     Color of text cursor.

See also `default-frame-plist', which specifies properties which apply
to all frames, not just Gtk frames.
										 */ );
	Vdefault_gtk_frame_plist = Qnil;

	gtk_console_methods->device_specific_frame_props =
	    &Vdefault_gtk_frame_plist;
}
