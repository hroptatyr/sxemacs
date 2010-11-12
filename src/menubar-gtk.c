/* Implements an elisp-programmable menubar -- Gtk interface.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

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

/* created 16-dec-91 by jwz */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "gui-gtk.h"

#include "buffer.h"
#include "commands.h"		/* zmacs_regions */
#include "ui-gtk.h"
#include "gui.h"
#include "events.h"
#include "frame.h"
#include "opaque.h"
#include "window.h"

#ifdef HAVE_GNOME
#include <libgnomeui/libgnomeui.h>
#endif

#define MENUBAR_TYPE	0
#define SUBMENU_TYPE	1
#define POPUP_TYPE	2

static GtkWidget *menu_descriptor_to_widget_1(Lisp_Object descr,
					      GtkAccelGroup * accel_group);

#define FRAME_MENUBAR_DATA(frame) ((frame)->menubar_data)
#define XFRAME_MENUBAR_DATA_LASTBUFF(frame) (XCAR ((frame)->menubar_data))
#define XFRAME_MENUBAR_DATA_UPTODATE(frame) (XCDR ((frame)->menubar_data))

/* This is a bogus subclass of GtkMenuBar so that the menu never tries
** to be bigger than the text widget.  This prevents weird resizing
** when jumping around between buffers with radically different menu
** sizes.
*/

#define GTK_XEMACS_MENUBAR(obj)		GTK_CHECK_CAST (obj, gtk_xemacs_menubar_get_type (), GtkXEmacsMenubar)
#define GTK_XEMACS_MENUBAR_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_xemacs_menubar_get_type (), GtkXEmacsMenubarClass)
#define GTK_IS_XEMACS_MENUBAR(obj)	GTK_CHECK_TYPE (obj, gtk_xemacs_menubar_get_type ())
#define GTK_XEMACS_MENUBAR_FRAME(obj)	GTK_XEMACS_MENUBAR (obj)->f

typedef struct _GtkXEmacsMenubar GtkXEmacsMenubar;
typedef struct _GtkXEmacsMenubarClass GtkXEmacsMenubarClass;

struct _GtkXEmacsMenubar {
	GtkMenuBar menu;
	struct frame *frame;
};

struct _GtkXEmacsMenubarClass {
	GtkMenuBarClass parent_class;
};

guint gtk_xemacs_menubar_get_type(void);
GtkWidget *gtk_xemacs_menubar_new(struct frame *f);

static void gtk_xemacs_menubar_class_init(GtkXEmacsMenubarClass * klass);
static void gtk_xemacs_menubar_init(GtkXEmacsMenubar * xemacs);
static void gtk_xemacs_menubar_size_request(GtkWidget * widget,
					    GtkRequisition * requisition);

guint gtk_xemacs_menubar_get_type(void)
{
	static guint xemacs_menubar_type;

	if (!xemacs_menubar_type) {
		static const GtkTypeInfo xemacs_menubar_info = {
			"GtkXEmacsMenubar",
			sizeof(GtkXEmacsMenubar),
			sizeof(GtkXEmacsMenubarClass),
			(GtkClassInitFunc) gtk_xemacs_menubar_class_init,
			(GtkObjectInitFunc) gtk_xemacs_menubar_init,
			/* reserved_1 */ NULL,
			/* reserved_2 */ NULL,
			(GtkClassInitFunc) NULL,
		};

		xemacs_menubar_type =
		    gtk_type_unique(gtk_menu_bar_get_type(),
				    &xemacs_menubar_info);
	}

	return xemacs_menubar_type;
}

static GtkWidgetClass *menubar_parent_class;

static void gtk_xemacs_menubar_class_init(GtkXEmacsMenubarClass * klass)
{
	GtkWidgetClass *widget_class;

	widget_class = (GtkWidgetClass *) klass;
	menubar_parent_class =
	    (GtkWidgetClass *) gtk_type_class(gtk_menu_bar_get_type());

	widget_class->size_request = gtk_xemacs_menubar_size_request;
}

static void gtk_xemacs_menubar_init(GtkXEmacsMenubar * xemacs)
{
}

static void gtk_xemacs_menubar_size_request(GtkWidget * widget,
					    GtkRequisition * requisition)
{
	GtkXEmacsMenubar *x = GTK_XEMACS_MENUBAR(widget);
	GtkRequisition frame_size;

	menubar_parent_class->size_request(widget, requisition);

	/* #### BILL!
	 ** We should really only do this if the menu has not been detached!
	 **
	 ** WMP 9/9/2000
	 */

	gtk_widget_size_request(FRAME_GTK_TEXT_WIDGET(x->frame), &frame_size);

	requisition->width = frame_size.width;
}

GtkWidget *gtk_xemacs_menubar_new(struct frame *f)
{
	GtkXEmacsMenubar *menubar = gtk_type_new(gtk_xemacs_menubar_get_type());

	menubar->frame = f;

	return (GTK_WIDGET(menubar));
}

/*
 * Label with XEmacs accelerator character support.
 *
 * The default interfaces to GtkAccelLabel does not understand XEmacs
 * keystroke printing conventions, nor is it convenient in the places where is
 * it needed.  This subclass provides an alternative interface more suited to
 * XEmacs needs but does not add new functionality.
 */
#define GTK_TYPE_XEMACS_ACCEL_LABEL	       (gtk_xemacs_accel_label_get_type ())
#define GTK_XEMACS_ACCEL_LABEL(obj)	       (GTK_CHECK_CAST ((obj), GTK_TYPE_ACCEL_LABEL, GtkXEmacsAccelLabel))
#define GTK_XEMACS_ACCEL_LABEL_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_ACCEL_LABEL, GtkXEmacsAccelLabelClass))
#define GTK_IS_XEMACS_ACCEL_LABEL(obj)	       (GTK_CHECK_TYPE ((obj), GTK_TYPE_XEMACS_ACCEL_LABEL))
#define GTK_IS_XEMACS_ACCEL_LABEL_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_XEMACS_ACCEL_LABEL))

typedef struct _GtkXEmacsAccelLabel GtkXEmacsAccelLabel;
typedef struct _GtkXEmacsAccelLabelClass GtkXEmacsAccelLabelClass;

/* Instance structure. No additional fields required. */
struct _GtkXEmacsAccelLabel {
	GtkAccelLabel label;
};

/* Class structure. No additional fields required. */
struct _GtkXEmacsAccelLabelClass {
	GtkAccelLabelClass parent_class;
};

static GtkType gtk_xemacs_accel_label_get_type(void);
static GtkWidget *gtk_xemacs_accel_label_new(const gchar * string);
static void gtk_xemacs_set_accel_keys(GtkXEmacsAccelLabel * l,
				      Lisp_Object keys);
static void gtk_xemacs_accel_label_class_init(GtkXEmacsAccelLabelClass * klass);
static void gtk_xemacs_accel_label_init(GtkXEmacsAccelLabel * xemacs);

static GtkType gtk_xemacs_accel_label_get_type(void)
{
	static GtkType xemacs_accel_label_type = 0;

	if (!xemacs_accel_label_type) {
		static const GtkTypeInfo xemacs_accel_label_info = {
			"GtkXEmacsAccelLabel",
			sizeof(GtkXEmacsAccelLabel),
			sizeof(GtkXEmacsAccelLabelClass),
			(GtkClassInitFunc) gtk_xemacs_accel_label_class_init,
			(GtkObjectInitFunc) gtk_xemacs_accel_label_init,
			/* reserved_1 */ NULL,
			/* reserved_2 */ NULL,
			(GtkClassInitFunc) NULL,
		};

		xemacs_accel_label_type =
		    gtk_type_unique(gtk_accel_label_get_type(),
				    &xemacs_accel_label_info);
	}

	return xemacs_accel_label_type;
}

static void gtk_xemacs_accel_label_class_init(GtkXEmacsAccelLabelClass * klass)
{
	/* Nothing to do. */
}

static void gtk_xemacs_accel_label_init(GtkXEmacsAccelLabel * xemacs)
{
	/* Nothing to do. */
}

static GtkWidget *gtk_xemacs_accel_label_new(const gchar * string)
{
	GtkXEmacsAccelLabel *xemacs_accel_label;

	xemacs_accel_label = gtk_type_new(GTK_TYPE_XEMACS_ACCEL_LABEL);

	if (string && *string)
		gtk_label_set_text(GTK_LABEL(xemacs_accel_label), string);

	return GTK_WIDGET(xemacs_accel_label);
}

/* Make the string <keys> the accelerator string for the label. */
static void gtk_xemacs_set_accel_keys(GtkXEmacsAccelLabel * l, Lisp_Object keys)
{
	g_return_if_fail(l != NULL);
	g_return_if_fail(GTK_IS_XEMACS_ACCEL_LABEL(l));

	/* Disable the standard way of finding the accelerator string for the
	   label. */
	gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(l), NULL);

	/* Set the string straight from the object. */
	if (STRINGP(keys) && XSTRING_LENGTH(keys)) {
		C_STRING_TO_EXTERNAL_MALLOC(XSTRING_DATA(keys),
					    l->label.accel_string, Qctext);
	} else {
		/* l->label.accel_string = NULL; */
	}
}

/* We now return you to your regularly scheduled menus... */

int dockable_menubar;

/* #define TEAR_OFF_MENUS */

#ifdef TEAR_OFF_MENUS
int tear_off_menus;
#endif

/* Converting from XEmacs to GTK representation */
static Lisp_Object menu_name_to_accelerator(char *name)
{
	while (*name) {
		if (*name == '%') {
			++name;
			if (!(*name))
				return Qnil;
			if (*name == '_' && *(name + 1)) {
				int accelerator =
				    (int)(unsigned char)(*(name + 1));
				return make_char(tolower(accelerator));
			}
		}
		++name;
	}
	return Qnil;
}

#define XEMACS_MENU_DESCR_TAG "xemacs::menu::description"
#define XEMACS_MENU_FILTER_TAG "xemacs::menu::filter"
#define XEMACS_MENU_GUIID_TAG "xemacs::menu::gui_id"
#define XEMACS_MENU_FIRSTTIME_TAG "xemacs::menu::first_time"

static void __activate_menu(GtkMenuItem *, gpointer);

#ifdef TEAR_OFF_MENUS
static void __torn_off_sir(GtkMenuItem * item, gpointer user_data)
{
	GtkWidget *menu_item = GTK_WIDGET(user_data);

	if (GTK_TEAROFF_MENU_ITEM(item)->torn_off) {
		/* Menu was just torn off */
		GUI_ID id = new_gui_id();
		Lisp_Object menu_desc = Qnil;
		GtkWidget *old_submenu = GTK_MENU_ITEM(menu_item)->submenu;

		VOID_TO_LISP(menu_desc,
			     gtk_object_get_data(GTK_OBJECT(menu_item),
						 XEMACS_MENU_DESCR_TAG));

		/* GCPRO all of our very own */
		gcpro_popup_callbacks(id, menu_desc);

		/* Hide the now detached menu from the attentions of
		   __activate_menu destroying the old submenu */
#if 0
		gtk_widget_ref(old_submenu);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item),
					  gtk_menu_new());
		gtk_widget_show_all(old_submenu);
#endif
	}
}
#endif

/* This is called when a menu is about to be shown... this is what
   does the delayed creation of the menu items.  We populate the
   submenu and away we go. */
static void __maybe_destroy(GtkWidget * child, GtkWidget * precious)
{
	if (GTK_IS_MENU_ITEM(child) && !GTK_IS_TEAROFF_MENU_ITEM(child)) {
		if (GTK_WIDGET_VISIBLE(child)) {
			/* If we delete the menu item that was 'active' when the
			   menu was cancelled, GTK gets upset because it tries to
			   remove the focus rectangle from a (now) dead widget.

			   This widget will eventually get killed because it will
			   not be visible the next time the window is shown.
			 */
			gtk_widget_set_sensitive(child, FALSE);
			gtk_widget_hide_all(child);
		} else {
			gtk_widget_destroy(child);
		}
	}
}

/* If user_data != 0x00 then we are using a hook to build the menu. */
static void __activate_menu(GtkMenuItem * item, gpointer user_data)
{
	Lisp_Object desc;
	gpointer force_clear =
	    gtk_object_get_data(GTK_OBJECT(item), XEMACS_MENU_FIRSTTIME_TAG);

	gtk_object_set_data(GTK_OBJECT(item), XEMACS_MENU_FIRSTTIME_TAG, 0x00);

	/* Delete the old contents of the menu if we are the top level menubar */
	if (GTK_IS_MENU_BAR(GTK_WIDGET(item)->parent) || force_clear) {
		GtkWidget *selected =
		    gtk_menu_get_active(GTK_MENU(item->submenu));

		gtk_container_foreach(GTK_CONTAINER(item->submenu),
				      (GtkCallback) __maybe_destroy, selected);
	} else if (gtk_container_children(GTK_CONTAINER(item->submenu))) {
		return;
	}

	VOID_TO_LISP(desc,
		     gtk_object_get_data(GTK_OBJECT(item),
					 XEMACS_MENU_DESCR_TAG));

#ifdef TEAR_OFF_MENUS
	/* Lets stick in a detacher just for giggles */
	if (tear_off_menus
	    && !gtk_container_children(GTK_CONTAINER(item->submenu))) {
		GtkWidget *w = gtk_tearoff_menu_item_new();
		gtk_widget_show(w);
		gtk_menu_append(GTK_MENU(item->submenu), w);
		gtk_signal_connect(GTK_OBJECT(w), "activate",
				   GTK_SIGNAL_FUNC(__torn_off_sir), item);
	}
#endif

	if (user_data) {
		GUI_ID id =
		    (GUI_ID) gtk_object_get_data(GTK_OBJECT(item),
						 XEMACS_MENU_GUIID_TAG);
		Lisp_Object hook_fn;
		struct gcpro gcpro1, gcpro2;

		VOID_TO_LISP(hook_fn,
			     gtk_object_get_data(GTK_OBJECT(item),
						 XEMACS_MENU_FILTER_TAG));

		GCPRO2(desc, hook_fn);

		desc = call1(hook_fn, desc);

		UNGCPRO;

		ungcpro_popup_callbacks(id);
		gcpro_popup_callbacks(id, desc);
	}

	/* Build the child widgets */
	for (; !NILP(desc); desc = Fcdr(desc)) {
		GtkWidget *next = NULL;
		Lisp_Object child = Fcar(desc);

		if (NILP(child)) {	/* the partition */
			/* Signal an error here?  The NILP handling is handled a
			   layer higher where appropriate */
		} else {
			next = menu_descriptor_to_widget_1(child,
							   gtk_menu_ensure_uline_accel_group
							   (GTK_MENU
							    (item->submenu)));
		}

		if (!next) {
			continue;
		}

		gtk_widget_show_all(next);
		gtk_menu_append(GTK_MENU(item->submenu), next);
	}
}

/* This is called whenever an item with a GUI_ID associated with it is
   destroyed.  This allows us to remove the references in gui-gtk.c
   that made sure callbacks and such were GCPRO-ed
*/
static void __remove_gcpro_by_id(gpointer user_data)
{
	ungcpro_popup_callbacks((GUI_ID) user_data);
}

static void __kill_stupid_gtk_timer(GtkObject * obj, gpointer user_data)
{
	GtkMenuItem *mi = GTK_MENU_ITEM(obj);

	if (mi->timer) {
		gtk_timeout_remove(mi->timer);
		mi->timer = 0;
	}
}

/* Convert the XEmacs menu accelerator representation to Gtk mnemonic form. If
  no accelerator has been provided, put one at the start of the string (this
  mirrors the behaviour under X).  This algorithm is also found in
  dialog-gtk.el:gtk-popup-convert-underscores.
*/
static char *convert_underscores(const char *name)
{
	char *rval;
	int i, j;
	int found_accel = FALSE;
	int underscores = 0;

	for (i = 0; name[i]; ++i)
		if (name[i] == '%' && name[i + 1] == '_') {
			found_accel = TRUE;
		} else if (name[i] == '_') {
			underscores++;
		}

	/* Allocate space for the original string, plus zero byte plus extra space
	   for all quoted underscores plus possible additional leading accelerator. */
	rval = xmalloc_and_zero(strlen(name) + 1 + underscores
				+ (found_accel ? 0 : 1));

	if (!found_accel)
		rval[0] = '_';

	for (i = 0, j = (found_accel ? 0 : 1); name[i]; i++) {
		if (name[i] == '%') {
			i++;
			if (!(name[i]))
				continue;

			if ((name[i] != '_') && (name[i] != '%'))
				i--;

			found_accel = TRUE;
		} else if (name[i] == '_') {
			rval[j++] = '_';
		}

		rval[j++] = name[i];
	}

	return rval;
}

/* Remove the XEmacs menu accellerator representation from a string. */
static char *remove_underscores(const char *name)
{
	char *rval = xmalloc_and_zero(strlen(name) + 1);
	int i, j;

	for (i = 0, j = 0; name[i]; i++) {
		if (name[i] == '%') {
			i++;
			if (!(name[i]))
				continue;

			if ((name[i] != '_') && (name[i] != '%'))
				i--;
			else
				continue;
		}
		rval[j++] = name[i];
	}
	return rval;
}

/* This converts an entire menu into a GtkMenuItem (with an attached
   submenu).  A menu is a list of (STRING [:keyword value]+ [DESCR]+)
   DESCR is either a list (meaning a submenu), a vector, or nil (if
   you include a :filter keyword) */
static GtkWidget *menu_convert(Lisp_Object desc, GtkWidget * reuse,
			       GtkAccelGroup * menubar_accel_group)
{
	GtkWidget *menu_item = NULL;
	GtkWidget *submenu = NULL;
	Lisp_Object key, val;
	Lisp_Object include_p = Qnil, hook_fn = Qnil, config_tag = Qnil;
	Lisp_Object active_p = Qt;
	Lisp_Object accel;
	int included_spec = 0;
	int active_spec = 0;

	if (STRINGP(XCAR(desc))) {
		accel = menu_name_to_accelerator(XSTRING_DATA(XCAR(desc)));

		if (!reuse) {
			char *temp_menu_name =
			    convert_underscores(XSTRING_DATA(XCAR(desc)));
			GtkWidget *accel_label =
			    gtk_xemacs_accel_label_new(NULL);
			guint accel_key;

			gtk_misc_set_alignment(GTK_MISC(accel_label), 0.0, 0.5);
			accel_key =
			    gtk_label_parse_uline(GTK_LABEL(accel_label),
						  temp_menu_name);

			menu_item = gtk_menu_item_new();
			gtk_container_add(GTK_CONTAINER(menu_item),
					  accel_label);
			gtk_widget_show(accel_label);

			if (menubar_accel_group)
				gtk_widget_add_accelerator(menu_item,
							   "activate_item",
							   menubar_accel_group,
							   accel_key,
							   GDK_MOD1_MASK,
							   GTK_ACCEL_LOCKED);
			free(temp_menu_name);
		} else {
			menu_item = reuse;
		}

		submenu = gtk_menu_new();
		gtk_widget_show(menu_item);
		gtk_widget_show(submenu);

		if (!reuse)
			gtk_signal_connect(GTK_OBJECT(menu_item), "destroy",
					   GTK_SIGNAL_FUNC
					   (__kill_stupid_gtk_timer), NULL);

		/* Without this sometimes a submenu gets left on the screen -
		 ** urk
		 */
		if (GTK_MENU_ITEM(menu_item)->submenu) {
			gtk_widget_destroy(GTK_MENU_ITEM(menu_item)->submenu);
		}

		gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), submenu);

		/* We put this bogus menu item in so that GTK does the right
		 ** thing when the menu is near the screen border.
		 **
		 ** Aug 29, 2000
		 */
		{
			GtkWidget *bogus_item =
			    gtk_menu_item_new_with_label
			    ("A suitably long label here...");

			gtk_object_set_data(GTK_OBJECT(menu_item),
					    XEMACS_MENU_FIRSTTIME_TAG,
					    (gpointer) 0x01);
			gtk_widget_show_all(bogus_item);
			gtk_menu_append(GTK_MENU(submenu), bogus_item);
		}

		desc = Fcdr(desc);

		while (key = Fcar(desc), KEYWORDP(key)) {
			Lisp_Object cascade = desc;
			desc = Fcdr(desc);
			if (NILP(desc))
				signal_simple_error
				    ("keyword in menu lacks a value", cascade);
			val = Fcar(desc);
			desc = Fcdr(desc);
			if (EQ(key, Q_included))
				include_p = val, included_spec = 1;
			else if (EQ(key, Q_config))
				config_tag = val;
			else if (EQ(key, Q_filter))
				hook_fn = val;
			else if (EQ(key, Q_active))
				active_p = val, active_spec = 1;
			else if (EQ(key, Q_accelerator)) {
#if 0
				if (SYMBOLP(val)
				    || CHARP(val))
					wv->accel = LISP_TO_VOID(val);
				else
					signal_simple_error
					    ("bad keyboard accelerator", val);
#endif
			} else if (EQ(key, Q_label)) {
				/* implement in 21.2 */
			} else
				signal_simple_error
				    ("unknown menu cascade keyword", cascade);
		}

		gtk_object_set_data(GTK_OBJECT(menu_item),
				    XEMACS_MENU_DESCR_TAG, LISP_TO_VOID(desc));
		gtk_object_set_data(GTK_OBJECT(menu_item),
				    XEMACS_MENU_FILTER_TAG,
				    LISP_TO_VOID(hook_fn));

		if ((!NILP(config_tag)
		     && NILP(Fmemq(config_tag, Vmenubar_configuration)))
		    || (included_spec && NILP(Feval(include_p)))) {
			return (NULL);
		}

		if (active_spec)
			active_p = Feval(active_p);

		gtk_widget_set_sensitive(GTK_WIDGET(menu_item),
					 !NILP(active_p));
	} else {
		signal_simple_error
		    ("menu name (first element) must be a string", desc);
	}

	/* If we are reusing a widget, we need to make sure we clean
	 ** everything up.
	 */
	if (reuse) {
		gpointer id =
		    gtk_object_get_data(GTK_OBJECT(reuse),
					XEMACS_MENU_GUIID_TAG);

		if (id) {
			/* If the menu item had a GUI_ID that means it was a filter menu */
			__remove_gcpro_by_id(id);
			gtk_signal_disconnect_by_func(GTK_OBJECT(reuse),
						      GTK_SIGNAL_FUNC
						      (__activate_menu),
						      (gpointer) 0x01);
		} else {
			gtk_signal_disconnect_by_func(GTK_OBJECT(reuse),
						      GTK_SIGNAL_FUNC
						      (__activate_menu), NULL);
		}

		GTK_MENU_ITEM(reuse)->right_justify = 0;
	}

	if (NILP(hook_fn)) {
		/* Generic menu builder */
		gtk_signal_connect(GTK_OBJECT(menu_item), "activate",
				   GTK_SIGNAL_FUNC(__activate_menu), NULL);
	} else {
		GUI_ID id = new_gui_id();

		gtk_object_set_data(GTK_OBJECT(menu_item),
				    XEMACS_MENU_GUIID_TAG, (gpointer) id);

		/* Make sure we gcpro the menu descriptions */
		gcpro_popup_callbacks(id, desc);
		gtk_object_weakref(GTK_OBJECT(menu_item), __remove_gcpro_by_id,
				   (gpointer) id);

		gtk_signal_connect(GTK_OBJECT(menu_item), "activate",
				   GTK_SIGNAL_FUNC(__activate_menu),
				   (gpointer) 0x01);
	}

	return (menu_item);
}

/* Called whenever a button, radio, or toggle is selected in the menu */
static void __generic_button_callback(GtkMenuItem * item, gpointer user_data)
{
	Lisp_Object callback, function, data, channel;

	XSETFRAME(channel, gtk_widget_to_frame(GTK_WIDGET(item)));

	VOID_TO_LISP(callback, user_data);

	get_gui_callback(callback, &function, &data);

	signal_special_gtk_user_event(channel, function, data);
}

/* Convert a single menu item descriptor to a suitable GtkMenuItem */
/* This function cannot GC.
   It is only called from menu_item_descriptor_to_widget_value, which
   prohibits GC. */
static GtkWidget *menu_descriptor_to_widget_1(Lisp_Object descr,
					      GtkAccelGroup * accel_group)
{
	if (STRINGP(descr)) {
		/* It is a separator.  Unfortunately GTK does not allow us to
		   specify what our separators look like, so we can't do all the
		   fancy stuff that the X code does.
		 */
		return (gtk_menu_item_new());
	} else if (LISTP(descr)) {
		/* It is a submenu */
		return (menu_convert(descr, NULL, accel_group));
	} else if (VECTORP(descr)) {
		/* An actual menu item description!  This gets yucky. */
		Lisp_Object name = Qnil;
		Lisp_Object callback = Qnil;
		Lisp_Object suffix = Qnil;
		Lisp_Object active_p = Qt;
		Lisp_Object include_p = Qt;
		Lisp_Object selected_p = Qnil;
		Lisp_Object keys = Qnil;
		Lisp_Object style = Qnil;
		Lisp_Object config_tag = Qnil;
		Lisp_Object accel = Qnil;
		GtkWidget *main_label = NULL;
		int length = XVECTOR_LENGTH(descr);
		Lisp_Object *contents = XVECTOR_DATA(descr);
		int plist_p;
		int selected_spec = 0, included_spec = 0;
		GtkWidget *widget = NULL;
		guint accel_key;

		if (length < 2)
			signal_simple_error
			    ("button descriptors must be at least 2 long",
			     descr);

		/* length 2:              [ "name" callback ]
		   length 3:              [ "name" callback active-p ]
		   length 4:              [ "name" callback active-p suffix ]
		   or                     [ "name" callback keyword  value  ]
		   length 5+:             [ "name" callback [ keyword value ]+ ]
		 */
		plist_p = (length >= 5
			   || (length > 2 && KEYWORDP(contents[2])));

		if (!plist_p && length > 2)
			/* the old way */
		{
			name = contents[0];
			callback = contents[1];
			active_p = contents[2];
			if (length == 4)
				suffix = contents[3];
		} else {
			/* the new way */
			int i;
			if (length & 1)
				signal_simple_error
				    ("button descriptor has an odd number of keywords and values",
				     descr);

			name = contents[0];
			callback = contents[1];
			for (i = 2; i < length;) {
				Lisp_Object key = contents[i++];
				Lisp_Object val = contents[i++];
				if (!KEYWORDP(key))
					signal_simple_error_2("not a keyword",
							      key, descr);

				if (EQ(key, Q_active))
					active_p = val;
				else if (EQ(key, Q_suffix))
					suffix = val;
				else if (EQ(key, Q_keys))
					keys = val;
				else if (EQ(key, Q_key_sequence)) ;	/* ignored for FSF compat */
				else if (EQ(key, Q_label)) ;	/* implement for 21.0 */
				else if (EQ(key, Q_style))
					style = val;
				else if (EQ(key, Q_selected))
					selected_p = val, selected_spec = 1;
				else if (EQ(key, Q_included))
					include_p = val, included_spec = 1;
				else if (EQ(key, Q_config))
					config_tag = val;
				else if (EQ(key, Q_accelerator)) {
					if (SYMBOLP(val) || CHARP(val))
						accel = val;
					else
						signal_simple_error
						    ("bad keyboard accelerator",
						     val);
				} else if (EQ(key, Q_filter))
					signal_simple_error
					    (":filter keyword not permitted on leaf nodes",
					     descr);
				else
					signal_simple_error_2
					    ("unknown menu item keyword", key,
					     descr);
			}
		}

#ifdef HAVE_MENUBARS
		if ((!NILP(config_tag)
		     && NILP(Fmemq(config_tag, Vmenubar_configuration)))
		    || (included_spec && NILP(Feval(include_p)))) {
			/* the include specification says to ignore this item. */
			return 0;
		}
#endif				/* HAVE_MENUBARS */

		CHECK_STRING(name);

		if (NILP(accel))
			accel = menu_name_to_accelerator(XSTRING_DATA(name));

		if (!NILP(suffix))
			suffix = Feval(suffix);

		if (!separator_string_p(XSTRING_DATA(name))) {
			char *label_buffer = NULL;
			char *temp_label = NULL;

			if (STRINGP(suffix) && XSTRING_LENGTH(suffix)) {
				label_buffer =
				    alloca(XSTRING_LENGTH(name) + 15 +
					   XSTRING_LENGTH(suffix));
				sprintf(label_buffer, "%s %s ",
					XSTRING_DATA(name),
					XSTRING_DATA(suffix));
			} else {
				label_buffer =
				    alloca(XSTRING_LENGTH(name) + 15);
				sprintf(label_buffer, "%s ",
					XSTRING_DATA(name));
			}

			temp_label = convert_underscores(label_buffer);
			main_label = gtk_xemacs_accel_label_new(NULL);
			accel_key =
			    gtk_label_parse_uline(GTK_LABEL(main_label),
						  temp_label);
			free(temp_label);
		}

		/* Evaluate the selected and active items now */
		if (selected_spec) {
			if (NILP(selected_p) || EQ(selected_p, Qt)) {
				/* Do nothing */
			} else {
				selected_p = Feval(selected_p);
			}
		}

		if (NILP(active_p) || EQ(active_p, Qt)) {
			/* Do Nothing */
		} else {
			active_p = Feval(active_p);
		}

		if (0 ||
#ifdef HAVE_MENUBARS
		    menubar_show_keybindings
#endif
		    ) {
			/* Need to get keybindings */
			if (!NILP(keys)) {
				/* User-specified string to generate key bindings with */
				CHECK_STRING(keys);

				keys = Fsubstitute_command_keys(keys);
			} else if (SYMBOLP(callback)) {
				char buf[1024];

				/* #### Warning, dependency here on current_buffer and point */
				where_is_to_char(callback, buf);

				keys = build_string(buf);
			}
		}

		/* Now we get down to the dirty business of creating the widgets */
		if (NILP(style) || EQ(style, Qtext) || EQ(style, Qbutton)) {
			/* A normal menu item */
			widget = gtk_menu_item_new();
		} else if (EQ(style, Qtoggle) || EQ(style, Qradio)) {
			/* They are radio or toggle buttons.

			   XEmacs' menu descriptions are fairly lame in that they do
			   not have the idea of a 'group' of radio buttons.  They
			   are exactly like toggle buttons except that they get
			   drawn differently.

			   GTK rips us a new one again.  If you have a radio button
			   in a group by itself, it always draws it as highlighted.
			   So we dummy up and create a second radio button that does
			   not get added to the menu, but gets invisibly set/unset
			   when the other gets unset/set.  *sigh*

			 */
			if (EQ(style, Qradio)) {
				GtkWidget *dummy_sibling = NULL;
				GSList *group = NULL;

				dummy_sibling = gtk_radio_menu_item_new(group);
				group =
				    gtk_radio_menu_item_group
				    (GTK_RADIO_MENU_ITEM(dummy_sibling));
				widget = gtk_radio_menu_item_new(group);

				/* We need to notice when the 'real' one gets destroyed
				   so we can clean up the dummy as well. */
				gtk_object_weakref(GTK_OBJECT(widget),
						   (GtkDestroyNotify)
						   gtk_widget_destroy,
						   dummy_sibling);
			} else {
				widget = gtk_check_menu_item_new();
			}

			/* What horrible defaults you have GTK dear!  The default
			   for a toggle menu item is to not show the toggle unless it
			   is turned on or actively highlighted.  How absolutely
			   hideous. */
			gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM
							    (widget), TRUE);
			gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM
						       (widget),
						       NILP(selected_p) ? FALSE
						       : TRUE);
		} else {
			signal_simple_error_2("unknown style", style, descr);
		}

		gtk_widget_set_sensitive(widget, !NILP(active_p));

		gtk_signal_connect(GTK_OBJECT(widget), "activate-item",
				   GTK_SIGNAL_FUNC(__generic_button_callback),
				   LISP_TO_VOID(callback));

		gtk_signal_connect(GTK_OBJECT(widget), "activate",
				   GTK_SIGNAL_FUNC(__generic_button_callback),
				   LISP_TO_VOID(callback));

		/* Now that all the information about the menu item is know, set the
		   remaining properties.
		 */

		if (main_label) {
			gtk_container_add(GTK_CONTAINER(widget), main_label);

			gtk_misc_set_alignment(GTK_MISC(main_label), 0.0, 0.5);
			gtk_xemacs_set_accel_keys(GTK_XEMACS_ACCEL_LABEL
						  (main_label), keys);

			if (accel_group)
				gtk_widget_add_accelerator(widget,
							   "activate_item",
							   accel_group,
							   accel_key, 0,
							   GTK_ACCEL_LOCKED);
		}

		return (widget);
	} else {
		return (NULL);
		/* abort (); ???? */
	}
}

static GtkWidget *menu_descriptor_to_widget(Lisp_Object descr,
					    GtkAccelGroup * accel_group)
{
	int count = specpdl_depth();
	GtkWidget *rval = NULL;

	record_unwind_protect(restore_gc_inhibit,
			      make_int(gc_currently_forbidden));

	gc_currently_forbidden = 1;

	/* Cannot GC from here on out... */
	rval = menu_descriptor_to_widget_1(descr, accel_group);
	unbind_to(count, Qnil);
	return (rval);

}

static gboolean menu_can_reuse_widget(GtkWidget * child, const char *label)
{
	/* Everything up at the top level was done using
	 ** gtk_xemacs_accel_label_new(), but we still double check to make
	 ** sure we don't seriously foobar ourselves.
	 */
	gpointer possible_child =
	    g_list_nth_data(gtk_container_children(GTK_CONTAINER(child)), 0);
	gboolean ret_val = FALSE;

	if (possible_child && GTK_IS_LABEL(possible_child)) {
		char *temp_label = remove_underscores(label);

		if (!strcmp(GTK_LABEL(possible_child)->label, temp_label))
			ret_val = TRUE;

		free(temp_label);
	}

	return ret_val;
}

/* Converts a menubar description into a GtkMenuBar... a menubar is a
   list of menus or buttons 
*/
static void menu_create_menubar(struct frame *f, Lisp_Object descr)
{
	gboolean right_justify = FALSE;
	Lisp_Object tail = Qnil;
	Lisp_Object value = descr;
	Lisp_Object item_descr = Qnil;
	GtkWidget *menubar = FRAME_GTK_MENUBAR_WIDGET(f);
	GUI_ID id =
	    (GUI_ID) gtk_object_get_data(GTK_OBJECT(menubar),
					 XEMACS_MENU_GUIID_TAG);
	guint menu_position = 0;
	GtkAccelGroup *menubar_accel_group;

	/* Remove any existing protection for old menu items */
	ungcpro_popup_callbacks(id);

	/* GCPRO the whole damn thing */
	gcpro_popup_callbacks(id, descr);

	menubar_accel_group = gtk_accel_group_new();

	EXTERNAL_LIST_LOOP(tail, value) {
		gpointer current_child =
		    g_list_nth_data(GTK_MENU_SHELL(menubar)->children,
				    menu_position);

		item_descr = XCAR(tail);

		if (NILP(item_descr)) {
			/* Need to start right-justifying menus */
			right_justify = TRUE;
			menu_position--;
		} else if (VECTORP(item_descr)) {
			/* It is a button description */
			GtkWidget *item;

			item =
			    menu_descriptor_to_widget(item_descr,
						      menubar_accel_group);
			gtk_widget_set_name(item, "XEmacsMenuButton");

			if (!item) {
				item =
				    gtk_menu_item_new_with_label
				    ("ITEM CREATION ERROR");
			}

			gtk_widget_show_all(item);
			if (current_child)
				gtk_widget_destroy(GTK_WIDGET(current_child));
			gtk_menu_bar_insert(GTK_MENU_BAR(menubar), item,
					    menu_position);
		} else if (LISTP(item_descr)) {
			/* Need to actually convert it into a menu and slap it in */
			GtkWidget *widget;
			gboolean reused_p = FALSE;

			/* We may be able to reuse the widget, let's at least check. */
			if (current_child
			    && menu_can_reuse_widget(GTK_WIDGET(current_child),
						     XSTRING_DATA(XCAR
								  (item_descr))))
			{
				widget =
				    menu_convert(item_descr,
						 GTK_WIDGET(current_child),
						 menubar_accel_group);
				reused_p = TRUE;
			} else {
				widget =
				    menu_convert(item_descr, NULL,
						 menubar_accel_group);
				if (current_child)
					gtk_widget_destroy(GTK_WIDGET
							   (current_child));
				gtk_menu_bar_insert(GTK_MENU_BAR(menubar),
						    widget, menu_position);
			}

			if (widget) {
				if (right_justify)
					gtk_menu_item_right_justify
					    (GTK_MENU_ITEM(widget));
			} else {
				widget = gtk_menu_item_new_with_label("ERROR");
				/* abort() */
			}
			gtk_widget_show_all(widget);
		} else if (STRINGP(item_descr)) {
			/* Do I really want to be this careful?  Anything else in a
			   menubar description is illegal */
		}
		menu_position++;
	}

	/* Need to delete any menu items that were past the bounds of the new one */
	{
		GList *l = NULL;

		while ((l =
			g_list_nth(GTK_MENU_SHELL(menubar)->children,
				   menu_position))) {
			gpointer data = l->data;
			g_list_remove_link(GTK_MENU_SHELL(menubar)->children,
					   l);

			if (data) {
				gtk_widget_destroy(GTK_WIDGET(data));
			}
		}
	}

	/* Attach the new accelerator group to the frame. */
	gtk_window_add_accel_group(GTK_WINDOW(FRAME_GTK_SHELL_WIDGET(f)),
				   menubar_accel_group);
}

/* Deal with getting/setting the menubar */
#ifndef GNOME_IS_APP
#define GNOME_IS_APP(x) 0
#define gnome_app_set_menus(x,y)
#endif

static gboolean
run_menubar_hook(GtkWidget * widget, GdkEventButton * event, gpointer user_data)
{
	if (!GTK_MENU_SHELL(widget)->active) {
		run_hook(Qactivate_menubar_hook);
	}
	return (FALSE);
}

static void create_menubar_widget(struct frame *f)
{
	GUI_ID id = new_gui_id();
	GtkWidget *handlebox = NULL;
	GtkWidget *menubar = gtk_xemacs_menubar_new(f);

	if (GNOME_IS_APP(FRAME_GTK_SHELL_WIDGET(f))) {
		gnome_app_set_menus(GNOME_APP(FRAME_GTK_SHELL_WIDGET(f)),
				    GTK_MENU_BAR(menubar));
	} else if (dockable_menubar) {
		handlebox = gtk_handle_box_new();
		gtk_handle_box_set_handle_position(GTK_HANDLE_BOX(handlebox),
						   GTK_POS_LEFT);
		gtk_container_add(GTK_CONTAINER(handlebox), menubar);
		gtk_box_pack_start(GTK_BOX(FRAME_GTK_CONTAINER_WIDGET(f)),
				   handlebox, FALSE, FALSE, 0);
	} else {
		gtk_box_pack_start(GTK_BOX(FRAME_GTK_CONTAINER_WIDGET(f)),
				   menubar, FALSE, FALSE, 0);
	}

	gtk_signal_connect(GTK_OBJECT(menubar), "button-press-event",
			   GTK_SIGNAL_FUNC(run_menubar_hook), NULL);

	FRAME_GTK_MENUBAR_WIDGET(f) = menubar;
	gtk_object_set_data(GTK_OBJECT(menubar), XEMACS_MENU_GUIID_TAG,
			    (gpointer) id);
	gtk_object_weakref(GTK_OBJECT(menubar), __remove_gcpro_by_id,
			   (gpointer) id);
}

static int set_frame_menubar(struct frame *f, int first_time_p)
{
	Lisp_Object menubar;
	int menubar_visible;
	/* As for the toolbar, the minibuffer does not have its own menubar. */
	struct window *w = XWINDOW(FRAME_LAST_NONMINIBUF_WINDOW(f));

	if (!FRAME_GTK_P(f))
		return 0;

  /***** first compute the contents of the menubar *****/

	if (!first_time_p) {
		/* evaluate `current-menubar' in the buffer of the selected window
		   of the frame in question. */
		menubar = symbol_value_in_buffer(Qcurrent_menubar, w->buffer);
	} else {
		/* That's a little tricky the first time since the frame isn't
		   fully initialized yet. */
		menubar = Fsymbol_value(Qcurrent_menubar);
	}

	if (NILP(menubar)) {
		menubar = Vblank_menubar;
		menubar_visible = 0;
	} else {
		menubar_visible = !NILP(w->menubar_visible_p);
	}

	if (!FRAME_GTK_MENUBAR_WIDGET(f)) {
		create_menubar_widget(f);
	}

	/* Populate the menubar, but nothing is shown yet */
	{
		Lisp_Object old_buffer;
		int count = specpdl_depth();

		old_buffer = Fcurrent_buffer();
		record_unwind_protect(Fset_buffer, old_buffer);
		Fset_buffer(XWINDOW(FRAME_SELECTED_WINDOW(f))->buffer);

		menu_create_menubar(f, menubar);

		Fset_buffer(old_buffer);
		unbind_to(count, Qnil);
	}

	FRAME_MENUBAR_DATA(f) =
	    Fcons(XWINDOW(FRAME_LAST_NONMINIBUF_WINDOW(f))->buffer, Qt);

	return (menubar_visible);
}

/* Called from gtk_create_widgets() to create the inital menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible. */
int gtk_initialize_frame_menubar(struct frame *f)
{
	create_menubar_widget(f);
	return set_frame_menubar(f, 1);
}

static void gtk_update_frame_menubar_internal(struct frame *f)
{
	/* We assume the menubar contents has changed if the global flag is set,
	   or if the current buffer has changed, or if the menubar has never
	   been updated before.
	 */
	int menubar_contents_changed =
	    (f->menubar_changed || NILP(FRAME_MENUBAR_DATA(f))
	     || (!EQ(XFRAME_MENUBAR_DATA_LASTBUFF(f),
		     XWINDOW(FRAME_LAST_NONMINIBUF_WINDOW(f))->buffer)));

	gboolean menubar_was_visible =
	    GTK_WIDGET_VISIBLE(FRAME_GTK_MENUBAR_WIDGET(f));
	gboolean menubar_will_be_visible = menubar_was_visible;
	gboolean menubar_visibility_changed;

	if (menubar_contents_changed) {
		menubar_will_be_visible = set_frame_menubar(f, 0);
	}

	menubar_visibility_changed =
	    menubar_was_visible != menubar_will_be_visible;

	if (!menubar_visibility_changed) {
		return;
	}

	/* We hide and show the menubar's parent (which is actually the
	   GtkHandleBox)... this is to simplify the code that destroys old
	   menu items, etc.  There is no easy way to get the child out of a
	   handle box, and I didn't want to add yet another stupid widget
	   slot to struct gtk_frame. */
	if (menubar_will_be_visible) {
		gtk_widget_show_all(FRAME_GTK_MENUBAR_WIDGET(f)->parent);
	} else {
		gtk_widget_hide_all(FRAME_GTK_MENUBAR_WIDGET(f)->parent);
	}

	MARK_FRAME_SIZE_SLIPPED(f);
}

static void gtk_update_frame_menubars(struct frame *f)
{
	GtkWidget *menubar = NULL;

	assert(FRAME_GTK_P(f));

	menubar = FRAME_GTK_MENUBAR_WIDGET(f);

	if ((GTK_MENU_SHELL(menubar)->active) ||
	    (GTK_MENU_SHELL(menubar)->have_grab) ||
	    (GTK_MENU_SHELL(menubar)->have_xgrab)) {
		return;
	}

	gtk_update_frame_menubar_internal(f);
}

static void gtk_free_frame_menubars(struct frame *f)
{
	GtkWidget *menubar_widget;

	assert(FRAME_GTK_P(f));

	menubar_widget = FRAME_GTK_MENUBAR_WIDGET(f);
	if (menubar_widget) {
		gtk_widget_destroy(menubar_widget);
	}
}

static void popdown_menu_cb(GtkMenuShell * menu, gpointer user_data)
{
	popup_up_p--;
}

static void gtk_popup_menu(Lisp_Object menu_desc, Lisp_Object event)
{
	struct Lisp_Event *eev = NULL;
	GtkWidget *widget = NULL;
	GtkWidget *menu = NULL;
	gpointer id = NULL;

	/* Do basic error checking first... */
	if (SYMBOLP(menu_desc))
		menu_desc = Fsymbol_value(menu_desc);
	CHECK_CONS(menu_desc);
	CHECK_STRING(XCAR(menu_desc));

	/* Now lets get down to business... */
	widget = menu_descriptor_to_widget(menu_desc, NULL);
	menu = GTK_MENU_ITEM(widget)->submenu;
	gtk_widget_set_name(widget, "XEmacsPopupMenu");
	id = gtk_object_get_data(GTK_OBJECT(widget), XEMACS_MENU_GUIID_TAG);

	__activate_menu(GTK_MENU_ITEM(widget), id);

	if (!NILP(event)) {
		CHECK_LIVE_EVENT(event);
		eev = XEVENT(event);

		if ((eev->event_type != button_press_event) &&
		    (eev->event_type != button_release_event))
			wrong_type_argument(Qmouse_event_p, event);
	} else if (!NILP(Vthis_command_keys)) {
		/* If an event wasn't passed, use the last event of the event
		   sequence currently being executed, if that event is a mouse
		   event. */
		eev = XEVENT(Vthis_command_keys);
		if ((eev->event_type != button_press_event) &&
		    (eev->event_type != button_release_event))
			eev = NULL;
	}

	gtk_widget_show(menu);

	popup_up_p++;
	gtk_signal_connect(GTK_OBJECT(menu), "deactivate",
			   GTK_SIGNAL_FUNC(popdown_menu_cb), NULL);

	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
		       eev ? eev->event.button.button : 0,
		       eev ? eev->timestamp : GDK_CURRENT_TIME);
}

DEFUN("gtk-build-xemacs-menu", Fgtk_build_xemacs_menu, 1, 1, 0,	/*
Returns a GTK menu item from MENU, a standard XEmacs menu description.
See the definition of `popup-menu' for more information on the format of MENU.
<<<<<<< HEAD
								 */
=======
*/
>>>>>>> origin/master
      (menu))
{
	GtkWidget *w = menu_descriptor_to_widget(menu, NULL);

	return (w ? build_gtk_object(GTK_OBJECT(w)) : Qnil);
}

void syms_of_menubar_gtk(void)
{
	DEFSUBR(Fgtk_build_xemacs_menu);
}

void console_type_create_menubar_gtk(void)
{
	CONSOLE_HAS_METHOD(gtk, update_frame_menubars);
	CONSOLE_HAS_METHOD(gtk, free_frame_menubars);
	CONSOLE_HAS_METHOD(gtk, popup_menu);
}

void reinit_vars_of_menubar_gtk(void)
{
	dockable_menubar = 1;
#ifdef TEAR_OFF_MENUS
	tear_off_menus = 1;
#endif
}

void vars_of_menubar_gtk(void)
{
	Fprovide(intern("gtk-menubars"));
	DEFVAR_BOOL("menubar-dockable-p", &dockable_menubar	/*
If non-nil, the frame menubar can be detached into its own top-level window.
								 */ );
#ifdef TEAR_OFF_MENUS
	DEFVAR_BOOL("menubar-tearable-p", &tear_off_menus	/*
If non-nil, menus can be torn off into their own top-level windows.
								 */ );
#endif
	reinit_vars_of_menubar_gtk();
}
